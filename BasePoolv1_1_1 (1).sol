// SPDX-License-Identifier: GPL-3.0

pragma solidity 0.8.17;

import "@openzeppelin/contracts/token/ERC20/extensions/IERC20Metadata.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import {IBasePool_v_1_1} from "./interfaces/IBasePool_v_1_1.sol";

abstract contract BasePool_v_1_1 is IBasePool_v_1_1 {
    using SafeERC20 for IERC20Metadata;
    uint256 constant MIN_LPING_PERIOD = 120; // in seconds
    uint256 constant BASE = 10 ** 18;
    uint256 constant MAX_FEE = 500 * 10 ** 14; // 5%, denominated in BASE
    uint256 minLiquidity; // denominated in loanCcy decimals
    address public poolCreator;
    address poolCreatorProposal;
    address collCcyToken;
    address loanCcyToken;
    uint128 totalLpShares; // LP shares are denominated and discretized in 1/1000th of minLiquidity
    uint256 loanTenor; // in seconds
    uint256 collTokenDecimals;
    uint256 maxLoanPerColl; // denominated in loanCcy decimals
    uint256 public creatorFee; // denominated in BASE
    uint256 totalLiquidity; // denominated in loanCcy decimals
    uint256 loanIdx;
    uint256 r1; // denominated in BASE and w.r.t. tenor (i.e., not annualized)
    uint256 r2; // denominated in BASE and w.r.t. tenor (i.e., not annualized)
    uint256 liquidityBnd1; // denominated in loanCcy decimals
    uint256 liquidityBnd2; // denominated in loanCcy decimals
    uint256 minLoan; // denominated in loanCcy decimals
    uint256 baseAggrBucketSize; // must be a multiple of 10
    mapping(address => LpInfo) addrToLpInfo;
    mapping(address => uint256) lastAddOfTxOrigin;
    mapping(uint256 => LoanInfo) public loanIdxToLoanInfo;
    mapping(uint256 => address) public loanIdxToBorrower;
    mapping(address => bool) public lpWhitelist;
    mapping(address => mapping(address => mapping(IBasePool_v_1_1.ApprovalTypes => bool)))
        public isApproved;
    mapping(uint256 => AggClaimsInfo) collAndRepayTotalBaseAgg1;
    mapping(uint256 => AggClaimsInfo) collAndRepayTotalBaseAgg2;
    mapping(uint256 => AggClaimsInfo) collAndRepayTotalBaseAgg3;

    constructor(address _loanCcyToken,address _collCcyToken,uint256 _loanTenor,uint256 _maxLoanPerColl,uint256 _r1,
        uint256 _r2,
        uint256 _liquidityBnd1,
        uint256 _liquidityBnd2,
        uint256 _minLoan,
        uint256 _baseAggrBucketSize,
        uint256 _creatorFee,
        uint256 _minLiquidity
    ) {
        if (_collCcyToken == _loanCcyToken) revert IdenticalLoanAndCollCcy();
        if (_loanCcyToken == address(0) || _collCcyToken == address(0))
            revert InvalidZeroAddress();
        if (_loanTenor < 60 * 60) revert InvalidLoanTenor();
        if (_maxLoanPerColl == 0) revert InvalidMaxLoanPerColl();
        if (_r1 <= _r2 || _r2 == 0) revert InvalidRateParams();
        if (_liquidityBnd2 <= _liquidityBnd1 || _liquidityBnd1 == 0)
            revert InvalidLiquidityBnds();
        // ensure LP shares can be minted based on 1/1000th of minLp discretization
        if (_minLiquidity < 1000) revert InvalidMinLiquidity();
        if (_baseAggrBucketSize < 10 || _baseAggrBucketSize % 10 != 0)
            revert InvalidBaseAggrSize();
        if (_creatorFee > MAX_FEE) revert InvalidFee();
        poolCreator = msg.sender;
        loanCcyToken = _loanCcyToken;
        collCcyToken = _collCcyToken;
        loanTenor = _loanTenor;
        maxLoanPerColl = _maxLoanPerColl;
        r1 = _r1;
        r2 = _r2;
        liquidityBnd1 = _liquidityBnd1;
        liquidityBnd2 = _liquidityBnd2;
        minLoan = _minLoan;
        loanIdx = 1;
        collTokenDecimals = IERC20Metadata(_collCcyToken).decimals();
        baseAggrBucketSize = _baseAggrBucketSize;
        creatorFee = _creatorFee;
        minLiquidity = _minLiquidity;
        emit NewSubPool(_loanCcyToken,_collCcyToken,_loanTenor,_maxLoanPerColl,_r1,_r2,_liquidityBnd1,_liquidityBnd2,_minLoan,_creatorFee
        );
    }

    function addLiquidity(
        address _onBehalfOf,
        uint128 _sendAmount,
        uint256 _deadline,
        uint256 _referralCode
    ) external override {
        // verify LP info and eligibility
        if (!(msg.sender == _onBehalfOf && lpWhitelist[msg.sender])) {
            revert UnapprovedSender();
        }
        checkTimestamp(_deadline);

        uint128 _inAmountAfterFees = _sendAmount -
            getLoanCcyTransferFee(_sendAmount);

        (
            uint256 dust,
            uint256 newLpShares,
            uint32 earliestRemove
        ) = _addLiquidity(_onBehalfOf, _inAmountAfterFees);

        // transfer liquidity
        IERC20Metadata(loanCcyToken).safeTransferFrom(
            msg.sender,
            address(this),
            _sendAmount
        );

        // transfer dust to creator if any
        if (dust > 0) {
            IERC20Metadata(loanCcyToken).safeTransfer(poolCreator, dust);
        }
     
    }

    // put in number of shares to remove, up to all of them
    function removeLiquidity(
        address _onBehalfOf,
        uint128 numShares
    ) external override {
        delete lastAddOfTxOrigin[_onBehalfOf];
        // verify LP info and eligibility
        checkSenderApproval(
            _onBehalfOf,
            IBasePool_v_1_1.ApprovalTypes.REMOVE_LIQUIDITY
        );

        LpInfo storage lpInfo = addrToLpInfo[_onBehalfOf];
        uint256 shareLength = lpInfo.sharesOverTime.length;
        if (
            shareLength * numShares == 0 ||
            lpInfo.sharesOverTime[shareLength - 1] < numShares
        ) revert InvalidRemove();
        if (block.timestamp < lpInfo.earliestRemove)
            revert BeforeEarliestRemove();
        uint256 _totalLiquidity = totalLiquidity;
        uint128 _totalLpShares = totalLpShares;
        // update state of pool
        uint256 liquidityRemoved = (numShares *
            (_totalLiquidity - minLiquidity)) / _totalLpShares;
        totalLpShares -= numShares;
        totalLiquidity = _totalLiquidity - liquidityRemoved;

        // update LP arrays and check for auto increment
        updateLpArrays(lpInfo, numShares, false);

        // transfer liquidity
        IERC20Metadata(loanCcyToken).safeTransfer(msg.sender, liquidityRemoved);
        // spawn event
        emit RemoveLiquidity(
            _onBehalfOf,
            liquidityRemoved,
            numShares,
            totalLiquidity,
            _totalLpShares - numShares,
            loanIdx
        );
    }

    function borrow(
        address _onBehalf,
        uint128 _sendAmount,
        uint128 _minLoanLimit,
        uint128 _maxRepayLimit,
        uint256 _deadline,
        uint256 _referralCode
    ) external override {
        uint256 _timestamp = checkTimestamp(_deadline);
        // check if atomic add and borrow as well as sanity check of onBehalf address
        if (
            lastAddOfTxOrigin[tx.origin] == _timestamp ||
            _onBehalf == address(0)
        ) revert Invalid();
        uint128 _inAmountAfterFees = _sendAmount -
            getCollCcyTransferFee(_sendAmount);
        // get borrow terms and do checks
        (
            uint128 loanAmount,
            uint128 repaymentAmount,
            uint128 pledgeAmount,
            uint32 expiry,
            uint256 _creatorFee,
            uint256 _totalLiquidity
        ) = _borrow(
                _inAmountAfterFees,
                _minLoanLimit,
                _maxRepayLimit,
                _timestamp
            );
        {
            // update pool state
            totalLiquidity = _totalLiquidity - loanAmount;

            uint256 _loanIdx = loanIdx;
            uint128 _totalLpShares = totalLpShares;

            // update loan info
            loanIdxToBorrower[_loanIdx] = _onBehalf;
            LoanInfo memory loanInfo;
            loanInfo.repayment = repaymentAmount;
            loanInfo.totalLpShares = _totalLpShares;
            loanInfo.expiry = expiry;
            loanInfo.collateral = pledgeAmount;
            loanIdxToLoanInfo[_loanIdx] = loanInfo;

            // update aggregations
            updateAggregations(
                _loanIdx,
                pledgeAmount,
                0,
                _totalLpShares,
                false
            );

            // update loan idx counter
            loanIdx = _loanIdx + 1;
        }
        {
            // transfer _sendAmount (not pledgeAmount) in collateral ccy
            IERC20Metadata(collCcyToken).safeTransferFrom(
                msg.sender,
                address(this),
                _sendAmount
            );

            // transfer creator fee to creator in collateral ccy
            IERC20Metadata(collCcyToken).safeTransfer(poolCreator, _creatorFee);

            // transfer loanAmount in loan ccy
            IERC20Metadata(loanCcyToken).safeTransfer(msg.sender, loanAmount);
        }
    
    }

    function repay(
        uint256 _loanIdx,
        address _recipient,
        uint128 _sendAmount
    ) external override {
        // verify loan info and eligibility
        if (_loanIdx == 0 || _loanIdx >= loanIdx) revert InvalidLoanIdx();
        address _loanOwner = loanIdxToBorrower[_loanIdx];

        if (!(_loanOwner == _recipient || msg.sender == _recipient))
            revert InvalidRecipient();
        checkSenderApproval(_loanOwner, IBasePool_v_1_1.ApprovalTypes.REPAY);
        LoanInfo storage loanInfo = loanIdxToLoanInfo[_loanIdx];
        uint256 timestamp = block.timestamp;
        if (timestamp > loanInfo.expiry) revert CannotRepayAfterExpiry();
        if (loanInfo.repaid) revert AlreadyRepaid();
        if (timestamp == loanInfo.expiry - loanTenor)
            revert CannotRepayInSameBlock();
        // update loan info
        loanInfo.repaid = true;
        uint128 _repayment = loanInfo.repayment;

        // transfer repayment amount
        uint128 repaymentAmountAfterFees = checkAndGetSendAmountAfterFees(
            _sendAmount,
            _repayment
        );
        // if repaymentAmountAfterFees was larger then update loan info
        // this ensures the extra repayment goes to the LPs
        if (repaymentAmountAfterFees != _repayment) {
            loanInfo.repayment = repaymentAmountAfterFees;
        }
        uint128 _collateral = loanInfo.collateral;
        uint128 _totalLpShares = loanInfo.totalLpShares;
        // update the aggregation mappings
        updateAggregations(
            _loanIdx,
            _collateral,
            repaymentAmountAfterFees,
            _totalLpShares,
            true
        );

        IERC20Metadata(loanCcyToken).safeTransferFrom(
            msg.sender,
            address(this),
            _sendAmount
        );
        // transfer collateral to _recipient (allows for possible
        // transfer directly to someone other than payer/sender)
        IERC20Metadata(collCcyToken).safeTransfer(_recipient, _collateral);
        // spawn event
        emit Repay(_loanOwner, _loanIdx, repaymentAmountAfterFees);
    }

    function claim(
        address _onBehalfOf,
        uint256[] calldata _loanIdxs,
        bool _isReinvested,
        uint256 _deadline
    ) external override {
        // check if reinvested is chosen that deadline is valid and sender can add liquidity on behalf of
        if (_isReinvested) {
            claimReinvestmentCheck(_deadline, _onBehalfOf);
        }
        checkSenderApproval(_onBehalfOf, IBasePool_v_1_1.ApprovalTypes.CLAIM);
        LpInfo storage lpInfo = addrToLpInfo[_onBehalfOf];

        // verify LP info and eligibility
        uint256 loanIdxsLen = _loanIdxs.length;
        // length of sharesOverTime array for LP
        uint256 sharesOverTimeLen = lpInfo.sharesOverTime.length;
        if (loanIdxsLen * sharesOverTimeLen == 0) revert NothingToClaim();
        if (_loanIdxs[0] == 0) revert InvalidLoanIdx();

        (
            uint256 sharesUnchangedUntilLoanIdx,
            uint256 applicableShares
        ) = claimsChecksAndSetters(
                _loanIdxs[0],
                _loanIdxs[loanIdxsLen - 1],
                lpInfo
            );

        // iterate over loans to get claimable amounts
        (uint256 repayments, uint256 collateral) = getClaimsFromList(
            _loanIdxs,
            loanIdxsLen,
            applicableShares
        );

        // update LP's from loan index to prevent double claiming and check share pointer
        checkSharePtrIncrement(
            lpInfo,
            _loanIdxs[loanIdxsLen - 1],
            lpInfo.currSharePtr,
            sharesUnchangedUntilLoanIdx
        );

        claimTransferAndReinvestment(
            _onBehalfOf,
            repayments,
            collateral,
            _isReinvested
        );

        // spawn event
        emit Claim(_onBehalfOf, _loanIdxs, repayments, collateral);
    }

    function overrideSharePointer(uint256 _newSharePointer) external {
        LpInfo storage lpInfo = addrToLpInfo[msg.sender];
        if (lpInfo.fromLoanIdx == 0) revert MustBeLp();
        // check that passed in pointer is greater than current share pointer
        // and less than length of LP's shares over time array
        if (
            _newSharePointer <= lpInfo.currSharePtr ||
            _newSharePointer + 1 > lpInfo.sharesOverTime.length
        ) revert InvalidNewSharePointer();
        lpInfo.currSharePtr = uint32(_newSharePointer);
        lpInfo.fromLoanIdx = uint32(
            lpInfo.loanIdxsWhereSharesChanged[_newSharePointer - 1]
        );
    }

    function claimFromAggregated(
        address _onBehalfOf,
        uint256[] calldata _aggIdxs,
        bool _isReinvested,
        uint256 _deadline
    ) external override {
        // check if reinvested is chosen that deadline is valid and sender can add liquidity on behalf of
        if (_isReinvested) {
            claimReinvestmentCheck(_deadline, _onBehalfOf);
        }
        checkSenderApproval(_onBehalfOf, IBasePool_v_1_1.ApprovalTypes.CLAIM);
        LpInfo storage lpInfo = addrToLpInfo[_onBehalfOf];

        // verify LP info and eligibility
        // length of loanIdxs array LP wants to claim
        uint256 lengthArr = _aggIdxs.length;
        // checks if length loanIds passed in is less than 2 (hence does not make even one valid claim interval)
        // OR if sharesOverTime array is empty.
        if (lpInfo.sharesOverTime.length == 0 || lengthArr < 2)
            revert NothingToClaim();

        (
            uint256 sharesUnchangedUntilLoanIdx,
            uint256 applicableShares
        ) = claimsChecksAndSetters(
                _aggIdxs[0],
                _aggIdxs[lengthArr - 1] - 1,
                lpInfo
            );

        // local variables to track repayments and collateral claimed
        uint256 totalRepayments;
        uint256 totalCollateral;

        // local variables for each iteration's repayments and collateral
        uint256 repayments;
        uint256 collateral;

        // iterate over the length of the passed in array
        for (uint256 counter = 0; counter < lengthArr - 1; ) {
            // make sure input loan indices are strictly increasing
            if (_aggIdxs[counter] >= _aggIdxs[counter + 1])
                revert NonAscendingLoanIdxs();

            // get aggregated claims
            (repayments, collateral) = getClaimsFromAggregated(
                _aggIdxs[counter],
                _aggIdxs[counter + 1],
                applicableShares
            );
            // update total repayment amount and total collateral amount
            totalRepayments += repayments;
            totalCollateral += collateral;

            unchecked {
                //increment local counter
                counter++;
            }
        }

        // update LP's from loan index to prevent double claiming and check share pointer
        checkSharePtrIncrement(
            lpInfo,
            _aggIdxs[lengthArr - 1] - 1,
            lpInfo.currSharePtr,
            sharesUnchangedUntilLoanIdx
        );

        claimTransferAndReinvestment(
            _onBehalfOf,
            totalRepayments,
            totalCollateral,
            _isReinvested
        );
       
    }

    function setApprovals(
        address _approvee,
        uint256 _packedApprovals
    ) external {
        if (msg.sender == _approvee || _approvee == address(0))
            revert InvalidApprovalAddress();
        _packedApprovals &= 0x1f;
        for (uint256 index = 0; index < 5; ) {
            bool approvalFlag = ((_packedApprovals >> index) & uint256(1)) == 1;
            if (
                isApproved[msg.sender][_approvee][
                    IBasePool_v_1_1.ApprovalTypes(index)
                ] != approvalFlag
            ) {
                isApproved[msg.sender][_approvee][
                    IBasePool_v_1_1.ApprovalTypes(index)
                ] = approvalFlag;
                _packedApprovals |= uint256(1) << 5;
            }
            unchecked {
                index++;
            }
        }
        if (((_packedApprovals >> 5) & uint256(1)) == 1) {
            emit ApprovalUpdate(msg.sender, _approvee, _packedApprovals & 0x1f);
        }
    }

    function proposeNewCreator(address newAddr) external {
        if (msg.sender != poolCreator) {
            revert UnapprovedSender();
        }
        poolCreatorProposal = newAddr;
    }

    function claimCreator() external {
        if (msg.sender != poolCreatorProposal) {
            revert UnapprovedSender();
        }
        address prevPoolCreator = poolCreator;
        lpWhitelist[prevPoolCreator] = false;
        lpWhitelist[msg.sender] = true;
        poolCreator = msg.sender;
        emit LpWhitelistUpdate(prevPoolCreator, false);
        emit LpWhitelistUpdate(msg.sender, true);
    }

    function toggleLpWhitelist(address newAddr) external {
        if (msg.sender != poolCreator) {
            revert UnapprovedSender();
        }
        bool newIsApproved = !lpWhitelist[newAddr];
        lpWhitelist[newAddr] = newIsApproved;
        emit LpWhitelistUpdate(newAddr, newIsApproved);
    }

    function getLpInfo(
        address _lpAddr
    )
        external
        view
        returns (
            uint32 fromLoanIdx,
            uint32 earliestRemove,
            uint32 currSharePtr,
            uint256[] memory sharesOverTime,
            uint256[] memory loanIdxsWhereSharesChanged
        )
    {
        LpInfo memory lpInfo = addrToLpInfo[_lpAddr];
        fromLoanIdx = lpInfo.fromLoanIdx;
        earliestRemove = lpInfo.earliestRemove;
        currSharePtr = lpInfo.currSharePtr;
        sharesOverTime = lpInfo.sharesOverTime;
        loanIdxsWhereSharesChanged = lpInfo.loanIdxsWhereSharesChanged;
    }

    function getRateParams()
        external
        view
        returns (
            uint256 _liquidityBnd1,
            uint256 _liquidityBnd2,
            uint256 _r1,
            uint256 _r2
        )
    {
        _liquidityBnd1 = liquidityBnd1;
        _liquidityBnd2 = liquidityBnd2;
        _r1 = r1;
        _r2 = r2;
    }

    function getPoolInfo()
        external
        view
        returns (
            address _loanCcyToken,
            address _collCcyToken,
            uint256 _maxLoanPerColl,
            uint256 _minLoan,
            uint256 _loanTenor,
            uint256 _totalLiquidity,
            uint256 _totalLpShares,
            uint256 _baseAggrBucketSize,
            uint256 _loanIdx
        )
    {
        _loanCcyToken = loanCcyToken;
        _collCcyToken = collCcyToken;
        _maxLoanPerColl = maxLoanPerColl;
        _minLoan = minLoan;
        _loanTenor = loanTenor;
        _totalLiquidity = totalLiquidity;
        _totalLpShares = totalLpShares;
        _baseAggrBucketSize = baseAggrBucketSize;
        _loanIdx = loanIdx;
    }

    function loanTerms(
        uint128 _inAmountAfterFees
    )
        public
        view
        returns (
            uint128 loanAmount,
            uint128 repaymentAmount,
            uint128 pledgeAmount,
            uint256 _creatorFee,
            uint256 _totalLiquidity
        )
    {
        // compute terms (as uint256)
        _creatorFee = (_inAmountAfterFees * creatorFee) / BASE;
        uint256 pledge = _inAmountAfterFees - _creatorFee;
        _totalLiquidity = totalLiquidity;
        if (_totalLiquidity <= minLiquidity) revert InsufficientLiquidity();
        uint256 loan = (pledge * maxLoanPerColl) / 10 ** collTokenDecimals;
        uint256 L_k = ((_totalLiquidity - minLiquidity) * BASE * 9) /
            (BASE * 10);
        if (loan > L_k) {
            uint256 x_k = (L_k * 10 ** collTokenDecimals) / maxLoanPerColl;
            loan =
                ((pledge - x_k) *
                    maxLoanPerColl *
                    (_totalLiquidity - minLiquidity - L_k)) /
                ((pledge - x_k) *
                    maxLoanPerColl +
                    (_totalLiquidity - minLiquidity - L_k) *
                    10 ** collTokenDecimals) +
                L_k;
        }

        if (loan < minLoan) revert LoanTooSmall();
        uint256 postLiquidity = _totalLiquidity - loan;
        assert(postLiquidity >= minLiquidity);
        // we use the average rate to calculate the repayment amount
        uint256 avgRate = (getRate(_totalLiquidity) + getRate(postLiquidity)) /
            2;
        // if pre- and post-borrow liquidity are within target liquidity range
        // then the repayment amount exactly matches the amount of integrating the
        // loan size over the infinitesimal rate; else the repayment amount is
        // larger than the amount of integrating loan size over rate;
        uint256 repayment = (loan * (BASE + avgRate)) / BASE;
        // return terms (as uint128)
        assert(uint128(loan) == loan);
        loanAmount = uint128(loan);
        assert(uint128(repayment) == repayment);
        repaymentAmount = uint128(repayment);
        assert(uint128(pledge) == pledge);
        pledgeAmount = uint128(pledge);
        if (repaymentAmount <= loanAmount) revert ErroneousLoanTerms();
    }

    function getClaimsFromAggregated(
        uint256 _fromLoanIdx,
        uint256 _toLoanIdx,
        uint256 _shares
    ) public view returns (uint256 repayments, uint256 collateral) {
        uint256 fromToDiff = _toLoanIdx - _fromLoanIdx;
        uint256 _baseAggrBucketSize = baseAggrBucketSize;
        // expiry check to make sure last loan in aggregation (one prior to _toLoanIdx for bucket) was taken out and expired
        uint32 expiryCheck = loanIdxToLoanInfo[_toLoanIdx - 1].expiry;
        if (expiryCheck == 0 || expiryCheck + 1 > block.timestamp) {
            revert InvalidSubAggregation();
        }
        AggClaimsInfo memory aggClaimsInfo;
        // find which bucket to which the current aggregation belongs and get aggClaimsInfo
        if (
            _toLoanIdx % _baseAggrBucketSize == 0 &&
            fromToDiff == _baseAggrBucketSize
        ) {
            aggClaimsInfo = collAndRepayTotalBaseAgg1[
                _fromLoanIdx / _baseAggrBucketSize + 1
            ];
        } else if (
            _toLoanIdx % (10 * _baseAggrBucketSize) == 0 &&
            fromToDiff == _baseAggrBucketSize * 10
        ) {
            aggClaimsInfo = collAndRepayTotalBaseAgg2[
                (_fromLoanIdx / (_baseAggrBucketSize * 10)) + 1
            ];
        } else if (
            _toLoanIdx % (100 * _baseAggrBucketSize) == 0 &&
            fromToDiff == _baseAggrBucketSize * 100
        ) {
            aggClaimsInfo = collAndRepayTotalBaseAgg3[
                (_fromLoanIdx / (_baseAggrBucketSize * 100)) + 1
            ];
        } else {
            revert InvalidSubAggregation();
        }

        // return repayment and collateral amounts
        repayments = (aggClaimsInfo.repayments * _shares) / BASE;
        collateral = (aggClaimsInfo.collateral * _shares) / BASE;
    }
    function updateAggregations(
        uint256 _loanIdx,
        uint128 _collateral,
        uint128 _repayment,
        uint128 _totalLpShares,
        bool _isRepay
    ) internal {
        uint256 _baseAggFirstIndex = _loanIdx / baseAggrBucketSize + 1;
        uint256 _baseAggSecondIndex = ((_baseAggFirstIndex - 1) / 10) + 1;
        uint256 _baseAggThirdIndex = ((_baseAggFirstIndex - 1) / 100) + 1;

        uint128 collateralUpdate = uint128(
            (_collateral * BASE) / _totalLpShares
        );
        uint128 repaymentUpdate = uint128((_repayment * BASE) / _totalLpShares);

        if (_isRepay) {
            collAndRepayTotalBaseAgg1[_baseAggFirstIndex]
                .collateral -= collateralUpdate;
            collAndRepayTotalBaseAgg2[_baseAggSecondIndex]
                .collateral -= collateralUpdate;
            collAndRepayTotalBaseAgg3[_baseAggThirdIndex]
                .collateral -= collateralUpdate;
            collAndRepayTotalBaseAgg1[_baseAggFirstIndex]
                .repayments += repaymentUpdate;
            collAndRepayTotalBaseAgg2[_baseAggSecondIndex]
                .repayments += repaymentUpdate;
            collAndRepayTotalBaseAgg3[_baseAggThirdIndex]
                .repayments += repaymentUpdate;
        } else {
            collAndRepayTotalBaseAgg1[_baseAggFirstIndex]
                .collateral += collateralUpdate;
            collAndRepayTotalBaseAgg2[_baseAggSecondIndex]
                .collateral += collateralUpdate;
            collAndRepayTotalBaseAgg3[_baseAggThirdIndex]
                .collateral += collateralUpdate;
        }
    }
    function checkSharePtrIncrement(
        LpInfo storage _lpInfo,
        uint256 _lastIdxFromUserInput,
        uint256 _currSharePtr,
        uint256 _sharesUnchangedUntilLoanIdx
    ) internal {
        // update LPs from loan index
        _lpInfo.fromLoanIdx = uint32(_lastIdxFromUserInput) + 1;
        if (
            _currSharePtr < _lpInfo.sharesOverTime.length - 1 &&
            _lastIdxFromUserInput + 1 == _sharesUnchangedUntilLoanIdx
        ) {
            unchecked {
                _lpInfo.currSharePtr++;
            }
        }
    }
    function claimsChecksAndSetters(
        uint256 _startIndex,
        uint256 _endIndex,
        LpInfo storage _lpInfo
    )
        internal
        returns (
            uint256 _sharesUnchangedUntilLoanIdx,
            uint256 _applicableShares
        )
    {
        uint256 currSharePtr = _lpInfo.currSharePtr;
        if (_lpInfo.sharesOverTime[currSharePtr] == 0) {
            // if share ptr at end of shares over time array, then LP still has 0 shares and should revert right away
            if (currSharePtr == _lpInfo.sharesOverTime.length - 1)
                revert ZeroShareClaim();
            _lpInfo.fromLoanIdx = uint32(
                _lpInfo.loanIdxsWhereSharesChanged[currSharePtr]
            );
            unchecked {
                currSharePtr = ++_lpInfo.currSharePtr;
            }
        }
        if (
            _startIndex < _lpInfo.fromLoanIdx &&
            !(_startIndex == 0 && _lpInfo.fromLoanIdx == 1)
        ) revert UnentitledFromLoanIdx();

        // infer applicable upper loan idx for which number of shares didn't change
        _sharesUnchangedUntilLoanIdx = currSharePtr ==
            _lpInfo.sharesOverTime.length - 1
            ? loanIdx
            : _lpInfo.loanIdxsWhereSharesChanged[currSharePtr];

        // check passed last loan idx is consistent with constant share interval
        if (_endIndex >= _sharesUnchangedUntilLoanIdx)
            revert LoanIdxsWithChangingShares();

        // get applicable number of shares for pro-rata calculations (given current share pointer position)
        _applicableShares = _lpInfo.sharesOverTime[currSharePtr];
    }
    function claimTransferAndReinvestment(
        address _onBehalfOf,
        uint256 _repayments,
        uint256 _collateral,
        bool _isReinvested
    ) internal {
        if (_repayments > 0) {
            if (_isReinvested) {
                // allows reinvestment and transfer of any dust from claim functions
                (
                    uint256 dust,
                    uint256 newLpShares,
                    uint32 earliestRemove
                ) = _addLiquidity(_onBehalfOf, _repayments);
                if (dust > 0) {
                    IERC20Metadata(loanCcyToken).safeTransfer(
                        poolCreator,
                        dust
                    );
                }
                // spawn event
                emit Reinvest(
                    _onBehalfOf,
                    _repayments,
                    newLpShares,
                    earliestRemove,
                    loanIdx
                );
            } else {
                IERC20Metadata(loanCcyToken).safeTransfer(
                    msg.sender,
                    _repayments
                );
            }
        }
        // transfer collateral
        if (_collateral > 0) {
            IERC20Metadata(collCcyToken).safeTransfer(msg.sender, _collateral);
        }
    }
    function _addLiquidity(address _onBehalfOf,uint256 _inAmountAfterFees
    )
        internal
        returns (uint256 dust, uint256 newLpShares, uint32 earliestRemove)
    {
        uint256 _totalLiquidity = totalLiquidity;
        if (_inAmountAfterFees < minLiquidity / 1000) revert InvalidAddAmount();
        // retrieve lpInfo of sender
        LpInfo storage lpInfo = addrToLpInfo[_onBehalfOf];

        // calculate new lp shares
        if (totalLpShares == 0) {
            dust = _totalLiquidity;
            _totalLiquidity = 0;
            newLpShares = (_inAmountAfterFees * 1000) / minLiquidity;
        } else {
            assert(_totalLiquidity > 0);
            newLpShares =
                (_inAmountAfterFees * totalLpShares) /
                _totalLiquidity;
        }
        if (newLpShares == 0 || uint128(newLpShares) != newLpShares)
            revert InvalidAddAmount();
        totalLpShares += uint128(newLpShares);
        totalLiquidity = _totalLiquidity + _inAmountAfterFees;
        // update LP info
        bool isFirstAddLiquidity = lpInfo.fromLoanIdx == 0;
        if (isFirstAddLiquidity) {
            lpInfo.fromLoanIdx = uint32(loanIdx);
            lpInfo.sharesOverTime.push(newLpShares);
        } else {
            // update both LP arrays and check for auto increment
            updateLpArrays(lpInfo, newLpShares, true);
        }
        earliestRemove = uint32(block.timestamp + MIN_LPING_PERIOD);
        lpInfo.earliestRemove = earliestRemove;
        // keep track of add timestamp per tx origin to check for atomic add and borrows/rollOvers
        lastAddOfTxOrigin[tx.origin] = block.timestamp;
    }
    function updateLpArrays(
        LpInfo storage _lpInfo,
        uint256 _newLpShares,
        bool _add
    ) internal {
        uint256 _loanIdx = loanIdx;
        uint256 _originalSharesLen = _lpInfo.sharesOverTime.length;
        uint256 _originalLoanIdxsLen = _originalSharesLen - 1;
        uint256 currShares = _lpInfo.sharesOverTime[_originalSharesLen - 1];
        uint256 newShares = _add
            ? currShares + _newLpShares
            : currShares - _newLpShares;
        bool loanCheck = (_originalLoanIdxsLen > 0 &&
            _lpInfo.loanIdxsWhereSharesChanged[_originalLoanIdxsLen - 1] ==
            _loanIdx);
        if (_lpInfo.fromLoanIdx == _loanIdx) {
            if (_originalSharesLen == 1 || loanCheck) {
                _lpInfo.sharesOverTime[_originalSharesLen - 1] = newShares;
            }
            else {
                pushLpArrays(_lpInfo, newShares, _loanIdx);
                unchecked {
                    _lpInfo.currSharePtr++;
                }
            }
        }
        else if (loanCheck) {
            if (_lpInfo.sharesOverTime[_originalSharesLen - 2] == newShares) {
                _lpInfo.sharesOverTime.pop();
                _lpInfo.loanIdxsWhereSharesChanged.pop();
            }
            else {
                _lpInfo.sharesOverTime[_originalSharesLen - 1] = newShares;
            }
        } else {
            pushLpArrays(_lpInfo, newShares, _loanIdx);
        }
    }
    function pushLpArrays(
        LpInfo storage _lpInfo,
        uint256 _newShares,
        uint256 _loanIdx
    ) internal {
        _lpInfo.sharesOverTime.push(_newShares);
        _lpInfo.loanIdxsWhereSharesChanged.push(_loanIdx);
    }
    function _borrow(
        uint128 _inAmountAfterFees,
        uint128 _minLoanLimit,
        uint128 _maxRepayLimit,
        uint256 _timestamp
    )
        internal
        view
        returns (uint128 loanAmount,uint128 repaymentAmount,uint128 pledgeAmount,uint32 expiry,uint256 _creatorFee,uint256 _totalLiquidity
        )
    {        (
            loanAmount,
            repaymentAmount,
            pledgeAmount,
            _creatorFee,
            _totalLiquidity
        ) = loanTerms(_inAmountAfterFees);
        assert(_inAmountAfterFees != 0); // if 0 must have failed in loanTerms(...)
        if (loanAmount < _minLoanLimit) revert LoanBelowLimit();
        if (repaymentAmount > _maxRepayLimit) revert RepaymentAboveLimit();
        expiry = uint32(_timestamp + loanTenor);
    }
    function checkTimestamp(
        uint256 _deadline
    ) internal view returns (uint256 timestamp) {
        timestamp = block.timestamp;
        if (timestamp > _deadline) revert PastDeadline();
    }
    function claimReinvestmentCheck(
        uint256 _deadline,
        address /*_onBehalfOf*/
    ) internal view {
        checkTimestamp(_deadline);
    }
    function checkSenderApproval(
        address _ownerOrBeneficiary,
        IBasePool_v_1_1.ApprovalTypes _approvalType
    ) internal view {
        if (
            !(_ownerOrBeneficiary == msg.sender ||
                isApproved[_ownerOrBeneficiary][msg.sender][_approvalType])
        ) revert UnapprovedSender();
    }
    function getClaimsFromList(
        uint256[] calldata _loanIdxs,
        uint256 arrayLen,
        uint256 _shares
    ) internal view returns (uint256 repayments, uint256 collateral) {
        // aggregate claims from list
        for (uint256 i = 0; i < arrayLen; ) {
            LoanInfo memory loanInfo = loanIdxToLoanInfo[_loanIdxs[i]];
            if (i > 0) {
                if (_loanIdxs[i] <= _loanIdxs[i - 1])
                    revert NonAscendingLoanIdxs();
            }
            if (loanInfo.repaid) {
                repayments +=
                    (loanInfo.repayment * BASE) /
                    loanInfo.totalLpShares;
            } else if (loanInfo.expiry < block.timestamp) {
                collateral +=
                    (loanInfo.collateral * BASE) /
                    loanInfo.totalLpShares;
            } else {
                revert CannotClaimWithUnsettledLoan();
            }
            unchecked {
                i++;
            }
        }
        // return claims
        repayments = (repayments * _shares) / BASE;
        collateral = (collateral * _shares) / BASE;
    }
    function getRate(uint256 _liquidity) internal view returns (uint256 rate) {
        if (_liquidity < liquidityBnd1) {
            rate = (r1 * liquidityBnd1) / _liquidity;
        } else if (_liquidity <= liquidityBnd2) {
            rate =
                r2 +
                ((r1 - r2) * (liquidityBnd2 - _liquidity)) /
                (liquidityBnd2 - liquidityBnd1);
        } else {
            rate = r2;
        }
    }
    function checkAndGetSendAmountAfterFees(
        uint128 _sendAmount,
        uint128 lowerBnd
    ) internal view returns (uint128 sendAmountAfterFees) {
        sendAmountAfterFees = _sendAmount - getLoanCcyTransferFee(_sendAmount);
        if (
            sendAmountAfterFees < lowerBnd ||
            sendAmountAfterFees > (101 * lowerBnd) / 100
        ) revert InvalidSendAmount();
        return sendAmountAfterFees;
    }}
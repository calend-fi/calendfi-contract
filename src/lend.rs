
/*定义数据结构：您需要定义借贷信息，如借入金额、利率、还款日期等。

实现借贷逻辑：实现借入方申请借款、出借方审批借款、借入方按时还款的逻辑。*/



{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Prelude
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.AddressDerivation as AD
import qualified Ledger
import Ledger (TxOut)
import Ledger.Val
import Ledger.Constraints (TxConstraints, WalletConstraints)
import Ledger.Address (Address)
import Ledger.Scripts (Script, Validator)
import Ledger.Typed.Scripts (ScriptHash)
import Ledger.Typed.TScript (TScript)

data LoanState = NotStarted | LoanOffered | LoanAccepted | LoanRepaid | LoanDefaulted

data Loan = Loan
  { loanAmount   :: W.Coin
  , loanInterest :: W.Coin
  , loanDueDate  :: W.Timestamp
  , loanState    :: LoanState
  }

instance Show Loan where
  show (Loan amount interest dueDate state) =
    "Loan(" ++ show amount ++ ", " ++ show interest ++ ", " ++ show dueDate ++ ", " ++ show state ++ ")"

instance Ledger.Scripts.HasValidator Loan where
  type ValidatorScript Loan = LoanValidator

instance TScript Loan where
  type Instance Loan = Loan
  type SigHash Loan = ScriptHash LoanValidator

type LoanValidator = Validator Loan

loanOffer :: Loan -> TxOut Loan
loanOffer loan = TxOut
  { address = loanOfferAddress
  , value = loanAmount loan
  , validator = Ledger.Scripts.wrapValidator loanValidator
  }

loanValidator :: LoanValidator
loanValidator = Ledger.Scripts.validator $ \sig loan -> do
  let signer = Ledger.Scripts.signer sig
  when (loanState loan /= NotStarted) $
    Ledger.Scripts.failWith "Loan offer can only be made in NotStarted state."
  Ledger.Scripts.withSignature signer $ do
    let loan' = loan { loanState = LoanOffered }
    return loan'

loanAccept :: Loan -> TxOut Loan ->
TScript Loan
loanAccept loan txOut = do
when (loanState loan /= LoanOffered) $
Ledger.Scripts.failWith "Loan can only be accepted in LoanOffered state."
let loan' = loan { loanState = LoanAccepted }
return loan'

loanRepay :: Loan -> W.Coin -> TxOut Loan
loanRepay loan repayAmount = do
when (loanState loan /= LoanAccepted) $
Ledger.Scripts.failWith "Loan can only be repaid in LoanAccepted state."
let remainingAmount = loanAmount loan + loanInterest loan - repayAmount
if remainingAmount > 0
then let loan' = loan { loanAmount = remainingAmount }
in return loan'
else let loan' = loan { loanAmount = 0, loanState = LoanRepaid }
in return loan'

loanDefault :: Loan -> TxOut Loan
loanDefault loan = do
when (loanState loan /= LoanAccepted) $
Ledger.Scripts.failWith "Loan can only be defaulted in LoanAccepted state."
let loan' = loan { loanState = LoanDefaulted }
return loan'


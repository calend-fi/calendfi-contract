testLoan :: Loan
testLoan = Loan
{ loanAmount = W.Coin 1000
, loanInterest = W.Coin 100
, loanDueDate = W.Timestamp 123456
, loanState = NotStarted
}

testRepayAmount :: W.Coin
testRepayAmount = W.Coin 500

testLoanAccepted :: TxOut Loan
testLoanAccepted = loanAccept testLoan loanOffer

testLoanRepaid :: TxOut Loan
testLoanRepaid = loanRepay testLoanAccepted testRepayAmount

testLoanDefaulted :: TxOut Loan
testLoanDefaulted = loanDefault testLoanAccepted
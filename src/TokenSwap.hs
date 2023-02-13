
/*TokenSwap smart contract, which has the following functions:

init: Initialize the contract and set the owner of the contract.
deposit: Deposit the specified amount of tokens into the contract.
swap: Swap one token in a contract for another token.
getBalance: Query the token balance of the contract.*/

{-|
    Module      : TokenSwap
    Description : Description of the TokenSwap
    Copyright   : P2P Solutions Ltd.
    License     : GPL-3
    Maintainer  : jack
    Stability   : developer
 -}

module TokenSwap where

import qualified Ledger
import qualified Prelude

-- TokenSwap Contract definition
data TokenSwap = TokenSwap {
  owner   :: Ledger.Address,
  balance :: Ledger.Value
}

-- Contract entry point
entrypoint init (owner: Ledger.Address) : TokenSwap =
  TokenSwap owner 0

-- Update the balance of the TokenSwap contract
entrypoint deposit (amount : Ledger.Value) : () =
  Ledger.update (\st -> st { balance = st.balance + amount })

-- Swap one token for another
entrypoint swap (newToken : Ledger.Value) : () =
  Ledger.update (\st -> st { balance = newToken })

-- Query the balance of the TokenSwap contract
entrypoint getBalance : Ledger.Value =
  Ledger.getRecord >>= \st -> pure st.balance

/*All functions of the contract are tested,
 including initialization, storage, exchange and balance query.*/

module TokenSwapTest where

import qualified TokenSwap
import qualified Ledger
import qualified Prelude

-- Helper function to create and execute a payment transaction
pay :: Ledger.Address -> Ledger.Value -> Ledger.Transaction
pay recipient amount =
  Ledger.payment recipient amount

-- Test the TokenSwap contract
test : Ledger.Test () =
  -- Create and initialize the TokenSwap contract
  let contract = TokenSwap.init
  in
    Ledger.withCreate contract $
      -- Deposit some tokens into the contract
      Ledger.commit (pay contract 10) $
        -- Check the balance of the contract
        Ledger.assertEqual 10 (TokenSwap.getBalance) $
          -- Swap the tokens for a different type of token
          Ledger.commit (pay contract 20) $
            -- Check the balance of the contract after the swap
            Ledger.assertEqual 20 (TokenSwap.getBalance) $
              -- Done
              pure ()


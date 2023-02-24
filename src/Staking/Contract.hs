-- This code including the Onchain && Offchain part
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ledger (Ada, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (ScriptContext, scriptContextTxInfo),
               valuePaidTo)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, Promise, collectFromScript, endpoint, logInfo, selectList,
                        submitTxConstraints, submitTxConstraintsSpending, type (.\/), utxosAt)
import PlutusTx qualified
import PlutusTx.Prelude (Bool, Semigroup ((<>)), ($), (&&), (-), (.), (>=))
import Prelude qualified as Haskell
import Schema (ToSchema)
import Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)
import Playground.Contract
import PlutusTx.Prelude
import Wallet.Emulator.Wallet
import qualified Ledger.Contexts as Validation
import qualified Ledger.Tx as Tx
import qualified Data.Map as Map

data IOSplitData =
    IOSplitData
        { recipient1 :: PaymentPubKeyHash -- ^ First recipient
        , recipient2 :: PaymentPubKeyHash -- ^ Send recipient
        , amount     :: Ada -- ^ How much Ada we want to lock
        }
    deriving stock (Haskell.Show, Generic)

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
PlutusTx.unstableMakeIsData ''IOSplitData
PlutusTx.makeLift ''IOSplitData

-- To do: add validation logic
validateOutput :: IOSplitData -> () -> ScriptContext -> Bool
validateOutput IOSplitData{recipient1, recipient2, amount} _ ScriptContext{scriptContextTxInfo} = True


data IOSplit
instance Scripts.ValidatorTypes IOSplit where
    type instance RedeemerType IOSplit = ()
    type instance DatumType IOSplit = IOSplitData


outputValidator :: Scripts.TypedValidator IOSplit
outputValidator = Scripts.mkTypedValidator @IOSplit
    $$(PlutusTx.compile [|| validateOutput ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @IOSplitData @()


data LockArgs =
        LockArgs
            { recipient1Wallet :: Integer
            , recipient2Wallet :: Integer
            , totalAda         :: Ada
            }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type IOSplitSchema =
        Endpoint "stake" LockArgs
        .\/ Endpoint "withdraw" LockArgs


--endpoint for Stake
stake :: Promise () IOSplitSchema T.Text ()
stake = endpoint @"stake" (lockFunds . mkIOSplitData)

--endpoint for Withdraw
withdraw :: Promise () IOSplitSchema T.Text ()
withdraw = endpoint @"withdraw" (withdrawFunds . mkIOSplitData)

--transfer input value to a mockWallet KeyHash
mkIOSplitData :: LockArgs -> IOSplitData
mkIOSplitData LockArgs{recipient1Wallet, recipient2Wallet, totalAda} =
    IOSplitData
        { recipient1 = mockWalletPaymentPubKeyHash $ knownWallet recipient1Wallet
        , recipient2 = mockWalletPaymentPubKeyHash $ knownWallet recipient2Wallet
        , amount = totalAda
        }



-- generate transaction for staking
lockFunds :: IOSplitData -> Contract () IOSplitSchema T.Text ()
lockFunds s@IOSplitData{amount} = do
    logInfo $ "Staking " <> Haskell.show amount
    let tx = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints outputValidator tx


-- generate transaction for withdraw
withdrawFunds :: IOSplitData -> Contract () IOSplitSchema T.Text ()
withdrawFunds IOSplitData{recipient1, recipient2, amount} = do
    let contractAddress = Scripts.validatorAddress outputValidator
    utxos <- utxosAt contractAddress
    let half    = Ada.divide amount 2
        --value   = foldMap (Validation.txOutValue . Tx.txOutTxOut . snd) (Map.toList utxos) -- Get value in script
        tx      =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient1 (Ada.toValue amount)
            <> Constraints.mustPayToPubKey recipient2 (Ada.toValue $ amount - half)
    void $ submitTxConstraintsSpending outputValidator utxos tx

endpoints :: Contract () IOSplitSchema T.Text ()
endpoints = selectList [stake, withdraw]

mkSchemaDefinitions ''IOSplitSchema

$(mkKnownCurrencies [])


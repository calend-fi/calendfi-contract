-- Import the necessary libraries
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


{-|
Module      : StakingPool
Description : Main module of staking pool logic V1
Maintainer  : Contact@calend.io
-}


module StakingPool where

import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               (Address, PubKeyHash, Validator, ValidatorHash)
import           Ledger.Value         as Value
import           Ledger.Contexts      as Validation
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (ToSchema)
import           Playground.Contract


-- Name of Token
tokenName :: TokenName
tokenName = TokenName "MYTOKEN"

-- Name of LP Token
lpTokenName :: TokenName
lpTokenName = TokenName "LPTOKEN"

-- Type of Staking Token
lpTokenType :: KnownCurrency
lpTokenType = KnownCurrency (ValidatorHash "lpToken-validator-hash") lpTokenName

-- Address of Staking Pool
lpAddress :: Address
lpAddress = undefined

-- State of Staking Pool
data LPStakingDatum = LPStakingDatum
  { owner :: !PubKeyHash 
  , amount :: !Integer   
  } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LPStakingDatum

-- Implementaion of Validatator
validateLPStaking :: LPStakingDatum -> () -> ValidatorCtx -> Bool
validateLPStaking dat () ctx =
  let info = scriptContextTxInfo ctx
      -- varify the type of the input and output tokens
      inputLPToken = assetClassValueOf (Validation.ownCurrencySymbol ctx) lpTokenName
      outputLPToken = assetClassValueOf (Validation.ownCurrencySymbol ctx) lpTokenName
      inVal = Validation.valueLockedBy info (Validation.ownHash ctx)
      outVal = Validation.valueLockedBy info lpAddress
      isValidLPToken = inputLPToken == outputLPToken
      isValidValue = outVal `geq` inVal
      -- verify owner
      isSigned = txSignedBy info (owner dat)
  in isValidLPToken && isValidValue && isSigned


lpStakingValidator :: Validator
lpStakingValidator = Scripts.validatorScript $$(PlutusTx.compile [|| validateLPStaking ||])

-- Datatype if the assets
data LPStaking
instance Scripts.ScriptType LPStaking where
    type instance DatumType LPStaking = LPStakingDatum
    type instance RedeemerType LPStaking = ()

lpStakingInstance :: Scripts.ScriptInstance LPStaking
lpStakingInstance = Scripts.validator @LPStaking lpStakingValidator

lpStakingAddress :: Address
lpStakingAddress = Scripts.scriptAddress lpStakingInstance


lpStaking :: Integer -> Contract () LPStakingError (Maybe TxId)
lpStaking amount = do

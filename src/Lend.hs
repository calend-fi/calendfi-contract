import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Schema               (ToSchema)
import           Text.Printf          (printf)

-----------Data structure of Datum and Redeemer-----------

-- | Pool action
data PoolAction = MKDeposit | MKWithdraw | MKBorrow | MKReturn
    deriving Show

PlutusTx.unstableMakeIsData ''PoolAction
PlutusTx.makeLift ''PoolAction

-- | Datum of a lend pool
data PoolDataum = PoolDataum 
    { supplier    :: !PubKeyHash
    , borrower    :: !(Maybe PubKeyHash)
    -- Token can be borrowed
    , loanableTokenSymbol  :: !CurrencySymbol
    , loanableTokenName    :: !TokenName
    -- Token collateralized
    , collateralTokenSymbol  :: !CurrencySymbol
    , collateralTokenName    :: !TokenName
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''PoolDataum
PlutusTx.makeLift ''PoolDataum

-- | Pool Data
data PoolData
instance Scripts.ValidatorTypes PoolData where
    type instance RedeemerType PoolData = PoolAction
    type instance DatumType PoolData = PoolDataum

-----------Validator(On Chain)-----------

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolDataum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator pooldata redeemer ctx =
    case redeemer of
        MKDeposit   ->
            traceIfFalse "To Do" False
        MKWithdraw  ->
            traceIfFalse "supplier only" supplierOnly
        MKBorrow    ->
            traceIfFalse "To Do" False
        MKReturn    ->
            traceIfFalse "To Do" False

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check whether the transaction is signed by supplier.
    supplierOnly :: Bool
    supplierOnly = txSignedBy info $ supplier pooldata

poolTypedValidator :: Scripts.TypedValidator PoolData
poolTypedValidator = Scripts.mkTypedValidator @PoolData
    $$(PlutusTx.compile [|| mkPoolValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

poolValidator :: Validator
poolValidator = Scripts.validatorScript poolTypedValidator

poolAddress :: Ledger.ValidatorHash
poolAddress = Scripts.validatorHash poolValidator

-----------Endpoints(Off Chain)-----------

-- | Endpoint of Create lend pool
data CreateParams = CreateParams{
    -- Token can be borrowed
      pLoanableTokenSymbol  :: !CurrencySymbol
    , pLoanableTokenName    :: !TokenName
    -- Token collateralized
    , pCollateralTokenSymbol  :: !CurrencySymbol
    , pCollateralTokenName    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

create :: AsContractError e => Promise () PoolSchema e ()
create = endpoint @"create" @CreateParams $ \(CreateParams{..}) -> do
    logInfo @String $ printf "Pool created with param %s" (show pLoanableTokenSymbol)
    pkh <- pubKeyHash <$> ownPubKey
    let poolDataum = PoolDataum
                { supplier = pkh
                , borrower = Nothing
                , loanableTokenSymbol = pLoanableTokenSymbol
                , loanableTokenName = pLoanableTokenName
                , collateralTokenSymbol = pCollateralTokenSymbol
                , collateralTokenName = pCollateralTokenName
                }
        value = Value.singleton pLoanableTokenSymbol pLoanableTokenName 1000
        tx = mustPayToTheScript poolDataum value
    void (submitTxConstraints poolTypedValidator tx)
    awaitPromise withdraw

-- | Endpoint of Withdraw (TODO)
data WithdrawParams = WithdrawParams{
          amount  :: Integer
        , wTokenSymbol  :: !CurrencySymbol
        , wTokenName    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

withdraw :: AsContractError e => Promise () PoolSchema e ()
withdraw = endpoint @"withdraw" @WithdrawParams $ \(WithdrawParams{..})  -> do
    logInfo @String $ printf "Try to withdraw %s" (show amount)
    let addr = Scripts.validatorAddress poolTypedValidator
    -- withdraw value
    let value = Value.singleton wTokenSymbol wTokenName amount
    -- find utxos have enough value
    utxos <- fundsAtAddressGeq addr value
    let wredeemer = MKWithdraw
    -- let wredeemer = Redeemer $ PlutusTx.toData $ PlutusTx.dataToBuiltinData MKWithdraw
    let    tx       = collectFromScript utxos wredeemer
    void (submitTxConstraintsSpending poolTypedValidator utxos tx)

-----------Config schema,endpoints and currencies-----------

-- | endpoints
poolEndpoints :: AsContractError e => Contract () PoolSchema e ()
poolEndpoints = do
    logInfo @String "Waiting for pool endpoint..."
    logInfo @String $ printf "Ada name %s, Ada symbol %s" (show Ada.adaToken) (show Ada.adaSymbol)
    selectList [create, withdraw]

endpoints :: AsContractError e => Contract () PoolSchema e ()
endpoints = poolEndpoints

-- | Pool schema
type PoolSchema = 
        Endpoint "create" CreateParams
        .\/ Endpoint "withdraw" WithdrawParams
mkSchemaDefinitions ''PoolSchema

-- | currencies
usdToken :: KnownCurrency
usdToken = KnownCurrency (ValidatorHash "f") "USD" (TokenName "USD" :| [])

mkKnownCurrencies ['usdToken]

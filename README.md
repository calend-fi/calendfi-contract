# calendfi-contract

# calend staking compile && delpoy，useful in interact with frontend.

It may be somewhat useful as an addition to the code base to highlight some of the more important bits of code, and also to describe a typical work flow to build a dapp.

1. [Write a Plutus Script (Plutus is a subset of Haskell)](#1-write-a-plutus-script)
2. [Compile the Plutus Script into bytecode](#2-compile-the-plutus-script-into-bytecode)
3. [Hash the bytecode into an address](#3-hash-the-bytecode-into-an-address)
4. [Send some ADA/Tokens to this address with a corresponding Datum](#4-send-some-adatokens-to-this-address-with-a-corresponding-datum)
5. [Redeem the ADA/Tokens based on some logic, with a corresponding redeemer](#5-redeem-the-adatokens-based-on-some-logic-with-a-corresponding-redeemer)

## 1. Write a Plutus Script

All the script code can be found in the `swap` folder

[Script Code](https://github.com/twwu123/example-nft-marketplace/blob/764f29c4c9147b23abea20141cfe1636ebb516d0/swap/src/Swap.hs#L76)

```haskell
data Offer = Offer 
    { price           :: !Integer
    , seller          :: !PaymentPubKeyHash
    } deriving (Show, Generic, ToJSON, FromJSON)
PlutusTx.makeIsDataIndexed ''Offer [('Offer, 0)]

data Action
    = Buy   -- buyer purchases the offer --
    | Cancel -- seller withdraws the offer --
    deriving (Show)
PlutusTx.makeIsDataIndexed ''Action [('Buy, 0), ('Cancel, 1)]

{-# INLINABLE script' #-}
script' :: Offer -> Action -> ScriptContext -> Bool
script' Offer{price, seller} act ScriptContext{scriptContextTxInfo} = 
    case act of 
        Buy -> sellerPaid' && input
        Cancel -> sellerSigned'
    where
        input =
            let 
                isScriptInput i = case (txOutDatum . txInInfoResolved) i of
                    NoOutputDatum -> Haskell.False
                    _  -> Haskell.True
                xs = [i | i <- txInfoInputs scriptContextTxInfo, isScriptInput i]
            in
                case xs of
                    [i] -> Haskell.True
                    _   -> traceIfFalse "expected 1 script input" Haskell.False

        sellerPaid' = traceIfFalse "seller not paid" sellerPaid
        sellerPaid =
            let val = valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash seller)
            in val `geq` lovelaceValueOf price

        sellerSigned' = traceIfFalse "only seller may cancel" sellerSigned
        sellerSigned = txSignedBy scriptContextTxInfo (unPaymentPubKeyHash seller)

validator :: Validator
validator = mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = mkUntypedValidator script'
```

Let's take a look at our swap script. This is an extremely simple script, but has a lot of relatively common features to pay attention to. A significant amount of it is simply boilerplate that is done the same way in every script, so you may simply copy the form and focus on the actual Haskell code.

First we define our types for the Datum and Redeemer, both of which has to be converted to the `IsData` type through the `PlutusTx` API `makeIsDataIndexed`.

You will then notice a line

```haskell
{-# INLINABLE script' #-}
```

This is important due to how Plutus Code is compiled to Plutus Core bytecode, it makes use of something called Template Haskell which I don't fully understand, but essentially all code that will be used by and compiled into the validator must include this `INLINABLE` pragma.

As we can see

```haskell
validator :: Validator
validator = mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = mkUntypedValidator script'
```

`script'` is compiled as the validator and therefore we need to add the `INLINABLE` pragma before it. If we had other functions `script'` also used, we would need to add the `INLINABLE` pragma to all functions used by `script'` also.

A very common pattern for Cardano contracts is to have separate validators based on the type of the redeemer. This means that a user can declare their action by inputting the corresponding redeemer.

```haskell
script' :: Offer -> Action -> ScriptContext -> Bool
script' Offer{price, seller} act ScriptContext{scriptContextTxInfo} = 
case act of 
        Buy -> sellerPaid' && input
        Cancel -> sellerSigned'
```

For example, if the user's intention was to buy the Offer, then we must check that the correct price was paid. 

The `input` check is extremely important also, and actually checks that the transaction only has a single script input. This is to prevent something called "The double satisfaction attack", which is explained in more detail here https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/double-satisfaction.html.

However, if the user's intention was to cancel, we only need to check that the seller has signed the transaction, there is also no need for us to check for a single script input, as it would be reasonable for someone to cancel multiple offers at once.

The rest of the code shouldn't be too hard to follow with a basic understanding of Haskell.

## 2. Compile the Plutus Script into bytecode

[Compile code](https://github.com/twwu123/example-nft-marketplace/blob/764f29c4c9147b23abea20141cfe1636ebb516d0/swap/src/Swap.hs#L102)

```haskell
validator :: Validator
validator = mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = mkUntypedValidator script'

script :: Script
script = unValidatorScript validator

bytes :: ShortByteString
bytes = (toShort . toStrict) (serialise script)

plutusScript :: PlutusScript PlutusScriptV2
plutusScript = PlutusScriptSerialised bytes

writeScriptToFile :: Haskell.IO ()
writeScriptToFile = do
    result <- writeFileTextEnvelope "swap.plutus" Haskell.Nothing plutusScript
    case result of
        Haskell.Left err -> Haskell.print Haskell.$ displayError err
        Haskell.Right () -> Haskell.return ()
```

We subsequently compile and write our Plutus Script to file. The simplest way to do this is to install Nix and GHCi, then

```
git clone https://github.com/input-output-hk/plutus-apps.git

git checkout c2b310968d0915e2af0ea4680186b41ad88ffbe9
```

Make sure to checkout the correct commit, because our cabal.project file needs a specific `plutus-apps` version.

Follow instructions at https://github.com/input-output-hk/plutus-apps#nix-1 to avoid rebuilding GHCi in Nix-shell.

Run

```
nix-shell
```

in the `plutus-apps` repository.

Then navigate back to the `example-nft-marketplace` and run

```
cabal repl
```

and 

```
writeScriptToFile
```

This writes the hexadecimal bytecode to a file called `swap.plutus`, this is needed for our next steps.

# Frontend Part

The next steps can all be done away from Haskell and Plutus tooling. Of course, there are ways to continue using Haskell to build and submit transactions, such as using the `Plutus Application Backend`, and continuous testing of contracts using the `Contract Emulator Trace Monad`. 

But I would recommend a more traditional tech stack once our contract has been written. This is because a lot of Crypto users currently prefer using browser extension wallets, and therefore dapps tend to interact with these extension wallets a lot.

`cardano-serialization-lib` is a library maintained by EMURGO that helps dapps build Cardano transactions at a very low level. These transactions can then be submitted however you like, I will be describing a flow where the transactions are submitted by a browser extension light wallet.

The first thing we'll do is use `create-react-app`

```
npx create-react-app example-nft-marketplace
```

Then we need to install `cardano-serialization-lib`

```
npm i @emurgo/cardano-serialization-lib-browser
```

However, it should be noted that `cardano-serialization-lib` is a wasm library, which `create-react-app` doesn't automatically support. We can install `craco` to change the `web-pack` settings of `create-react-app` to support wasm libraries.

```
npm i @craco/craco
```

Then we need to do 2 things, first create a `craco.config.js` at the project's root, and paste this into it

```javascript
// needed since create-react-app doesn't support wasm by default
module.exports = {
    webpack: {
      configure: config => {
        const wasmExtensionRegExp = /\.wasm$/
        config.resolve.extensions.push('.wasm')
        config.experiments = {
          syncWebAssembly: true,
        }
  
        config.module.rules.forEach(rule => {
          ;(rule.oneOf || []).forEach(oneOf => {
            if (oneOf.type === 'asset/resource') {
              oneOf.exclude.push(wasmExtensionRegExp)
            }
          })
        })
  
        return config
      },
    },
  }
```

and change the scripts in `package.json` to use `craco`

```json
"scripts": {
    "start": "craco start",
    "build": "craco build",
    "test": "craco test",
    "eject": "craco eject"
  },
```

Finally we need to set up some hooks, since wasm libraries cannot be imported synchronously. You should be able to find example of the hooks I'm talking about in the `frontend/src/hooks` folder.

[Wasm hook code](https://github.com/twwu123/example-nft-marketplace/blob/764f29c4c9147b23abea20141cfe1636ebb516d0/frontend/src/hooks/useWasm.js#L3)

```javascript
const useWasm = () => {
    const [CardanoWasm, setCardanoWasm] = useState(null)

    useEffect(() => {
        const getWasm = async () => {
            try {
                setCardanoWasm(await import("@emurgo/cardano-serialization-lib-browser"))
            } catch (e) {
                console.error(e)
            }
        }
        getWasm()
    }, [])
    return CardanoWasm
}
```

This snippet of code asynchronously imports `cardano-serialization-lib`, and handles everything for us, so we can simply use the library by calling this hook.

The other hook, `useYoroi` is needed to interact with the extension wallet `Yoroi`, but should be fairly self explanatory.

[Yoroi hook code](https://github.com/twwu123/example-nft-marketplace/blob/764f29c4c9147b23abea20141cfe1636ebb516d0/frontend/src/hooks/useYoroi.js#L31)

## 3. Hash the bytecode into an address

We can then hash the bytecode into a corresponding Cardano address

```javascript
const plutusScriptHex = "590b7c590b79010000323322323233223233223232323232323232323232323232323232323232323232323232323233223232322323223223232533532323235003223500322533500613302b330374910f73656c6c6572206e6f742070616964003232333553018120013502e502c2350012233355301b1200135031502f2350012233350012330374800000488cc0e00080048cc0dc005200000133012002001335032335503402a335032335503402a00550335033333553012120012233553017120012350012233550380023355301a1200123500122335503b00233350012330384800000488cc0e40080048cc0e000520000013301200200150323500222222222222233355301e1200122350022222350042233500225335333573466e3c0600041381344cd41180180204020802140f80294cd4c0c0d40088888888888880304c0d9262215335001103b221303a4984cc0dd2401166f6e6c792073656c6c6572206d61792063616e63656c0035002222222222222533533355302212001335039225335002210031001504025335333573466e3c0400041181144d410800454104010841184110cccd5cd19b8735573aa0089000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233502202335742a01866a0440466ae85402ccd4088090d5d0a805199aa8133ae502535742a012666aa04ceb94094d5d0a80419a8110159aba150073335502602c75a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233503675a6ae854008c0dcd5d09aba2500223263203933573807407206e26aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a06ceb4d5d0a801181b9aba135744a004464c6407266ae700e80e40dc4d55cf280089baa001357426ae8940088c98c80d4cd5ce01b01a81989aab9e5001137540026ae854014cd4089d71aba1500433355026028200135742a006666aa04ceb88004d5d0a80118151aba135744a004464c6406266ae700c80c40bc4d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a802180d1aba135744a008464c6404666ae7009008c084cccd5cd19b8750054800884880048cccd5cd19b8750064800084880088c98c808ccd5ce0120118108101999ab9a3370e6aae75401d2000232321233001003002375c6ae84d5d128041bad35742a00e464c6404266ae7008808407c40804c98c8080cd5ce24810350543500020135573ca00226ea80044d55ce9baa001135573ca00226ea800488cd54c01c480048d400488cd540a0008ccd40048cd54c02c480048d400488cd540b0008d5403400400488ccd5540200380080048cd54c02c480048d400488cd540b0008d54030004004ccd55400c024008004444888ccd54c01048005408ccd54c01c480048d400488cd540a0008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd409088ccd400c88008008004d40048800448cc004894cd400840c840040bc8d400488cc028008014018400c4cd409c01000d4090004cd54c01c480048d400488c8cd540a400cc004014c8004d540bc894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d540a088448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540948844894cd400454084884cd4088c010008cd54c01848004010004c8004d5409088448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000488ccd5cd19b8f0020010240231232230023758002640026aa046446666aae7c004940788cd4074c010d5d080118019aba2002012232323333573466e1cd55cea8012400046644246600200600460146ae854008c014d5d09aba2500223263201233573802602402026aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea8012400046644246600200600460266ae854008cd4034048d5d09aba2500223263201733573803002e02a26aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900c99ab9c01a019017016015135573aa00226ea8004d5d0a80119a804bae357426ae8940088c98c804ccd5ce00a00980889aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5408088c8cccd55cf8011280e119a80d99aa80e98031aab9d5002300535573ca00460086ae8800c0404d5d080089119191999ab9a3370ea002900011a80e98029aba135573ca00646666ae68cdc3a801240044a03a464c6402066ae700440400380344d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004404003803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae700340300284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801601401026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802802602202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00d00c00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00500480380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00b00a008007006135573aa00226ea80048c8cccd5cd19b87500148008805c8cccd5cd19b87500248000805c8c98c8018cd5ce00380300200189aab9d37540029309000a490350543100488100112330010020102253350021001100f12335002223335003220020020013500122001122123300100300222333573466e2000800403003488cdc0001000990009aa80511299a8008a802110a99aa999a991a80091110011a8011100088061080710807099a80280118020008980200088910010910911980080200188910919800801801090911801001889100091980124917657870656374656420312073637269707420696e707574000032253350011004133573800400624400424400222464600200244660066004004003"
const wasmScript = wasm.PlutusScript.from_bytes_v2(hexToBytes(plutusScriptHex))
const addr = wasm.EnterpriseAddress.new(
    0,
    wasm.StakeCredential.from_scripthash(wasmScript.hash())
).to_address()
console.log(addr.to_bech32())
```

```
addr_test1wq0acvhyvhxgcq7kp6gpcv6m44v7cvrp4uyv8lw9ttju35gqk8egf
```

We can log it as a `bech32` address if we wish, and simply use this `bech32` address from here on.

## 4. Send some ADA/Tokens to this address with a corresponding Datum

[Sell token code](https://github.com/twwu123/example-nft-marketplace/blob/764f29c4c9147b23abea20141cfe1636ebb516d0/frontend/src/pages/sell/components/SellNFTCard.js#L34)

```javascript
const sellToken = async () => {
        const txBuilder = wasm?.TransactionBuilder.new(
            wasm.TransactionBuilderConfigBuilder.new()
                .fee_algo(
                    wasm.LinearFee.new(
                        wasm.BigNum.from_str("44"),
                        wasm.BigNum.from_str("155381")
                    )
                )
                .coins_per_utxo_word(wasm.BigNum.from_str('34482'))
                .pool_deposit(wasm.BigNum.from_str('500000000'))
                .key_deposit(wasm.BigNum.from_str('2000000'))
                .ex_unit_prices(wasm.ExUnitPrices.new(
                    wasm.UnitInterval.new(wasm.BigNum.from_str("577"), wasm.BigNum.from_str("10000")),
                    wasm.UnitInterval.new(wasm.BigNum.from_str("721"), wasm.BigNum.from_str("10000000"))
                ))
                .max_value_size(5000)
                .max_tx_size(16384)
                .build()
        )

        // build output value, so we can do utxo selection for it. We will use 2 ADA and token
        const wasmValue = wasm.Value.new(wasm.BigNum.from_str("2000000"))
        const wasmMultiasset = wasm.MultiAsset.new()
        const wasmAssets = wasm.Assets.new()
        wasmAssets.insert(wasm.AssetName.new(Buffer.from(tokenName, "utf8")), wasm.BigNum.from_str("1"))
        wasmMultiasset.insert(wasm.ScriptHash.from_hex(tokenPolicyId), wasmAssets)
        wasmValue.set_multiasset(wasmMultiasset)

        const contractAddress = "addr_test1wq0acvhyvhxgcq7kp6gpcv6m44v7cvrp4uyv8lw9ttju35gqk8egf"
        const wasmContractAddress = wasm.Address.from_bech32(contractAddress)
        const wasmOutput = wasm.TransactionOutput.new(
            wasmContractAddress,
            wasmValue
        )

        // we first build the datum so we can inline it in the output
        // note that we're using the first used address of a wallet as the seller's address
        const usedAddresses = await api?.getUsedAddresses({ page: 0, limit: 1 })
        const wasmSellerAddress = wasm.Address.from_hex(usedAddresses[0])
        const wasmDatum = wasm.encode_json_str_to_plutus_datum(JSON.stringify({
            "constructor": 0,
            "fields": [
                {
                    "int": Math.trunc(Number(tokenPrice) * 1000000)
                },
                {
                    "bytes": wasm.BaseAddress.from_address(wasmSellerAddress).payment_cred().to_keyhash().to_hex()
                }
            ]
        }), wasm.PlutusDatumSchema.DetailedSchema)
        wasmOutput.set_plutus_data(wasmDatum)

        // finally we can add the output to our txBuilder
        txBuilder.add_output(wasmOutput)

        // the next step is to get our utxos from the wallet API, so we can perform UTXO selection on it
        const hexBalanceUtxos = await api?.getUtxos()
        const wasmUtxos = wasm.TransactionUnspentOutputs.new()
        for (let i = 0; i < hexBalanceUtxos.length; i++) {
            const wasmUtxo = wasm.TransactionUnspentOutput.from_hex(hexBalanceUtxos[i])
            wasmUtxos.add(wasmUtxo)
        }

        // this performs utxo selection from all the utxos we got from our wallet PAI
        txBuilder.add_inputs_from(wasmUtxos, wasm.CoinSelectionStrategyCIP2.LargestFirstMultiAsset)

        // now that we have all inputs and outputs handled, we can finally handle change and fees
        const hexChangeAddress = await api.getChangeAddress()
        const wasmChangeAddress = wasm.Address.from_hex(hexChangeAddress)
        txBuilder.add_change_if_needed(wasmChangeAddress)

        // then build the transaction so we can sign it with out wallet
        const unsignedTransactionHex = txBuilder.build_tx().to_hex()
        api?.signTx(unsignedTransactionHex)
            .then((witnessSetHex) => {
                // the wallet api returns the witness set required
                const wasmWitnessSet = wasm.TransactionWitnessSet.from_hex(witnessSetHex)
                const wasmTx = wasm.Transaction.from_hex(unsignedTransactionHex)
                const wasmSignedTransaction = wasm.Transaction.new(
                    wasmTx.body(),
                    wasmWitnessSet,
                    wasmTx.auxiliary_data()
                )
                const transactionHex = wasmSignedTransaction.to_hex()

                api.submitTx(transactionHex)
                    .then(txId => {
                        toast('success', `Transaction successfully submitted`)
                        console.log(`Transaction successfully submitted: ${txId}`)

                        // I'm using local storage here as our database, since it's just an example
                        // but there should be a dedicated database for your app
                        const wasmTxBody = wasmTx.body()
                        const wasmTxOutputs = wasmTxBody.outputs()
                        const jsonOutputs = JSON.parse(wasmTxOutputs.to_json())
                        let scriptOutputId = 0
                        for (let i = 0; i < jsonOutputs.length; i++) {
                            if (jsonOutputs[i]["address"] === contractAddress) {
                                scriptOutputId = i;
                            }
                        }

                        const strOffers = localStorage.getItem("offers")
                        const offerJSON = {
                            seller: wasmSellerAddress.to_bech32(),
                            policyId: tokenPolicyId,
                            name: tokenName,
                            description: tokenDescription,
                            image: imageURL,
                            price: Math.trunc(Number(tokenPrice) * 1000000),
                            transactionId: txId,
                            outputId: scriptOutputId
                        }
                        if (strOffers) {
                            const offers = JSON.parse(strOffers)
                            localStorage.setItem("offers", JSON.stringify([...offers, offerJSON]))
                        } else {
                            localStorage.setItem("offers", JSON.stringify([offerJSON]))
                        }
                    })
                    .catch(err => {
                        toast('error', err.info)
                        console.log(err)
                    })
            })
            .catch(err => {
                toast('error', err.info)
                console.log(err)
            })
    }
```

This snippet of code deploys an offer. Most of the code should be relatively self explanatory with the comments, but one thing we should pay careful attention to is the form of the datum. In general, the `datum` is provided at deployment of the contract, while the `redeemer` is provided when redeeming the contract UTXO, hence the name "`redeemer`".

```javascript
const wasmDatum = wasm.encode_json_str_to_plutus_datum(JSON.stringify({
    "constructor": 0,
    "fields": [
        {
            "int": Math.trunc(Number(tokenPrice) * 1000000)
        },
        {
            "bytes": wasm.BaseAddress.from_address(wasmSellerAddress).payment_cred().to_keyhash().to_hex()
        }
    ]
}), wasm.PlutusDatumSchema.DetailedSchema)
```

This JSON corresponds to our Haskell type 

```haskell
data Offer = Offer 
    { price           :: !Integer
    , seller          :: !PaymentPubKeyHash
    } deriving (Show, Generic, ToJSON, FromJSON)
PlutusTx.makeIsDataIndexed ''Offer [('Offer, 0)]
```

## 5. Redeem the ADA/Tokens based on some logic, with a corresponding redeemer

There are two operations available in our contract, and the contract logic depends entirely on which `redeemer` is provided.

[Cancel offer code](https://github.com/twwu123/example-nft-marketplace/blob/764f29c4c9147b23abea20141cfe1636ebb516d0/frontend/src/pages/sell/components/CancelUserOfferCard.js#L11)

```javascript
const cancelOffer = async () => {
        const txBuilder = wasm?.TransactionBuilder.new(
            wasm.TransactionBuilderConfigBuilder.new()
                .fee_algo(
                    wasm.LinearFee.new(
                        wasm.BigNum.from_str("44"),
                        wasm.BigNum.from_str("155381")
                    )
                )
                .coins_per_utxo_word(wasm.BigNum.from_str('34482'))
                .pool_deposit(wasm.BigNum.from_str('500000000'))
                .key_deposit(wasm.BigNum.from_str('2000000'))
                .ex_unit_prices(wasm.ExUnitPrices.new(
                    wasm.UnitInterval.new(wasm.BigNum.from_str("577"), wasm.BigNum.from_str("10000")),
                    wasm.UnitInterval.new(wasm.BigNum.from_str("721"), wasm.BigNum.from_str("10000000"))
                ))
                .max_value_size(5000)
                .max_tx_size(16384)
                .build()
        )

        // These fields will be specific to your script, the script hex is the compiled version of your plutus contract
        const plutusScriptHex = "590b7c590b79010000323322323233223233223232323232323232323232323232323232323232323232323232323233223232322323223223232533532323235003223500322533500613302b330374910f73656c6c6572206e6f742070616964003232333553018120013502e502c2350012233355301b1200135031502f2350012233350012330374800000488cc0e00080048cc0dc005200000133012002001335032335503402a335032335503402a00550335033333553012120012233553017120012350012233550380023355301a1200123500122335503b00233350012330384800000488cc0e40080048cc0e000520000013301200200150323500222222222222233355301e1200122350022222350042233500225335333573466e3c0600041381344cd41180180204020802140f80294cd4c0c0d40088888888888880304c0d9262215335001103b221303a4984cc0dd2401166f6e6c792073656c6c6572206d61792063616e63656c0035002222222222222533533355302212001335039225335002210031001504025335333573466e3c0400041181144d410800454104010841184110cccd5cd19b8735573aa0089000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233502202335742a01866a0440466ae85402ccd4088090d5d0a805199aa8133ae502535742a012666aa04ceb94094d5d0a80419a8110159aba150073335502602c75a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233503675a6ae854008c0dcd5d09aba2500223263203933573807407206e26aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a06ceb4d5d0a801181b9aba135744a004464c6407266ae700e80e40dc4d55cf280089baa001357426ae8940088c98c80d4cd5ce01b01a81989aab9e5001137540026ae854014cd4089d71aba1500433355026028200135742a006666aa04ceb88004d5d0a80118151aba135744a004464c6406266ae700c80c40bc4d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a802180d1aba135744a008464c6404666ae7009008c084cccd5cd19b8750054800884880048cccd5cd19b8750064800084880088c98c808ccd5ce0120118108101999ab9a3370e6aae75401d2000232321233001003002375c6ae84d5d128041bad35742a00e464c6404266ae7008808407c40804c98c8080cd5ce24810350543500020135573ca00226ea80044d55ce9baa001135573ca00226ea800488cd54c01c480048d400488cd540a0008ccd40048cd54c02c480048d400488cd540b0008d5403400400488ccd5540200380080048cd54c02c480048d400488cd540b0008d54030004004ccd55400c024008004444888ccd54c01048005408ccd54c01c480048d400488cd540a0008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd409088ccd400c88008008004d40048800448cc004894cd400840c840040bc8d400488cc028008014018400c4cd409c01000d4090004cd54c01c480048d400488c8cd540a400cc004014c8004d540bc894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d540a088448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540948844894cd400454084884cd4088c010008cd54c01848004010004c8004d5409088448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000488ccd5cd19b8f0020010240231232230023758002640026aa046446666aae7c004940788cd4074c010d5d080118019aba2002012232323333573466e1cd55cea8012400046644246600200600460146ae854008c014d5d09aba2500223263201233573802602402026aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea8012400046644246600200600460266ae854008cd4034048d5d09aba2500223263201733573803002e02a26aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900c99ab9c01a019017016015135573aa00226ea8004d5d0a80119a804bae357426ae8940088c98c804ccd5ce00a00980889aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5408088c8cccd55cf8011280e119a80d99aa80e98031aab9d5002300535573ca00460086ae8800c0404d5d080089119191999ab9a3370ea002900011a80e98029aba135573ca00646666ae68cdc3a801240044a03a464c6402066ae700440400380344d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004404003803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae700340300284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801601401026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802802602202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00d00c00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00500480380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00b00a008007006135573aa00226ea80048c8cccd5cd19b87500148008805c8cccd5cd19b87500248000805c8c98c8018cd5ce00380300200189aab9d37540029309000a490350543100488100112330010020102253350021001100f12335002223335003220020020013500122001122123300100300222333573466e2000800403003488cdc0001000990009aa80511299a8008a802110a99aa999a991a80091110011a8011100088061080710807099a80280118020008980200088910010910911980080200188910919800801801090911801001889100091980124917657870656374656420312073637269707420696e707574000032253350011004133573800400624400424400222464600200244660066004004003"
        // This particular redeemer corresponds to the "Cancel" data type in  my script
        const wasmRedeemData = wasm.encode_json_str_to_plutus_datum(JSON.stringify({
            "fields": [],
            "constructor": 1
        }), wasm.PlutusDatumSchema.DetailedSchema)

        const wasmRedeemer = wasm.Redeemer.new(
            wasm.RedeemerTag.new_spend(),
            wasm.BigNum.from_str("0"),
            wasmRedeemData,
            wasm.ExUnits.new(
                wasm.BigNum.from_str("2180128"),
                wasm.BigNum.from_str("632475719")
            )
        )

        // Next build the Tx Input and Value
        const wasmTxInputsBuilder = wasm.TxInputsBuilder.new()
        const wasmOfferTxInput = wasm.TransactionInput.new(wasm.TransactionHash.from_hex(offer.transactionId), offer.outputId)

        // The datum is inlined, so we can build a DatumSource and point it to the offer UTXO
        const plutusScriptWitness = wasm.PlutusWitness.new_with_ref(
            wasm.PlutusScriptSource.new(wasm.PlutusScript.from_hex_with_version(plutusScriptHex, wasm.Language.new_plutus_v2())),
            wasm.DatumSource.new_ref_input(wasmOfferTxInput),
            wasmRedeemer
        )

        // Should grab the value of the input from backend
        const wasmValue = wasm.Value.new(wasm.BigNum.from_str("2000000"))
        const wasmMultiasset = wasm.MultiAsset.new()
        const wasmAssets = wasm.Assets.new()
        wasmAssets.insert(wasm.AssetName.new(Buffer.from(offer.name, "utf8")), wasm.BigNum.from_str("1"))
        wasmMultiasset.insert(wasm.ScriptHash.from_hex(offer.policyId), wasmAssets)
        wasmValue.set_multiasset(wasmMultiasset)

        // Finally we add the plutus script input to the inputs builder
        wasmTxInputsBuilder.add_plutus_script_input(plutusScriptWitness, wasmOfferTxInput, wasmValue)
        // Maybe add some more value to pay fees
        const hexInputUtxos = await api.getUtxos("2000000")
        for (let i = 0; i < hexInputUtxos.length; i++) {
            const wasmUtxo = wasm.TransactionUnspentOutput.from_hex(hexInputUtxos[i])
            wasmTxInputsBuilder.add_input(wasmUtxo.output().address(), wasmUtxo.input(), wasmUtxo.output().amount())
        }
        // Then we can set the tx inputs to the tx inputs builder
        txBuilder.set_inputs(wasmTxInputsBuilder)

        // For plutus transactions, we need some collateral also
        const hexCollateralUtxos = await api?.getCollateral(3000000)
        const collateralTxInputsBuilder = wasm.TxInputsBuilder.new()
        for (let i = 0; i < hexCollateralUtxos.length; i++) {
            const wasmUtxo = wasm.TransactionUnspentOutput.from_hex(hexCollateralUtxos[i])
            collateralTxInputsBuilder.add_input(wasmUtxo.output().address(), wasmUtxo.input(), wasmUtxo.output().amount())
        }
        txBuilder.set_collateral(collateralTxInputsBuilder)

        // We need to handle hashing of plutus witness. Because the datum is actually included inline within the script UTXO
        // therefore, we need to intentionally leave out the datum in the witness set for the hash.
        const wasmRedeemers = wasm.Redeemers.new()
        wasmRedeemers.add(txBuilder.get_plutus_input_scripts().get(0).redeemer())
        // The cost models of v2 scripts must be manually built currently
        const costModel = wasm.TxBuilderConstants.plutus_vasil_cost_models().get(wasm.Language.new_plutus_v2());
        const costmdls = wasm.Costmdls.new()
        costmdls.insert(wasm.Language.new_plutus_v2(), costModel)
        // I intentionally put an undefined where the datum should go to make it clearer, but the argument can simply be left empty
        const plutusWitnessHash = wasm.hash_script_data(wasmRedeemers, costmdls, undefined)
        txBuilder.set_script_data_hash(plutusWitnessHash)

        // We need to add the required signer of our first used address, since the script
        // requires the seller's signature for a cancel operation
        txBuilder.add_required_signer(wasm.BaseAddress.from_address(wasm.Address.from_bech32(offer.seller)).payment_cred().to_keyhash())

        // Handle change (this should include the NFT in the offer)
        const hexChangeAddress = await api?.getChangeAddress()
        const wasmChangeAddress = wasm.Address.from_hex(hexChangeAddress)
        txBuilder.add_change_if_needed(wasmChangeAddress)

        const unsignedTransactionHex = txBuilder.build_tx().to_hex()
        api?.signTx(unsignedTransactionHex)
            .then((witnessSetHex) => {
                const wasmWitnessSet = wasm.TransactionWitnessSet.from_hex(witnessSetHex)
                const wasmTx = wasm.Transaction.from_hex(unsignedTransactionHex)
                const wasmSignedTransaction = wasm.Transaction.new(
                    wasmTx.body(),
                    wasmWitnessSet,
                    wasmTx.auxiliary_data()
                )
                const transactionHex = wasmSignedTransaction.to_hex()
                api.submitTx(transactionHex)
                    .then(txId => {
                        const strOffers = localStorage.getItem("offers")
                        const offers = JSON.parse(strOffers)
                        offers.splice(index, 1)
                        localStorage.setItem("offers", JSON.stringify(offers))

                        toast('success', `Transaction successfully submitted`)
                        console.log(`Transaction successfully submitted: ${txId}`)
                    })
                    .catch(err => {
                        toast('error', err.info)
                        console.log(err.info)
                    })
            }).catch(err => {
                toast('error', err.info)
                console.log(err.info)
            })
    }
```

[Buy offer code](https://github.com/twwu123/example-nft-marketplace/blob/764f29c4c9147b23abea20141cfe1636ebb516d0/frontend/src/pages/home/components/BuyPublicOfferCard.js#L11)

```javascript
const buyOffer = async () => {
        const txBuilder = wasm?.TransactionBuilder.new(
            wasm.TransactionBuilderConfigBuilder.new()
                .fee_algo(
                    wasm.LinearFee.new(
                        wasm.BigNum.from_str("44"),
                        wasm.BigNum.from_str("155381")
                    )
                )
                .coins_per_utxo_word(wasm.BigNum.from_str('34482'))
                .pool_deposit(wasm.BigNum.from_str('500000000'))
                .key_deposit(wasm.BigNum.from_str('2000000'))
                .ex_unit_prices(wasm.ExUnitPrices.new(
                    wasm.UnitInterval.new(wasm.BigNum.from_str("577"), wasm.BigNum.from_str("10000")),
                    wasm.UnitInterval.new(wasm.BigNum.from_str("721"), wasm.BigNum.from_str("10000000"))
                ))
                .max_value_size(5000)
                .max_tx_size(16384)
                .build()
        )

        // These fields will be specific to your script, the script hex is the compiled version of your plutus contract
        const plutusScriptHex = "590b7c590b79010000323322323233223233223232323232323232323232323232323232323232323232323232323233223232322323223223232533532323235003223500322533500613302b330374910f73656c6c6572206e6f742070616964003232333553018120013502e502c2350012233355301b1200135031502f2350012233350012330374800000488cc0e00080048cc0dc005200000133012002001335032335503402a335032335503402a00550335033333553012120012233553017120012350012233550380023355301a1200123500122335503b00233350012330384800000488cc0e40080048cc0e000520000013301200200150323500222222222222233355301e1200122350022222350042233500225335333573466e3c0600041381344cd41180180204020802140f80294cd4c0c0d40088888888888880304c0d9262215335001103b221303a4984cc0dd2401166f6e6c792073656c6c6572206d61792063616e63656c0035002222222222222533533355302212001335039225335002210031001504025335333573466e3c0400041181144d410800454104010841184110cccd5cd19b8735573aa0089000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233502202335742a01866a0440466ae85402ccd4088090d5d0a805199aa8133ae502535742a012666aa04ceb94094d5d0a80419a8110159aba150073335502602c75a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233503675a6ae854008c0dcd5d09aba2500223263203933573807407206e26aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a06ceb4d5d0a801181b9aba135744a004464c6407266ae700e80e40dc4d55cf280089baa001357426ae8940088c98c80d4cd5ce01b01a81989aab9e5001137540026ae854014cd4089d71aba1500433355026028200135742a006666aa04ceb88004d5d0a80118151aba135744a004464c6406266ae700c80c40bc4d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a802180d1aba135744a008464c6404666ae7009008c084cccd5cd19b8750054800884880048cccd5cd19b8750064800084880088c98c808ccd5ce0120118108101999ab9a3370e6aae75401d2000232321233001003002375c6ae84d5d128041bad35742a00e464c6404266ae7008808407c40804c98c8080cd5ce24810350543500020135573ca00226ea80044d55ce9baa001135573ca00226ea800488cd54c01c480048d400488cd540a0008ccd40048cd54c02c480048d400488cd540b0008d5403400400488ccd5540200380080048cd54c02c480048d400488cd540b0008d54030004004ccd55400c024008004444888ccd54c01048005408ccd54c01c480048d400488cd540a0008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd409088ccd400c88008008004d40048800448cc004894cd400840c840040bc8d400488cc028008014018400c4cd409c01000d4090004cd54c01c480048d400488c8cd540a400cc004014c8004d540bc894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d540a088448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540948844894cd400454084884cd4088c010008cd54c01848004010004c8004d5409088448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000488ccd5cd19b8f0020010240231232230023758002640026aa046446666aae7c004940788cd4074c010d5d080118019aba2002012232323333573466e1cd55cea8012400046644246600200600460146ae854008c014d5d09aba2500223263201233573802602402026aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea8012400046644246600200600460266ae854008cd4034048d5d09aba2500223263201733573803002e02a26aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900c99ab9c01a019017016015135573aa00226ea8004d5d0a80119a804bae357426ae8940088c98c804ccd5ce00a00980889aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5408088c8cccd55cf8011280e119a80d99aa80e98031aab9d5002300535573ca00460086ae8800c0404d5d080089119191999ab9a3370ea002900011a80e98029aba135573ca00646666ae68cdc3a801240044a03a464c6402066ae700440400380344d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004404003803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae700340300284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801601401026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802802602202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00d00c00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00500480380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00b00a008007006135573aa00226ea80048c8cccd5cd19b87500148008805c8cccd5cd19b87500248000805c8c98c8018cd5ce00380300200189aab9d37540029309000a490350543100488100112330010020102253350021001100f12335002223335003220020020013500122001122123300100300222333573466e2000800403003488cdc0001000990009aa80511299a8008a802110a99aa999a991a80091110011a8011100088061080710807099a80280118020008980200088910010910911980080200188910919800801801090911801001889100091980124917657870656374656420312073637269707420696e707574000032253350011004133573800400624400424400222464600200244660066004004003"
        // This particular redeemer corresponds to the "Buy" data type in  my script
        const wasmRedeemData = wasm.encode_json_str_to_plutus_datum(JSON.stringify({
            "fields": [],
            "constructor": 0
        }), wasm.PlutusDatumSchema.DetailedSchema)

        const wasmRedeemer = wasm.Redeemer.new(
            wasm.RedeemerTag.new_spend(),
            wasm.BigNum.from_str("0"),
            wasmRedeemData,
            wasm.ExUnits.new(
                wasm.BigNum.from_str("2180128"),
                wasm.BigNum.from_str("632475719")
            )
        )

        // Next build the Tx Input and Value
        const wasmTxInputsBuilder = wasm.TxInputsBuilder.new()
        const wasmOfferTxInput = wasm.TransactionInput.new(wasm.TransactionHash.from_hex(offer.transactionId), offer.outputId)

        // The datum is inlined, so we can build a DatumSource and point it to the offer UTXO
        const plutusScriptWitness = wasm.PlutusWitness.new_with_ref(
            wasm.PlutusScriptSource.new(wasm.PlutusScript.from_hex_with_version(plutusScriptHex, wasm.Language.new_plutus_v2())),
            wasm.DatumSource.new_ref_input(wasmOfferTxInput),
            wasmRedeemer
        )

        // Should grab the value of the input from backend
        const wasmValue = wasm.Value.new(wasm.BigNum.from_str("2000000"))
        const wasmMultiasset = wasm.MultiAsset.new()
        const wasmAssets = wasm.Assets.new()
        wasmAssets.insert(wasm.AssetName.new(Buffer.from(offer.name, "utf8")), wasm.BigNum.from_str("1"))
        wasmMultiasset.insert(wasm.ScriptHash.from_hex(offer.policyId), wasmAssets)
        wasmValue.set_multiasset(wasmMultiasset)

        // Finally we add the plutus script input to the inputs builder
        wasmTxInputsBuilder.add_plutus_script_input(plutusScriptWitness, wasmOfferTxInput, wasmValue)
        // We need to be sure that we can pay the seller and fees
        const hexInputUtxos = await api.getUtxos(String(offer.price + 2000000))
        for (let i = 0; i < hexInputUtxos.length; i++) {
            const wasmUtxo = wasm.TransactionUnspentOutput.from_hex(hexInputUtxos[i])
            wasmTxInputsBuilder.add_input(wasmUtxo.output().address(), wasmUtxo.input(), wasmUtxo.output().amount())
        }
        // Then we can set the tx inputs to the tx inputs builder
        txBuilder.set_inputs(wasmTxInputsBuilder)

        // For plutus transactions, we need some collateral also
        const hexCollateralUtxos = await api?.getCollateral(3000000)
        const collateralTxInputsBuilder = wasm.TxInputsBuilder.new()
        for (let i = 0; i < hexCollateralUtxos.length; i++) {
            const wasmUtxo = wasm.TransactionUnspentOutput.from_hex(hexCollateralUtxos[i])
            collateralTxInputsBuilder.add_input(wasmUtxo.output().address(), wasmUtxo.input(), wasmUtxo.output().amount())
        }
        txBuilder.set_collateral(collateralTxInputsBuilder)

        // We need to handle hashing of plutus witness. Because the datum is actually included inline within the script UTXO
        // therefore, we need to intentionally leave out the datum in the witness set for the hash.
        const wasmRedeemers = wasm.Redeemers.new()
        wasmRedeemers.add(txBuilder.get_plutus_input_scripts().get(0).redeemer())
        // The cost models of v2 scripts must be manually built currently
        const costModel = wasm.TxBuilderConstants.plutus_vasil_cost_models().get(wasm.Language.new_plutus_v2());
        const costmdls = wasm.Costmdls.new()
        costmdls.insert(wasm.Language.new_plutus_v2(), costModel)
        // I intentionally put an undefined where the datum should go to make it clearer, but the argument can simply be left empty
        const plutusWitnessHash = wasm.hash_script_data(wasmRedeemers, costmdls, undefined)
        txBuilder.set_script_data_hash(plutusWitnessHash)

        // ensure the seller gets paid in a buy transaction
        const wasmSellerAddress = wasm.Address.from_bech32(offer.seller)
        const wasmOutput = wasm.TransactionOutput.new(
            wasmSellerAddress,
          wasm.Value.new(wasm.BigNum.from_str(String(offer.price)))
        )
        txBuilder.add_output(wasmOutput)

        // Handle change (this should include the NFT in the offer)
        const hexChangeAddress = await api?.getChangeAddress()
        const wasmChangeAddress = wasm.Address.from_hex(hexChangeAddress)
        txBuilder.add_change_if_needed(wasmChangeAddress)

        const unsignedTransactionHex = txBuilder.build_tx().to_hex()
        api?.signTx(unsignedTransactionHex)
            .then((witnessSetHex) => {
                const wasmWitnessSet = wasm.TransactionWitnessSet.from_hex(witnessSetHex)
                const wasmTx = wasm.Transaction.from_hex(unsignedTransactionHex)
                const wasmSignedTransaction = wasm.Transaction.new(
                    wasmTx.body(),
                    wasmWitnessSet,
                    wasmTx.auxiliary_data()
                )
                const transactionHex = wasmSignedTransaction.to_hex()
                api.submitTx(transactionHex)
                    .then(txId => {
                        const strOffers = localStorage.getItem("offers")
                        const offers = JSON.parse(strOffers)
                        offers.splice(index, 1)
                        localStorage.setItem("offers", JSON.stringify(offers))

                        toast('success', `Transaction successfully submitted`)
                        console.log(`Transaction successfully submitted: ${txId}`)
                    })
                    .catch(err => {
                        toast('error', err.info)
                        console.log(err.info)
                    })
            }).catch(err => {
                toast('error', err.info)
                console.log(err.info)
            })
    }
```

You can see that the code snippets of both `redeem` transactions are almost identical, except for two parts.

### Cancel redeemer

```json
{
    "fields": [],
    "constructor": 1
}
```

### Buy redeemer

```json
{
    "fields": [],
    "constructor": 0
}
```

### Cancel Logic

In the cancel transaction we require an extra line that adds a required signer, which corresponds to the seller's address. Without this, the script would fail. Also the Yoroi API automatically attempts to detect required signers in the transaction that the wallet controls, and will sign it with the correct private key.

```javascript
// We need to add the required signer of our first used address, since the script
// requires the seller's signature for a cancel operation
txBuilder.add_required_signer(wasm.BaseAddress.from_address(wasm.Address.from_bech32(offer.seller)).payment_cred().to_keyhash())
```

### Buy Logic

For buying however, the only check is that there is only 1 script input, and that the seller is paid, so we need to have an extra output that pays our seller the correct amount.

```javascript
// ensure the seller gets paid in a buy transaction
const wasmSellerAddress = wasm.Address.from_bech32(offer.seller)
const wasmOutput = wasm.TransactionOutput.new(
    wasmSellerAddress,
    wasm.Value.new(wasm.BigNum.from_str(String(offer.price)))
)
txBuilder.add_output(wasmOutput)
```

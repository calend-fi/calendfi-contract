{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OnChain.Compound
Description : A test to simulate an attach on the on-chain code of compound.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OnChain.Compound where

-- GHC libraries.
import Control.Monad

-- Third-praty libraries.
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Scripts
import Test.Tasty

-- Internal modules.
import MainToken
import Staking

import Tests.Attacks.Compound
import Tests.TestUtils

tests :: TestTree
tests = testGroup "onChainCompoundTests"
        [ buildTest "compoundAttackTest"
            compoundAttackError
            compoundAttackTrace
        , buildTest "fakeCompoundAttackTest"
            fakeCompoundAttackError
            fakeCompoundAttackTrace
        ]

compoundAttackTrace :: EmulatorTrace ()
compoundAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                         (MicroToken 7_777_777)
                                                         testStakingSettings
    pool       <- getStaking hAdminWallet
    void $ waitNSlots 2

    hUser     <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack    <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hUser ()
    void $ waitNSlots 2

    callEndpoint @"deposit" hUser (MicroToken 33_333_333)
    void $ waitNSlots 100

    callEndpoint @"compoundAttack" hAttack ()
    void $ waitNSlots 2

compoundAttackError :: [ScriptError]
compoundAttackError =
    [ EvaluationError
      [ "checkUserCompound: User output UTxO datum is wrong."
      , "PT5"
      ]
      "CekEvaluationFailure"
    , EvaluationError
      [ "checkPoolClaim: Pool ouput UTxO value is wrong."
      , "PT5"
      ]
      "CekEvaluationFailure"
    ]

fakeCompoundAttackTrace :: EmulatorTrace ()
fakeCompoundAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                         (MicroToken 7_777_777)
                                                         testStakingSettings
    pool       <- getStaking hAdminWallet
    void $ waitNSlots 2

    hUser     <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack    <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hUser ()
    void $ waitNSlots 2

    callEndpoint @"deposit" hUser (MicroToken 33_333_333)
    void $ waitNSlots 100

    callEndpoint @"fakeCompoundAttack" hAttack ()
    void $ waitNSlots 2

fakeCompoundAttackError :: [ScriptError]
fakeCompoundAttackError = pure $
    EvaluationError
    [ "checkUserCompound: User output UTxO datum is wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"

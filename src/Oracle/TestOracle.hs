{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Oracle.TestOracle where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..), IO, show)
import           Wallet.Emulator.Wallet

import           Oracle.Swap
import           Oracle.Core
import qualified Control.Monad.Freer.Extras as Extras


assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList $ [(knownWallet 1, vOracle)] <> [(knownWallet i, v) | i <- [2 .. 3]]) def def

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

    vOracle :: Value
    vOracle = Ada.lovelaceValueOf                    100_000_000 <>
              Value.singleton oracleSymbol oracleTokenName 1


checkOracle :: Oracle -> Contract () w Text ()
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle

myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 2_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                , opExchange = 2
                }

    h1 <- activateContractWallet (knownWallet 1) $ runOracle op
    h2 <- activateContractWallet (knownWallet 2) $ runOracle op    

    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1
    void $ Emulator.waitNSlots 1

    ---h3 <- activateContractWallet (knownWallet 3) $ swap oracle
    
    callEndpoint @"update" h1 5
    void $ Emulator.waitNSlots 1
    void $ getOracle h1    
    void $ Emulator.waitNSlots 1

    -- callEndpoint @"offer" h3 10_000_000
    -- void $ Emulator.waitNSlots 1

    callEndpoint @"update" h1 8
    void $ Emulator.waitNSlots 1
    void $ getOracle h1    
    void $ Emulator.waitNSlots 1


  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle

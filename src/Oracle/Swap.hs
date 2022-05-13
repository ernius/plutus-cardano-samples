{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores #-}

module Oracle.Swap where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe, fromJust)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), (<$>), String, show)

import           Oracle.Core
-- import           Oracle.Funds
import Ledger (getCardanoTxId)

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh || -- the owner can do what he pleases
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    minPrice :: Integer
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid = 
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (AssetClass (oCurrency oracle, oToken oracle))
      in
        pricePaid >= minPrice

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

swapInst :: Oracle -> Scripts.TypedValidator Swapping
swapInst oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode oracleAddress)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . swapInst

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

offer :: AsContractError e => Oracle -> Integer -> Contract w s e ()
offer oracle amt = do
    pkh <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (swapInst oracle) tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

findSwaps ::  AsContractError e => Oracle -> (PubKeyHash -> Bool) -> Contract w s e [(TxOutRef, ChainIndexTxOut, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxosAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: ChainIndexTxOut -> Maybe PubKeyHash
    f o = case _ciTxOutDatum o of
            Left _ -> Nothing
            Right (Datum d) -> PlutusTx.fromBuiltinData d

    g :: (TxOutRef, ChainIndexTxOut) -> Maybe (TxOutRef , ChainIndexTxOut,  PubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)

retrieve :: AsContractError e => Oracle -> Contract w s e ()
retrieve oracle = do
    pkh <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
    xs <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

use :: forall w s e. AsContractError e => Oracle -> Integer -> Contract w s e ()
use oracle amt = do
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show (oExchange x)
            pkh <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt (oExchange oracle)) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    logInfo @String "found suitable swap"
                    let v       = txOutValue (toTxOut o) <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (AssetClass (oCurrency oracle, oToken oracle)) (price (lovelaces $ txOutValue $ toTxOut o') (oExchange oracle)) 
                                  <> lovelaceValueOf 2_000_000 -- min ada  
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript oracleValidator                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash oracleValidator)
                                    (Datum $ PlutusTx.toBuiltinData oracle)
                                    v                                                                      <>
                                  Constraints.mustPayToPubKey (PaymentPubKeyHash pkh') p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> ChainIndexTxOut -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ toTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, ChainIndexTxOut, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
            Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      Integer
        -- .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = awaitPromise (offer' `select` retrieve' `select` use') >> swap oracle
  where
    offer' =  endpoint @"offer" $ offer oracle
    retrieve' = endpoint @"retrieve" $ const $ retrieve oracle
    use' = endpoint @"use" $ use oracle

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores #-}

module Auction
    ( Auction (..)
    , StartParams (..), BidParams (..), CloseParams (..)
    , AuctionSchema
    , start, bid, close
    , endpoints
    , ensureKnownCurrencies
    , printJson
    , printSchemas
    , stage
    , testAuction
    , testAuction2
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import           Schema               (ToSchema)
import           Text.Printf          (printf)
--import Data.CharSet.Unicode (currencySymbol)
import Data.Default               (Default (..))
import Ledger.TimeSlot
import qualified Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
import Control.Monad (forM_)
import Wallet.Emulator.MultiAgent
import Plutus.Trace.Emulator.Types
import System.IO
import Data.Text.Internal.Fusion.Size (codePointsSize)


minLovelace :: Integer
minLovelace = 2_000_000

data Auction = Auction
    { aSeller   :: !PaymentPubKeyHash
    , aDeadline :: !POSIXTime
    , aMinBid   :: !Integer
    , aCurrency :: !CurrencySymbol
    , aToken    :: !TokenName
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Auction where
    {-# INLINABLE (==) #-}
    a == b = (aSeller   a == aSeller   b) &&
             (aDeadline a == aDeadline b) &&
             (aMinBid   a == aMinBid   b) &&
             (aCurrency a == aCurrency b) &&
             (aToken    a == aToken    b)

PlutusTx.unstableMakeIsData ''Auction


data Bid = Bid
    { bBidder :: !PaymentPubKeyHash
    , bBid    :: !Integer
    } deriving P.Show

instance Eq Bid where
    {-# INLINABLE (==) #-}
    b == c = (bBidder b == bBidder c) &&
             (bBid    b == bBid    c)

PlutusTx.unstableMakeIsData ''Bid

data AuctionAction = MkBid Bid | Close
    deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionAction

data AuctionDatum = AuctionDatum
    { adAuction    :: !Auction
    , adHighestBid :: !(Maybe Bid)
    } deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionDatum

data Auctioning
instance Scripts.ValidatorTypes Auctioning where
    type instance RedeemerType Auctioning = AuctionAction
    type instance DatumType Auctioning = AuctionDatum

{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx =
    traceIfFalse "wrong input value" correctInputValue &&
    case redeemer of
        MkBid b@Bid{..} ->
            traceIfFalse "bid too low"        (sufficientBid bBid)         &&
            traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund"       correctBidRefund             &&
            traceIfFalse "too late"           correctBidSlotRange
        Close           ->
            traceIfFalse "too early" correctCloseSlotRange &&
            case adHighestBid ad of
                Nothing      ->
                    traceIfFalse "expected seller to get token" (getsValue (aSeller auction) $ tokenValue <> Ada.lovelaceValueOf minLovelace)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case adHighestBid ad of
        Nothing      -> tokenValue <> Ada.lovelaceValueOf minLovelace
        Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf (minLovelace + bBid)

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput   :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = (adAuction outputDatum == auction)   &&
                              (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
        txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
        Nothing      -> True
        Just Bid{..} ->
          let
            os = [ o
                 | o <- txInfoOutputs info
                 , txOutAddress o == pubKeyHashAddress bBidder Nothing
                 ]
          in
            case os of
                [o] -> txOutValue o == Ada.lovelaceValueOf bBid
                _   -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info
    --                       (--- valid range of x --)
    --                   (-inf .....            deadline)
    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info
    --                       (--- valid range of x --)
    --                   (-deadline .....            inf)

    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

typedAuctionValidator :: Scripts.TypedValidator Auctioning
typedAuctionValidator = Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction

-- Off chain code
auctionValidator :: Validator
auctionValidator = Scripts.validatorScript typedAuctionValidator

auctionHash :: Ledger.ValidatorHash
auctionHash = Scripts.validatorHash typedAuctionValidator

auctionAddress :: Ledger.Address
auctionAddress = scriptHashAddress auctionHash

data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams
    { bpCurrency :: !CurrencySymbol
    , bpToken    :: !TokenName
    , bpBid      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams
    { cpCurrency :: !CurrencySymbol
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =
        Endpoint "start" StartParams
    .\/ Endpoint "bid"   BidParams
    .\/ Endpoint "close" CloseParams

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
    logInfo @P.String "---------------- Start Auction ----------------------------------------"  
    pkh <- ownPaymentPubKeyHash
           
    let a = Auction
                { aSeller   = pkh
                , aDeadline = spDeadline
                , aMinBid   = spMinBid
                , aCurrency = spCurrency
                , aToken    = spToken
                }
        d = AuctionDatum
                { adAuction    = a
                , adHighestBid = Nothing
                }
        v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints typedAuctionValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "started auction %s for token %s" (P.show a) (P.show v)

-- homework try to log an error message when the deadline is reached
bid :: forall w s. BidParams -> Contract w s Text ()
bid BidParams{..} = do
    logInfo @P.String "---------------- Make a bid ----------------------------------------"
    (oref, o, d@AuctionDatum{..}) <- findAuction bpCurrency bpToken
    logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

    when (bpBid < minBid d) $
        throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
    pkh <- ownPaymentPubKeyHash
    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf (minLovelace + bpBid)
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

        -- explained here https://cardano.stackexchange.com/questions/2296/lecture-6-it-2-core-hs-explaining-lookups-use-of-both-typedvalidatorlook
        lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<> -- used for the output utxo with the new contract instance
                  Constraints.otherScript auctionValidator                P.<> -- used for consuming the input contract instance
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToTheScript d' v                            <>
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                    Constraints.mustSpendScriptOutput oref r
                    Just Bid{..} -> Constraints.mustPayToTheScript d' v                            <>
                                    Constraints.mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made bid of %d lovelace in auction %s for token (%s, %s)"
        bpBid
        (P.show adAuction)
        (P.show bpCurrency)
        (P.show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    logInfo @P.String "---------------- Close Auction ----------------------------------------"    
    (oref, o, d@AuctionDatum{..}) <- findAuction cpCurrency cpToken
    logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

    let t      = Value.singleton cpCurrency cpToken 1
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction

        lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                  Constraints.otherScript auctionValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)  <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r
                    Just Bid{..} -> Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace) <>
                                    Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBid)              <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed auction %s for token (%s, %s)"
        (P.show adAuction)
        (P.show cpCurrency)
        (P.show cpToken)

findAuction :: CurrencySymbol
            -> TokenName
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, AuctionDatum)
findAuction cs tn = do
    utxos <- utxosAt $ scriptHashAddress auctionHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@AuctionDatum{..}
                    | aCurrency adAuction == cs && aToken adAuction == tn -> return (oref, o, d)
                    | otherwise                                           -> throwError "auction token missmatch"
        _           -> throwError "auction utxo not found"

endpoints :: Contract () AuctionSchema Text ()
endpoints = awaitPromise (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" start
    bid'   = endpoint @"bid"   bid
    close' = endpoint @"close" close

-- mkSchemaDefinitions ''AuctionSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

-- mkKnownCurrencies ['myToken]

customShowEvent :: EmulatorEvent' -> Maybe P.String
customShowEvent = \case
  UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg <> "\n"
  InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _)   -> Just $ "*** CONTRACT LOG: " <> P.show msg <> "\n"
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> P.show err <> "\n"
  ev                                                                   -> Nothing

traceConfig :: Trace.TraceConfig
traceConfig =
  Trace.TraceConfig
    customShowEvent
    stdout

emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig (Left $ Map.fromList [(knownWallet 1, v1),  (knownWallet 2, v2), (knownWallet 3, v3) ]) def def
  where
    v1 :: Value
    v1 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 1
    v2 :: Value
    v2 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token2 1
    v3 :: Value
    v3 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token3 1
    

testAuction :: IO ()
testAuction = Trace.runEmulatorTraceIO' traceConfig emCfg myTrace

testAuction2 :: IO ()
testAuction2 = Trace.runEmulatorTraceIO' traceConfig emCfg myTrace2

currSymbol :: CurrencySymbol
currSymbol = currencySymbol "dcddcaa"

tName :: TokenName
tName = tokenName "T1"

tName2 :: TokenName
tName2 = tokenName "T2"

tName3 :: TokenName
tName3 = tokenName "T3"

token :: AssetClass
token = AssetClass (currSymbol, tName)

token2 :: AssetClass
token2 = AssetClass (currSymbol, tName2)

token3 :: AssetClass
token3 = AssetClass (currSymbol, tName3)

myTrace :: Trace.EmulatorTrace ()
myTrace = do
    let wallets = P.show $ P.map P.show knownWallets
    Extras.logInfo $ "Wallets: " ++ wallets    

    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints 
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints
    h3 <- Trace.activateContractWallet (knownWallet 3) $ endpoints
    
    Trace.callEndpoint @"start" h1 $ StartParams
        { spDeadline     = slotToBeginPOSIXTime def 10
        , spMinBid       = 5_000_000
        , spCurrency     = currSymbol
        , spToken        = tName
        }
    void $ Trace.waitUntilSlot 5
    Trace.callEndpoint @"bid" h2 (BidParams currSymbol tName 20_000_000)
    s <- Trace.waitNSlots 1
    Trace.callEndpoint @"bid" h3 (BidParams currSymbol tName 10_000_000)    
    s <- Trace.waitNSlots 10
    Trace.callEndpoint @"close" h2 (CloseParams currSymbol tName)
    void $ Trace.waitNSlots 2    

myTrace2 :: Trace.EmulatorTrace ()
myTrace2 = do
    let wallets = P.show $ P.map P.show knownWallets
    Extras.logInfo $ "Wallets: " ++ wallets    

    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints 
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints
    h3 <- Trace.activateContractWallet (knownWallet 3) $ endpoints
    
    Trace.callEndpoint @"start" h1 $ StartParams
        { spDeadline     = slotToBeginPOSIXTime def 10
        , spMinBid       = 5_000_000
        , spCurrency     = currSymbol
        , spToken        = tName
        }
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"bid" h2 (BidParams currSymbol tName 6_000_000)
    s <- Trace.waitNSlots 2
    Trace.callEndpoint @"bid" h3 (BidParams currSymbol tName 30_000_000)
    s <- Trace.waitNSlots 2
    Trace.callEndpoint @"bid" h1 (BidParams currSymbol tName 60_000_000)
    s <- Trace.waitNSlots 10
    Trace.callEndpoint @"close" h2 (CloseParams currSymbol tName)
    void $ Trace.waitNSlots 2    

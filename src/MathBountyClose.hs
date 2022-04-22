{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

-- Simple implementation of a math bounty contract
module MathBountyClose where

import qualified Data.Aeson as Aeson
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import Data.Default               (def, Default (..))
import           Data.Void            (Void)
import qualified Data.Map as Map
import           Playground.Contract
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure)

import           Plutus.Contract
import           Plutus.Contract.Error
import           Ledger                    
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Playground.Contract
import qualified Prelude
import Prelude (String)
import           Text.Printf          (printf)
import Ledger.TimeSlot
import Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
import Wallet.Emulator.MultiAgent
import Plutus.Trace.Emulator.Types
import System.IO
------------------------------------------------------------
-- | On-Chain code
------------------------------------------------------------

data MathBountyDatum = MathBountyDatum {
     mTarget   :: Integer
   , mDeadline :: POSIXTime
   , mOwner    :: !PaymentPubKeyHash   
 } deriving Show

PlutusTx.unstableMakeIsData ''MathBountyDatum

data MauthBountyRedeemer = Solution Integer | Close
    deriving Show

PlutusTx.unstableMakeIsData ''MauthBountyRedeemer

-- | This method is the spending validator (which gets lifted to its on-chain representation).
--   validate that the square of the proposed value is the expected solution
--   The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
{-# INLINABLE validateSolution #-}
validateSolution :: MathBountyDatum -> MauthBountyRedeemer -> ScriptContext -> Bool
validateSolution (MathBountyDatum y d o) redeemer ctx =
  traceIfFalse "Deadline not reached" deadlineReached
  && case redeemer of
       Solution x ->
         traceIfFalse "Wrong guess" $ x*x == y
       Close ->
         traceIfFalse "Not owner of the bid" $ signedByOwner

  where
    deadlineReached :: Bool
    deadlineReached = (from d) `contains` (txInfoValidRange $ scriptContextTxInfo ctx)

    signedByOwner :: Bool
    signedByOwner = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash o


--                                 validrange
--                               ( ..........         )
--  -----------------------(--------------------------)--> time
--                       Deadline                    

-- | Datum and redeemer parameter types
data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = MauthBountyRedeemer
    type instance DatumType MathBounty = MathBountyDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
bountyInstance :: Scripts.TypedValidator MathBounty
bountyInstance = Scripts.mkTypedValidator @MathBounty
  $$(PlutusTx.compile [|| validateSolution ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @MathBountyDatum @MauthBountyRedeemer

------------------------------------------------------------
-- | Off-Chain code
------------------------------------------------------------

validator :: Validator
validator = Scripts.validatorScript bountyInstance

-- | The address of the bounty (the hash of its validator script)
bountyAddress :: Address
bountyAddress = Ledger.scriptAddress (Scripts.validatorScript bountyInstance)

-- | Parameters for the "bounty" endpoint
data BountyParams = BountyParams
    { bTarget :: Integer
    , bAmount :: Value
    , bDeadline :: POSIXTime
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

--  | Parameters for the "solution" endpoint
newtype SolutionParams = SolutionParams
    { proposed_solution :: Integer
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The schema of the contract, with one endpoint to publish the problem with a bounty and another to sbumit solutions
type MathBountySchema =
            Endpoint "bounty" BountyParams
        .\/ Endpoint "solution" SolutionParams
        .\/ Endpoint "close" ()

-- | The "bounty" contract endpoint.
bounty :: forall  e. AsContractError e => BountyParams -> Contract () MathBountySchema e ()
bounty (BountyParams t amt deadline) = do
    pkh <- ownPaymentPubKeyHash

    let  datum = MathBountyDatum t deadline pkh
         tx   = Constraints.mustPayToTheScript datum amt

    logInfo @String $ "made a bounty with this datum " <> Prelude.show datum
    
    ledgerTx <- submitTxConstraints bountyInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bounty of %d" $ getLovelace $ fromValue amt

-- | The "solution" contract endpoint.
solution :: forall  e. AsContractError e => SolutionParams -> Contract () MathBountySchema e ()
solution (SolutionParams theProposal) = do
    now <- currentTime
    unspentOutputs <- utxosAt bountyAddress

    -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
    -- all unspent transaction outputs at a script address and pays them to a
    -- public key address owned by this wallet. 
    let tx = collectFromScript unspentOutputs (Solution theProposal)
           <> Constraints.mustValidateIn (from now)
    ledgerTx <- submitTxConstraintsSpending bountyInstance unspentOutputs tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "proposed solution is %d" theProposal

close :: forall w s e. AsContractError e => Contract w s e ()
close = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt bountyAddress
    if Map.null utxos
        then logInfo @String $ "no bounties available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  Prelude.<>
                          Constraints.otherScript validator
                redeemer = Redeemer $ PlutusTx.toBuiltinData Close
                tx :: Constraints.TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref redeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "closed my bounties"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> mOwner d == pkh && mDeadline d <= now
    

-- | Math bounty endpoints.
endpoints :: forall  e. AsContractError e => Contract () MathBountySchema e ()
endpoints = awaitPromise (bounty' `select` solution' `select` close') >> endpoints
  where
    bounty' = endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution
    close' = endpoint @"close" $ const close

mkSchemaDefinitions ''MathBountySchema

-- Contract w s e a
-- EmulatorTrace a

testAll :: IO ()
testAll = do
  test1
  test2
  test3

emulatorConfig :: Trace.EmulatorConfig
emulatorConfig =
  Trace.EmulatorConfig
    (Left $ Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 30_000_000),
                          (knownWallet 2, Ada.lovelaceValueOf 10_000_000)])
    def
    def

customShowEvent :: EmulatorEvent' -> Maybe String
customShowEvent = \case
  UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg
  InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _)   -> Just $ "*** CONTRACT LOG: " <> Prelude.show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> Prelude.show err
  ev                                                                   -> Nothing

traceConfig :: Trace.TraceConfig
traceConfig =
  Trace.TraceConfig
    customShowEvent
    stdout
  
test1 :: IO ()
test1 = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTrace

-- test consuming the bounty when the deadline has been reached
myTrace :: Trace.EmulatorTrace ()
myTrace = do
    Extras.logInfo @String "Starts emulation"
    Extras.logInfo @String $ "Wallet 1 is: " <> (Prelude.show $ mockWalletAddress (knownWallet 1))
    Extras.logInfo @String $ "Wallet 2 is: " <> (Prelude.show $ mockWalletAddress (knownWallet 2))
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints @ContractError
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints @ContractError
    Trace.callEndpoint @"bounty" h1 $ BountyParams
        { bTarget       = 4
        , bDeadline     = slotToBeginPOSIXTime def 2
        , bAmount       = Ada.lovelaceValueOf 10_000_000
        }
        
    void $ Trace.waitUntilSlot 3
    Trace.callEndpoint @"solution" h2 (SolutionParams 2)
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Prelude.show s

test3 :: IO ()
test3 = Trace.runEmulatorTraceIO myTrace3

-- test consuming the bounty when the deadline has been reached but bad square root solution
myTrace3 :: Trace.EmulatorTrace ()
myTrace3 = do
    Extras.logInfo @String "Starts emulation"
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints @ContractError
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints @ContractError
    Trace.callEndpoint @"bounty" h1 $ BountyParams
        { bTarget       = 4
        , bDeadline     = slotToBeginPOSIXTime def 1
        , bAmount       = Ada.lovelaceValueOf 10_000_000
        }
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"solution" h2 (SolutionParams 4)
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Prelude.show s


test2 :: IO ()
test2 = Trace.runEmulatorTraceIO myTrace2

-- test consuming the bounty when the deadline has not been reached
myTrace2 :: Trace.EmulatorTrace ()
myTrace2 = do
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints @ContractError
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints @ContractError
    Trace.callEndpoint @"bounty" h1 $ BountyParams
        { bTarget       = 4
        , bDeadline     = slotToBeginPOSIXTime def 10
        , bAmount       = Ada.lovelaceValueOf 10_000_000
        }
    void $ Trace.waitUntilSlot 1
    Trace.callEndpoint @"solution" h2 (SolutionParams 2)
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Prelude.show s


testClose :: IO ()
testClose = Trace.runEmulatorTraceIO' traceConfig emulatorConfig myTraceClose

-- test consuming the bounty when the deadline has not been reached
myTraceClose :: Trace.EmulatorTrace ()
myTraceClose = do
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints @ContractError
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints @ContractError
    Trace.callEndpoint @"bounty" h1 $ BountyParams
        { bTarget       = 4
        , bDeadline     = slotToBeginPOSIXTime def 1
        , bAmount       = Ada.lovelaceValueOf 10_000_000
        }

    Trace.callEndpoint @"bounty" h2 $ BountyParams
        { bTarget       = 4
        , bDeadline     = slotToBeginPOSIXTime def 10
        , bAmount       = Ada.lovelaceValueOf 5_000_000
        }
        
    void $ Trace.waitUntilSlot 1
    Trace.callEndpoint @"close" h1 ()
    s <- Trace.waitNSlots 1
    Extras.logInfo $ "reached " ++ Prelude.show s


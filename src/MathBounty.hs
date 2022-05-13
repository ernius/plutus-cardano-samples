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

-- Simple implementation of a math bounty contract
module MathBounty where
  
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import Data.Default               (Default (..))
import Data.Void
import Data.Map as Map

import           Codec.Serialise
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>))

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
import qualified Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
import qualified Data.OpenApi.Schema as OApi
import PlutusTx.IsData.Class (toBuiltinData)
------------------------------------------------------------
-- | On-Chain code
------------------------------------------------------------

data MathBountyDatum = MathBountyDatum {
     mTarget   :: Integer
--   , mDeadline :: POSIXTime
 }

PlutusTx.unstableMakeIsData ''MathBountyDatum

-- | This method is the spending validator (which gets lifted to its on-chain representation).
--   validate that the square of the proposed value is the expected solution
--   The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
{-# INLINABLE validateSolution #-}
validateSolution :: MathBountyDatum -> Integer -> ScriptContext -> Bool
validateSolution (MathBountyDatum y) x ctx =
  traceIfFalse "Wrong guess" $ x*x == y
  -- && traceIfFalse "Deadline not reached" deadlineReached
  -- where
  --   deadlineReached :: Bool
  --   deadlineReached = (from d) `contains` (txInfoValidRange $ scriptContextTxInfo ctx)

--                                 validrange
--                               ( ..........         )
--  -----------------------(--------------------------)--> time
--                       Deadline                    

-- | Datum and redeemer parameter types
data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
bountyInstance :: Scripts.TypedValidator MathBounty
bountyInstance = Scripts.mkTypedValidator @MathBounty
  $$(PlutusTx.compile [|| validateSolution ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @MathBountyDatum @Integer

------------------------------------------------------------
-- | Off-Chain code
------------------------------------------------------------

validator :: Validator
validator = Scripts.validatorScript bountyInstance

bountyScript :: Script
bountyScript = unValidatorScript validator

-- | The address of the bounty (the hash of its validator script)
bountyAddress :: Address
bountyAddress = Ledger.scriptAddress validator

-- | Parameters for the "bounty" endpoint
data BountyParams = BountyParams
    { bTarget :: Integer
    , bAmount :: Integer
--    , bDeadline :: POSIXTime
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, OApi.ToSchema)

instance Prelude.Ord BountyParams where
  _ <= _ = True

--  | Parameters for the "solution" endpoint
newtype SolutionParams = SolutionParams
    { proposed_solution :: Integer
    }
    deriving stock (Prelude.Eq, Prelude.Show, Prelude.Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument, OApi.ToSchema)

-- | The schema of the contract, with one endpoint to publish the problem with a bounty and another to sbumit solutions
type MathBountySchema =
            Endpoint "bounty" BountyParams
        .\/ Endpoint "solution" SolutionParams

-- | The "bounty" contract endpoint.
bounty :: forall  e. AsContractError e => BountyParams -> Contract () MathBountySchema e ()
bounty (BountyParams t amt) = do
    let  datum = MathBountyDatum t 
         tx   = Constraints.mustPayToTheScript datum (lovelaceValueOf amt)
         
    ledgerTx <- submitTxConstraints bountyInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bounty of %d" $ amt

-- | The "solution" contract endpoint.
-- solution :: forall  e. AsContractError e => SolutionParams -> Contract () MathBountySchema e ()
-- solution (SolutionParams theProposal) = do
--     now <- currentTime
--     unspentOutputs <- utxosAt bountyAddress

--     -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
--     -- all unspent transaction outputs at a script address and pays them to a
--     -- public key address owned by this wallet. 
--     let tx = collectFromScript unspentOutputs theProposal
--            -- <> Constraints.mustValidateIn (from now)
--     ledgerTx <- submitTxConstraintsSpending bountyInstance unspentOutputs tx
--     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--     logInfo @String $ printf "proposed solution is %d" theProposal

-- | The "solution" contract endpoint.
solution :: forall e . AsContractError e => SolutionParams -> Contract () MathBountySchema e ()
solution (SolutionParams theProposal) = do
    logInfo @String $ "the proposal is" <> Prelude.show theProposal
    unspentOutputs <- utxosAt bountyAddress

    -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
    -- all unspent transaction outputs at a script address and pays them to a
    -- public key address owned by this wallet.
    logInfo @String $ Prelude.show $ Map.toList unspentOutputs
    case Map.toList unspentOutputs of
      (oref,a):utxos -> do
        let lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) Prelude.<>
                      Constraints.otherScript validator
        
        let tx = Constraints.mustSpendScriptOutput oref  (Redeemer $ toBuiltinData theProposal)
        logInfo @String $ printf "Trying to build tx"
        --ledgerTx <- submitTxConstraintsSpending bountyInstance unspentOutputs tx
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ printf "proposed solution is %d" theProposal
      _ ->
        logInfo @String $ printf "no bunties"



-- | Math bounty endpoints.
endpoints :: forall  e. AsContractError e => Contract () MathBountySchema e ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where
    bounty' = endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

--mkSchemaDefinitions ''MathBountySchema

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = Trace.runEmulatorTraceIO myTrace

myTrace :: Trace.EmulatorTrace ()
myTrace = do
    Extras.logInfo @String "Starts emulation"
    h1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints @ContractError
    h2 <- Trace.activateContractWallet (knownWallet 2) $ endpoints @ContractError
    Trace.callEndpoint @"bounty" h1 $ BountyParams
        { bTarget       = 4
--        , bDeadline     = slotToBeginPOSIXTime def 10
        , bAmount       = 10_000_000
        }
    void $ Trace.waitUntilSlot 20
    Trace.callEndpoint @"solution" h2 (SolutionParams 2)
    s <- Trace.waitNSlots 2
    Extras.logInfo $ "reached " ++ Prelude.show s


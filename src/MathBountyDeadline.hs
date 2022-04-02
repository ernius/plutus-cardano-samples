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


{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- Simple implementation of a math bounty contract
module MathBountyDeadline where

import           Data.Void                 (Void)
import           Control.Monad             (void)
import qualified Data.Map                  as Map
import qualified Data.ByteString.Char8     as C
import           Playground.Contract
import qualified PlutusTx                  as PlutusTx
import           PlutusTx.Prelude 

import           Plutus.Contract
import           Ledger
import           Ledger.Constraints        (TxConstraints)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Playground.Contract
import qualified Prelude
import Prelude   (String)
import           Text.Printf          (printf)

------------------------------------------------------------
-- | On-Chain code
------------------------------------------------------------

data MathBountyDatum = MathBountyDatum
    { mTarget      :: Integer
    , mDeadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''MathBountyDatum

-- | This method is the spending validator (which gets lifted to its on-chain representation).
--   validate that the square of the proposed value is the expected solution
--   The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
{-# INLINABLE validateSolution #-}
validateSolution :: MathBountyDatum -> Integer -> ScriptContext -> Bool
validateSolution (MathBountyDatum t deadline) x ctx =
  traceIfFalse "Wrong guess" $ x*x == t &&
  traceIfFalse "deadline not reached" deadlineReached
  where
    deadlineReached :: Bool
    deadlineReached = contains (from deadline) $ txInfoValidRange $ scriptContextTxInfo ctx

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

validator :: Validator
validator = Scripts.validatorScript bountyInstance

-- | Off-Chain code
-- | The address of the bounty (the hash of its validator script)
bountyAddress :: Address
bountyAddress = Ledger.scriptAddress (Scripts.validatorScript bountyInstance)

-- | Parameters for the "bounty" endpoint
data BountyParams = BountyParams
    { target    :: Integer
    , amount    :: Value
    , deadline  :: POSIXTime
    }
    deriving (Prelude.Eq, Prelude.Show, Generic, FromJSON, ToJSON, ToSchema)

--  | Parameters for the "solution" endpoint
data SolutionParams = SolutionParams
    { proposed_solution :: Integer
    }
    deriving (Prelude.Eq, Prelude.Show, Generic, FromJSON, ToJSON, ToSchema, ToArgument)

-- | The schema of the contract, with one endpoint to publish the problem with a bounty and another to sbumit solutions
type MathBountySchema =
            Endpoint "bounty" BountyParams
        .\/ Endpoint "solution" SolutionParams

-- | The "bounty" contract endpoint.
bounty :: AsContractError e => BountyParams -> Contract () MathBountySchema e ()
bounty (BountyParams t amt d) = do
    let datum = MathBountyDatum t d
        tx   = Constraints.mustPayToTheScript datum amt
        
    ledgerTx <- submitTxConstraints bountyInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bounty of %d" $ getLovelace $ fromValue amt

-- | The "solution" contract endpoint.
solution :: AsContractError e => SolutionParams -> Contract () MathBountySchema e ()
solution (SolutionParams theProposal) = do
    now   <- currentTime  
    -- filter all incorrect datum bounty scripts
    unspentOutputs <- Map.filter (hasCorrectDatum now) <$> utxosAt bountyAddress

    let redeemer = Redeemer $ PlutusTx.toBuiltinData theProposal

    if Map.null unspentOutputs
        then logInfo @String $ "no bounties available"
        else do
            logInfo @String $ "bounties are available"

            let tx = collectFromScript unspentOutputs theProposal
                  <> Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsSpending bountyInstance unspentOutputs tx
            
            -- let orefs   = fst <$> Map.toList unspentOutputs
            --     lookups = Constraints.otherScript validator Prelude.<> Constraints.unspentOutputs unspentOutputs
            --     tx :: Constraints.TxConstraints Integer MathBountyDatum
            --     tx      = mconcat [Constraints.mustSpendScriptOutput oref redeemer | oref <- orefs] <>
            --               Constraints.mustValidateIn (from now)
            -- ledgerTx <- submitTxConstraintsWith @MathBounty lookups tx

            logInfo @String $ "after submiting"            
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"


    -- -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
    -- -- all unspent transaction outputs at a script address and pays them to a
    -- -- public key address owned by this wallet. 
      where
        hasCorrectDatum :: POSIXTime -> ChainIndexTxOut -> Bool
        hasCorrectDatum now (ScriptChainIndexTxOut _ _ (Right (Datum datum)) _)    =
          case PlutusTx.fromBuiltinData datum of
          Just (MathBountyDatum t d)  -> theProposal * theProposal == t && d <= now
          Nothing                     -> False
        hasCorrectDatum _ _ = False

-- | Math bounty endpoints.
endpoints :: AsContractError e => Contract () MathBountySchema e ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where
    bounty' = endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

mkSchemaDefinitions ''MathBountySchema


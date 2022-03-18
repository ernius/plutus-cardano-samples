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
-- Simple implementation of a math bounty contract
module MathBounty where
  
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import           Playground.Contract
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>))

import           Plutus.Contract
import           Ledger                    (Address, Validator, ScriptContext, Value, scriptAddress, getCardanoTxId)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Playground.Contract
import qualified Prelude
import Prelude (String)
import           Text.Printf          (printf)
------------------------------------------------------------
-- | On-Chain code
------------------------------------------------------------


-- | This method is the spending validator (which gets lifted to its on-chain representation).
--   validate that the square of the proposed value is the expected solution
{-# INLINABLE validateSolution #-}
validateSolution :: Integer -> Integer -> ScriptContext -> Bool
validateSolution y x _ = traceIfFalse "Wrong guess" $ x*x == y

-- | Datum and redeemer parameter types
data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = Integer

-- | The script instance is the compiled validator (ready to go onto the chain)
bountyInstance :: Scripts.TypedValidator MathBounty
bountyInstance = Scripts.mkTypedValidator @MathBounty
  $$(PlutusTx.compile [|| validateSolution ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @Integer @Integer

-- | Off-Chain code
-- | The address of the bounty (the hash of its validator script)
bountyAddress :: Address
bountyAddress = Ledger.scriptAddress (Scripts.validatorScript bountyInstance)

-- | Parameters for the "bounty" endpoint
data BountyParams = BountyParams
    { target :: Integer
    , amount :: Value
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

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

-- | The "bounty" contract endpoint.
bounty :: AsContractError e => BountyParams -> Contract () MathBountySchema e ()
bounty (BountyParams t amt) = do
    let tx   = Constraints.mustPayToTheScript t amt
    ledgerTx <- submitTxConstraints bountyInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bounty of %d" $ getLovelace $ fromValue amt

-- | The "solution" contract endpoint.
solution :: AsContractError e => SolutionParams -> Contract () MathBountySchema e ()
solution (SolutionParams theProposal) = do
    unspentOutputs <- utxosAt bountyAddress

    -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
    -- all unspent transaction outputs at a script address and pays them to a
    -- public key address owned by this wallet. 
    let tx = collectFromScript unspentOutputs theProposal
    ledgerTx <- submitTxConstraintsSpending bountyInstance unspentOutputs tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "proposed solution is %d" theProposal

-- | Math bounty endpoints.
endpoints :: AsContractError e => Contract () MathBountySchema e ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where
    bounty' = endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

mkSchemaDefinitions ''MathBountySchema


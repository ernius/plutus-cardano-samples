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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Simple implementation of a math bounty contract
module MathBounty3 where
  
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import qualified Data.Map                  as Map
import           Playground.Contract
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>))

import           Plutus.Contract
import           Ledger                    
import           Ledger.Tx (ChainIndexTxOut (..))
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
--   The validation function (Datum -> Redeemer -> ScriptContext -> Bool)

-- Homework: check the script context so to validate that the outs are going to the Guessing Game
{-# INLINABLE validateSolution #-}
validateSolution :: Address -> Integer -> Integer -> ScriptContext -> Bool
validateSolution address y x ctx = traceIfFalse "Wrong guess" $ x*x == y 
                                && traceIfFalse "There is no output going to the guessing contract" condition
  where
    condition :: Bool
    condition = any ((==) address . txOutAddress) $ txInfoOutputs $ scriptContextTxInfo ctx 

-- | Datum and redeemer parameter types
data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = Integer

-- | The script instance is the compiled validator (ready to go onto the chain)
bountyInstance :: Address -> Scripts.TypedValidator MathBounty
bountyInstance p = Scripts.mkTypedValidator @MathBounty
  ($$(PlutusTx.compile [|| validateSolution ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @Integer @Integer

newtype HashedString = HashedString BuiltinByteString
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

newtype ClearString = ClearString BuiltinByteString
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HashedString @ClearString

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

------------------------------------------------------------
-- | Off-Chain code
------------------------------------------------------------

-- | The address of the bounty (the hash of its validator script)
bountyAddress :: Address -> Address
bountyAddress address = Ledger.scriptAddress (Scripts.validatorScript (bountyInstance address))

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

gameValidatorHash :: ValidatorHash
gameValidatorHash = Scripts.validatorHash gameInstance

-- | The address of the game (the hash of its validator script)
{-# INLINABLE gameAddress #-}
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Prelude.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Prelude.String -> ClearString
clearString = ClearString . toBuiltin . C.pack


-- | Parameters for the "bounty" endpoint
data BountyParams = BountyParams
    { target :: Integer
    , amount :: Value
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "solution" endpoint
data SolutionParams = SolutionParams
    { proposed_solution :: Integer
    , secret            :: String
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: String
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)    
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The schema of the contract, with one endpoint to publish the problem with a bounty and another to sbumit solutions
type MathBountySchema =
            Endpoint "bounty" BountyParams
        .\/ Endpoint "solution" SolutionParams
        .\/ Endpoint "solutionWallet" SolutionParams
        .\/ Endpoint "guess" GuessParams        

-- | The "bounty" contract endpoint.
bounty :: AsContractError e => BountyParams -> Contract () MathBountySchema e ()
bounty (BountyParams t amt) = do
    let tx   = Constraints.mustPayToTheScript t amt
    ledgerTx <- submitTxConstraints (bountyInstance gameAddress) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bounty of %d" $ getLovelace $ fromValue amt


-- | The "solution" contract endpoint that tries to put funds in the wallet
solutionWallet :: AsContractError e => SolutionParams -> Contract () MathBountySchema e ()
solutionWallet (SolutionParams theProposal _) = do
    unspentOutputs <- utxosAt (bountyAddress gameAddress)

    -- 'collectFromScript' is a function of the wallet API. It creates a tx consuming
    -- all unspent transaction outputs at a script address and pays them to a
    -- public key address owned by this wallet. 
    let tx = collectFromScript unspentOutputs theProposal
    ledgerTx <- submitTxConstraintsSpending (bountyInstance gameAddress) unspentOutputs tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "proposed solution is %d" theProposal


-- | The "solution" contract endpoint.
solution :: AsContractError e => SolutionParams -> Contract () MathBountySchema e ()
solution (SolutionParams theProposal secret) = do
    unspentOutputs <- utxosAt (bountyAddress gameAddress)

    let amt = Prelude.foldl1 (<>) (map _ciTxOutValue  (Map.elems unspentOutputs))

    let tx =  Constraints.mustPayToOtherScript gameValidatorHash (Datum $ PlutusTx.toBuiltinData $ hashString secret) amt
           <> collectFromScript unspentOutputs theProposal

    ledgerTx <- submitTxConstraintsSpending (bountyInstance gameAddress) unspentOutputs tx

    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "proposed solution is %d" theProposal


-- | The "guess" contract endpoint. See note [Contract endpoints]
guess :: AsContractError e => GuessParams -> Contract () MathBountySchema e ()
guess (GuessParams theGuess) = do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)

    let redeemer = clearString theGuess
        tx       = collectFromScript utxos redeemer

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the guess is
    -- wrong.
    logInfo @String "Submitting transaction to guess the secret word"
    ledgerTx <- submitTxConstraintsSpending gameInstance utxos tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "Submitted"


-- | Math bounty endpoints.
endpoints :: AsContractError e => Contract () MathBountySchema e ()
endpoints = awaitPromise (bounty' `select` solution' `select` solutionWallet' `select` guess') >> endpoints
  where
    bounty' = endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution
    guess' = endpoint @"guess" guess
    solutionWallet' = endpoint @"solutionWallet" solutionWallet

--mkSchemaDefinitions ''MathBountySchema


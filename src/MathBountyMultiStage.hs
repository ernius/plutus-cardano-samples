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
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Simple implementation of a math bounty contract
module MathBountyMultiStage where
  
import           Control.Monad             (void)
import           Control.Lens              (view)
import qualified Data.Map                  as Map
import qualified Data.ByteString.Char8     as C
import           Playground.Contract
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>))
import           PlutusTx.Builtins.Internal (BuiltinByteString (..))
import           Plutus.Contract
import           Ledger                    (Address, Validator, ScriptContext, Value, scriptAddress, getCardanoTxId, Datum (..), txOutValue, txOutTxOut, validatorHash )
import           Ledger.Tx
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts

import           Ledger.Ada                as Ada
import           Playground.Contract

import qualified Prelude                   as Prelude
import Prelude (String)

import           Text.Printf          (printf)
------------------------------------------------------------
-- | On-Chain code
------------------------------------------------------------

-- | This method is the spending validator (which gets lifted to its on-chain representation).
--   validate that the square of the proposed value is the expected solution
--   The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
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

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

-- | The pinlock validator
{-# INLINABLE validatePinLock #-}
validatePinLock :: HashedString -> ClearString -> ScriptContext -> Bool
validatePinLock (HashedString actual) (ClearString guess') _ = actual == sha2_256 guess'

data PinLock
instance Scripts.ValidatorTypes PinLock where
    type instance RedeemerType PinLock = ClearString
    type instance DatumType PinLock = HashedString

pinlockInstance :: Scripts.TypedValidator PinLock
pinlockInstance = Scripts.mkTypedValidator @PinLock
    $$(PlutusTx.compile [|| validatePinLock ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HashedString @ClearString

------------------------------------------------------------
-- | Off-Chain code
------------------------------------------------------------

-- | The address of the bounty (the hash of its validator script)
bountyAddress :: Address
bountyAddress = Ledger.scriptAddress (Scripts.validatorScript bountyInstance)

-- | The validator script of the game.
pinlockValidator :: Validator
pinlockValidator = Scripts.validatorScript pinlockInstance

pinlockHash :: ValidatorHash
pinlockHash = validatorHash pinlockValidator

-- | The address of the game (the hash of its validator script)
pinlockAddress :: Address
pinlockAddress = Ledger.scriptAddress pinlockValidator

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Prelude.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for unlocking by lifting the
-- string to its on-chain representation
clearString :: String -> ClearString
clearString = ClearString . BuiltinByteString . C.pack

-- | Parameters for the "bounty" endpoint
data BountyParams = BountyParams
    { target :: Integer
    , amount :: Value
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "solution" endpoint
data SolutionLockParams = SolutionParams
    { proposed_solution :: Integer
    , lockPin :: String
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "unlock" endpoint
newtype UnlockParams = UnlockParams
    { unlockPin :: String
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Endpoints
--   The schema of the contract, with one endpoint to publish the problem with a bounty and another to submit solutions
type MathBountyWithLockSchema =
            Endpoint "bounty" BountyParams
        .\/ Endpoint "solveAndLock" SolutionLockParams
        .\/ Endpoint "unlock" UnlockParams


-- | The "bounty" contract endpoint.
bounty :: AsContractError e => BountyParams -> Contract () MathBountyWithLockSchema e ()
bounty (BountyParams target amt) = do
    let tx         = Constraints.mustPayToTheScript target amt
    ledgerTx <- submitTxConstraints bountyInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bounty of %d" $ getLovelace $ fromValue amt

-- | The "solution" contract endpoint.
solution :: AsContractError e => SolutionLockParams -> Contract () MathBountyWithLockSchema e ()
solution (SolutionParams theProposal lockPin) = do
    unspentOutputs <- utxosAt bountyAddress

    -- calculate the total amount in the bounty
    let amt = Prelude.foldl1 (<>) $ map _ciTxOutValue (Map.elems unspentOutputs)

    let tx =  Constraints.mustPayToOtherScript pinlockHash (Datum $ PlutusTx.toBuiltinData (hashString lockPin)) amt     -- lock the amount 
          <>  collectFromScript unspentOutputs theProposal

    ledgerTx <- submitTxConstraintsSpending bountyInstance unspentOutputs tx

    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bounty of %d" $ getLovelace $ fromValue amt

-- | The "unlock" contract endpoint. See note [Contract endpoints]
unlock :: AsContractError e => UnlockParams -> Contract () MathBountyWithLockSchema e ()
unlock (UnlockParams unlockPin) = do
    unspentOutputs <- utxosAt pinlockAddress
    
    let redeemer = clearString unlockPin
        tx       = collectFromScript unspentOutputs redeemer
        
    ledgerTx <- submitTxConstraintsSpending pinlockInstance unspentOutputs tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "unlock" 

-- | endpoints.
endpoints :: AsContractError e => Contract () MathBountyWithLockSchema e ()
endpoints = awaitPromise (bounty' `select` solution' `select` unlock') >> endpoints
  where
    bounty' = endpoint @"bounty" bounty
    solution' = endpoint @"solveAndLock" solution
    unlock' = endpoint @"unlock" unlock
    
--mkSchemaDefinitions ''MathBountyWithLockSchema


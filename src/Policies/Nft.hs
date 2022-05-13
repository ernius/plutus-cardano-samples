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

module Policies.Nft where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf                 (printf)
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet

import           Utils                       (getCredentials)

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_curr, tn', amt')] -> tn' == tn && amt' == amt -- not needed curr == ownCurrencySymbol ctx &&
        _                    -> False

tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn
-- tokenCurSymbol oref tn amt = scriptCurrencySymbol  (tokenPolicy oref tn amnt)

-- Off chain code
data TokenParams = TokenParams
    { tpToken   :: !TokenName
    , tpAmount  :: !Integer
    , tpAddress :: !Address
    } deriving (Prelude.Eq, Prelude.Ord, Generic, FromJSON, ToJSON, ToSchema, Show)

adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
    unsigned <- balanceTx unbalanced
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned
    Contract.logDebug @String $ printf "signed: %s" $ show signed
    return signed

adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ Constraints.typedValidatorLookups inst

type FreeSchema = Endpoint "mint" TokenParams

mintToken :: TokenParams -> Contract w s Text CurrencySymbol
mintToken tp = do
    Contract.logDebug @String $ printf "started minting: %s" $ show tp
    let addr = tpAddress tp
    case getCredentials addr of
        Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got %s" $ show addr
        Just (x, my) -> do
            oref <- getUnspentOutput
            o    <- fromJust <$> Contract.txOutFromRef oref
            Contract.logDebug @String $ printf "picked UTxO at %s" (show oref) 

            let tn          = tpToken tp
                amt         = tpAmount tp
                cs          = tokenCurSymbol oref tn amt
                val         = Value.singleton cs tn amt
                c           = case my of
                    Nothing -> Constraints.mustPayToPubKey x val
                    Just y  -> Constraints.mustPayToPubKeyAddress x y val
                lookups     = Constraints.mintingPolicy (tokenPolicy oref tn amt) <>
                              Constraints.unspentOutputs (Map.singleton oref o)
                constraints = Constraints.mustMintValue val          <>
                              Constraints.mustSpendPubKeyOutput oref <>
                              c

            -- Check error that happens
            -- ledgerTx <- submitTxConstraintsWith @Void lookups constraints
            -- Fix previous error
            ledgerTx <- adjustAndSubmitWith @Void lookups constraints
            
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx            
            Contract.logInfo @String $ printf "minted %s" (show val)
            return cs

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mintToken

test :: Prelude.IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ TokenParams
        { tpToken  = tn
        , tpAmount = 1
        , tpAddress = mockWalletAddress (knownWallet 1)
        }
    callEndpoint @"mint" h2 $ TokenParams
        { tpToken  = tn
        , tpAmount = 1
        , tpAddress = mockWalletAddress (knownWallet 2)
        }
    void $ Emulator.waitNSlots 1

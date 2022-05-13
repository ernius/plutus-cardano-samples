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

module Oracle.Funds where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract hiding (when)
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          ((<$>), String, show)
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value


-- ownFunds :: AsContractError e => Contract w s e Value
-- ownFunds = do
--     pk    <- Contract.ownPaymentPubKeyHash
--     utxos <- utxosAt $ pubKeyAddress pk Nothing
--     let v = mconcat $ Map.elems $ _ciTxOutAddress <$> utxos
--     logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
--     return v

-- ownFunds' :: AsContractError e => Contract (Last Value) w e ()
-- ownFunds' = do
--     handleError logError $ ownFunds >>= tell . Last . Just
--     void $ Contract.waitNSlots 1
--     ownFunds'

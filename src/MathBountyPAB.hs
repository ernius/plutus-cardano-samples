{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MathBountyPAB where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           Data.Text
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)
import           Plutus.V1.Ledger.Ada
import           MathBounty

data MathBountyContracts = Bounty BountyParams | Solution SolutionParams
    deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty MathBountyContracts where
    pretty = viaShow

instance HasDefinitions MathBountyContracts where

    getDefinitions        = [Bounty exampleBounty , Solution exampleSolution]

    getContract (Bounty b)   = SomeBuiltin $ bounty @Text b
    getContract (Solution s) = SomeBuiltin $ solution @Text s

    getSchema = const $ endpointsToSchemas @Empty

exampleBounty :: BountyParams
exampleBounty = BountyParams 9 1_000_000 --(fromInteger 10000)

exampleSolution :: SolutionParams
exampleSolution = SolutionParams 3


import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import           Codec.Serialise
import           MathBountyPAB
import           MathBounty
import qualified PlutusTx

import Cardano.Ledger.Alonzo.TxInfo

main :: IO ()
main = 
  writePlutusScript "mathbounty.plutus" contractSerialised contractSBS

contractSBS :: SBS.ShortByteString
contractSBS =  SBS.toShort . LBS.toStrict $ serialise bountyScript

contractSerialised :: PlutusScript PlutusScriptV1
contractSerialised = PlutusScriptSerialised contractSBS

mathBountyRedeemJSON:: LBS.ByteString
mathBountyRedeemJSON = Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusTx.toData (3 :: Integer)

mathBountyDatumJSON:: LBS.ByteString
mathBountyDatumJSON = Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusTx.toData (MathBountyDatum 9)

writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript filename scriptSerial scriptSBS = do
  LBS.writeFile "redeemer.json" mathBountyRedeemJSON
  LBS.writeFile "datum.json" mathBountyDatumJSON
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> do
                    print ("Ex Budget" :: String) >> print exbudget
                    print ("Ex Units" :: String)  >> print (exBudgetToExUnits exbudget)
        Nothing -> error "defaultCostModelParams failed"
  result  <- writeFileTextEnvelope filename Nothing scriptSerial

  case result of
    Left err -> print $ displayError err
    Right () -> return ()

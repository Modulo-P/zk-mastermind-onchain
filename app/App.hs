

module Main where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short  as B
import           Mastermind             (compiledZkValidator)
import           PlutusLedgerApi.V2     (Datum (getDatum),
                                         FromData (fromBuiltinData),
                                         Interval (..), PubKeyHash (PubKeyHash),
                                         TxInInfo (TxInInfo, txInInfoResolved),
                                         TxOut (..), Value, from,
                                         serialiseCompiledCode)
import           PlutusTx               (CompiledCode, compile, getPlc,
                                         toBuiltinData)


main :: Prelude.IO ()
main = do
  B.writeFile "./output/mastermind.uplc" . Base16.encode $ B.fromShort serialisedScript
  where
    script = compiledZkValidator
    serialisedScript = serialiseCompiledCode script




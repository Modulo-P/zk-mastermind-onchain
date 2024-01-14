{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

--{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-pir #-}

module Main where

import Mastermind (zkValidator)
import Types (GameDatum, GameRedeemer)
import PlutusTx (compile, CompiledCode, toBuiltinData)
import PlutusTx.Prelude (Bool,(.),($))
import PlutusLedgerApi.V2 (serialiseCompiledCode, ScriptContext)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as B
import Prelude qualified


compiledZkValidator :: CompiledCode (GameDatum -> GameRedeemer -> ScriptContext -> Bool)
compiledZkValidator = $$(compile [|| zkValidator ||])


main :: Prelude.IO ()
main = B.writeFile "./output/mastermind.uplc" . Base16.encode $ B.fromShort serialisedScript
  where
    script = compiledZkValidator 
    serialisedScript = serialiseCompiledCode script




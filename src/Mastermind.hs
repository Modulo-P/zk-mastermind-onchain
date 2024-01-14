{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}


module Mastermind where

import PlutusLedgerApi.V2 (from, Datum (getDatum), FromData (fromBuiltinData), PubKeyHash (PubKeyHash), TxInInfo (TxInInfo, txInInfoResolved), TxOut (..), Value, serialiseCompiledCode)
import PlutusLedgerApi.V2.Contexts
  ( ScriptContext (scriptContextTxInfo),
    TxInfo(txInfoValidRange),
    findOwnInput,
    getContinuingOutputs,
    txSignedBy,
  )
import PlutusLedgerApi.V1.Interval  (contains)
import PlutusLedgerApi.V2.Tx  (OutputDatum (..))
import PlutusTx (compile, CompiledCode, toBuiltinData)
import PlutusTx.Trace 
import PlutusTx.Prelude 
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as B
import Types (GameDatum (..), GameRedeemer (..), Turn (..))
import Groth16 (VerificationKey, verify)

instance Eq VerificationKey where
  (==) vk1 vk2 = (toBuiltinData vk1) == (toBuiltinData vk2)

{-# INLINEABLE zkValidator #-}
zkValidator :: GameDatum -> GameRedeemer -> ScriptContext -> PlutusTx.Prelude.Bool
zkValidator d r ctx = case turn r of
  Start ->
    traceIfFalse "Incorrect turn counter" (currentTurn d == 0)
      && traceIfFalse "Value not conserved" ( valueFromScript == valueToScript + valueToScript)
      && traceIfFalse "Guess incorrect length" (length (guesses getNewDatum) == 4)
      && traceIfFalse "Incorrect new datum" (currentTurn getNewDatum == 1)
      && traceIfFalse "Tx not signed" (txSignedBy txInfo (codeBreaker getNewDatum))
      && traceIfFalse "Signatures alteration" (codeMaster d == codeMaster getNewDatum)
      && traceIfFalse "Hashsol cannot be modified" (hashSol d == hashSol getNewDatum)
  Guess ->
    traceIfFalse "Incorrect turn counter" (currentTurn d + 1 == currentTurn getNewDatum)
      && traceIfFalse "Incorrect turn order" (modulo (currentTurn d) 2 == 0)
      && traceIfFalse "Closed party" (currentTurn d < 10) 
      && traceIfFalse "Value not conserved" (valueToScript == valueFromScript)
      && traceIfFalse "Guess incorrect length" (length (guesses getNewDatum) == 4)
      && traceIfFalse "Tx not signed" (txSignedBy txInfo (codeBreaker d))
      && traceIfFalse "Signatures alteration" (codeMaster d == codeMaster getNewDatum) && (codeBreaker d == codeBreaker getNewDatum)
      && traceIfFalse "Hashsol cannot be modified" (hashSol d == hashSol getNewDatum)
      && traceIfFalse "Vk cannot be modified" (vk d == vk getNewDatum)
      && traceIfFalse "Wrong expiration set" (expirationTime getNewDatum  == expirationTime d + 3600000)
  Clue ->
    traceIfFalse "Incorrect turn counter" (currentTurn d + 1 == currentTurn getNewDatum)
      && traceIfFalse "Incorrect turn order" (modulo (currentTurn d) 2 == 1)
      && traceIfFalse "Closed party" (currentTurn d <= 10)
      && traceIfFalse "Value not conserved" (valueToScript == valueFromScript)
      && traceIfFalse "Tx not signed" (txSignedBy txInfo (codeMaster d))
      && traceIfFalse "Signatures alteration" (codeMaster d == codeMaster getNewDatum) && (codeBreaker d == codeBreaker getNewDatum)
      && traceIfFalse "Hashsol cannot be modified" (hashSol d == hashSol getNewDatum)
      && traceIfFalse "Vk cannot be modified" (vk d == vk getNewDatum)
      && traceIfFalse "Wrong expiration set" (expirationTime getNewDatum  == expirationTime d + 3600000)
      && traceIfFalse "zk-proof failure" (verify (vk d) (proof r) ([(hashSol d)] ++ (guesses d) ++ [(whitePegs d)] ++ [(blackPegs d)]))
  End -> 
    (blackPegs d == 4 && (modulo (currentTurn d) 2 == 0) && txSignedBy txInfo (codeBreaker d)) -- CodeBreaker wins
      || (blackPegs d < 4 && currentTurn d == 10 && txSignedBy txInfo (codeMaster d)) -- CodeMaster wins
      || expirationReached && (modulo (currentTurn d) 2 == 1) && txSignedBy txInfo (codeBreaker d) --  CodeBreaker wins by deafult.
      || expirationReached && (modulo (currentTurn d) 2 == 0) && txSignedBy txInfo (codeMaster d) --  CodeMaster wins by deafult.
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    valueFromScript :: Value
    valueFromScript = case findOwnInput ctx of 
        Just TxInInfo {txInInfoResolved = TxOut {txOutValue = v}} -> v
        Nothing -> traceError "Tx not found"

    valueToScript :: Value
    valueToScript = mconcat (txOutValue <$> getContinuingOutputs ctx)

    getOutputToContract :: TxOut
    getOutputToContract = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "exactly one output expected"

    getNewDatum :: GameDatum
    getNewDatum = case txOutDatum getOutputToContract of
      OutputDatum ns -> case fromBuiltinData (getDatum ns) of
        Just nd -> nd
        Nothing -> traceError "datum wrong type"
      _ -> traceError "datum not found"

    expirationReached :: Bool
    expirationReached = contains (from $ expirationTime d) $ txInfoValidRange txInfo


compiledZkValidator :: CompiledCode (GameDatum -> GameRedeemer -> ScriptContext -> Bool)
compiledZkValidator = $$(compile [|| zkValidator ||])







{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Types where

import PlutusTx.Prelude
import GHC.Generics (Generic)
import PlutusLedgerApi.V2(PubKeyHash, POSIXTime)
import PlutusTx (unstableMakeIsData)
import Groth16 (Proof, VerificationKey)

unstableMakeIsData ''VerificationKey
unstableMakeIsData ''Proof

data GameDatum = GameDatum
  { codeMaster :: PubKeyHash,
    codeBreaker :: PubKeyHash,
    hashSol :: Integer,
    guesses :: [Integer],
    blackPegs :: Integer,
    whitePegs :: Integer,
    currentTurn :: Integer,
    expirationTime :: POSIXTime,
    -- Verification
    vk :: VerificationKey,
    proof :: Proof
  }
  
  deriving (Generic)
unstableMakeIsData ''GameDatum



data GameRedeemer = Start | Clue | End | Guess
unstableMakeIsData ''GameRedeemer
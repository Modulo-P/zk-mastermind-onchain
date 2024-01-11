{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import PlutusTx.Prelude
import GHC.Generics (Generic)
import PlutusLedgerApi.V2(PubKeyHash, POSIXTime)
import PlutusTx (unstableMakeIsData)

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
    nPublic :: Integer,
    vkAlpha1 :: [Integer],
    vkBeta2 :: [[Integer]],
    vkGamma2 :: [[Integer]],
    vkDelta2 :: [[Integer]],
    vkAlphabeta12 :: [[[Integer]]],
    ic :: [[Integer]],
    -- Proof
    piA :: [Integer],
    piB :: [[Integer]],
    piC :: [Integer]
  }
  deriving (Generic)

unstableMakeIsData ''GameDatum

data GameRedeemer = Start | Clue | End | Guess

unstableMakeIsData ''GameRedeemer
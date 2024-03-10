{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types where

import           GHC.Generics       (Generic)
import           Groth16            (Proof, VerificationKey)
import           PlutusLedgerApi.V2 (POSIXTime, PubKeyHash)
import           PlutusTx           (unstableMakeIsData)
import           PlutusTx.Prelude

-- unstableMakeIsData ''VerificationKey
-- unstableMakeIsData ''Proof

data GameDatum = GameDatum
  { codeMaster     :: PubKeyHash,
    codeBreaker    :: PubKeyHash,
    hashSol        :: Integer,
    guesses        :: [Integer],
    blackPegs      :: Integer,
    whitePegs      :: Integer,
    currentTurn    :: Integer,
    expirationTime :: POSIXTime,
    -- Verification
    vk             :: VerificationKey,
    proof          :: Proof
  }

  deriving (Generic)
unstableMakeIsData ''GameDatum



data GameRedeemer = Start | Clue | End | Guess
unstableMakeIsData ''GameRedeemer

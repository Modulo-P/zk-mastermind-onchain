{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:verbosity=2 #-}

--import Cardano.Ledger.Babbage.PParams qualified as Babbage
import PlutusLedgerApi.V2 (Value, PubKeyHash, ScriptContext, TxOutRef)

import Plutus.Model (spendScript, utxoAt, userSpend, UserSpend, noErrors,DatumMode (InlineDatum), payToScript, submitTx,Tx, testNoErrors,defaultBabbage,mustFail, adaValue, Run, newUser,ada,Ada (Lovelace),TypedValidator (TypedValidator), toV2)
import Plutus.Model.V2 (mkTypedValidator, toBuiltinValidator, spend)
import Test.Tasty (defaultMain, testGroup)
import PlutusTx.Prelude (($),(.), Integer, Bool (..))
import Control.Monad (replicateM)
import Mastermind (zkValidator, compiledZkValidator)
import Types (GameDatum (..), GameRedeemer (..), Turn (..))
import Groth16 (Proof, VerificationKey)
import SnarkInputs (verificationkey, proof01)
import Prelude (IO, mconcat)
import PlutusTx (compile)


main :: Prelude.IO ()
main = do  
  defaultMain $ do 
    testGroup "Testing Mastermind Game Logic "
      [
        testGroup "Full Game Tests"
          [ 
            good "Full Game" testFullGame
          ]
      ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 1000000000) defaultBabbage
    --good = testNoErrors (adaValue 100000000) (skipLimits (defaultBabbage {mockConfigProtocol = babbaPParams}))

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 5000000)

--type SnarkScript = TypedValidator datum0 redeemer0

--snarkValidator :: GameDatum -> GameRedeemer -> SnarkScript
--snarkValidator = TypedValidator . toV2 . compiledZkValidator

{--
{-# INLINABLE mkBetRefValidator #-}
-- | Untyped wrapper around `mkBetRefValidator'`.
mkBetRefValidator :: BetRefParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBetRefValidator params dat' red' ctx'
  | mkBetRefValidator' params (unsafeFromBuiltinData dat') (unsafeFromBuiltinData red') (unsafeFromBuiltinData ctx') = ()
  | otherwise                     = error ()

-- | Generates validator given params.
betRefValidator :: BetRefParams -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
betRefValidator betRefParams =
    $$(PlutusTx.compile [|| mkBetRefValidator||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 betRefParams


--}

type SnarkValidator = TypedValidator GameDatum GameRedeemer

snarkValidator :: SnarkValidator
snarkValidator = mkTypedValidator $$(PlutusTx.compile [|| toBuiltinValidator zkValidator ||])

--snarkValidator = TypedValidator $ toV2 $ fromCompiledCode $$(PlutusTx.compile [|| zkValidator ||])



-- [Turn 0](CodeMaster): Create transaction that deploy the game 

datum0 :: PubKeyHash -> PubKeyHash ->  GameDatum
datum0 pkh1 pkh2   = 
  GameDatum 
    {
      codeMaster = pkh1,
      codeBreaker = pkh2,
      hashSol = 50470988812045707708375033903681453285217735455216815939546570682147418599455,
      guesses = [],
      blackPegs = 0,
      whitePegs = 0,
      currentTurn = 0,
      expirationTime = 1000,
      vk = verificationkey
    } 

turn0 :: GameDatum -> UserSpend -> Value  -> Tx
turn0 dat usp val = 
  mconcat
    [ 
      userSpend usp,
      payToScript snarkValidator (InlineDatum dat) val
    ]


datum1 :: PubKeyHash -> PubKeyHash ->  GameDatum
datum1 pkh1 pkh2   = 
  GameDatum 
    {
      codeMaster = pkh1,
      codeBreaker = pkh2,
      hashSol = 50470988812045707708375033903681453285217735455216815939546570682147418599455,
      guesses = [1,2,3,4],
      blackPegs = 0,
      whitePegs = 0,
      currentTurn = 1,
      expirationTime = 3601000,
      vk = verificationkey
    } 

rdm1 = GameRedeemer { proof = proof01, turn = Start }

turn1 :: GameDatum -> GameRedeemer -> TxOutRef -> UserSpend -> Value -> Tx 
turn1 dat redeemer outRef usp val =
  mconcat
    [ spendScript snarkValidator outRef redeemer dat
    , userSpend usp
    , payToScript snarkValidator (InlineDatum dat) val
    ]
 

testFullGame :: Run Bool
testFullGame = do 
  -- Turn 0 
  users <- setupUsers
  let [codeMaster,codeBreaker] = users
  let bet = (adaValue 2)
  sp <- spend codeMaster bet
  let tx0 = turn0 (datum0 codeMaster codeBreaker) sp bet
  submitTx codeMaster tx0

  -- Turn 1
  utxos <- utxoAt snarkValidator
  let [(snarkOutRef, snarkOut)] = utxos
  let bet' = (adaValue 4)
  sp <- spend codeBreaker bet
  let tx1 = turn1 (datum0 codeMaster codeBreaker) rdm1 snarkOutRef sp bet'
  submitTx codeBreaker tx1
  --isOk <- noErrors 
  noErrors






{--
customBabbageParamsFn :: Babbage.PParams (BabbageEra StandardCrypto) -> Babbage.PParams (BabbageEra StandardCrypto)
customBabbageParamsFn pp =
  pp
    { Babbage._maxBlockExUnits = Alonzo.ExUnits 100000000000000000000000000000000000 100000000000000000000000000000000000,
      Babbage._maxTxExUnits = Alonzo.ExUnits 100000000000000000000000000000000000 100000000000000000000000000000000000
    }

getDefaultParams :: PParams -> Babbage.PParams (BabbageEra StandardCrypto)
getDefaultParams (BabbageParams a) = a
getDefaultParams _ = error "error: not a BabbageParams"

babbaPParams :: PParams
babbaPParams = BabbageParams $ customBabbageParamsFn $ getDefaultParams defaultBabbageParams
--}

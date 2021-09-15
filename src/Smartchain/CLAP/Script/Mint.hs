{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Smartchain.CLAP.Script.Mint
    ( runWithUserInteraction ) where
    -- , run) where

import Shh

import Smartchain.Adapter.Cardano.CardanoCLI
import           Prelude
import           Ledger
import Data.Aeson
import qualified Data.Maybe as M
import Smartchain.CLAP.Contract.MonetaryPolicy


import           Data.List.Split

import qualified Data.ByteString.Lazy.UTF8 as BLU


{-# ANN module "HLINT: ignore Use camelCase" #-}


load SearchPath ["echo","ssh","cat","scp"]

runWithUserInteraction :: IO ()
runWithUserInteraction = do
    echo "------------------------------------------------------"
    echo "Creating Token with one-time minting policy open burning policy "
    echo "------------------------------------------------------"
    echo "Node tip"
    echo "-------------------------"

    query_tip

    echo "-------------------------"

    tokenName       <- echo "-n" "> Token Name : "             >>  getLine
    amount          <- echo "-n" "> Total Supply to Mint : "   >>  read @Int <$> getLine

    minterAddr     <- echo "-n" "> Minter address : "           >> getLine

    echo "-------------------------"
    echo "Minter Wallet Content :"
    echo "-------------------------"
    query_utxo minterAddr
    echo "-------------------------"

    utxiWithCollateral       <- echo "-n" "> UTxO(TxHash#TxIx) for collateral :" >> getLine
    (txIdGiven,txIndexGiven) <- echo "-n" "> UTxO(TxHash#TxIx) used for creating the Token :"
                        >> (\case
                                [a,b] -> (a, read @Integer b)
                                _ -> error "unexpected input") . splitOn "#" <$> getLine

    let txOutRef = TxOutRef ((M.fromJust . decode . BLU.fromString )  ("{\"getTxId\" : \"" <> txIdGiven <> "\"}")) txIndexGiven
        monetaryPolicy = mkCLAPMonetaryPolicyScript txOutRef
        policyhash = scriptCurrencySymbol monetaryPolicy

    echo "-------------------------"
    echo $ "Policy hash will be : " <> show policyhash
    echo "-------------------------"

    privateKeyPath <- echo "-n" "> Minter Private key path : "  >> getLine

    monetaryScriptFilePath <- register_minting_script_file monetaryPolicy
    run_tx privateKeyPath
            [ "--tx-in"  , txIdGiven <> "#" <>  show txIndexGiven
            , "--tx-out" , minterAddr <> " + 1344798 lovelace + " <> show amount <> " " <> show policyhash <> "." <> tokenName
            , "--tx-in-collateral", utxiWithCollateral
            , "--change-address"  , minterAddr
            , "--mint" , show amount <> " " <> show policyhash <> "." <> tokenName
            , "--mint-script-file" , monetaryScriptFilePath
            , "--mint-redeemer-value",  "[]"]
            
    echo "------------------------------------------------------"
    echo "Done"
    echo "------------------------------------------------------"


{- -- cardano-cli transaction build 
    --alonzo-era 
	--testnet-magic $(cat magic ) 
	--tx-in 95f644032e4e2f516ddee5ae9ceb8d467f53f630c4d4f481771cb56063adb244#0 
	--tx-out "$(cat address) + 1344798 lovelace + 1000000000000 $(cat alwaystrue.policyid).CLAP" 
	--tx-in-collateral 3f2c36bed71474b2f18642efeb4db3890c984304d2b723169264552d97446783#1 
	--change-address $(cat address ) 
    --mint="1000000000000 $(cat alwaystrue.policyid).CLAP" 
	--mint-script-file alwaystrue.plutus 
	--mint-redeemer-value [] 
	--protocol-params-file protocol_params --out-file minting_clap.tx #-}


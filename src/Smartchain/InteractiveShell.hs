{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Smartchain.InteractiveShell (main) where

import Shh ( load, ExecReference(SearchPath) )
import Data.Function ((&))
import  qualified Smartchain.CLAP.Script.Mint as Mint
import  qualified Smartchain.CLAP.Script.Transfer as Transfer
import qualified Smartchain.Adapter.Cardano.CardanoCLI as CardanoCLI
import Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )
import Byline.Menu
    ( runBylineT,
      text,
      askWithMenuRepeatedly,
      menu,
      menuSuffix,
      Stylized,
      ToStylizedText(..),
      Menu )
import Data.Text ( Text )
import Data.Maybe ( fromJust )
import Byline.Internal.Stylized ()

load SearchPath ["echo","ssh","cat"]

main :: IO ()
main = do
    echo "----------------------"
    echo "Select An action"
    echo "----------------------"
    showActionMenu >>=
     \case
        ListWallet   -> do
          echo "-----------------------------------"
          wallets <-  CardanoCLI.query_registered_wallets
          mapM_ (echo . show ) wallets
          echo "-----------------------------------"
          main
        NewWallet  -> do
          echo "-----------------------------------"
          walletName <- echo "-n" "> Wallet Name : " >>  getLine
          CardanoCLI.register_shelley_wallet walletName
          echo "Wallet Created and Registered!"
          echo "-----------------------------------"
          main
        Mint   -> Mint.runWithUserInteraction >> main
        Transfer  -> Transfer.run  >> main
        Burn  ->  main




showActionMenu :: IO Action
showActionMenu = fmap fromJust (runBylineT $ askWithMenuRepeatedly menuConfig prompt onError)

menuConfig :: Menu Action
menuConfig = menu actions & menuSuffix "- "

prompt :: Stylized Text
prompt = text "> please choose an action (provide the index) : "

onError :: Stylized Text
onError = text "> please choose an action (provide the index) : "

actions :: NonEmpty Action
actions = NonEmpty.fromList [
    ListWallet,
    NewWallet,
    Mint,
    Burn
    ]

data Action
  = ListWallet
  | NewWallet
  | Transfer
  | Mint
  | Burn
  deriving (Show)


instance ToStylizedText Action where
  toStylizedText item = case item of
    ListWallet -> "Show Wallet Registered"
    NewWallet  -> "Create and Register a new Shelley Wallet"
    Transfer   -> "Transfer Crypto"
    Mint       -> "Mint a Native Token"
    Burn       -> "Burn a Native Token"

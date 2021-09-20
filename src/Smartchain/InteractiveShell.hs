{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Smartchain.InteractiveShell (main) where

import Shh 
import Data.Function ((&))
import  qualified Smartchain.CLAP.Script.Mint as Mint
import  qualified Smartchain.CLAP.Script.Transfer as Transfer
import Smartchain.Adapter.Cardano.CardanoCLI as CardanoCLI
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
import Smartchain.Wallet.Script (selectWallet) 

load SearchPath ["echo","ssh","cat"]

main :: IO ()
main = do
    echo "----------------------"
    echo "Select An action"
    echo "----------------------"
    showActionMenu >>=
     \case
        WalletList -> listWallet 
        WalletAdd  -> addWallet
        WalletRemove  -> echo "TODO"
        TokenMint  -> 
          echo "Select the Minter Wallet :"
          >>  selectWallet 
          >>= \case 
              Nothing -> 
                echo "No Wallet Registered !"
              Just wallet -> Mint.mintI wallet 
        TokenBurn  ->  echo "TODO"
        Transfer   -> Transfer.run 
        ReceiveByFaucet -> receiveByFaucet  
    main


addWallet :: IO ()
addWallet = do
  echo "-----------------------------------"
  walletName <- echo "-n" "> Wallet Name : " >>  getLine
  CardanoCLI.register_shelley_wallet walletName
  echo "Wallet Created and Registered!"
  echo "-----------------------------------"

receiveByFaucet :: IO ()
receiveByFaucet = do 
  echo "-----------------------------------"
  echo "Visit this website for asking ADAs :"
  echo "https://testnets.cardano.org/en/testnets/cardano/tools/faucet/"
  echo "-----------------------------------"

listWallet :: IO ()
listWallet = do
  CardanoCLI.query_registered_wallets
   >>= \case 
         Nothing -> echo "No Wallet Registered!"
         Just wallets ->  mapM_ (\Wallet{..} ->  
            echo "######################"
              <> echo ("Name : " <> name)
              <> echo ("Payment Address : " <> paymentAddress)
              <> query_utxo paymentAddress) wallets
  echo "######################"
  


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
    WalletList,
    WalletAdd,
    WalletRemove,
    TokenMint,
    TokenBurn,
    Transfer,
    ReceiveByFaucet
    ]

data Action
  = WalletList
  | WalletAdd
  | WalletRemove
  | TokenMint
  | TokenBurn
  | Transfer
  | ReceiveByFaucet
  deriving (Show)


instance ToStylizedText Action where
  toStylizedText item = case item of
    WalletList   -> "[Wallet] - Show ones registered"
    WalletAdd    -> "[Wallet] - Add "
    WalletRemove -> "[Wallet] - Remove (TODO)"
    TokenMint    -> "[Token]  - Mint"
    TokenBurn    -> "[Token]  - Burn (TODO)"
    Transfer     -> "Transfer "
    ReceiveByFaucet -> "Ask ADAs from Faucet (Testnet)"

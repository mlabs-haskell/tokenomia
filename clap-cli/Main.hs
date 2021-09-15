{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Main (main) where

import Shh ( load, ExecReference(SearchPath) )

import  qualified Smartchain.InteractiveShell as InteractiveShell

main :: IO ()
main = InteractiveShell.main


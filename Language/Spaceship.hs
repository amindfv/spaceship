{-# LANGUAGE NoImplicitPrelude #-}
module Language.Spaceship
   ( module Prelude
   , module Data.List
   , module Language.Spaceship.GbTree
   ) where

import Prelude
   -- Add what you need from the prelude here:
   ( Integer , Float , Bool , Char , String , Int
   , IO() , Maybe(..)
   , Eq(..) , Show(..) , Ord(..)
   , head , tail , length
   , ($)
   , putStrLn
   )
import Data.List
   ( (\\) )
import Language.Spaceship.GbTree
-- import Language.Spaceship.GenServer

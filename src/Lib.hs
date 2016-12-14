{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Shelly

default (Text)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

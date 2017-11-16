#!/usr/bin/env stack
-- stack --resolver lts-9.6 script

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Foldable
import Data.Scientific
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment

main :: IO ()
main = do
  bs <- LBS.getContents
  case eitherDecode bs of
    Left e -> fail e
    Right v ->
      for_ (prune v) $ \(name, time) ->
        putStrLn $
          T.unpack name
            ++ ";"
            ++ formatScientific Exponent (Just 3 {- sig. fig. -}) time

prune :: Value -> [(Text, Scientific)]
prune (Array a) | V.length a >= 3 && a V.! 0 == String "criterio" = pruneContents (a V.! 2)
prune v = error "Unrecognized structure"

pruneContents :: Value -> [(Text, Scientific)]
pruneContents (Array a) = fmap pruneItem (toList a)

pruneItem :: Value -> (Text, Scientific)
pruneItem (Object o) = (name, time)
  where
    String name = o HM.! "reportName"
    Object results = o HM.! "reportAnalysis"
    Object mean = results HM.! "anMean"
    Number time = mean HM.! "estPoint"

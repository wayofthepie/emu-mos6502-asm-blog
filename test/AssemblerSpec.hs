{-# LANGUAGE OverloadedStrings #-}
module AssemblerSpec where

import qualified Data.Text as T
import Text.Megaparsec

import Assembler

import Test.Hspec.Megaparsec
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

spec :: Spec
spec = do
  asmSpec

asmSpec = do
  describe "byte" $
    it "should parse two consecutive characters in the hex range into a two character string" $
      property prop_byte_parse

newtype TwoCharHexString = TwoCharHexString T.Text deriving Show

instance Arbitrary TwoCharHexString where
  arbitrary = do
    upper <- choose ('A', 'F')
    lower <- choose ('a', 'f')
    num   <- choose ('0', '9')
    let vals = [upper, lower, num]
    x <- elements vals
    y <- elements vals
    pure $ TwoCharHexString (T.pack (x:[y]))

prop_byte_parse t@(TwoCharHexString s) = parse byte "" s  `shouldParse` s

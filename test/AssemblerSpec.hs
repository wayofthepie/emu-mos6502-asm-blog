{-# LANGUAGE OverloadedStrings #-}
module AssemblerSpec where

import qualified Data.Text as T
import Text.Megaparsec

import Assembler

import Test.Hspec.Megaparsec
import Test.QuickCheck.Instances
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Debug.Trace
spec :: Spec
spec = asmSpec

asmSpec = do
  describe "byte" $ do
    it "should parse two consecutive characters in the hex range into a two character string" $ do
      property prop_byte_parseValidData
    it "should parse two chars and leave the rest of the string unconsumed, if successful" $ do
      property prop_byte_parseSuccessShouldNotConsume

--------------------------------------------------------------------------------
-- byte
--------------------------------------------------------------------------------
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

newtype ValidFirstTwoChars = ValidFirstTwoChars T.Text deriving Show

-- Shoud parse valid two char hexstring.
prop_byte_parseValidData (TwoCharHexString s) = parse byte "" s  `shouldParse` s

-- When successful should not consume more input.
prop_byte_parseSuccessShouldNotConsume (TwoCharHexString s) extra =
  runParser' byte (initialState (T.append s extra)) `succeedsLeaving` extra


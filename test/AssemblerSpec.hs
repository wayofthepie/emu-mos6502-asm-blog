{-# LANGUAGE OverloadedStrings #-}
module AssemblerSpec where

import Data.Monoid ((<>))
import Data.Char (isAlpha)
import qualified Data.Text as T
import Text.Megaparsec hiding (Label, label)

import Assembler

import Test.Hspec.Megaparsec
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Instances
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (label)

spec :: Spec
spec = asmSpec

asmSpec = do
  describe "byte" $ do
    it "should parse two consecutive characters in the hex range into a two character string" $
      property prop_byte_parseValidData
    it "should parse two chars and leave the rest of the string unconsumed, if successful" $
      property prop_byte_parseSuccessShouldNotConsume

  describe "mnemonic" $ do
    it "should parse a valid mnemonic string" $
      property prop_mnemonic_parseValidMnemString

  describe "label" $ do
    it "should parse a valid label string" $
      property prop_label_validLabelString
    it "should fail to parse string that starts with a non-alpha character" $
      property prop_label_invalidLabelString

  describe "labelAssign" $ do
    it "should discard ':' and parse any valid label, giving a Label" $
      property prop_labelAssign_shouldDiscardColon

--------------------------------------------------------------------------------
-- mnemonic
--------------------------------------------------------------------------------
newtype ValidMnemonic = ValidMnemonic T.Text deriving Show

instance Arbitrary ValidMnemonic where
  arbitrary = do
    upper  <- choose ('A', 'Z')
    pure $ ValidMnemonic (T.pack [upper, upper, upper])

prop_mnemonic_parseValidMnemString (ValidMnemonic s) =
  parse mnemonic "" s `shouldParse` (Mnemonic s)

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

--------------------------------------------------------------------------------
-- label
--------------------------------------------------------------------------------
newtype LabelWithLetter = LabelWithLetter T.Text deriving Show
newtype LabelWithNonLetter = LabelWithNonLetter T.Text deriving Show

instance Arbitrary LabelWithLetter where
  arbitrary = do
    lbl <- genAlphaNum
    lowerLetter <- choose ('a', 'z')
    upperLetter <- choose ('A', 'Z')
    start <- elements [lowerLetter, upperLetter]
    pure . LabelWithLetter $ T.pack (start:lbl)

instance Arbitrary LabelWithNonLetter where
  arbitrary = do
    (LabelWithLetter lbl) <- arbitrary
    nonAlphaChar <- suchThat (arbitrary :: Gen Char) (\s -> not $ isAlpha s)
    pure . LabelWithNonLetter $ T.append (T.pack [nonAlphaChar]) lbl

prop_label_validLabelString (LabelWithLetter lbl) =
  parse label "" lbl `shouldParse` (Label lbl)

prop_label_invalidLabelString (LabelWithNonLetter lbl) =
  parse label "" lbl `shouldFailWith`  err posI (utok (T.head lbl) <> elabel "letter")

--------------------------------------------------------------------------------
-- labelAssign
--------------------------------------------------------------------------------
prop_labelAssign_shouldDiscardColon (LabelWithLetter lbl) =
  parse labelAssign "" (T.snoc lbl ':') `shouldParse` Label lbl

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
genAlphaNum :: Gen String
genAlphaNum = do
  lower <- choose ('a', 'z')
  upper <- choose ('A', 'Z')
  numeric <- choose ('0', '9')
  listOf . elements $ [lower, upper, numeric]



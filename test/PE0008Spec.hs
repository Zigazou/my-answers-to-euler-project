module PE0008Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0008

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "fixedLength" $ do
        it "gives [\"abc\",\"bcd\",\"cde\",\"def\",\"efg\",\"fgh\"] for \
           \3 and \"abcdefgh\"" $
            fixedLength 3 (T.pack "abcdefgh")
                `shouldBe` T.pack <$> ["abc","bcd","cde","def","efg","fgh"]

        it "gives [] for 10 and \"abc\"" $
            fixedLength 10 (T.pack "abc") `shouldBe` []

    describe "stringProduct" $
        it "gives 24 for \"1234\"" $ stringProduct (T.pack "1234") `shouldBe` 24

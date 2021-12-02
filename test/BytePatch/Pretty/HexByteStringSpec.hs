module BytePatch.Pretty.HexByteStringSpec ( spec ) where

import           BytePatch.Pretty.HexByteString
import           Test.Hspec
import           Util

import qualified Data.ByteString                as BS

spec :: Spec
spec = do
    let parse = parseFromCharStream parseHexByteString
        bs = BS.pack
    it "parses valid hex bytestrings" $ do
      parse "00" `shouldBe` Just (bs [0x00])
      parse "FF" `shouldBe` Just (bs [0xFF])
      parse "1234" `shouldBe` Just (bs [0x12, 0x34])
      parse "01 9A FE" `shouldBe` Just (bs [0x01, 0x9A, 0xFE])
      parse "FFFFFFFF" `shouldBe` Just (BS.replicate 4 0xFF)
      parse "12 34    AB CD" `shouldBe` Just (bs [0x12, 0x34, 0xAB, 0xCD])
    it "fails to parse invalid hex bytestrings" $ do
      parse "-00" `shouldBe` Nothing
      parse "FG" `shouldBe` Nothing
      parse "0x1234" `shouldBe` Nothing

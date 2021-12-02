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
    it "fails to parse invalid hex bytestrings" $ do
      parse "-00" `shouldBe` Nothing
      parse "FG" `shouldBe` Nothing
      parse "0x1234" `shouldBe` Nothing

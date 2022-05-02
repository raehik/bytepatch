{-# LANGUAGE AllowAmbiguousTypes #-}

module Raehik.CLI.Stream where

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal' )
import GHC.Exts ( proxy#, Proxy# )

import Options.Applicative

import Data.Char qualified as Char

data Stream (d :: Direction) (s :: Symbol)
  = Path' (Path d s)
  | Std
    deriving stock (Generic, Typeable, Data, Show, Eq)

newtype Path (d :: Direction) (s :: Symbol)
  = Path { unPath :: FilePath }
    deriving stock (Generic, Typeable, Data)
    deriving (Show, Eq) via FilePath

data Direction = In | Out
    deriving stock (Generic, Typeable, Data, Show, Eq)

-- | Either a positional filepath, or standalone @--stdin@ switch.
pStreamIn :: forall s. KnownSymbol s => Parser (Stream 'In s)
pStreamIn = (Path' <$> pPathIn) <|> pStdinOpt
  where
    pStdinOpt = flag' Std $  long "stdin"
                          <> help ("Get "<>sym @s<>" from stdin")

-- | Either an @--out-file X@ option, or default to stdout.
pStreamOut :: forall s. KnownSymbol s => Parser (Stream 'Out s)
pStreamOut = (Path' <$> pPathOut) <|> pure Std
  where pPathOut = Path <$> strOption (modFileOut (sym @s))

-- | Positional filepath.
pPathIn :: forall s. KnownSymbol s => Parser (Path 'In s)
pPathIn = Path <$> strArgument (modFileIn (sym @s))

--------------------------------------------------------------------------------

-- | Generate a base 'Mod' for a file type using the given descriptive
--   name (the "type" of input, e.g. file format) and the given direction.
modFile :: HasMetavar f => String -> String -> Mod f a
modFile dir desc =  metavar "FILE" <> help (dir<>" "<>desc)

modFileIn :: HasMetavar f => String -> Mod f a
modFileIn = modFile "Input"

modFileOut :: (HasMetavar f, HasName f) => String -> Mod f a
modFileOut s = modFile "Output" s <> long "out-file" <> short 'o'

metavarify :: String -> String
metavarify = map $ Char.toUpper . spaceToUnderscore
  where spaceToUnderscore = \case ' ' -> '_'; ch -> ch

--------------------------------------------------------------------------------

-- | More succint 'symbolVal' via type application.
sym :: forall s. KnownSymbol s => String
sym = symbolVal' (proxy# :: Proxy# s)

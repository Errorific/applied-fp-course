{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString, readFile)

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first, bimap)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try, SomeException)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM (AppM, runAppM), liftEither)
import           Level06.Types              (ConfigError (BadConfFile, ReadException),
                                             PartialConf (PartialConf), partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = AppM $ first ReadException <$> try (Data.ByteString.readFile fp)
  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp = liftEither 
  . first (\(e, _) -> BadConfFile e) 
  . D.pureDecodeFromByteString AB.parseOnly partialConfDecoder 
  =<< readConfFile fp

-- Go to 'src/Level06/Conf.hs' next.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WalColors
  ( Colors(..)
  , SpecialColors(..)
  , ColorPalette(..)
  , fallbackColors
  , getWal
  , getWalWithFallback
  , getWalWithFile
  , getWalWithFileAndFallback
  )
where

import           Control.Exception              ( catch
                                                , SomeException
                                                )
import           Data.Aeson                     ( decode
                                                , FromJSON
                                                )
import qualified Data.ByteString.Lazy          as BS
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics                   ( Generic )
import           System.Directory               ( getHomeDirectory )

data Colors = Colors
  { special :: SpecialColors
  , colors  :: ColorPalette
  } deriving (Show, Generic)

instance FromJSON Colors

data SpecialColors = SpecialColors
  { background :: String
  , foreground :: String
  , cursor     :: String
  } deriving (Show, Generic)

instance FromJSON SpecialColors

data ColorPalette = ColorPalette
  { color0  :: String
  , color1  :: String
  , color2  :: String
  , color3  :: String
  , color4  :: String
  , color5  :: String
  , color6  :: String
  , color7  :: String
  , color8  :: String
  , color9  :: String
  , color10 :: String
  , color11 :: String
  , color12 :: String
  , color13 :: String
  , color14 :: String
  , color15 :: String
  } deriving (Show, Generic)

instance FromJSON ColorPalette

-- | Some hardcoded colors.
--
fallbackColors :: Colors
fallbackColors = Colors
  { special = SpecialColors { background = "#282c34"
                            , foreground = "#abb2bf"
                            , cursor     = "#abb2bf"
                            }
  , colors  = ColorPalette { color0  = "#282c34"
                           , color1  = "#e06c75"
                           , color2  = "#98c379"
                           , color3  = "#e5c07b"
                           , color4  = "#61afef"
                           , color5  = "#c678dd"
                           , color6  = "#56b6c2"
                           , color7  = "#abb2bf"
                           , color8  = "#545862"
                           , color9  = "#e06c75"
                           , color10 = "#98c379"
                           , color11 = "#e5c07b"
                           , color12 = "#61afef"
                           , color13 = "#c678dd"
                           , color14 = "#56b6c2"
                           , color15 = "#c8ccd4"
                           }
  }

-- | A wrapper for 'readFile'.  It returns 'Nothing' where there would
-- usually be an exception.
--
safeReadFile :: FilePath -> IO (Maybe ByteString)
safeReadFile file =
  (Just <$> BS.readFile file) `catch` \(_ :: SomeException) -> return Nothing

-- | Get colour scheme based off of the generated colors.json file
-- from pywal.  Reads "~/.cache/wal/colors.json".
--
getWal :: IO (Maybe Colors)
getWal = do
  home <- getHomeDirectory
  getWalWithFile $ home ++ "/.cache/wal/colors.json"

-- | Same as 'getWal', but will use 'fallbackColors' if the
-- colors.json file cannot be read.
--
getWalWithFallback :: IO Colors
getWalWithFallback = fromMaybe fallbackColors <$> getWal

{-|
  Get colour scheme from a given json file.

  Example json file contents:

  @
    {
        "wallpaper": "None",
        "alpha": "100",

        "special": {
            "background": "#282c34",
            "foreground": "#abb2bf",
            "cursor": "#abb2bf"
        },
        "colors": {
            "color0": "#282c34",
            "color1": "#e06c75",
            "color2": "#98c379",
            "color3": "#e5c07b",
            "color4": "#61afef",
            "color5": "#c678dd",
            "color6": "#56b6c2",
            "color7": "#abb2bf",
            "color8": "#545862",
            "color9": "#e06c75",
            "color10": "#98c379",
            "color11": "#e5c07b",
            "color12": "#61afef",
            "color13": "#c678dd",
            "color14": "#56b6c2",
            "color15": "#c8ccd4"
        }
    }
  @
-}
getWalWithFile :: FilePath -> IO (Maybe Colors)
getWalWithFile filepath = do
  json <- safeReadFile filepath
  return $ json >>= decode

-- | Get colour scheme from a given json file.  Uses 'fallbackColors'
-- if the file cannot be read.
--
getWalWithFileAndFallback :: FilePath -> IO Colors
getWalWithFileAndFallback = (fromMaybe fallbackColors <$>) . getWalWithFile

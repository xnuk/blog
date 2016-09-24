{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-
  Copyright (C) 2011-2016 John MacFarlane <jgm@berkeley.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc., 59
    Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
Module      : Text.Pandoc.ImageSize
Copyright   : Copyright (C) 2011-2016 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

Functions for determining the size of a PNG, JPEG, or GIF image.
-}
module ImageSize (Dimension(Percent), Direction(Width, Height), dimension, showInPixel) where
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.Binary
import Text.Pandoc.Shared (safeRead)
import Numeric (showFFloat)
import Text.Pandoc.Definition
import Text.Pandoc.Options

-- quick and dirty functions to get image sizes
-- algorithms borrowed from wwwis.pl

data ImageType = Png | Gif | Jpeg | Pdf | Eps deriving Show
data Direction = Width | Height
instance Show Direction where
  show Width  = "width"
  show Height = "height"

data Dimension = Pixel Integer
               | Centimeter Double
               | Inch Double
               | Percent Double
instance Show Dimension where
  show (Pixel a)      = show   a ++ "px"
  show (Centimeter a) = showFl a ++ "cm"
  show (Inch a)       = showFl a ++ "in"
  show (Percent a)    = show   a ++ "%"

showFl :: (RealFloat a) => a -> String
showFl a = showFFloat (Just 5) a ""

-- | Convert a Dimension to a String denoting its equivalent in pixels, for example "600".
-- Note: Dimensions in percentages are converted to the empty string.
showInPixel :: WriterOptions -> Dimension -> String
showInPixel opts dim =
  case dim of
    (Pixel a)      -> show a
    (Centimeter a) -> show (floor $ dpi * a * 0.3937007874 :: Int)
    (Inch a)       -> show (floor $ dpi * a :: Int)
    (Percent _)    -> ""
  where
    dpi = fromIntegral $ writerDpi opts

-- | Maybe split a string into a leading number and trailing unit, e.g. "3cm" to Just (3.0, "cm")
numUnit :: String -> Maybe (Double, String)
numUnit s =
  let (nums, unit) = span (\c -> isDigit c || ('.'==c)) s
  in  case safeRead nums of
        Just n  -> Just (n, unit)
        Nothing -> Nothing

-- | Read a Dimension from an Attr attribute.
-- `dimension Width attr` might return `Just (Pixel 3)` or for example `Just (Centimeter 2.0)`, etc.
dimension :: Direction -> Attr -> Maybe Dimension
dimension dir (_, _, kvs) =
  case dir of
    Width  -> extractDim "width"
    Height -> extractDim "height"
  where
    extractDim key =
      case lookup key kvs of
        Just str ->
          case numUnit str of
            Just (num, unit) -> toDim num unit
            Nothing -> Nothing
        Nothing  -> Nothing
    toDim a "cm"   = Just $ Centimeter a
    toDim a "mm"   = Just $ Centimeter (a / 10)
    toDim a "in"   = Just $ Inch a
    toDim a "inch" = Just $ Inch a
    toDim a "%"    = Just $ Percent a
    toDim a "px"   = Just $ Pixel (floor a::Integer)
    toDim a ""     = Just $ Pixel (floor a::Integer)
    toDim _ _      = Nothing

data DataFormat = UnsignedByte Word8
                | AsciiString BL.ByteString
                | UnsignedShort Word16
                | UnsignedLong Word32
                | UnsignedRational Rational
                | SignedByte Word8
                | Undefined BL.ByteString
                | SignedShort Word16
                | SignedLong Word32
                | SignedRational Rational
                | SingleFloat Word32
                | DoubleFloat Word64
                deriving (Show)

data TagType = ImageDescription
             | Make
             | Model
             | Orientation
             | XResolution
             | YResolution
             | ResolutionUnit
             | Software
             | DateTime
             | WhitePoint
             | PrimaryChromaticities
             | YCbCrCoefficients
             | YCbCrPositioning
             | ReferenceBlackWhite
             | Copyright
             | ExifOffset
             | ExposureTime
             | FNumber
             | ExposureProgram
             | ISOSpeedRatings
             | ExifVersion
             | DateTimeOriginal
             | DateTimeDigitized
             | ComponentConfiguration
             | CompressedBitsPerPixel
             | ShutterSpeedValue
             | ApertureValue
             | BrightnessValue
             | ExposureBiasValue
             | MaxApertureValue
             | SubjectDistance
             | MeteringMode
             | LightSource
             | Flash
             | FocalLength
             | MakerNote
             | UserComment
             | FlashPixVersion
             | ColorSpace
             | ExifImageWidth
             | ExifImageHeight
             | RelatedSoundFile
             | ExifInteroperabilityOffset
             | FocalPlaneXResolution
             | FocalPlaneYResolution
             | FocalPlaneResolutionUnit
             | SensingMethod
             | FileSource
             | SceneType
             | UnknownTagType
             deriving (Show, Eq, Ord)

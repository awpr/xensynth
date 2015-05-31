{-# LANGUAGE ScopedTypeVariables #-}
module System where

import Prelude hiding (writeFile)

import           Data.Ratio (Ratio, (%))
import           Data.Int (Int16)

import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import           Sound.File.Sndfile.Buffer.Vector (toBuffer)
import           Sound.File.Sndfile
    ( Format(..)
    , Info(..)
    , HeaderFormat(HeaderFormatWav)
    , SampleFormat(SampleFormatPcm16)
    , EndianFormat(EndianFile)
    , writeFile
    )

type Pitch = Rational

data System = System
    { fund_  :: Pitch
    , cycle_ :: Ratio Integer
    , scale_ :: [Ratio Integer]
    }

pythagorean :: Pitch -> System
pythagorean p = System
    { fund_  = p
    , cycle_ = 2
    , scale_ = [1, 256%243, 9%8, 32%27, 81%64, 4%3, 729%512, 3%2, 128%81, 27%16, 16%9, 243%128]
    }

type Note = Int

data Composition = Composition
    { system_ :: System
    , notes_  :: [Note]
    }

ionian :: Composition
ionian = Composition
    { system_ = pythagorean 440
    , notes_  = [0, 2, 4, 5, 7, 9, 11, 12]
    }

type Sequencing = [Pitch]

pitch :: System -> Int -> Pitch
pitch (System f c s) n = f * (c ^ fromIntegral octave) * (s !! step)
    where (octave, step) = n `divMod` length s

interp :: Composition -> Sequencing
interp (Composition _ []    ) = []
interp (Composition s (x:xs)) = pitch s x : interp (Composition s xs)

type Sample = Int16
type Frame = Vector Sample

type SampleRate = Integer -- Hz

-- | Quantize the range (-1, 1) in the source type onto the full range of the
-- target type.
quantize :: forall a b. (Real a, Bounded b, Integral b) => a -> b
quantize x = floor $ toRational x * toRational scale / 2
    where scale = fromIntegral (maxBound :: b) - fromIntegral (minBound :: b)

pitchFrame :: SampleRate -> Int -> Pitch -> Frame
pitchFrame rate n p = V.generate n $
    quantize . sin . (2 * pi * fromRational p *) . fromRational . (% rate) . fromIntegral

simpleWavInfo :: Info
simpleWavInfo = Info
    { frames = 44100
    , samplerate = 44100
    , channels = 1
    , format = Format
        { headerFormat = HeaderFormatWav
        , sampleFormat = SampleFormatPcm16
        , endianFormat = EndianFile
        }
    , sections = 1
    , seekable = True
    }

main :: IO ()
main = print =<<
    writeFile simpleWavInfo "/tmp/blah.wav" (toBuffer (pitchFrame 44100 44100 440))

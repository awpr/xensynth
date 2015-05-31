{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module System where

import Prelude hiding (writeFile)

import           Control.Arrow (first, second)
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

-- TODO ideas:
-- - deal with harsh note onsets (window function?)
-- - note profiles: crescendo, accent, tremolo?
-- - pitch profiles: vibrato, bends?
-- - polyphonic compositions

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

bohlenPierce :: System
bohlenPierce = System
    { fund_ = 440
    , cycle_ = 3
    , scale_ = [1, 27%25, 25%21, 9%7, 7%5, 75%49, 5%3, 9%5, 49%25, 15%7, 7%3, 63%25, 25%9]
    }

type Note = (Int, Tone)

data Composition = Composition
    { system_ :: System
    , notes_  :: [Note]
    }

scale :: [a] -> [a]
scale []     = []
scale (x:xs) = x:x:xs ++ reverse (x:x:xs)

ionian :: Pitch -> Composition
ionian p = Composition
    { system_ = pythagorean p
    , notes_  = map (,harmonic) $ scale [0, 2, 4, 5, 7, 9, 11, 12]
    }

melodicMinor :: Pitch -> Composition
melodicMinor p = Composition
    { system_ = pythagorean p
    , notes_  = map (,harmonic) $ scale [0, 2, 3, 5, 7, 9, 11, 12]
    }

bpChromatic :: Composition
bpChromatic = Composition
    { system_ = bohlenPierce
    , notes_  = map (,oddHarmonics) $ scale [-13..0]
    }

lambda :: Composition
lambda = Composition
    { system_ = bohlenPierce
    , notes_  = map (,oddHarmonics) $ scale [-13, -11, -10, -9, -7, -6, -4, -3, -1, 0]
    }

type Spectrum = [(Pitch, Double)]
type Tone = Pitch -> Spectrum

synthesizeSpectrum :: Spectrum -> Waveform
synthesizeSpectrum =
    foldl addWaveform (const 0) .
    map (uncurry (flip scaleWaveform) . first pitchWaveform)

octaved :: Tone
octaved p = [(p, 0.5), (2*p, 0.2)]

tritaved :: Tone
tritaved p = [(p, 0.5), (3*p, 0.2)]

harmonic :: Tone
harmonic p = map (second (/2)) [(p, 0.5), (2*p, 0.2), (3*p, 0.1), (4*p, 0.05), (5*p, 0.025)]

oddHarmonics :: Tone
oddHarmonics p = [(p, 0.5), (2*p, 0.05), (3*p, 0.2), (4*p, 0.04), (5*p, 0.1), (7*p, 0.05)]

fifthed :: Tone
fifthed p = [(p, 0.3), (p * (3%2), 0.3), (p * 2, 0.15)]

tritoned :: Tone
tritoned p = [(p, 0.5), (p * (729%512), 0.2)]

type Waveform = Rational -> Double

type Sequencing = [(Waveform, Int)]

pitch :: System -> Int -> Pitch
pitch (System f c s) n = f * (c ^^ fromIntegral octave) * (s !! step)
    where (octave, step) = n `divMod` length s

pitchWaveform :: Pitch -> Rational -> Double
pitchWaveform p = sin . (2 * pi * fromRational p *) . fromRational

addWaveform :: Waveform -> Waveform -> Waveform
addWaveform wvl wvr = \t -> wvl t + wvr t

scaleWaveform :: Double -> Waveform -> Waveform
scaleWaveform f wv = (f*) . wv

interp :: Composition -> Sequencing
interp (Composition _ []    ) = []
interp (Composition s ((n, tone):xs)) =
    (scaleWaveform 0.2 . synthesizeSpectrum . tone $ pitch s n, 11025) : interp (Composition s xs)

type Sample = Int16

type SampleRate = Integer -- Hz

-- | Quantize the range (-1, 1) in the source type onto the full range of the
-- target type.
quantize :: forall a b. (Real a, Bounded b, Integral b) => a -> b
quantize x = floor $ toRational x * toRational scale / 2
    where
        scale :: Integer
        scale = fromIntegral (maxBound :: b) - fromIntegral (minBound :: b)

synthesizeFragment :: SampleRate -> (Waveform, Int) -> Vector Sample
synthesizeFragment rate (wv, n) = V.generate n $
    quantize . wv . (% rate) . fromIntegral

synthesize :: SampleRate -> Sequencing -> Vector Sample
synthesize rate ps = V.concat $ map (synthesizeFragment rate) ps

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

generateWav :: (FilePath, Composition) -> IO ()
generateWav (path, comp) = do
    n <- writeFile simpleWavInfo path $ toBuffer $ synthesize 44100 $ interp comp
    putStrLn $ path ++ ": " ++ show n ++ " samples"

main :: IO ()
main = mapM_ generateWav
    [ ("/tmp/chromatic.wav",     bpChromatic)
    , ("/tmp/lambda.wav",        lambda)
    , ("/tmp/ionian.wav",        ionian 220)
    , ("/tmp/melodic_minor.wav", melodicMinor 220)
    ]

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import Data.Foldable (fold)
import Data.Word

volume :: Float
volume = 0.05

frequency :: Float
frequency = 0.015

samples :: [Float]
samples = map (volume*) $ map sin $ map (frequency*) [0.0 .. 44100.0]

buildWav :: [Float] -> BS.Builder
buildWav samples = header <> samplesToBytes samples
  where sampleRate = 44100 :: Int
        numSamples = length samples :: Int
        numChannels = 1 :: Int
        bitsPerSample = 32 :: Int
        bytesPerSample = bitsPerSample `div` 8 :: Int
        subChunk1Size = 16 :: Int
        audioFormat = 1 :: Int
        dataSize = 4 * length samples :: Int
        chunkSize = 36 + dataSize :: Int
        byteRate = sampleRate * numChannels * bytesPerSample :: Int
        blockAlign = numChannels * bytesPerSample :: Int
        header = fold [ BS.string8 "RIFF"
                      , BS.word32LE $ fromIntegral chunkSize
                      , BS.string8 "WAVE"
                      , BS.string8 "fmt "
                      , BS.word32LE $ fromIntegral subChunk1Size
                      , BS.word16LE $ fromIntegral audioFormat
                      , BS.word16LE $ fromIntegral numChannels
                      , BS.word32LE $ fromIntegral sampleRate
                      , BS.word32LE $ fromIntegral byteRate
                      , BS.word16LE $ fromIntegral blockAlign
                      , BS.word16LE $ fromIntegral bitsPerSample
                      , BS.string8 "data"
                      , BS.word32LE $ fromIntegral dataSize
                      ]
        samplesToBytes = fold . map BS.floatLE

writeWav :: String -> [Float] -> IO ()
writeWav filename samples = BS.writeFile filename . BS.toLazyByteString $ buildWav samples

main :: IO ()
main = writeWav "test.wav" samples
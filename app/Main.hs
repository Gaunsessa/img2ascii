module Main where

import GHC.Float
import Codec.BMP
import Data.List.Split
import Control.Monad
import GHC.Environment
import System.Exit

import qualified Data.ByteString as B

palette :: String
palette = " .,:+*&@#"

rgbToRgbf :: (Int, Int, Int) -> (Float, Float, Float)
rgbToRgbf (r, g, b) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

rgbfToRgb :: (Float, Float, Float) -> (Int, Int, Int)
rgbfToRgb (r, g, b) = (round (r * 255), round (g * 255), round (b * 255))

colorToLim :: Float -> Float
colorToLim x
   | x <= 0.04045 = x / 12.92
   | otherwise = powerFloat ((x + 0.055) / 1.055) 2.4

rgbfToPLum :: (Float, Float, Float) -> Float
rgbfToPLum (r, g, b)
   | lum <= 0.008856 = lum * (24389 / 27)
   | otherwise = powerFloat lum (1 / 3) * 116 - 16
   where lum = colorToLim r * 0.2126 + colorToLim g * 0.7152 + colorToLim b * 0.0722

bmpToRgbf :: BMP -> [[(Float, Float, Float)]]
bmpToRgbf x = 
   chunksOf (fst $ bmpDimensions x) 
      $ map (\[r, g, b, _] -> rgbToRgbf (r, g, b)) 
      $ chunksOf 4 d
   where d = map fromIntegral $ B.unpack $ unpackBMPToRGBA32 x

rgbfToBmp :: [[(Float, Float, Float)]] -> BMP
rgbfToBmp x = 
   packRGBA32ToBMP (length $ head x) (length x) 
      $ B.pack 
      $ map fromIntegral
      $ concatMap ((\(r, g, b) -> [r, g, b, 255]) . rgbfToRgb) 
      $ concat x

reduce2D :: ([[a]] -> b) -> Int -> [[a]] -> [[b]]
reduce2D rf s a = 
   chunksOf (length (head ta) `div` s) 
      $ reduce2D' rf s
      $ foldl1 (zipWith (++)) (chunksOf s ta)
   where ta = map (drop (length (head a) `mod` s)) $ drop (length a `mod` s) a

reduce2D' :: ([[a]] -> b) -> Int -> [[a]] -> [b]
reduce2D' rf s a
   | null (head a) = []
   | otherwise = rf (map (take s) a) : reduce2D' rf s (map (drop s) a)

avg2D :: [[Float]] -> Float
avg2D x = sum (concat x) / fromIntegral (length $ concat x)

pLumToChar :: Float -> Char
pLumToChar x = palette !! floor (x / 100 * fromIntegral (length palette - 1))

main :: IO ()
main = do
   args <- getFullArgs

   when (length args /= 3) $ do 
      putStrLn $ "Usage: " ++ head args ++ " <DOWNSCALE FACTOR> <BMP PATH>"
      exitFailure

   inp <- readBMP $ args !! 2

   let img = case inp of
                  Right x -> x
                  Left _ -> error "Failed to read bmp!"

   let parsed = reverse $ map (map pLumToChar) $ reduce2D avg2D (read $ args !! 1) $ map (map rgbfToPLum) $ bmpToRgbf img

   forM_ parsed putStrLn

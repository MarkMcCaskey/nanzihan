{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where

import Lib
import Options.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Shelly
import Shelly.Background
import System.IO
import Development.IncludeFile
import Control.Lens
import Data.Char (isSpace)

default (Text)

$(includeFileInSource "data/HSK1u.txt" "hsk1u")
$(includeFileInSource "data/HSK2u.txt" "hsk2u")
$(includeFileInSource "data/HSK3u.txt" "hsk3u")
$(includeFileInSource "data/HSK4u.txt" "hsk4u")
$(includeFileInSource "data/HSK5u.txt" "hsk5u")
$(includeFileInSource "data/HSK6u.txt" "hsk6u")
$(includeFileInSource "data/commonly-used.txt" "common")

data Verbosity = Normal | Verbose deriving (Show,Eq)
  
[h1,h2,h3,h4,h5,h6,com] = fmap (T.group . E.decodeUtf8) [hsk1u,hsk2u,hsk3u,hsk4u,hsk5u,hsk6u,common]

data Nanzihan = Nanzihan
  { _hskLevel  :: Int
  , _verbose   :: Verbosity
  , _outputDir :: Text
  , _extension :: Text
  , _font      :: Text
  , _numCores  :: Int
  , _noiseType :: Text
  , _generate  :: Bool
  } deriving(Show, Eq)

makeLenses ''Nanzihan

nanzihan :: Parser Nanzihan
nanzihan = Nanzihan
  <$> option auto
  (long "hsk-level"
    <> short 'l'
    <> help "Level up to which characters should be generated for"
    <> showDefault
    <> value 6
    <> metavar "HSK-LEVEL")
  <*> flag Normal Verbose
  (   long "verbose"
    <> short 'v'
    <> help "Enable verbose output" )
  <*> (T.pack <$> strOption
       (long "output"
        <> short 'o'
        <> help "Specify output directory"
        <> showDefault
        <> value "out"
        <> metavar "DIRECTORY" ))
  <*> (T.pack <$> strOption
       (long "extension"
       <> short 'e'
       <> help "Extra string to add to file names (useful for distinguishing different fonts"
       <> showDefault
       <> value ""
       <> metavar "STRING"))
  <*> (T.pack <$> strOption
        (long "font"
         <> short 'f'
         <> help "The font to render the Chinese character in (run `convert -list font` to see which fonts you have installed)"
         <> showDefault
         <> value "WenQuanYi-Zen-Hei"
         <> metavar "FONT"))
  <*> option auto
  (long "jobs"
    <> short 'j'
    <> help "The number of jobs which should be used to generate images"
    <> showDefault
    <> value 1
    <> metavar "NUM-JOBS")
  <*> (T.pack <$> strOption
        (long "noise"
         <> short 'n'
         <> help "Add noise to the image.  If unspecified no noise will be applied. See `convert -list noise` for types"
         <> showDefault
         <> value ""
         <> metavar "NOISE-TYPE"))
  <*> flag False True
  (   long "generate"
    <> short 'g'
    <> help "Generate test data.  Will use a number of fonts and filters.  This will take a while." )



getHanzi n
  | n < 0      = error "Invalid HSK-level"
  | n > 6      = com
  | otherwise  = concat $ take n [h1,h2,h3,h4,h5,h6]
  

main' :: Nanzihan -> IO ()
main' nz = do
  shelly $ (if nz^.verbose == Verbose then verbosely else id) $ do
    let hanziList = filter(\a -> not (T.any isSpace a) && a /=T.empty) $
                    (nz^.hskLevel.to getHanzi)
    mkdir_p (nz^.outputDir.to fromText)
    jobs (if nz^.numCores > 0 then nz^.numCores else 1)
      (\job -> mapM (\ch -> background job $ imagemagick (80,80)
                            (nz^.font)
                            64 ch (nz^.outputDir)
                            (nz^.extension)
                            (nz^.noiseType))
               hanziList)
    return ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  p <- execParser opts
  if (p^.generate)
    then sequence_
         [let v = p & font.~fonts & noiseType.~effects & extension.~("."<>fonts<>effects)
          in main' v
         | effects <- ["Gaussian", "Impulse", "Laplacian","Multiplicative","Poisson", "Random", "Uniform", ""]
         , fonts <-["WenQuanYi-Zen-Hei-Mono"
                   ,"Noto-Sans-CJK-TC-Black"
                   ,"Noto-Sans-CJK-SC-Black"
                   ,"Noto-Sans-CJK-TC-Regular"
                   ,"Noto-Sans-CJK-SC-Regular"
                   ,"Noto-Sans-Mono-CJK-SC-Regular"
                   ,"Noto-Sans-Mono-CJK-TC-Regular"
                   ,"cwTeXKai"
                   ,"TW-MOE-Std-Kai"
                   ,"AR-PL-UKai-CN"
                   ,"AR-PL-UKai-TW"
                   ,"cwTeXFangSong"
                   ,"MOESongUN"]]

    else main' p
  return ()
  where
    opts = info (helper <*> nanzihan)
           ( fullDesc
             <> progDesc "Generate training images of Chinese characters"
             <> header "nanzihan - a Chinese character image generator")

imagemagick :: (Int,Int) -> Text -> Int -> Text -> Text -> Text -> Text -> Sh (Text)
imagemagick (h,w) font fontsize char outdir extension noiseType
  = escaping False $
    run "convert" (["-size " <> (T.pack (show h)) <> "x" <> (T.pack (show w))
                    , "-depth 8 pattern:GRAY100"
                    , "-gravity center"
                    , "-font " <> font
                    , "-pointsize " <> (T.pack (show fontsize))
                    , "-encoding Unicode"
                    , "-draw \"text 0,0 '" <> char <> "'\""] ++
                    (if noiseType == T.empty then [] else ["+noise " <> noiseType]) ++
                    [outdir <> "/" <> char <> extension <> ".jpg"])

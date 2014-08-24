-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List as L
import System.Environment(getArgs)
import MusicDataParser
import ConvDataToHTML(defVarMNameStr)

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ "./" ++ "defHtmlFileName.txt"
  let outFileName = head $ lines contents
      outHtmlFileName = outFileName ++ ".html"
  musicContents <- readFile $ head args
  headerContents <- readFile $ "./" ++ "tableHeader.html"
  footerContents <- readFile $ "./" ++ "tableFooter.html"
  let outputSrc = T.concat [T.pack headerContents,defData,forStr,ifStr musicDatas,T.pack footerContents]
      musicDataTsv = T.pack $ unlines $ tail $ lines musicContents
      musicDatas = getMusicDataSrc musicDataTsv
      defData = defVarMNameStr musicDatas
  T.writeFile outHtmlFileName outputSrc

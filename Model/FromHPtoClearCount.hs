-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List as L
import Data.Time
import System.Locale
import Network.HTTP
import MusicDataParser
import Text.Printf

musicURLBase :: String
musicURLBase = "http://www.dream-pro.info/~lavalse/LR2IR/search.cgi?mode=ranking&bmsid="

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest　x)

main :: IO()
main = do
  contents <- readFile $ "./" ++ "tableURL.txt"
  defHtmlFileNames <- readFile $ "./" ++ "defHtmlFileName.txt"
  htmlSrc <- openURL $ head $ lines contents
  zonedTime <- getZonedTime
  let musicDatas = getMusicDatas $ T.pack htmlSrc
      ymdString = formatTime defaultTimeLocale "%F" zonedTime
      hmsString = T.splitOn ":" $ T.pack $ formatTime defaultTimeLocale "%T" zonedTime
      timeString = concatMap T.unpack $ intersperse "-" hmsString
      outFileName = head $ lines defHtmlFileNames
      outHtmlFileName = outFileName ++ "_ClearData.html"
  headerContents <- readFile $ "./" ++ "tableHeader.html"
  footerContents <- readFile $ "./" ++ "tableFooter.html"
  mClearDataTpl <- forM musicDatas $ \mData -> do
    mHtml <- openURL $ musicURLBase ++ T.unpack (dBmsID mData)
    let mClearData = getMusicClearStatus mHtml
    return (mData, mClearData)
  let outputSrc = T.concat [T.pack headerContents,updateTimeStr,defData,T.pack footerContents]
      updateTimeStr = T.pack $ "document.write(\"<font color='white'>update : " ++ ymdString ++ "_" ++ timeString ++ "</font>\")\n"
      defData = defVarMNameStr mClearDataTpl
  T.writeFile　outHtmlFileName outputSrc

defVarMNameStr :: [(MusicData, MusicClearStatus)] -> T.Text
defVarMNameStr datas = T.concat varMName
  where mNames = getVarMNames datas
        varMName = ["var mname = [",T.concat mNames,"];\n"]

getVarMNames :: [(MusicData, MusicClearStatus)] -> [T.Text] 
--getVarMNames datas = L.intersperse "," $ map getVarMName $ zip [1..] datas 
getVarMNames datas = L.intersperse "," $ zipWith (curry getVarMName) [1 ..] datas

getVarMName :: (Int, (MusicData, MusicClearStatus)) -> T.Text
getVarMName (n,(x,y)) = T.concat dataDef
  where strLevel = dLevel x
        strMusicTitle = dMusicTitle x
        strBmsID = dBmsID x
        strFcRate = T.pack fcRate
        strHardClearRate = T.pack hardClearRate
        strClearRate = T.pack clearRate
        fcRate = printf "%.2f" $ (fromIntegral fcClear::Float) / (fromIntegral player::Float) * 100
        fcClear = sum $ map (read . T.unpack) [strFC]
        hardClearRate = printf "%.2f" $ (fromIntegral hardClear::Float) / (fromIntegral player::Float) * 100
        hardClear = sum $ map (read . T.unpack) [strFC, strHard]
        clearRate = printf "%.2f" $ (fromIntegral clear::Float) / (fromIntegral player::Float) * 100
        clear = sum $ map (read . T.unpack) [strFC, strHard, strNormal]
        player = sum $ map (read . T.unpack) [strFC, strHard, strNormal, strEasy, strFailed]
        strPlayer = T.pack $ show player
        strFC = dFC y
        strHard = dHard y
        strNormal = dNormal y
        strEasy = dEasy y
        strFailed = dFailed y
        dataDef = ["\n[",T.pack (show n),",\"",strLevel,p,strMusicTitle,p,strBmsID,p,strFcRate,p,strHardClearRate,p,strClearRate,p,strFC,p,strHard,p,strNormal,p,strEasy,p,strFailed,p,strPlayer,"\",\n]"]
        p = "\",\n\""

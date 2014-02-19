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

type PageID = Int

musicURLBase :: String
musicURLBase = "http://www.dream-pro.info/~lavalse/LR2IR/search.cgi?mode=ranking&bmsid="

musicURLDerived :: String -> [PageID] -> [String]
musicURLDerived urlBase = map ((urlPage ++) . show) 
  where urlPage = urlBase ++ "&page="

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
      outHtmlFileName = outFileName ++ ".html"
  headerContents <- readFile $ "./" ++ "tableHeader.html"
  footerContents <- readFile $ "./" ++ "tableFooter.html"
  mIRDataTpl <- forM musicDatas $ \mData -> do
    let musicIRPage = musicURLBase ++ T.unpack (dBmsID mData)
    mHtml <- openURL musicIRPage
    let playerCnt = getPlayerCntFromIRSite mHtml
        accessURLPages = musicURLDerived musicIRPage [1..((playerCnt-1) `div` 100 + 1)]
    mIRPlayerData <- forM accessURLPages $ \mAccsessURLPage -> do
      print mAccsessURLPage
      iRPageSrc <- openURL mAccsessURLPage
      return $ getPlayerMusicDataInfoFromIRSite iRPageSrc
    return (mData, concat mIRPlayerData)
  let rankData = getScratchSkillPtRanking mIRDataTpl
      updateTimeStr = T.pack $ "document.write(\"<font color='white'>update : " ++ ymdString ++ "_" ++ timeString ++ "</font>\")\n"
      outputSrc = T.concat [T.pack headerContents, updateTimeStr, defVarMNameStr rankData, T.pack footerContents]
  T.writeFile　outHtmlFileName outputSrc

defVarMNameStr :: [ScratchSkillPtRanking] -> T.Text
defVarMNameStr datas = T.concat varMName
  where mNames = getVarMNames datas
        varMName = ["var mname = [",T.concat mNames,"];\n"]

getVarMNames :: [ScratchSkillPtRanking] -> [T.Text] 
getVarMNames datas = L.intersperse "," $ zipWith (curry getVarMName) [1 ..] datas

getVarMName :: (Int, ScratchSkillPtRanking) -> T.Text
getVarMName (n, dat) = T.concat dataDef
  where djname = dDJNAME' dat
        playid = dPLAYERID' dat
        sspt = (T.pack . show) $ dSKILLPT dat
        aaacnt = (T.pack . show) $ dAAA dat
        aacnt = (T.pack . show) $ dAA dat
        acnt = (T.pack . show) $ dA dat
        fccnt = (T.pack . show) $ dFCCNT' dat
        hcnt = (T.pack . show) $ dHCNT' dat
        ncnt = (T.pack . show) $ dNCNT' dat
        ecnt = (T.pack . show) $ dECNT' dat
        fcnt = (T.pack . show) $ dFCNT' dat
        dataDef = ["\n[",T.pack (show n),",\"",djname,p,playid,p,sspt,p,aaacnt,p,aacnt,p,acnt,p,fccnt,p,hcnt,p,ncnt,p,ecnt,p,fcnt,"\",\n]"]
        p = "\",\n\""

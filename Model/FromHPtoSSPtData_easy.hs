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
import Control.Applicative

type PageID = Int
type Rank = T.Text
type DaniData = (Rank, [PlayerMusicDataInfo])
type PlayerID = T.Text
type DaniHard = T.Text
type DaniNormal = T.Text
type DaniEasy = T.Text

musicURLBase :: String
musicURLBase = "http://www.dream-pro.info/~lavalse/LR2IR/search.cgi?mode=ranking&bmsid="

musicURLDerived :: String -> [PageID] -> [String]
musicURLDerived urlBase = map ((urlPage ++) . show) 
  where urlPage = urlBase ++ "&page="

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest　x)

easyLv :: [String]
easyLv = map show [-1..4]

normalLv :: [String]
normalLv = map show [5..10]

hardLv :: [String]
hardLv = map show [11..16]

outFilter :: MusicData -> Bool
outFilter md = lv `elem` easyLv
  where lv = concat $ drop 2 $ map T.unpack $ T.group lvStr
        lvStr = dLevel md

main :: IO()
main = do
  contents <- readFile $ "./" ++ "tableURL.txt"
  defHtmlFileNames <- readFile $ "./" ++ "defHtmlFileName.txt"
  htmlSrc <- openURL $ head $ lines contents
  zonedTime <- getZonedTime
  let musicDatas = filter outFilter $ getMusicDatas $ T.pack htmlSrc
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
  danis <- (T.lines . T.pack) <$> readFile ("./" ++ "dani.csv")
  let daniURLTpls = L.filter (\x -> snd x /= "") $ map divDaniAndUrl danis
  mDaniDataTpl <- forM daniURLTpls $ \dDataTpl -> do
    let musicIRPage = tail $ T.unpack $ snd dDataTpl
    mHtml <- openURL musicIRPage
    let playerCnt = getPlayerCntFromIRSite mHtml
        accessURLPages = musicURLDerived musicIRPage [1..((playerCnt-1) `div` 100 + 1)]
    mIRPlayerData <- forM accessURLPages $ \mAccsessURLPage -> do
      iRPageSrc <- openURL mAccsessURLPage
      return $ getPlayerMusicDataInfoFromIRSite iRPageSrc
    return (fst dDataTpl, concat mIRPlayerData)
  (print . show) mDaniDataTpl
  let rankData = take 300 $ getScratchSkillPtRanking mIRDataTpl
      updateTimeStr = T.pack $ "document.write(\"<font color='white'>update : " ++ ymdString ++ "_" ++ timeString ++ "</font>\")\n"
      outputSrc = T.concat [T.pack headerContents, updateTimeStr, defVarMNameStr (length mIRDataTpl) rankData mDaniDataTpl, T.pack footerContents]
  T.writeFile　outHtmlFileName outputSrc

divDaniAndUrl :: T.Text -> (Rank , T.Text)
divDaniAndUrl str = (x, y)
  where (x:y:_) = T.splitOn "," str

defVarMNameStr :: Int -> [ScratchSkillPtRanking] -> [DaniData] -> T.Text
defVarMNameStr mDataCnt datas daniDatas = T.concat varMName
  where mNames = getVarMNames mDataCnt datas daniDatas
        varMName = ["var mname = [",T.concat mNames,"];\n"]

getVarMNames :: Int -> [ScratchSkillPtRanking] -> [DaniData] -> [T.Text] 
getVarMNames mDataCnt datas daniDatas = L.intersperse "," $ zipWith (curry getVarMName') [1 ..] datas
  where getVarMName' = getVarMName mDataCnt daniDatas

getVarMName :: Int -> [DaniData] -> (Int, ScratchSkillPtRanking) -> T.Text
getVarMName mDataCnt daniDats (n, dat) = T.concat dataDef
  where djname = dDJNAME' dat
        playid = dPLAYERID' dat
        sspt = (T.pack . show) $ dSKILLPT dat `div` 10000
        aaacnt = (T.pack . show) $ dAAA dat
        aacnt = (T.pack . show) $ dAA dat
        acnt = (T.pack . show) $ dA dat
        fccnt = (T.pack . show) $ dFCCNT' dat
        hcnt = (T.pack . show) $ dHCNT' dat
        ncnt = (T.pack . show) $ dNCNT' dat
        ecnt = (T.pack . show) $ dECNT' dat
        fcnt = (T.pack . show) $ dFCNT' dat
        noPlayCnt = (T.pack . show) $ mDataCnt - sum [dFCCNT' dat, dHCNT' dat, dNCNT' dat, dECNT' dat, dFCNT' dat]
        (dhard, dnormal) = getDaniOfPlayerStatus daniDats playid
        hStr = "<font color='red'>" `T.append` dhard `T.append` "</font>"
        nStr = "<font color='blue'>" `T.append` dnormal `T.append` "</font>"
        dataDef = ["\n[",T.pack (show n),",\"",djname,p,playid,p,hStr,p,nStr,p,sspt,p,aaacnt,p,aacnt,p,acnt,p,fccnt,p,hcnt,p,ncnt,p,ecnt,p,fcnt,p,noPlayCnt,"\",\n]"]
        p = "\",\n\""

getDaniOfPlayerStatus :: [DaniData] -> PlayerID -> (DaniHard, DaniNormal)
getDaniOfPlayerStatus dData pId = getDaniOfPlayer dData pId ("-", "-")

getDaniOfPlayer :: [DaniData] -> PlayerID -> (DaniHard, DaniNormal) -> (DaniHard, DaniNormal)
getDaniOfPlayer [] _ ret = ret
getDaniOfPlayer (x:xs) pId ret | null daniResults = getDaniOfPlayer xs pId ret
                               | otherwise = getDaniOfPlayer xs pId (setDaniOfStatus (fst x) (head daniResults) ret)
  where daniResults = filter (\p -> dPlayerID p == pId) (snd　x)

setDaniOfStatus :: Rank -> PlayerMusicDataInfo -> (DaniHard, DaniNormal) -> (DaniHard, DaniNormal)
setDaniOfStatus r p (h, n) | ("FULLCOMBO" `T.isInfixOf` c || "HARD" `T.isInfixOf` c) && h == "-" = (r, n)
                              | "CLEAR" `T.isInfixOf` c && n == "-" = (h, r)
                              | otherwise = (h, n)
  where c = dClearStatus p

-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

module MusicDataParser
(
  MusicData(..),
  MusicClearStatus(..),
  ClearRanking(..),
  ScratchSkillPtRanking(..),
  getMusicDatas,
  getMusicDataSrc,
  getMusicClearStatus,
  getPlayerMusicDataInfoFromIRSite,
  getPlayerCntFromIRSite,
  convertDatas,
  getClearRanking,
  getScratchSkillPtRanking
) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T hiding (takeWhile)
import Data.List as L (isInfixOf, filter, length, sortBy)
import Data.Map as M
import Control.Applicative
import Data.Ord

data MusicData = MusicData {
--  dID :: T.Text,
  dLevelColor :: T.Text,
  dLevel :: T.Text,
  dMusicTitle :: T.Text,
  dBmsID :: T.Text,
  dOrgArtist :: T.Text,
  dOrgArtistURL :: T.Text,
  dScoreSite :: T.Text,
  dScoreSiteURL :: T.Text,
  dComment :: T.Text
} deriving (Show)

data MusicClearStatus = MusicClearStatus {
  dFC :: T.Text,
  dHard :: T.Text,
  dNormal :: T.Text,
  dEasy :: T.Text,
  dFailed :: T.Text
} deriving (Show)

data PlayerMusicDataInfo = PlayerMusicDataInfo {
  dRank :: T.Text,
  dPlayerID :: T.Text,
  dPlayerName :: T.Text,
  dSPDan :: T.Text,
  dDPDan :: T.Text,
  dClearStatus :: T.Text,
  dDJLv :: T.Text,
  dScore :: T.Text,
  dCombo :: T.Text,
  dBP :: T.Text,
  dPG :: T.Text,
  dGR :: T.Text,
  dGD :: T.Text,
  dBD :: T.Text,
  dPR :: T.Text,
  dGaugeOP :: T.Text,
  dNoteOP :: T.Text,
  dInput :: T.Text,
  dBody :: T.Text,
  dCom :: T.Text
} deriving (Show)

data DJ = DJ {
  dDJName :: T.Text,
  dPlayerId :: T.Text
} deriving (Show, Ord, Eq)

data Result = Result {
  dBmsId :: T.Text,
  dLv :: T.Text,
  dExScore :: ExScore,
  dDjLv :: DjLv,
  dClear :: Clear
} deriving (Show)

type ExScore = T.Text
type DjLv = T.Text
type Clear = T.Text
type PlayerData = Map DJ [Result]

data ClearRanking = ClearRanking {
  dDJNAME :: T.Text,
  dPLAYERID :: T.Text,
  dCLEARPT :: Int,
  dFCCNT :: Int,
  dHCNT :: Int,
  dNCNT :: Int,
  dECNT :: Int,
  dFCNT :: Int
} deriving (Show, Ord, Eq)

data ScratchSkillPtRanking = ScratchSkillPtRanking {
  dDJNAME' :: T.Text,
  dPLAYERID' :: T.Text,
  dSKILLPT :: Int,
  dAAA :: Int,
  dAA :: Int,
  dA :: Int,
  dFCCNT' :: Int,
  dHCNT' :: Int,
  dNCNT' :: Int,
  dECNT' :: Int,
  dFCNT' :: Int
} deriving (Show, Ord, Eq)

-- FromHPtoData
getMusicDatas :: T.Text -> [MusicData]
getMusicDatas str =
  case T.parseOnly pMusicDatas formatStr of
    Left _ -> error "parse error."
    Right results -> results
  where formatStr = formatMusicData str

pMusicDatas :: T.Parser [MusicData]
pMusicDatas = do
  many T.space
  T.string "var"
  many T.space
  T.string "mname"
  many T.space
  T.char '='
  many T.space
  T.char '['
  musicDatas <- many pMusicData
  many T.space
  T.string "];"
  return musicDatas

pMusicData :: T.Parser MusicData
pMusicData = do
  many T.space
  T.char '['
  many T.space
  pID
  pComma
  (color, level) <- pLevel
  pComma
  musicTitle <- pMusicTitle
  pComma
  bmsID <- pBmsID
  pComma
  (artUrl, artName) <- pOrgArtistInfo
  pComma
  (siteURL, siteName) <- pScoreSiteInfo
  pComma
  comment <- T.try pComment <|> return ""
  pDataEnd
  T.char ']'
  T.try pComma <|> return ""
  return $ MusicData color level musicTitle bmsID artName artUrl siteName siteURL comment

pComma :: T.Parser T.Text
pComma = do
  many T.space
  T.char ','
  many T.space
  return ""

pTab :: T.Parser T.Text
pTab = do
  T.char '\t'
  return ""

pDataEnd :: T.Parser T.Text
pDataEnd = pComma <|> many T.space *> return ""

pID :: T.Parser T.Text
pID = T.takeTill $ T.inClass ","

pLevel :: T.Parser (T.Text, T.Text)
pLevel = pLevelHtmlTag <|> pLevelHtmlNoTag

pMusicTitle :: T.Parser T.Text
pMusicTitle = pGetInfo

pBmsID :: T.Parser T.Text
pBmsID = pGetInfo

pOrgArtistInfo :: T.Parser (T.Text, T.Text)
pOrgArtistInfo = pURLAndName <|> pNoURLAndName

pScoreSiteInfo :: T.Parser (T.Text, T.Text)
pScoreSiteInfo = pURLAndName <|> pNoURLAndName

pComment :: T.Parser T.Text
pComment = pGetInfo

pLevelHtmlTag :: T.Parser (T.Text, T.Text)
pLevelHtmlTag = do
  T.char '"'
  color <- T.try (pHtmlTagGetAttribute "color" <|> pHtmlTag *> "")
  level <- T.takeTill $ T.inClass "<"
  pHtmlTag
  T.char '"'
  return (color, level)

pLevelHtmlNoTag :: T.Parser (T.Text, T.Text)
pLevelHtmlNoTag = do
  level <- pGetInfo
  return ("", level)

pURLAndName :: T.Parser (T.Text, T.Text)
pURLAndName = do
  T.char '"'
  T.char '<'
  T.takeTill $ T.inClass "'"
  T.char '\''
  url <- T.takeTill $ T.inClass "'"
  T.char '\''
  T.char '>'
  name <- T.takeTill $ T.inClass "<"
  pHtmlTag
  T.char '"'
  return (url, name)

pNoURLAndName :: T.Parser (T.Text, T.Text)
pNoURLAndName = do
  name <- pGetInfo
  return ("", name)

pHtmlTag :: T.Parser T.Text
pHtmlTag = do
  T.char '<'
  content <- T.takeTill $ T.inClass ">"
  T.char '>'
  return content

pHtmlTagGetAttribute :: T.Text -> T.Parser T.Text
pHtmlTagGetAttribute att = do
  T.char '<'
  contents <- T.takeTill $ T.inClass ">"
  T.char '>'
  return $ getAttribute att contents

getAttribute :: T.Text -> T.Text-> T.Text
getAttribute att cont | not (attStr `T.isInfixOf` cont) = ""
                      | otherwise = ret
  where contStr = T.toLower cont
        attStr = T.toLower att
        attElemStr = snd $ T.breakOnEnd attStr contStr
        ret = T.takeWhile endCond $ T.tail $ T.dropWhile beginCond attElemStr
        beginCond x = x /= '"' && x /= '\''
        endCond x = x /= '"' && x /= '\''

pGetInfo :: T.Parser T.Text
pGetInfo = do
  T.char '"'
  info <- T.takeTill $ T.inClass "\""
  T.char '"'
  return info

formatMusicData :: T.Text -> T.Text
formatMusicData str = defMusicSrc $ T.lines str

defMusicSrc :: [T.Text] -> T.Text
defMusicSrc = remOnlyNewLineAndTab . T.unlines . searchMusicDefEnd . searchMusicDefStart

searchMusicDefStart :: [T.Text] -> [T.Text]
searchMusicDefStart (x:xs) | "var mname" `T.isInfixOf` x = x:xs
                           | otherwise = searchMusicDefStart xs

searchMusicDefEnd :: [T.Text] -> [T.Text]
searchMusicDefEnd src = searchMusicDefEnd' src [""]
  where searchMusicDefEnd' [] _ = error "Don't search Music Def End"
        searchMusicDefEnd' (x:xs) ret | "];" `T.isInfixOf` x = reverse (x:ret)
                                      | otherwise = searchMusicDefEnd' xs (x:ret)

remOnlyNewLineAndTab :: T.Text -> T.Text
remOnlyNewLineAndTab = T.filter (/= '\t') . T.filter (/= '\n') . T.filter (/= '\r')

-- FromDatatoHTML
getMusicDataSrc :: T.Text -> [MusicData]
getMusicDataSrc str = 
  case T.parseOnly pMusicDataFromTsvs str of
    Left _ -> error "parse error."
    Right results -> results

pMusicDataFromTsvs :: T.Parser [MusicData]
pMusicDataFromTsvs = many pMusicDataFromTsv

pMusicDataFromTsv :: T.Parser MusicData
pMusicDataFromTsv = do
  color <- T.takeTill $ T.inClass "\t"
  pTab
  level <- T.takeTill $ T.inClass "\t"
  pTab
  musicTitle <- T.takeTill $ T.inClass "\t"
  pTab
  bmsID <- T.takeTill $ T.inClass "\t"
  pTab
  artName <- T.takeTill $ T.inClass "\t"
  pTab
  artUrl <- T.takeTill $ T.inClass "\t"
  pTab
  siteName <- T.takeTill $ T.inClass "\t"
  pTab
  siteURL <- T.takeTill $ T.inClass "\t"
  pTab
  comment <- T.takeTill $ T.inClass "\n"
  T.endOfLine
  return $ MusicData color level musicTitle bmsID artName artUrl siteName siteURL comment

-- FromHPtoHtml
getMusicClearStatus :: String -> MusicClearStatus
getMusicClearStatus mHtml = 
  case T.parseOnly pGetClearStatus (T.pack mHtml) of
    Left _ -> error "parse error."
    Right results -> results

pGetClearStatus :: T.Parser MusicClearStatus
pGetClearStatus = do
  mClearDatas <- many (T.try pClearStatus <|> pReadLine)
  let mClearData = L.filter (\x -> dFC x /= T.pack "") mClearDatas
  return $ head mClearDatas

-- LRのIRページのフォーマット依存
pClearStatus :: T.Parser MusicClearStatus
pClearStatus = do
  many T.space
  trS
  thS *> T.takeTill (T.inClass "<") *> thE
  fc <- T.pack <$> (tdS *> many T.digit <* tdE)
  h <- T.pack <$> (tdS *> many T.digit <* tdE)
  n <- T.pack <$> (tdS *> many T.digit <* tdE)
  e <- T.pack <$> (tdS *> many T.digit <* tdE)
  f <- T.pack <$> (tdS *> many T.digit <* tdE)
  trE
  T.endOfLine
  return $ MusicClearStatus fc h n e f

pReadLine :: T.Parser MusicClearStatus
pReadLine = do
  T.takeTill $ T.inClass "\n"
  T.endOfLine
  return $ MusicClearStatus "" "" "" "" ""

trS :: T.Parser T.Text
trS = T.string "<tr>"

trE :: T.Parser T.Text
trE = T.string "</tr>"

thS :: T.Parser T.Text
thS = T.string "<th>"

thE :: T.Parser T.Text
thE = T.string "</th>"

tdS :: T.Parser T.Text
tdS = T.string "<td>"

tdE :: T.Parser T.Text
tdE = T.string "</td>"

fontS :: T.Parser T.Text
fontS = do
  T.string "<font "
  T.takeTill $ T.inClass ">"
  T.string ">"

fontE :: T.Parser T.Text
fontE = T.string "</font>"

-- 曲をプレイしているプレイヤーの人数を取得する
getPlayerCntFromIRSite :: String -> Int
getPlayerCntFromIRSite str = 
  case T.parseOnly pPlayerCntFromIRSite (T.pack str) of
    Left _ -> error "parse error PlayerCntFromIRSite."
    Right results -> fst (results !! 1)

pPlayerCntFromIRSite :: T.Parser [(Int, Int)]
pPlayerCntFromIRSite = do
  plays <- many (T.try pPlayerCnt <|> pReadLinePlayerCntFromIRSite)
  return $ concat plays

pReadLinePlayerCntFromIRSite :: T.Parser [a]
pReadLinePlayerCntFromIRSite = do
  T.takeTill $ T.inClass "\n"
  T.endOfLine
  return []

pPlayerCnt :: T.Parser [(Int, Int)]
pPlayerCnt = do
  many T.space
  trS
  thS *> T.takeTill (T.inClass "<") *> thE
  play <- tdS *> many T.digit <* tdE
  clear <- tdS *> many T.digit <* tdE
  tdS *> T.takeTill (T.inClass "<") *> tdE
  trE
  T.endOfLine
  return [(read play :: Int, read clear :: Int)]

-- 曲をプレイしているプレイヤーのデータを取得する
pPlayerDataBeginStr :: T.Parser T.Text
pPlayerDataBeginStr = T.string "  <tr><td rowspan=\"2\" align=\"center\">"

pPlayerDataEndStr :: T.Parser T.Text
pPlayerDataEndStr = T.string " </table>"

getPlayerMusicDataInfoFromIRSite :: String -> [PlayerMusicDataInfo]
getPlayerMusicDataInfoFromIRSite str =
  case T.parseOnly pGetPlayerMDataInfos (T.pack str) of
    Left _ -> error "parse error PlayerMusicDataInfo."
    Right results -> results

pGetPlayerMDataInfos :: T.Parser [PlayerMusicDataInfo]
pGetPlayerMDataInfos = do
  datas <- many (T.try pPlayerMusicDataInfos <|> pReadLineIRSite)
  return $ concat datas

pReadLineIRSite :: T.Parser [a]
pReadLineIRSite = do
  T.takeTill $ T.inClass "\n"
  T.endOfLine
  return []

pPlayerMusicDataInfos :: T.Parser [PlayerMusicDataInfo]
pPlayerMusicDataInfos = do
  playerMDataInfos <- many pPlayerMusicDataInfo
  pPlayerDataEndStr
  return playerMDataInfos

pPlayerMusicDataInfo :: T.Parser PlayerMusicDataInfo
pPlayerMusicDataInfo = do
  pPlayerDataBeginStr
  rank <- T.takeTill (T.inClass "<") <* tdE
  playerid <- T.pack <$> (tdS *> T.takeTill (T.inClass "&") *> T.string "&playerid=" *> many T.digit <* T.string "\">" )
  playerName <- T.takeTill (T.inClass "<") <* T.string "</a>" <* tdE
  spDan <- tdS *> T.takeTill (T.inClass "/") <* T.string "/"
  dpDan <- T.takeTill (T.inClass "<") <* tdE
  clearStatus <- tdS *> T.takeTill (T.inClass "<") <* tdE
  djLv <- tdS *> T.takeTill (T.inClass "<") <* tdE
  score <- tdS *> T.takeTill (T.inClass "/") <* T.string "/"
  T.takeTill (T.inClass "<") *> tdE
  combo <- tdS *> T.takeTill (T.inClass "/") <* T.string "/"
  T.takeTill (T.inClass "<") *> tdE
  bp <- T.pack <$> (tdS *> many T.digit <* tdE)
  pg <- T.pack <$> (tdS *> many T.digit <* tdE)
  gr <- T.pack <$> (tdS *> many T.digit <* tdE)
  gd <- T.pack <$> (tdS *> many T.digit <* tdE)
  bd <- T.pack <$> (tdS *> many T.digit <* tdE)
  pr <- T.pack <$> (tdS *> many T.digit <* tdE)
  gaugeOP <- tdS *> T.takeTill (T.inClass "<") <* tdE
  noteOP <- tdS *> T.takeTill (T.inClass "<") <* tdE
  input <- tdS *> T.takeTill (T.inClass "<") <* tdE
  body <- pBody
  trE *> T.endOfLine
  many T.space
  trS
  pHtmlTag
  com <- T.takeTill (T.inClass "<") <* tdE
  trE *> T.endOfLine
  return $ PlayerMusicDataInfo rank playerid playerName spDan dpDan clearStatus djLv score combo bp pg gr gd bd pr gaugeOP noteOP input body com

pBody :: T.Parser T.Text
pBody = T.try (tdS *> T.takeTill (T.inClass "<") <* tdE) <|> (tdS *> fontS *> T.takeTill (T.inClass "<") <* fontE <* tdE)

convertDatas :: [(MusicData, [PlayerMusicDataInfo])] -> PlayerData
convertDatas datas = M.fromListWith (++) $ convertDatas' datas []
  where convertDatas' xs ret = concat $ Prelude.foldl (\ret' x -> convertData x : ret') ret xs

convertData :: (MusicData, [PlayerMusicDataInfo]) -> [(DJ, [Result])]
convertData dat = convertData' dat []
  where convertData' (_, []) ret = ret
        convertData' (mData, x:xs) ret = convertData' (mData, xs) (convData mData x : ret)

convData :: MusicData -> PlayerMusicDataInfo -> (DJ, [Result])
convData mData pData = (djData, [result])
  where bmsId = dBmsID mData
        level = dLevel mData
        exScore = dScore pData
        dJLv = dDJLv pData
        clear = dClearStatus pData
        djName = dPlayerName pData
        playerID = dPlayerID pData
        djData = DJ djName playerID
        result = Result bmsId level exScore dJLv clear 

-- Ranking 関連
clrPt :: T.Text -> Int
clrPt str | str == "★FULLCOMBO" = 30
          | str == "FULLCOMBO" = 30
          | str == "HARD" = 20
          | str == "CLEAR" = 10
          | str == "EASY" = 5
          | otherwise = 0

sortedClearRanking :: [ClearRanking] -> [ClearRanking]
sortedClearRanking = L.sortBy cmp
  where cmp x y = let c1 = comparing dCLEARPT y x
                  in if c1 /= EQ then c1
                     else let c2 = comparing dFCCNT y x
                          in if c2 /= EQ then c2
                             else let c3 = comparing dHCNT y x
                                  in if c3 /= EQ then c3
                                     else let c4 = comparing dNCNT y x
                                          in if c4 /= EQ then c4
                                             else let c5 = comparing dECNT y x
                                                  in if c5 /= EQ then c5
                                                     else let c6 = comparing dFCNT y x
                                                          in if c6 /= EQ then c6
                                                             else comparing dPLAYERID x y


getClearRanking :: [(MusicData, [PlayerMusicDataInfo])] -> [ClearRanking]
getClearRanking = createClearRanking . convertDatas

createClearRanking :: PlayerData -> [ClearRanking]
createClearRanking pData = sortedClearRanking createRanking
  where createRanking = Prelude.map calcClearRanking $ M.toList pData

calcClearRanking :: (DJ, [Result]) -> ClearRanking
calcClearRanking (dj, res) = ClearRanking djname playerid clearpt fccnt hcnt ncnt ecnt fcnt
  where djname = dDJName dj
        playerid = dPlayerId dj
        clearpt = fccnt * clrPt "FULLCOMBO" + hcnt * clrPt "HARD" + ncnt * clrPt "CLEAR" + ecnt * clrPt "EASY"
        (fccnt, hcnt, ncnt, ecnt, fcnt) = clearStatusCount res

djLvPt :: T.Text -> Int
djLvPt str | str == "AAA" = 20
           | str == "AA" = 15
           | str == "A" = 10
           | otherwise = 0

sortedSSPtRanking :: [ScratchSkillPtRanking] -> [ScratchSkillPtRanking]
sortedSSPtRanking = L.sortBy cmp
  where cmp x y = let c1 = comparing dSKILLPT y x
                  in if c1 /= EQ then c1
                     else let c2 = comparing dAAA y x
                  in if c2 /= EQ then c2
                     else let c3 = comparing dAA y x
                  in if c3 /= EQ then c3
                     else let c4 = comparing dA y x
                  in if c4 /= EQ then c4
                     else let c5 = comparing dFCCNT' y x
                  in if c5 /= EQ then c5
                     else let c6 = comparing dHCNT' y x
                  in if c6 /= EQ then c6
                     else let c7 = comparing dNCNT' y x
                  in if c7 /= EQ then c7
                     else let c8 = comparing dECNT' y x
                  in if c8 /= EQ then c8
                     else let c9 = comparing dFCNT' y x
                  in if c9 /= EQ then c9
                     else comparing dPLAYERID' x y

getScratchSkillPtRanking :: [(MusicData, [PlayerMusicDataInfo])] -> [ScratchSkillPtRanking]
getScratchSkillPtRanking = createScratchSkillPtRanking . convertDatas

createScratchSkillPtRanking :: PlayerData -> [ScratchSkillPtRanking]
createScratchSkillPtRanking pData = sortedSSPtRanking scratchSkillPtRanking
  where scratchSkillPtRanking = Prelude.map calcScratchSkillPtRanking $ M.toList pData

calcSSPt :: ExScore -> DjLv -> Clear -> Int
calcSSPt scoreStr djlvStr clearStr = score * (100 + djlvpt + clearpt)
  where score = read (T.unpack scoreStr) :: Int
        djlvpt = djLvPt djlvStr
        clearpt = clrPt clearStr

calcScratchSkillPtRanking :: (DJ, [Result]) -> ScratchSkillPtRanking
calcScratchSkillPtRanking (dj, res) = ScratchSkillPtRanking djname playerid sspt aaacnt aacnt acnt fccnt hcnt ncnt ecnt fcnt
  where djname = dDJName dj
        playerid = dPlayerId dj
        sspt = sum $ Prelude.map (\x -> calcSSPt (dExScore x) (dDjLv x) (dClear x)) res
--        clearpt = fccnt * clrPt "FULLCOMBO" + hcnt * clrPt "HARD" + ncnt * clrPt "CLEAR" + ecnt * clrPt "EASY"
        aaacnt = L.length $ L.filter (\x -> dDjLv x  == "AAA") res
        aacnt = L.length $ L.filter (\x -> dDjLv x == "AA") res
        acnt = L.length $ L.filter (\x -> dDjLv x == "A") res
        (fccnt, hcnt, ncnt, ecnt, fcnt) = clearStatusCount res

clearStatusCount :: [Result] -> (Int, Int, Int, Int, Int)
clearStatusCount res = (fccnt, hcnt, ncnt, ecnt, fcnt)
  where fccnt = L.length $ L.filter (\x -> "FULLCOMBO" `T.isInfixOf` dClear x) res
        hcnt = L.length $ L.filter (\x -> dClear x == "HARD") res
        ncnt = L.length $ L.filter (\x -> dClear x == "CLEAR") res
        ecnt = L.length $ L.filter (\x -> dClear x == "EASY") res
        fcnt = L.length $ L.filter (\x -> dClear x == "FAILED") res

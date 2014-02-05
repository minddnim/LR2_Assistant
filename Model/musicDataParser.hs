-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

module MusicDataParser
(
  MusicData(..),
  getMusicDatas,
  getMusicDataSrc
) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T hiding (takeWhile)
import Control.Applicative

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

--main :: IO()
--main = print $ show $ getMusicDatas "var mname = [[77,\"<font size=\"5\" color=\"#ff0000\">◎2</font>\",      \"P8107 [yumether◎]\",      \"126849\",      \"<a href='http://yumerusabun.web.fc2.com/nue.html'>夢瑠</a>\",      \"<a href='http://web.archive.org/web/20070111193502/www107.sakura.ne.jp/~onoken/htmls/bms.html'>ONOKEN</a>\",    ],];"

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

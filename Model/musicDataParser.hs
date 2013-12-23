-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

module MusicDataParser
(
  MusicData(..),
  getMusicDatas
) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T
import Control.Applicative

data MusicData = MusicData {
  dID :: T.Text,
  dLevel :: T.Text,
  dMusicTitle :: T.Text,
  dBmsID :: T.Text,
  dOrgArtist :: T.Text,
  dOrgArtistURL :: T.Text,
  dScoreSite :: T.Text,
  dScoreSiteURL :: T.Text,
  dComment :: T.Text
} deriving (Show)


getMusicDatas :: T.Text -> [MusicData]
getMusicDatas str =
  case T.parseOnly pMusicDatas formatStr of
    Left _ -> error "parse error."
    Right results -> results
  where formatStr = formatMusicData str

pMusicDatas :: T.Parser [MusicData]
pMusicDatas = do
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
  T.char '['
  iD <- pID
  T.char ','
  level <- pLevel
  T.char ','
  musicTitle <- pMusicTitle
  T.char ','
  bmsID <- pBmsID
  T.char ','
  (artUrl, artName) <- pOrgArtistInfo
  T.char ','
  (siteURL, siteName) <- pScoreSiteInfo
  T.char ','
  comment <- pComment
  T.char ','
  T.char ']'
  T.char ','
  many T.space
  return $ MusicData iD level musicTitle bmsID artName artUrl siteName siteURL comment

pID :: T.Parser T.Text
pID = T.takeTill $ T.inClass ","

pLevel :: T.Parser T.Text
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

pLevelHtmlTag :: T.Parser T.Text
pLevelHtmlTag = do
  T.char '"'
  pHtmlTag
  level <- T.takeTill $ T.inClass "<"
  pHtmlTag
  T.char '"'
  return level

pLevelHtmlNoTag :: T.Parser T.Text
pLevelHtmlNoTag = pGetInfo

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

pGetInfo :: T.Parser T.Text
pGetInfo = do
  T.char '"'
  info <- T.takeTill $ T.inClass "\""
  T.char '"'
  return info


formatMusicData :: T.Text -> T.Text
formatMusicData str = defMusicSrc $ T.lines str

defMusicSrc :: [T.Text] -> T.Text
defMusicSrc = remOnlyNewLine . T.unlines . remBeginningSpace . searchMusicDefEnd . searchMusicDefStart

searchMusicDefStart :: [T.Text] -> [T.Text]
searchMusicDefStart (x:xs) | "var mname" `T.isInfixOf` x = x:xs
                           | otherwise = searchMusicDefStart xs

searchMusicDefEnd :: [T.Text] -> [T.Text]
searchMusicDefEnd src = searchMusicDefEnd' src [""]
  where searchMusicDefEnd' [] _ = error "Don't search Music Def End"
        searchMusicDefEnd' (x:xs) ret | "];" `T.isInfixOf` x = reverse (x:ret)
                                      | otherwise = searchMusicDefEnd' xs (x:ret)

remBeginningSpace :: [T.Text] -> [T.Text]
remBeginningSpace = map T.stripStart

remOnlyNewLine :: T.Text -> T.Text
remOnlyNewLine = T.filter (/= '\n') . T.filter (/= '\r')

-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List as L
import Data.Time
import System.Locale
import System.Argv0
import Network.HTTP
import MusicDataParser

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest　x)

main :: IO()
main = do
  exeFileName <- getArgv0
  contents <- readFile $ getDirPath (show exeFileName) ++ "tableURL.txt"
  htmlSrc <- openURL $ head $ lines contents
  zonedTime <- getZonedTime
  let musicDatas = getMusicDatas $ T.pack htmlSrc
      ymdString = formatTime defaultTimeLocale "%F" zonedTime
      hmsString = T.splitOn ":" $ T.pack $ formatTime defaultTimeLocale "%T" zonedTime
      timeString = concatMap T.unpack $ intersperse "-" hmsString
      outFilename = "musicData_" ++ ymdString ++ "_" ++timeString ++ ".tsv"
  T.writeFile outFilename $ foldl T.append "" $ getColumnName : getMusicDatasText musicDatas

getColumnName :: T.Text
getColumnName = str
  where str = mLevel `T.append` mMusicTitle `T.append` mBmsID `T.append` mOrgArtist `T.append` mOrgArtistURL `T.append` mScoreSite `T.append` mScoreSiteURL `T.append` mComment `T.append` "\n"
        mLevel = "Level"
        mMusicTitle = derimita `T.append` "MusicTitle"
        mBmsID = derimita `T.append` "BmsID"
        mOrgArtist = derimita `T.append` "OrgArtist"
        mOrgArtistURL = derimita `T.append` "OrgArtistURL"
        mScoreSite = derimita `T.append` "ScoreSite"
        mScoreSiteURL = derimita `T.append` "ScoreSiteURL"
        mComment = derimita `T.append` "Comment"

getMusicDatasText :: [MusicData] -> [T.Text]
getMusicDatasText dats = getMusicDatasText' dats []
  where getMusicDatasText' [] ret = reverse ret
        getMusicDatasText' (x:xs) ret = getMusicDatasText' xs (getMusicDataText x : ret)

getMusicDataText :: MusicData -> T.Text
getMusicDataText dat = str
  where str = mLevel `T.append` mMusicTitle `T.append` mBmsID `T.append` mOrgArtist `T.append` mOrgArtistURL `T.append` mScoreSite `T.append` mScoreSiteURL `T.append` mComment `T.append` "\n"
        mLevel = dLevel dat
        mMusicTitle = derimita `T.append` dMusicTitle dat
        mBmsID = derimita `T.append` dBmsID dat
        mOrgArtist = derimita `T.append` dOrgArtist dat
        mOrgArtistURL = derimita `T.append` dOrgArtistURL dat
        mScoreSite = derimita `T.append` dScoreSite dat
        mScoreSiteURL = derimita `T.append` dScoreSiteURL dat
        mComment = derimita `T.append` dComment dat

getDirPath :: String -> String
getDirPath str = tail $ reverse $ dropWhile ('\\' /=) $ reverse $ dropWhile ('"' /=) str

derimita :: T.Text
derimita = "\t"
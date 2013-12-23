-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP

import MusicDataParser

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest　x)

main :: IO()
main = do
--  htmlSrc <- openURL "http://flowermaster.web.fc2.com/lrnanido_sara.html"
  htmlSrc <- openURL "http://ameria3141.web.fc2.com/hakkyou_nanidohyou_no.2/hakkyou_nanidohyou_no.2.html"
  let musicDatas = getMusicDatas $ T.pack htmlSrc
  T.writeFile "musicData.txt" $ foldl T.append "" $ getMusicDatasText musicDatas

getMusicDatasText :: [MusicData] -> [T.Text]
getMusicDatasText dats = getMusicDatasText' dats []
  where getMusicDatasText' [] ret = reverse ret
        getMusicDatasText' (x:xs) ret = getMusicDatasText' xs (getMusicDataText x : ret)

getMusicDataText :: MusicData -> T.Text
getMusicDataText dat = str
  where str = mId `T.append` mLevel `T.append` mMusicTitle `T.append` mBmsID `T.append` mOrgArtist `T.append` mOrgArtistURL `T.append` mScoreSite `T.append` mScoreSiteURL `T.append` mComment `T.append` "\n"
        mId = dID dat
        mLevel = ", " `T.append` dLevel dat
        mMusicTitle = ", " `T.append` dMusicTitle dat
        mBmsID = ", " `T.append` dBmsID dat
        mOrgArtist = ", " `T.append` dOrgArtist dat
        mOrgArtistURL = ", " `T.append` dOrgArtistURL dat
        mScoreSite = ", " `T.append` dScoreSite dat
        mScoreSiteURL = ", " `T.append` dScoreSiteURL dat
        mComment = ", " `T.append` dComment dat

import Network.HTTP
import Codec.Binary.UTF8.String

import Data.List
import MusicDataParser

main :: IO()
main = do
--  htmlSrc <- openURL "http://flowermaster.web.fc2.com/lrnanido_sara.html"
  htmlSrc <- openURL "http://ameria3141.web.fc2.com/hakkyou_nanidohyou_no.2/hakkyou_nanidohyou_no.2.html"
  let defMusicDatasSrc = defMusicSrc $ lines $ decodeString htmlSrc
      musicDatas = getMusicDatas defMusicDatasSrc
  writeFile "musicData.txt" $ encodeString $ show musicDatas
  print musicDatas
  aaa <- getLine
  print aaa

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest　x)

defMusicSrc :: [String] -> String
defMusicSrc = remOnlyNewLine . unlines . remBeginningSpace . searchMusicDefEnd . searchMusicDefStart

searchMusicDefStart :: [String] -> [String]
searchMusicDefStart (x:xs) | "var mname" `isInfixOf` x = x:xs
                           | otherwise = searchMusicDefStart xs

searchMusicDefEnd :: [String] -> [String]
searchMusicDefEnd src = searchMusicDefEnd' src [""]
  where searchMusicDefEnd' [] _ = error "Don't search Music Def End"
        searchMusicDefEnd' (x:xs) ret | "];" `isInfixOf` x = reverse ret
                                      | otherwise = searchMusicDefEnd' xs (x:ret)

remBeginningSpace :: [String] -> [String]
remBeginningSpace = map (dropWhile (== ' '))

remOnlyNewLine :: String -> String
remOnlyNewLine = filter (/= '\n') . filter (/= '\r')



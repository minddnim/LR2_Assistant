import Network.HTTP
import Data.List

main :: IO()
main = do
  htmlSrc <- openURL "http://flowermaster.web.fc2.com/lrnanido_sara.html"
  let musicDatas = defMusicSrc $ lines htmlSrc
  writeFile "musicData.txt" musicDatas
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



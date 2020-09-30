import Data.Maybe (listToMaybe)
import qualified System.Directory as Dir (getDirectoryContents)
import qualified System.Posix.Files as Files (getFileStatus, isDirectory)
import qualified System.Environment as Env (getArgs) 

main :: IO ()
main = do
  args <- Env.getArgs
  isDir <- mapM Files.getFileStatus args >>= mapM (return . Files.isDirectory)
  case listToMaybe isDir of
    Nothing -> error $ "Specify arguments pussy"
    Just False -> error $ "The file is not a directory"
    Just True -> putStrLn (head args) >>  drawDir (head args) (head args) 

drawFile :: FilePath -> FilePath -> FilePath -> IO ()
drawFile inPath fullPath name = do
  putStrLn $ (++) "|----" name
  isDir <- Files.getFileStatus (fullPath ++ name) >>= return . Files.isDirectory
  case isDir of
    True -> drawDir inPath (fullPath ++ name ++ "/")
    False -> return ()

drawDir :: FilePath -> FilePath -> IO ()
drawDir inPath fullPath = do
  dirContents  <- Dir.getDirectoryContents fullPath                                              
  mapM_ (spaceDrawer inPath fullPath) (filter (\file -> head file /= '.') dirContents)

amOfSpaces :: FilePath -> FilePath -> Int
amOfSpaces inPath fullPath = 5 * count '/' (drop (length inPath) fullPath)

count :: Char -> String -> Int
count c str = length $ filter (== c) str 

printSpaces :: FilePath -> FilePath -> FilePath -> IO () 
printSpaces inPath fullPath file = putStr (concat $ replicate (amOfSpaces inPath fullPath) " ")

spaceDrawer :: FilePath -> FilePath -> FilePath -> IO ()
spaceDrawer inPath fullPath fileName = printSpaces inPath fullPath fileName >> drawFile inPath fullPath fileName

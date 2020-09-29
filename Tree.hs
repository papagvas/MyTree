import Data.Maybe (listToMaybe)
import qualified System.Directory as Dir (getDirectoryContents)
import qualified System.Posix.Files as Files (getFileStatus, isDirectory)
import qualified System.Environment as Env (getArgs) 

main :: IO ()
main = do
  isDir <- Env.getArgs >>= mapM Files.getFileStatus >>= mapM (return . Files.isDirectory)
  inPath <- Env.getArgs >>= return . head
  case listToMaybe isDir of
    Nothing -> error $ "Specify arguments pussy"
    Just False -> error $ "The file is not a directory"
    Just True -> Env.getArgs >>= makeTree (0, length inPath)

makeTree :: (Int, Int) -> [FilePath] -> IO ()
makeTree spaces dirs = do
  putStr $ "|" ++ (concat $ replicate (fst spaces - snd spaces) " ") 
  putStrLn dirPath  
  dirContents  <- Dir.getDirectoryContents dirPath
  mapM_ (drawCont spaces) $ map (dirPath ++) (filter (\file -> head file /= '.') dirContents)
  where
    dirPath = head dirs

drawCont :: (Int, Int) -> FilePath -> IO ()
drawCont spaces fileOrDir = do
  isDir <- Files.getFileStatus fileOrDir >>= return . Files.isDirectory
  case isDir of
    True -> makeTree (fst spaces + (length $ fileOrDir ++ "/"), snd spaces) [fileOrDir ++ "/"] 
    False -> putStrLn $ drop (snd spaces) fileOrDir   

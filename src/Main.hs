module Main where

import System.FilePath.Find ((==?), find, always, fileType, (&&?), FileType(RegularFile), extension)
import Control.Exception (evaluate)
import Data.List (isSuffixOf)
import Java
import DependencyGraph (getUnused)
import Data.Either (rights)
import Config

main :: IO ()
main = do
  paths <- getFilePaths srcPath
  files <- mapM readFileStrict paths
  userProvidedClasses <- getAdditionalClasses

  let classes = asParseResult userProvidedClasses : parse (zip paths files)
  printUnusedComponents classes

  where parse = map parseAst . rights . map toAst
        printUnusedComponents = mapM_ print
                              . filter (\x -> removeBlacklistedClassSuffixes x && x /= "")
                              . map (fileName . fst3)
                              . getUnused

asParseResult :: [String] -> Result
asParseResult e = Result { fileName = ""
                         , imports = e
                         , references = []
                         , topLevelAnnotations = []
                         , methodAnnotations = []
                         , implements = []
                         , autowired = [] }

getFilePaths :: String -> IO [FilePath]
getFilePaths = fmap (filter removeBlacklistedFiles) . find always onlyJavaFiles
               where onlyJavaFiles = fileType ==? RegularFile
                                 &&? extension ==? ".java"
readFileStrict :: FilePath -> IO String
readFileStrict path = do
  file <- readFile path
  _ <- evaluate $ length file
  return file

removeBlacklistedFiles :: String -> Bool
removeBlacklistedFiles s = not.or $ map (\x -> isSuffixOf x s) blacklistedFiles

removeBlacklistedClassSuffixes :: String -> Bool
removeBlacklistedClassSuffixes s = not.or $ map (\x -> isSuffixOf x s) blacklistedClassSuffixes

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
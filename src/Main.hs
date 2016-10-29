module Main where

import Control.Exception (evaluate)
import Data.List (isSuffixOf)
import Java (importsToParseResult, parseAst, toAst, fileName, Result)
import DependencyGraph (getUnused)
import Data.Either (rights)
import Config
import System.FilePath.Find (
  (==?), find, always, fileType, (&&?),
  FileType(RegularFile), extension)

type FileContent = String
type File = (FilePath, FileContent)

main :: IO ()
main = do
  paths <- getFilePaths srcPath
  files <- mapM readFileStrict paths
  userProvidedClasses <- getAdditionalClasses

  let classes = importsToParseResult userProvidedClasses : parseFiles (zip paths files)
  printUnusedClasses classes

parseFiles :: [File] -> [Result]
parseFiles = map parseAst . rights . map toAst

printUnusedClasses :: [Result] -> IO ()
printUnusedClasses = mapM_ print
                   . filter (\x -> removeBlacklistedClassSuffixes x && x /= "")
                   . map (fileName . fst3)
                   . getUnused
  where fst3 (a, _, _) = a

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
removeBlacklistedFiles s = not.or $ map (`isSuffixOf` s) blacklistedFiles

removeBlacklistedClassSuffixes :: String -> Bool
removeBlacklistedClassSuffixes s = not.or $ map (`isSuffixOf` s) blacklistedClassSuffixes

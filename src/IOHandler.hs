module IOHandler where

import DataTypes
import Utils
import System.IO
import Control.Exception (catch, IOException)

readStudentData :: FilePath -> IO (Either String [Student])
readStudentData path = do
  catch (do
    content <- readFile path
    let students = parseCSV content
    return (Right students)
    ) handleError
  where
    handleError :: IOException -> IO (Either String [Student])
    handleError e = return (Left $ "Error reading file: " ++ show e)

parseCSV :: String -> [Student]
parseCSV content =
  let csvLines = lines content
      nonEmptyLines = filter (not . null) csvLines
  in map parseLine nonEmptyLines

parseLine :: String -> Student
parseLine line =
  let parts = splitOn ',' line
      trimmedParts = map trim parts
  in case trimmedParts of
       (idStr:name:markStrs) ->
         Student
           { studentId = read idStr :: Int
           , studentName = name
           , marks = map (\m -> read m :: Double) markStrs
           }
       _ -> Student 0 "Invalid" []

writeReport :: FilePath -> [StudentReport] -> ClassSummary -> IO ()
writeReport path reports summary = do
  let content = generateReportContent reports summary
  writeFile path content
  putStrLn $ "Report written to: " ++ path

generateReportContent :: [StudentReport] -> ClassSummary -> String
generateReportContent reports summary =
  let header = "STUDENT PERFORMANCE REPORT\n" ++
               "===========================\n\n"
      studentSection = unlines (map formatStudentReport reports)
      summarySection = "\n" ++ formatSummary summary
  in header ++ studentSection ++ summarySection

displayMenu :: IO ()
displayMenu = do
  putStrLn "\n================================"
  putStrLn "  STUDENT ANALYZER MENU"
  putStrLn "================================"
  putStrLn "1. Show All Students"
  putStrLn "2. Show Passing Students"
  putStrLn "3. Show Failing Students"
  putStrLn "4. Show Top Performers"
  putStrLn "5. Show Statistics"
  putStrLn "6. Export Report"
  putStrLn "7. Exit"
  putStrLn "================================"
  putStr "Enter choice: "
  hFlush stdout
module Main where

import DataTypes
import Processing
import IOHandler
import Utils
import System.IO
import System.Environment (getArgs)
import Control.Parallel.Strategies (parMap, rdeepseq)

main :: IO ()
main = do
  putStrLn "======================================"
  putStrLn "  STUDENT PERFORMANCE ANALYZER"
  putStrLn "  (with Parallel Processing Support)"
  putStrLn "======================================"
  putStrLn ""
  
  -- Read data from CSV
  putStrLn "Loading student data..."
  result <- readStudentData "data/students.csv"
  
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right students -> do
      let studentCount = length students
      putStrLn $ "Successfully loaded " ++ show studentCount ++ " students"
      
      -- DEMONSTRATE PARALLEL PROCESSING
      if studentCount > 10
        then do
          putStrLn "Using PARALLEL processing (dataset > 10 students)..."
          putStrLn "Processing distributed across CPU cores for faster results."
        else do
          putStrLn "Using sequential processing (small dataset)..."
      
      -- Process students (parallel for large datasets)
      let reports = if studentCount > 10
                    then processAllStudentsParallel students
                    else processAllStudentsSequential students
          
          summary = if studentCount > 10
                    then generateSummaryParallel reports
                    else generateSummary reports
          
          sortedReports = if studentCount > 10
                         then sortByAverageParallel reports
                         else sortByAverage reports
      
      putStrLn "✓ Processing complete!\n"
      
      -- Interactive menu loop
      menuLoop students reports summary sortedReports

-- Rest of menuLoop function stays the same...
menuLoop :: [Student] -> [StudentReport] -> ClassSummary -> [StudentReport] -> IO ()
menuLoop students reports summary sortedReports = do
  displayMenu
  choice <- getLine
  
  case choice of
    "1" -> do
      putStrLn "\n=== ALL STUDENTS ==="
      mapM_ (putStrLn . formatStudentReport) sortedReports
      menuLoop students reports summary sortedReports
      
    "2" -> do
      let passing = getPassingStudents reports
      putStrLn $ "\n=== PASSING STUDENTS (" ++ show (length passing) ++ ") ==="
      mapM_ (putStrLn . formatStudentReport) passing
      menuLoop students reports summary sortedReports
      
    "3" -> do
      let failing = getFailingStudents reports
      putStrLn $ "\n=== FAILING STUDENTS (" ++ show (length failing) ++ ") ==="
      mapM_ (putStrLn . formatStudentReport) failing
      menuLoop students reports summary sortedReports
      
    "4" -> do
      let top5 = take 5 sortedReports
      putStrLn "\n=== TOP 5 PERFORMERS ==="
      mapM_ (putStrLn . formatStudentReport) top5
      menuLoop students reports summary sortedReports
      
    "5" -> do
      putStrLn ""
      putStrLn (formatSummary summary)
      menuLoop students reports summary sortedReports
      
    "6" -> do
      writeReport "output_report.txt" sortedReports summary
      menuLoop students reports summary sortedReports
      
    "7" -> do
      putStrLn "\nThank you for using Student Analyzer!"
      return ()
      
    _ -> do
      putStrLn "\nInvalid choice. Please try again."
      menuLoop students reports summary sortedReports
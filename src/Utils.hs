module Utils where

import DataTypes
import Data.List (intercalate)

gradeToString :: Grade -> String
gradeToString APlus = "A+ (Excellent)"
gradeToString A = "A (Excellent)"
gradeToString BPlus = "B+ (Good)"
gradeToString B = "B (Average)"
gradeToString C = "C (Average)"
gradeToString F = "F (Failed)"

formatStudentReport :: StudentReport -> String
formatStudentReport report =
  let s = reportStudent report
      avg = average report
      grd = grade report
  in "ID: " ++ show (studentId s) ++
     " | Name: " ++ studentName s ++
     " | Average: " ++ formatDouble avg ++
     " | Grade: " ++ gradeToString grd

formatDouble :: Double -> String
formatDouble x = show (fromIntegral (round (x * 100)) / 100)

formatSummary :: ClassSummary -> String
formatSummary summary =
  unlines
    [ "================================"
    , "    CLASS SUMMARY STATISTICS    "
    , "================================"
    , "Total Students:   " ++ show (totalStudents summary)
    , "Passed:           " ++ show (passCount summary)
    , "Failed:           " ++ show (failCount summary)
    , "Pass Rate:        " ++ formatDouble (passRate summary) ++ "%"
    , "Class Average:    " ++ formatDouble (classAverage summary)
    , "Highest Score:    " ++ formatDouble (highestScore summary)
    , "Lowest Score:     " ++ formatDouble (lowestScore summary)
    , "================================"
    ]
  where
    passRate s = if totalStudents s > 0
                 then (fromIntegral (passCount s) / fromIntegral (totalStudents s)) * 100
                 else 0.0

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str =
  let (first, rest) = break (== delim) str
  in first : case rest of
               [] -> []
               (_:remainder) -> splitOn delim remainder

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')
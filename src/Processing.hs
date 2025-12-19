module Processing where

import DataTypes

import Control.Parallel.Strategies (parMap, rdeepseq, using, parList)
import Control.Parallel (par, pseq)

-- Calculate average marks
calculateAverage :: [Double] -> Double
calculateAverage [] = 0.0
calculateAverage marks = sum marks / fromIntegral (length marks)

assignGrade :: Double -> Grade
assignGrade avg
  | avg >= 90 = APlus
  | avg >= 80 = A
  | avg >= 70 = BPlus
  | avg >= 60 = B
  | avg >= 50 = C
  | otherwise = F

determinePerformance :: Grade -> Performance
determinePerformance APlus = Excellent
determinePerformance A = Excellent
determinePerformance BPlus = Good
determinePerformance B = Average
determinePerformance C = Average
determinePerformance F = Poor

processStudent :: Student -> StudentReport
processStudent student =
  let avg = calculateAverage (marks student)
      grd = assignGrade avg
      perf = determinePerformance grd
  in StudentReport student avg grd perf

-- Process list of students
processAllStudents :: [Student] -> [StudentReport]
processAllStudents [] = []
processAllStudents (s:ss) = processStudent s : processAllStudents ss

-- Get passing students (grade >= C)
getPassingStudents :: [StudentReport] -> [StudentReport]
getPassingStudents = filter (\r -> grade r /= F)

-- Get failing students
getFailingStudents :: [StudentReport] -> [StudentReport]
getFailingStudents = filter (\r -> grade r == F)

-- Find maximum average
findMaxAverage :: [StudentReport] -> Double
findMaxAverage [] = 0.0
findMaxAverage [r] = average r
findMaxAverage (r:rs) = max (average r) (findMaxAverage rs)

-- Find minimum average
findMinAverage :: [StudentReport] -> Double
findMinAverage [] = 0.0
findMinAverage [r] = average r
findMinAverage (r:rs) = min (average r) (findMinAverage rs)

-- Calculate class average
calculateClassAverage :: [StudentReport] -> Double
calculateClassAverage [] = 0.0
calculateClassAverage reports =
  let totalAvg = foldr (\r acc -> average r + acc) 0.0 reports
      count = fromIntegral (length reports)
  in totalAvg / count

--  Generate summary statistics
generateSummary :: [StudentReport] -> ClassSummary
generateSummary reports =
  let total = length reports
      passing = length (getPassingStudents reports)
      failing = length (getFailingStudents reports)
      classAvg = calculateClassAverage reports
      highest = if null reports then 0.0 else findMaxAverage reports
      lowest = if null reports then 0.0 else findMinAverage reports
  in ClassSummary total passing failing classAvg highest lowest

-- Sort students by average (descending)
sortByAverage :: [StudentReport] -> [StudentReport]
sortByAverage [] = []
sortByAverage (pivot:rest) =
  let higher = sortByAverage [r | r <- rest, average r >= average pivot]
      lower = sortByAverage [r | r <- rest, average r < average pivot]
  in higher ++ [pivot] ++ lower

  -- ==================================================================
-- PARALLEL AND CONCURRENT PROCESSING SECTION
-- ==================================================================

processAllStudentsParallel :: [Student] -> [StudentReport]
processAllStudentsParallel students = 
  parMap rdeepseq processStudent students

processAllStudentsSequential :: [Student] -> [StudentReport]
processAllStudentsSequential = map processStudent


calculateClassAverageParallel :: [StudentReport] -> Double
calculateClassAverageParallel [] = 0.0
calculateClassAverageParallel reports =
  let averages = map average reports `using` parList rdeepseq
      total = sum averages
      count = fromIntegral (length reports)
  in total / count


getPassingStudentsParallel :: [StudentReport] -> [StudentReport]
getPassingStudentsParallel reports =
  filter (\r -> grade r /= F) reports `using` parList rdeepseq


sortByAverageParallel :: [StudentReport] -> [StudentReport]
sortByAverageParallel [] = []
sortByAverageParallel [x] = [x]
sortByAverageParallel (pivot:rest) =
  let lowerPart = sortByAverageParallel [r | r <- rest, average r < average pivot]
      higherPart = sortByAverageParallel [r | r <- rest, average r >= average pivot]
  in higherPart `par` (lowerPart `pseq` (higherPart ++ [pivot] ++ lowerPart))
 
processTopNParallel :: Int -> [Student] -> [StudentReport]
processTopNParallel n students =
  take n (sortByAverageParallel (processAllStudentsParallel students))
  
processStudentsInChunks :: Int -> [Student] -> [StudentReport]
processStudentsInChunks chunkSize students =
  let chunks = chunksOf chunkSize students
      processChunk = map processStudent
      parallelChunks = parMap rdeepseq processChunk chunks
  in concat parallelChunks


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = 
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest


generateSummaryParallel :: [StudentReport] -> ClassSummary
generateSummaryParallel reports =
  let total = length reports
      passing = length (getPassingStudentsParallel reports)
      failing = total - passing
      classAvg = calculateClassAverageParallel reports
      highest = if null reports then 0.0 else findMaxAverage reports
      lowest = if null reports then 0.0 else findMinAverage reports
  in passing `par` failing `par` classAvg `par`
     ClassSummary total passing failing classAvg highest lowest


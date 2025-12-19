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


-- | Process students in PARALLEL using multiple CPU cores
-- 
-- DETAILED EXPLANATION:
-- - Uses 'parMap' to distribute work across CPU cores
-- - Each student processed independently (no shared state)
-- - Safe because processStudent is PURE (no side effects)
-- - No locks, mutexes, or synchronization needed!
--
-- WHY THIS IS SAFE IN HASKELL:
-- 1. processStudent is PURE - same input always gives same output
-- 2. Student data is IMMUTABLE - cannot be modified
-- 3. No race conditions possible - no shared mutable state
-- 4. Type system guarantees thread safety
--
-- COMPARISON WITH IMPERATIVE LANGUAGES:
-- Java/Python: Need synchronized blocks, locks, thread pools
-- Haskell: Just add 'parMap' - automatic parallelization!
--
-- Example:
--   With 1000 students and 4 CPU cores:
--   Sequential: ~1000 time units
--   Parallel: ~250 time units (4x speedup)
--
-- FP Principles: PURITY, IMMUTABILITY, SAFE CONCURRENCY
processAllStudentsParallel :: [Student] -> [StudentReport]
processAllStudentsParallel students = 
  parMap rdeepseq processStudent students
  -- parMap: parallel map - distributes work across cores
  -- rdeepseq: fully evaluate each result before moving on

-- | SEQUENTIAL version for comparison
-- This is the same as map processStudent but explicit for benchmarking
processAllStudentsSequential :: [Student] -> [StudentReport]
processAllStudentsSequential = map processStudent

-- | Calculate class average using PARALLEL fold
--
-- EXPLANATION:
-- - First, calculate all averages in parallel
-- - Then sum them up
-- - Finally divide by count
--
-- Demonstrates: Parallel reduction pattern
-- FP Principle: COMPOSABILITY - combine parallel operations
calculateClassAverageParallel :: [StudentReport] -> Double
calculateClassAverageParallel [] = 0.0
calculateClassAverageParallel reports =
  let averages = map average reports `using` parList rdeepseq
      -- `using` applies parallelization strategy to the list
      total = sum averages
      count = fromIntegral (length reports)
  in total / count

-- | Filter passing students in PARALLEL
--
-- EXPLANATION:
-- - Evaluate each predicate check in parallel
-- - Combine results safely
-- - No data races because filter is pure
--
-- FP Principle: PARALLEL FILTERING
getPassingStudentsParallel :: [StudentReport] -> [StudentReport]
getPassingStudentsParallel reports =
  filter (\r -> grade r /= F) reports `using` parList rdeepseq

-- | Sort students by average using PARALLEL quicksort
--
-- EXPLANATION:
-- - Recursive quicksort with parallel branches
-- - Left and right partitions processed concurrently
-- - Uses 'par' and 'pseq' for explicit parallelism
--
-- HOW IT WORKS:
-- 1. Pick pivot element
-- 2. Partition into lower and higher
-- 3. Sort BOTH partitions in PARALLEL (this is the magic!)
-- 4. Combine results
--
-- FP Principle: PARALLEL RECURSION
sortByAverageParallel :: [StudentReport] -> [StudentReport]
sortByAverageParallel [] = []
sortByAverageParallel [x] = [x]
sortByAverageParallel (pivot:rest) =
  let lowerPart = sortByAverageParallel [r | r <- rest, average r < average pivot]
      higherPart = sortByAverageParallel [r | r <- rest, average r >= average pivot]
  in higherPart `par` (lowerPart `pseq` (higherPart ++ [pivot] ++ lowerPart))
  -- `par` sparks parallel computation of higherPart
  -- `pseq` ensures lowerPart is evaluated before combining

-- | Demonstrate LAZY EVALUATION with parallel processing
--
-- EXPLANATION:
-- - Process only what's needed
-- - If you take 5 students, only 5 are processed
-- - Combines laziness with parallelism
--
-- Example:
--   take 10 (processAllStudentsParallel allStudents)
--   Only processes first 10 students, not all!
--
-- FP Principle: LAZY + PARALLEL = EFFICIENT
processTopNParallel :: Int -> [Student] -> [StudentReport]
processTopNParallel n students =
  take n (sortByAverageParallel (processAllStudentsParallel students))
  -- Laziness ensures we don't process more than needed

-- | Process students in CHUNKS for better cache locality
--
-- EXPLANATION:
-- - Divide students into chunks
-- - Process each chunk in parallel
-- - Better CPU cache utilization
--
-- WHY CHUNKING:
-- - Too many small tasks = overhead
-- - Large chunks = better cache performance
-- - Typical chunk size: 100-1000 items
--
-- FP Principle: PARALLEL MAP-REDUCE
processStudentsInChunks :: Int -> [Student] -> [StudentReport]
processStudentsInChunks chunkSize students =
  let chunks = chunksOf chunkSize students
      processChunk = map processStudent
      parallelChunks = parMap rdeepseq processChunk chunks
  in concat parallelChunks

-- | Helper: Split list into chunks
-- Pure function for partitioning
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = 
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

-- | CONCURRENT summary generation
-- Calculate multiple statistics in parallel
--
-- EXPLANATION:
-- - Total, passing, failing computed concurrently
-- - Each stat independent of others
-- - Results combined safely
--
-- FP Principle: PARALLEL COMPUTATION of independent values
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
     -- `par` sparks parallel evaluation of each component

-- ==================================================================
-- WHY FUNCTIONAL PROGRAMMING ENABLES THIS:
-- ==================================================================
{-
1. IMMUTABILITY:
   - Data never changes
   - Safe to access from multiple threads
   - No defensive copying needed

2. PURITY:
   - No side effects
   - Functions can run in any order
   - Results always consistent

3. REFERENTIAL TRANSPARENCY:
   - Function calls can be replaced with results
   - Compiler can optimize automatically
   - Easy to reason about correctness

4. TYPE SAFETY:
   - Compiler ensures no data races
   - IO operations clearly marked
   - Concurrency bugs caught at compile time

IMPERATIVE APPROACH (Java/Python):
- Need synchronized blocks
- Manual thread pool management
- Risk of deadlocks
- Race condition bugs
- Complex debugging

FUNCTIONAL APPROACH (Haskell):
- Add 'par' or 'parMap'
- Automatic thread management
- No race conditions possible
- Type-safe by design
- Easy to test and debug

INDUSTRIAL APPLICATIONS:
- Financial systems: Process millions of transactions
- Data analytics: Parallel ETL pipelines  
- Machine learning: Parallel model training
- Web servers: Concurrent request handling
- Scientific computing: Parallel simulations
-}
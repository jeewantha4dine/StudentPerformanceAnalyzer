# Student Performance Analytics System

# Group Members
- W.S.N Soysa - EG/2020/4321
- S.Y Abeysiriwardhana - EG/2020/4318
- K.S.P Kulasooriya - EG/2020/4329
- Senapathy J.D - EG/2020/4200

Project Title
# Functional Student Performance Analyzer using Haskell

# Real-World Scenario
Educational institutions process thousands of student records each semester. They need:
- Reliable grade calculation systems
- Performance analytics
- Pass/fail statistics
- Automated reporting

Traditional imperative systems using mutable state can lead to:
- Data corruption during concurrent processing
- Inconsistent calculations
- Difficult-to-debug code
- Race conditions in multi-threaded environments

# How to Run
# Run the analyzer
./analyzer

# Or use GHCi for interactive mode
ghci src/Main.hs
> main
```

## Sample Input

**File: `data/students.csv`**
```
1,John Doe,85,90,78,88
2,Jane Smith,92,88,95,90
3,Bob Johnson,65,70,68,72
```

## Sample Output
```
======================================
  STUDENT PERFORMANCE ANALYZER
======================================

Loading student data...
Successfully loaded 8 students

================================
  STUDENT ANALYZER MENU
================================
1. Show All Students
2. Show Passing Students
3. Show Failing Students
4. Show Top Performers
5. Show Statistics
6. Export Report
7. Exit
================================
Enter choice: 5

================================
    CLASS SUMMARY STATISTICS    
================================
Total Students:   8
Passed:           7
Failed:           1
Pass Rate:        87.5%
Class Average:    75.63
Highest Score:    95.75
Lowest Score:     48.75
================================
```

## Functional Programming Concepts Used

1. Pure Functions
Functions with no side effects - same input always gives same output.
```haskell
calculateAverage :: [Double] -> Double
calculateAverage marks = sum marks / fromIntegral (length marks)
```

It Makes testing easy and eliminates bugs from shared state.

2. Immutability
Data structures never change after creation.
```haskell
data Student = Student
  { studentId :: Int
  , studentName :: String
  , marks :: [Double]
  } deriving (Show, Eq)
```

It has thread-safe by default, no defensive copying needed.

3. Recursion
Instead of loops, functions call themselves.
```haskell
processAllStudents :: [Student] -> [StudentReport]
processAllStudents [] = []
processAllStudents (s:ss) = processStudent s : processAllStudents ss
```

This makes it more mathematical, easier to prove correct.

4. Higher-Order Functions
Functions that take or return other functions.
```haskell
-- 'filter' takes a function as argument
getPassingStudents :: [StudentReport] -> [StudentReport]
getPassingStudents = filter (\r -> grade r /= F)
```

This enables code reuse and abstraction.

5.Algebraic Data Types (ADTs)
Custom types that precisely model the domain.
```haskell
data Grade = APlus | A | BPlus | B | C | F
  deriving (Show, Eq, Ord)
```

Compiler enforces correctness at type level.

6.Pattern Matching
Elegant way to handle different cases.
```haskell
assignGrade :: Double -> Grade
assignGrade avg
  | avg >= 90 = APlus
  | avg >= 80 = A
  | avg >= 70 = BPlus
  | otherwise = F
```

When this is there, exhaustive checking prevents missing cases.

7.List Comprehensions
Concise data transformation syntax.
```haskell
-- Filter and transform in one line
higher = [r | r <- reports, average r >= 75]
```

Readable data pipelines.



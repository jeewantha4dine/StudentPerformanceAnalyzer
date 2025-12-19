# Student Performance Analytics System

## Group Members
- W.S.N Soysa - EG/2020/4321
- S.Y Abeysiriwardhana - EG/2020/4318
- K.S.P Kulasooriya - EG/2020/4329
- Senapathy J.D - EG/2020/4200

## Project Title
**Functional Student Performance Analyzer with Parallel Processing using Haskell**

---

## Problem Description

### Real-World Scenario
Educational institutions process thousands of student records each semester. They need:
- Reliable grade calculation systems
- Performance analytics
- Pass/fail statistics
- Automated reporting

### Challenges with Traditional Systems
Traditional imperative systems using mutable state can lead to:
- Data corruption during concurrent processing
- Inconsistent calculations
- Difficult-to-debug code
- Race conditions in multi-threaded environments

### Our Functional Programming Solution
A pure functional approach using Haskell that guarantees:
- **Immutability**: Data never changes, eliminating corruption
- **Predictability**: Same input always produces same output
- **Testability**: Each function can be tested in isolation
- **Safe Parallelism**: Multi-core processing without locks or race conditions

---

## How to Run

### Compilation
```bash
# Using build script (Windows)
./build.ps1

# Manual compilation
ghc -package deepseq -package parallel -threaded -O2 -rtsopts \
    -o analyzer \
    src/Main.hs src/DataTypes.hs src/Processing.hs src/IOHandler.hs src/Utils.hs
```

### Execution
```bash
# Normal execution
./analyzer

# With parallel processing (4 CPU cores)
./analyzer +RTS -N4 -s

# With parallel processing (all available cores)
./analyzer +RTS -N -s
```

The `-s` flag displays runtime statistics including parallel productivity metrics.

---

## Sample Input

**File: `data/students.csv`**
```
1,John Doe,85,90,78,88
2,Jane Smith,92,88,95,90
3,Bob Johnson,65,70,68,72
4,Alice Williams,45,50,48,52
5,Charlie Brown,88,85,90,87
```

Format: `StudentID,Name,Mark1,Mark2,Mark3,Mark4`

---

## Sample Output

### Program Start
```
======================================
  STUDENT PERFORMANCE ANALYZER
  (with Parallel Processing Support)
======================================

Loading student data...
Successfully loaded 50 students
Using PARALLEL processing (dataset > 10 students)...
Processing distributed across CPU cores for faster results.
[OK] Processing complete!

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
```

### Statistics Output (Option 5)
```
================================
    CLASS SUMMARY STATISTICS    
================================
Total Students:   50
Passed:           45
Failed:           5
Pass Rate:        90.0%
Class Average:    75.63
Highest Score:    95.75
Lowest Score:     48.25
================================
```

---

## Functional Programming Concepts Used

### 1. Pure Functions
Functions with no side effects - same input always gives same output.
```haskell
calculateAverage :: [Double] -> Double
calculateAverage marks = sum marks / fromIntegral (length marks)
```

**Benefits**: Makes testing easy and eliminates bugs from shared state.

### 2. Immutability
Data structures never change after creation.
```haskell
data Student = Student
  { studentId :: Int
  , studentName :: String
  , marks :: [Double]
  } deriving (Show, Eq)
```

**Benefits**: Thread-safe by default, no defensive copying needed.

### 3. Recursion
Functions call themselves instead of using loops.
```haskell
processAllStudents :: [Student] -> [StudentReport]
processAllStudents [] = []
processAllStudents (s:ss) = processStudent s : processAllStudents ss
```

**Benefits**: More mathematical, easier to prove correct.

### 4. Higher-Order Functions
Functions that take or return other functions.
```haskell
-- 'filter' takes a function as argument
getPassingStudents :: [StudentReport] -> [StudentReport]
getPassingStudents = filter (\r -> grade r /= F)
```

**Benefits**: Enables code reuse and abstraction.

### 5. Algebraic Data Types (ADTs)
Custom types that precisely model the domain.
```haskell
data Grade = APlus | A | BPlus | B | C | F
  deriving (Show, Eq, Ord)
```

**Benefits**: Compiler enforces correctness at type level.

### 6. Pattern Matching
Elegant way to handle different cases.
```haskell
assignGrade :: Double -> Grade
assignGrade avg
  | avg >= 90 = APlus
  | avg >= 80 = A
  | avg >= 70 = BPlus
  | otherwise = F
```

**Benefits**: Exhaustive checking prevents missing cases.

### 7. Parallel Processing
Safe concurrent execution through purity and immutability.
```haskell
processAllStudentsParallel :: [Student] -> [StudentReport]
processAllStudentsParallel students = 
  parMap rdeepseq processStudent students
```

**Why This Is Safe:**
- `processStudent` is **PURE** - no side effects
- `Student` data is **IMMUTABLE** - cannot be modified
- No race conditions possible - no shared mutable state
- Type system guarantees thread safety

**Benefits**: Multi-core processing without locks, mutexes, or complex synchronization code.

### 8. List Comprehensions
Concise data transformation syntax.
```haskell
-- Filter and transform in one line
higher = [r | r <- reports, average r >= 75]
```

**Benefits**: Readable data pipelines.







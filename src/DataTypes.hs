module DataTypes where

data Student = Student
  { studentId :: Int
  , studentName :: String
  , marks :: [Double]  
  } deriving (Show, Eq)

data Grade 
  = APlus   
  | A      
  | BPlus   
  | B       
  | C       
  | F       
  deriving (Show, Eq, Ord)

data Performance = Excellent | Good | Average | Poor
  deriving (Show, Eq)

data StudentReport = StudentReport
  { reportStudent :: Student
  , average :: Double
  , grade :: Grade
  , performance :: Performance
  } deriving (Show)

data ClassSummary = ClassSummary
  { totalStudents :: Int
  , passCount :: Int
  , failCount :: Int
  , classAverage :: Double
  , highestScore :: Double
  , lowestScore :: Double
  } deriving (Show)
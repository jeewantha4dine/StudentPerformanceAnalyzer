{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module DataTypes where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Student = Student
  { studentId :: Int
  , studentName :: String
  , marks :: [Double]  
  } deriving (Show, Eq, Generic, NFData)

data Grade 
  = APlus   
  | A      
  | BPlus   
  | B       
  | C       
  | F       
  deriving (Show, Eq, Ord, Generic, NFData)

data Performance = Excellent | Good | Average | Poor
  deriving (Show, Eq, Generic, NFData)

data StudentReport = StudentReport
  { reportStudent :: Student
  , average :: Double
  , grade :: Grade
  , performance :: Performance
  } deriving (Show, Generic, NFData)

data ClassSummary = ClassSummary
  { totalStudents :: Int
  , passCount :: Int
  , failCount :: Int
  , classAverage :: Double
  , highestScore :: Double
  , lowestScore :: Double
  } deriving (Show, Generic, NFData)
module Codegen where

import LLVM.AST

import qualified Data.Map as Map

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]

data BlockState
  = BlockState {
    idx :: Int
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator)
  } deriving Show

data CodegenState
  = CodegenState {
    currentBlock :: Name
  , blocks :: Map.Map Name BlockState
  , symtab :: SymbolTable
  , blockCount :: Int
  , count :: Word
  , names :: Names -- Name supply
  } deriving Show

type Names = Map.Map String Int

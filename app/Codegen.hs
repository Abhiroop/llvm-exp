{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen where

import Control.Monad.State

import LLVM.AST

import qualified Data.Map as Map

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]

data BlockState
  = BlockState {
    idx :: Int
  , stack :: [Named Instruction]  -- the actual stack of instructions
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

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Monad.State
import LLVM.AST

import qualified LLVM.AST as AST
import qualified Data.ByteString.Short as BS
import qualified Data.Map as Map
import qualified Data.String.UTF8 as U

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

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM s) = execState s mod

emptyModule :: [Char] -> AST.Module
emptyModule label
  = defaultModule { moduleName = (BS.pack . U.encode) label }

addDefinition :: Definition -> LLVM ()
addDefinition d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

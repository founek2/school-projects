{-# LANGUAGE ImportQualifiedPost #-}

module StackMachine where

import Control.Program
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Stack qualified as Stack

-- | Input is sequence of values (type Value is defined in Control.Program)
type Input = Seq.Seq Value

-- | Output is sequence of values (type Value is defined in Control.Program)
type Output = Seq.Seq Value

-- | Memory of computer stores on address of type Address a value of type Value (see Control.Program)
type Memory = Map.Map Address Value

-- | Lookup directory of subprograms (labels are marking starts of parts of a program where you can jump with JU or JZ)
type SubprogramDir = Map.Map Label Program

-- | Computer stack can store addresses and values
type ComputerStack = Stack.Stack (Either Address Value)

-- | Errors that may appear while running program on Stack Machine
data Error
  = EmptyStack
  | NotValue
  | NotAddress
  | NoInput
  | UnknownLabel
  | DivisionByZero
  | UninitializedMemory
  deriving (Show, Read, Eq)

-- | Result of program exection on Stack Machine with some input
type Result = Either Error Output

-- | Run program with given input (memory and stack should be empty at start)
-- | If there is a problem, result should indicate error (using Error type in Left)
-- TODO: implement running the program
runProgram :: Program -> Input -> Result
runProgram p input = runProgram' p input Stack.Empty dict Map.empty Seq.Empty where dict = loadSubprograms p Map.empty

loadSubprograms :: Program -> SubprogramDir -> SubprogramDir
loadSubprograms (label `Marks` instructions) dir = loadSubprograms instructions (Map.insert label instructions dir)
loadSubprograms (a `Then` b) dir = loadSubprograms b dir
loadSubprograms EOP dir = dir

-- 1. scan for labels and save their instructions into subprogramDir -> then you can call it during processing. Also it that case Marks can be removed completly -> just run instructions afterwards
runProgram' :: Program -> Input -> ComputerStack -> SubprogramDir -> Memory -> Output -> Result
runProgram' (_ `Marks` instructions) i s d m o = runProgram' instructions i s d m o
-- Push address to stack (shared stack for values and addresses)
runProgram' ((TA address) `Then` instructions) i s dict m out = runProgram' instructions i (pushAddress address s) dict m out
-- Push value to stack (shared stack for values and addresses)
runProgram' ((TV value) `Then` instructions) i s dict m out = runProgram' instructions i (pushValue value s) dict m out
-- Replace address at the top of stack by value from memory at that address
runProgram' (DR `Then` instructions) i s dict m out = case topAddress s of
  Right x -> case Map.lookup x m of
    Just value -> runProgram' instructions i (replaceValue value s) dict m out
    Nothing -> Left UninitializedMemory
  Left e -> Left e
-- Store value on top of stack to memory to address under the top of stack and pop those
runProgram' (ST `Then` instructions) i s dict m out = case extractValue s of
  Left e -> Left e
  Right (value, s) -> case extractAddress s of
    Right (addr, s) -> runProgram' instructions i s dict (Map.insert addr value m) out
    Left e -> Left e
-- Write value from top of stack to output
runProgram' (WR `Then` instructions) i s dict m out = case extractValue s of
  Right (value, s) -> runProgram' instructions i s dict m (value Seq.:<| out)
  Left e -> Left e
-- Read value from input to top of stack
runProgram' (RD `Then` instructions) i s dict m out = case i of
  x Seq.:<| i -> runProgram' instructions i (pushValue x s) dict m out
  _ -> Left NoInput
-- Add two values from stack and replace them by result
runProgram' (AD `Then` instructions) i s dict m out = case apply safePlus s of
  Left e -> Left e
  Right (v, s) -> runProgram' instructions i (pushValue v s) dict m out
-- Subtract (as above first - second)
runProgram' (SB `Then` instructions) i s dict m out = case apply safeMinus s of
  Left e -> Left e
  Right (v, s) -> runProgram' instructions i (pushValue v s) dict m out
-- Multiply (as above)
runProgram' (MT `Then` instructions) i s dict m out = case apply safeMult s of
  Left e -> Left e
  Right (v, s) -> runProgram' instructions i (pushValue v s) dict m out
-- Divide (as above first / second)
runProgram' (DI `Then` instructions) i s dict m out = case apply safeDiv s of
  Left e -> Left e
  Right (v, s) -> runProgram' instructions i (pushValue v s) dict m out
-- Jump to label
runProgram' (JU label `Then` instructions) i s dict m out = case Map.lookup label dict of
  Just instructions -> runProgram' instructions i s dict m out
  Nothing -> Left UnknownLabel
-- Jump to label if zero is on top of stack and remove it (otherwise just remove)
runProgram' (JZ label `Then` instructions) i s dict m out = case extractValue s of
  Right (0, s) -> case Map.lookup label dict of
    Just instructions -> runProgram' instructions i s dict m out
    Nothing -> Left UnknownLabel
  Right (_, s) -> runProgram' instructions i s dict m out
  Left e -> Left e
runProgram' EOP _ _ _ _ out = Right $ Seq.reverse out

-- Feel free to create more helper functions
pushAddress :: a -> Stack.Stack (Either a b) -> Stack.Stack (Either a b)
pushAddress addr = Stack.push (Left addr)

pushValue :: b -> Stack.Stack (Either a b) -> Stack.Stack (Either a b)
pushValue val = Stack.push (Right val)

replaceValue :: b -> Stack.Stack (Either a b) -> Stack.Stack (Either a b)
replaceValue val s = Stack.push (Right val) $ Stack.pop s

apply a s = case extractValue s of
  Left e -> Left e
  Right (v1, s) -> case extractValue s of
    Left e -> Left e
    Right (v2, s) -> case a v1 v2 of
      Right v -> Right (v, s)
      Left e -> Left e

topValue :: Stack.Stack (Either a b) -> Either Error b
topValue s = case Stack.topSafe s of
  Just (Left x) -> Left NotValue
  Just (Right v1) -> Right v1
  Nothing -> Left EmptyStack

extractValue :: Stack.Stack (Either a1 a2) -> Either Error (a2, Stack.Stack (Either a1 a2))
extractValue s = case topValue s of
  Right v -> Right (v, Stack.pop s)
  Left e -> Left e

topAddress :: Stack.Stack (Either b1 b2) -> Either Error b1
topAddress s = case Stack.topSafe s of
  Just (Left a) -> Right a
  Just (Right _) -> Left NotAddress
  Nothing -> Left EmptyStack

extractAddress :: Stack.Stack (Either a b2) -> Either Error (a, Stack.Stack (Either a b2))
extractAddress s = case topAddress s of
  Right v -> Right (v, Stack.pop s)
  Left e -> Left e

safeDiv :: Integral b => b -> b -> Either Error b
safeDiv a 0 = Left DivisionByZero
safeDiv a b = Right (div a b)

safePlus :: Num b => b -> b -> Either a b
safePlus a b = Right (a + b)

safeMinus :: Num b => b -> b -> Either a b
safeMinus a b = Right (a - b)

safeMult :: Num b => b -> b -> Either a b
safeMult a b = Right (a * b)

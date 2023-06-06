{-# LANGUAGE OverloadedStrings #-}

module StackMachineSpec (spec) where

import Test.Hspec

import StackMachine

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Stack as Stack

import Control.Program
import qualified Fixtures.ProgramsAFP as ProgsAFP
import qualified Fixtures.ProgramsMSU as ProgsMSU

spec :: Spec
spec = do
  describe "runProgram (input/output and constants)" $ do
    it "runs trivial 'empty' program" $
      runProgram EOP Seq.empty `shouldBe` Right (Seq.empty)
    it "runs WR to put on output from top of stack" $ do
      runProgram (TV 7 $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 7)
      runProgram (TV 7 $. TV 10 $. WR $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.fromList [10, 7])
      runProgram (RD $. TV 10 $. WR $. WR $. EOP) (Seq.singleton 7) `shouldBe` Right (Seq.fromList [10, 7])
      runProgram (RD $. RD $. WR $. WR $. EOP) (Seq.fromList [1, 2, 3]) `shouldBe` Right (Seq.fromList [2, 1])
    it "fails with WR with no value on top of stack" $ do
      runProgram (WR $. EOP) Seq.empty `shouldBe` Left EmptyStack
      runProgram (TA 7 $. WR $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TV 10 $. TA 7 $. WR $. EOP) Seq.empty `shouldBe`Left NotValue
    it "fails if RD with empty input" $
      runProgram (RD $. EOP) Seq.empty `shouldBe` Left NoInput

  describe "runProgram (math)" $ do
    it "runs AD to add two numbers at the top of stack" $ do
      runProgram (TV 3 $. TV 5 $. AD $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 8)
      runProgram (TV 3 $. TV (-5) $. AD $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton (-2))
      runProgram (TV 10 $. TA 7 $. AD $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TA 10 $. TV 7 $. AD $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (AD $. EOP) Seq.empty `shouldBe` Left EmptyStack
      runProgram (TV 5 $. AD $. EOP) Seq.empty `shouldBe` Left EmptyStack
    it "runs SB to subtract two numbers at the top of stack" $ do
      runProgram (TV 3 $. TV 5 $. SB $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 2)
      runProgram (TV 6 $. TV 2 $. SB $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton (-4))
      runProgram (TV 10 $. TA 7 $. SB $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TA 10 $. TV 7 $. SB $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (SB $. EOP) Seq.empty `shouldBe` Left EmptyStack
      runProgram (TV 5 $. SB $. EOP) Seq.empty `shouldBe` Left EmptyStack
    it "runs MT to multiply two numbers at the top of stack" $ do
      runProgram (TV 3 $. TV 5 $. MT $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 15)
      runProgram (TV (-3) $. TV 5 $. MT $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton (-15))
      runProgram (TV 10 $. TA 7 $. MT $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TA 10 $. TV 7 $. MT $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (MT $. EOP) Seq.empty `shouldBe` Left EmptyStack
      runProgram (TV 5 $. MT $. EOP) Seq.empty `shouldBe` Left EmptyStack
    it "runs DI to divides two numbers at the top of stack" $ do
      runProgram (TV 5 $. TV 15 $. DI $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 3)
      runProgram (TV 3 $. TV 2 $. DI $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 0)
      runProgram (TV 2 $. TV (-8) $. DI $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton (-4))
      runProgram (TV 2 $. TV 0 $. DI $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 0)
      runProgram (TV 0 $. TV 3 $. DI $. WR $. EOP) Seq.empty `shouldBe` Left DivisionByZero
      runProgram (TV 10 $. TA 7 $. DI $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TA 10 $. TV 7 $. DI $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (DI $. EOP) Seq.empty `shouldBe` Left EmptyStack
      runProgram (TV 5 $. DI $. EOP) Seq.empty `shouldBe` Left EmptyStack

  describe "runProgram (memory)" $ do
    it "stores values to the memory" $ do
      runProgram (TA 127 $. TV 5 $. ST $. TA 127 $. DR $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 5)
      runProgram (TA 0 $. TV 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 1)
      runProgram (TA 0 $. TV 1 $. ST $. TV 10 $. TA 0 $. DR $. WR $. WR $. EOP) Seq.empty `shouldBe` Right (Seq.fromList [1, 10])
    it "fails with incorrect address/value" $ do
      runProgram (TV 0 $. TV 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left NotAddress
      runProgram (TV 0 $. TA 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TA 0 $. TA 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TA 0 $. TV 1 $. ST $. TV 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left NotAddress
    it "fails with uninitialized memory cell" $ do
      runProgram (TA 0 $. TV 1 $. ST $. TA 1 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left UninitializedMemory
      runProgram (TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left UninitializedMemory
    it "fails with not enough entries on stack" $ do
      runProgram (ST $. TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left EmptyStack
      runProgram (TV 1 $. ST $. TA 0 $. DR $. WR $. EOP) Seq.empty `shouldBe` Left EmptyStack
      runProgram (TA 0 $. TV 1 $. ST $. DR $. WR $. EOP) Seq.empty `shouldBe` Left EmptyStack


  describe "runProgram (jumps)" $ do
    it "jumps unconditionally (JU) to label" $ do
      runProgram (TV 4 $. JU "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 4)
      runProgram (RD $. JU "end" $. "faulty" $: RD $. WR $. AD $. "end" $: WR $. EOP) (Seq.singleton 10) `shouldBe` Right (Seq.singleton 10)
    it "jumps conditionally (JZ) to label if zero at the top of stack" $ do
      runProgram (TV 4 $. TV 0 $. JZ "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 4)
      runProgram (TV 4 $. TV 1 $. JZ "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 5)
      runProgram (TV 4 $. TV (-1) $. JZ "end" $. TV 5 $. "end" $: WR $. EOP) Seq.empty `shouldBe` Right (Seq.singleton 5)
    it "is able to perform both forward and backward jump to the same label" $ do
        runProgram (TV (-1) $. TV 10 $. TV 0 $. TV 4 $. JU "stop" $. RD $. "stop" $: WR $. JZ "stop" $. EOP) Seq.empty `shouldBe` Right (Seq.fromList [4, 10])
    it "fails if not value at the top of stack (JZ)" $ do
      runProgram (TA 0 $. JZ "end" $. WR $. "end" $: EOP) Seq.empty `shouldBe` Left NotValue
      runProgram (TV 0 $. TA 10 $. JZ "end" $. WR $. "end" $: EOP) Seq.empty `shouldBe` Left NotValue
    it "fails if label not found" $ do
      runProgram (TV 0 $. JZ "nope" $. WR $. "end" $: EOP) Seq.empty `shouldBe` Left UnknownLabel
      runProgram (JU "abc" $. WR $. "end" $: EOP) Seq.empty `shouldBe` Left UnknownLabel

  describe "runProgram (complex programs)" $ do
    it "can sum list of numbers in cycle (first is input length)" $ do
      runProgram ProgsMSU.sumList (Seq.fromList (10:[1..10])) `shouldBe` Right (Seq.singleton 55)
      runProgram ProgsMSU.sumList (Seq.fromList (11:[-5..5])) `shouldBe` Right (Seq.singleton 0)
      runProgram ProgsMSU.sumList (Seq.fromList (5:[1..100])) `shouldBe` Right (Seq.singleton 15)
      runProgram ProgsMSU.sumList (Seq.fromList (2:[-6..0])) `shouldBe` Right (Seq.singleton (-11))
      runProgram ProgsMSU.sumList (Seq.singleton 0) `shouldBe` Right (Seq.singleton 0)
      runProgram ProgsMSU.sumList (Seq.singleton 10) `shouldBe` Left NoInput
    it "can compute GCD based on euclidean algorithm" $ do
      runProgram ProgsMSU.gcd (Seq.fromList [18, 12]) `shouldBe` Right (Seq.singleton 6)
      runProgram ProgsMSU.gcd (Seq.fromList [15, 11]) `shouldBe` Right (Seq.singleton 1)
      runProgram ProgsMSU.gcd (Seq.fromList [0, 1]) `shouldBe` Right (Seq.singleton 1)
      runProgram ProgsMSU.gcd (Seq.fromList [10, 10]) `shouldBe` Right (Seq.singleton 10)
      runProgram ProgsMSU.gcd (Seq.fromList [2154, 126]) `shouldBe` Right (Seq.singleton 6)
      runProgram ProgsMSU.gcd (Seq.fromList [12547505, 1654235]) `shouldBe` Right (Seq.singleton 95)
    it "can compute fibonacci number" $ do
      runProgram ProgsMSU.fib (Seq.singleton 0) `shouldBe` Right (Seq.singleton 0)
      runProgram ProgsMSU.fib (Seq.singleton 1) `shouldBe` Right (Seq.singleton 1)
      runProgram ProgsMSU.fib (Seq.singleton 10) `shouldBe` Right (Seq.singleton 55)
      runProgram ProgsMSU.fib (Seq.singleton 17) `shouldBe` Right (Seq.singleton 1597)
    it "can compute fibonacci sequence" $ do
      runProgram ProgsMSU.fibSeq (Seq.singleton 1) `shouldBe` Right (Seq.fromList [0, 1])
      runProgram ProgsMSU.fibSeq (Seq.singleton 7) `shouldBe` Right (Seq.fromList [0, 1, 1, 2, 3, 5, 8, 13])
      runProgram ProgsMSU.fibSeq (Seq.singleton 10) `shouldBe` Right (Seq.fromList [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55])
    it "can format binary numbers to decimal" $ do
      runProgram ProgsAFP.binNum (Seq.fromList [0]) `shouldBe` Right (Seq.singleton 0)
      runProgram ProgsAFP.binNum (Seq.fromList [1, 1]) `shouldBe` Right (Seq.singleton 1)
      runProgram ProgsAFP.binNum (Seq.fromList [2, 1, 1]) `shouldBe` Right (Seq.singleton 3)
      runProgram ProgsAFP.binNum (Seq.fromList [5, 0, 0, 0, 0, 1]) `shouldBe` Right (Seq.singleton 1)
      runProgram ProgsAFP.binNum (Seq.fromList [8, 1, 0, 1, 1, 0, 1, 0, 1]) `shouldBe` Right (Seq.singleton 181)
      runProgram ProgsAFP.binNum (Seq.fromList []) `shouldBe` Left NoInput

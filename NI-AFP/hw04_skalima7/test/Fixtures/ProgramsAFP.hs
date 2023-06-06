{-# LANGUAGE OverloadedStrings #-}

module Fixtures.ProgramsAFP where

import Control.Program

----------------------------------------------------------------------------------
-- All programs below written by students of NI-AFP course at FIT CTU in Prague --
----------------------------------------------------------------------------------


{-
    Binary number to decimal
    input( N A1 A2 ... AN )
    N -> number of digits in number a
    Ai -> 0 or 1
    output -> decimal representation of the number

    By: Filip Říha (rihafili) in B212 semester
-}

binNum :: Program
binNum =
        TA 0 $.
        RD   $.
        ST   $.          -- MEM[0] := N

        TA 1 $.
        TV 0 $.
        ST   $.          -- MEM[1] := output accumulator

        TA 0 $.
        DR   $.
        JZ "finish" $.   -- no digits -> end

        TA 3 $.
        TV 1 $.
        TA 0 $.
        DR   $.
        SB   $.
        ST   $.         -- MEM[3] := N - 1 , exponential counter

        TA 4 $.         -- address where counted exponential is going to be stored
        TV 1 $.         -- exponential initialization, it is counted on the stack

    "exp"$:             -- counting 2^(N-1)
        TA 3 $.
        DR   $.
        JZ "expFinish" $.

        TV 2 $.
        MT   $.         -- Multiply on stack by 2

        TA 3 $.
        TV 1 $.
        TA 3 $.
        DR   $.
        SB   $.
        ST   $.         -- decrement TA 3
        JU "exp" $.

    "expFinish"$:
        ST  $.          -- MEM[4] := 2^(N-1)

    "load"$:
        TA 0 $.
        DR   $.
        JZ "finish" $.

        TA 1 $.
        RD   $.
        TA 4 $.
        DR   $.
        MT   $.
        TA 1 $.
        DR   $.
        AD   $.
        ST   $.         -- MEM[1] := MEM[1] + ( input * 2(M-1) ),
                        -- where M is which digit is it from right

        TA 4 $.
        TV 2 $.
        TA 4 $.
        DR   $.
        DI   $.
        ST   $.         -- MEM[4] := MEM[4] / 2

        TA 0 $.
        TV 1 $.
        TA 0 $.
        DR   $.
        SB   $.
        ST   $.         -- decrement MEM[0]

        JU "load" $.

    "finish"$:
        TA 1 $.
        DR   $.
        WR   $.
        EOP


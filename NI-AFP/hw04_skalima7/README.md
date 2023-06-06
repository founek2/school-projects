# MI-AFP homework #04

Homework to practice types for text, containers, and work with errors

## Task

This time your task is more complex, you have some TODOs in three files and there is one more containing important data types for your implementation.

1. First *warm-up* task is to few simple functions in `Data.Stack` (`src/Data/Stack.hs`). Do not change the data type definition and just implement functions typical for stack data structure (`pop`, `push`, `top`, `size`, `null`) and also `popSafe` + `topSafe` which use `Maybe` data type instead of throwing errors for empty stack.
2. After implementing `Data.Stack` you can proceed to implement `StackMachine`. Take a look at `Control.Program` where you find definitions of `Instruction`, `Program`, and other data types which you have to use (but do not change them). Then in `StackMachine` are prepared type synonyms which should be used in your implementation of `runProgram` as well. Stack machine takes a program with input and returns output (or throws an appropriate error).   
  You will need some helper function that includes stack, memory, and directory of labels (used for jumping). Implement logic of each instruction, their semantics are described in `Control.Program` and by tests in `test/StackMachineSpec.hs`. When all of them working correctly, you should be able to run more complex programs from `test/Fixtures/Programs.hs`.

Basic hints and requirements:

* For stack and stack machine, use pattern matching to the maximum! It is possible to implement both **without** using `if-then-else` and `case-of` expressions. 
* You can think about a program as a special list of instructions which you can process recursively and pass some context (I/O and internal state) along the way.
* In this homework you will work with various containers, look up the documentation. *Do not re-invent the wheel!*
* You must understand your code completely!
* If you want to earn a **bonus point**, design your favorite algorithm in instructions as in `test/Fixtures/Programs.hs` (at least as complex as `sumList`) and add tests for it.

## If you get stuck (spoiler)

<p>
<details>
<summary>Click to see hints</summary>

* You need to use recursion to process instructions, passing "stack machine state" and then return the output back. You will need some helper function for that as `runProgram` is high-level.
* It is essential to use pattern matching to check correctness of the "stack machine state" (assumptions to execute a specific instructions, e.g. if there is a value on top of stack). Avoid conditions and `case` on the right side of `=`. Recall that you can use "deeper" pattern matching (e.g., `foo (Maybe (x:xs))`) and that you should use wildcard `_` if you don't need the value.
* Think about each instruction separately... when everything ready, you can think about being DRY (helper functions, or using order for pattern matching). Of course, start with simpler `TV`+`WR`+`RD`, then proceed with math instructions, then memory, and finish with jumps.
* Once you encounter error, do not continue with recursion.
* For jump instruction, first construct the map with labels as keys and programs as values. This map you will then pass all the time without changing. There are no global variables! There is no mutability!
* If still stuck, ask in issue or MR.

</details>
</p>

## Notes

 * In case of uncertainty, check the [dummy homework](https://github.com/MI-AFP/hw00) to recall what is the homework workflow for this course.
 * If you encounter some trouble, create an issue in your repository.
 * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.

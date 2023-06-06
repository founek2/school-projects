# MI-AFP Homework Elm #01

*Homework to practice Elm language and the basics of The Elm Architecture.*

## Part I

The goal of this part is to practice the Elm language. There are several functions in the file `src/Lib.elm` waiting for your implementation. Each function has a documentation comment with a description of what it is supposed to do. There are also unit tests for these functions in `tests/LibTest.elm` which can help if something is unclear. You can run the tests using [elm-test](https://github.com/elm-explorations/test).

 Most of the functions can be implemented using functions from `elm/core` modules, such as [List](https://package.elm-lang.org/packages/elm/core/latest/List), [Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe), [String](https://package.elm-lang.org/packages/elm/core/latest/String) or [Dict](https://package.elm-lang.org/packages/elm/core/latest/Dict). For the last one, `indexOf` function, you should find appropriate package at [package.elm-lang.org](https://package.elm-lang.org), install it and use it.


## Part II

This part focuses on practising The Elm Architecture. The goal is to create a simple application that shows a story separated into three tabs (Introduction, Plot, Conclusion) with navigation between these tabs. When the app is opened, it should display the Introduction tab.

In the file `src/Main.elm`, there is already a structure for the application prepared. Your goal is to

- Define necessary messages you need for your implementation,
- Define the type for the `Model` and implement `init` function to return initial `Model`,
- Implement the `update` function to update the model correctly according to the message,
- Update `view` functions to
    - Dispatch messages when clicking on tabs,
    - Display correct content based on active tab,
    - Use `tabStyleActive` for active tab.

To open the application in the web browser, use `elm reactor` then navigate to [http://localhost:8000](http://localhost:8000) and open the `Main.elm` file.


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
# # MI-AFP homework #07

Homework to practice manipulation with JSON, testing, and project documentation

## Task

This time it is up to you to create the whole project. But no worries, it will be quite a simple app. You just need to be able to process a JSON file with debt records, do some summary over that and allow to add new records, edit or delete existing. However, you must design your project well and include documentation and tests for the basic logics (that should be pure functions).

### Debt Diary

* The structure of the diary in JSON is very simple, it is a single object with `title`, `description`, and a list of `records`, see `debt-diary.json`.
* A record has:
  
  * `person` - identification of a person (unique name, nickname, or code)
  * `amount` - decimal number indicating the amount of the debt; negative number means money (back) to the user
  * `note` - optional string with a custom note about the record
  * `timestamp` - UTC timestamp (ISO-8601) of the records (hint: use `UTCTime` from `Data.Time`)

### Functional Requirements

1. By invoking `debt-diary -f debt-diary.json list`, a pretty-printed list with indexes of all debt records will be put on stdout. Include title and description of the diary.
2. By invoking `debt-diary -f debt-diary.json item <N>`, a pretty-printed Nth record will be put on stdout.
3. By invoking `debt-diary -f debt-diary.json overview`, a list of people with sum of their debts is printed (as a nice table). Include title and description of the diary.
4. By invoking `debt-diary -f debt-diary.json balance`, a total of all debts (and returns) is printed to see if the user is a debtor or a creditor.
5. By invoking `debt-diary -f new-debt-diary.json create`, a new diary is created based on provided title and description interactively.
6. By invoking `debt-diary -f debt-diary.json add`, a new record is prompted and added to JSON.
7. By invoking `debt-diary -f debt-diary.json edit <N>`, the Nth record is prompted and edited to JSON.
8. By invoking `debt-diary -f debt-diary.json delete <N>`, the Nth record is deleted from JSON.
9. File errors (permissions or parsing) must be nicely printed out to stderr (e.g. *Cannot read file debt-diary.json* or *Cannot parse debts from debt-diary.json*), try to describe well the cause of the error. Then the app must fail with error code 1 (alt. 2 for no such file).
10. You need to also nicely inform user about bad inputs for `create`, `add`, and `edit`... or non-existing index of desired record (N).

### Documentation and Tests

* You have to separate "business logic" and IO manipulation to different modules; however, do not put all the IO stuff into `app/Main.hs`, have at least two modules in `src`.
* Use sensible naming for modules, types, and functions.
* The exposed logics (functions from library module) must be documented with Haddock comments (`stack haddock`).
* All the logics (manipulation debts and eventually some pretty-printing to string) must be tested using HSpec (`stack test`).
* You must use GitLab CI to build the application (and execute tests). You can reuse `.gitlab-ci.yml` from previous homeworks; however, your `.gitlab-ci.yml` and `stack.yaml` must use the same GHC version, so GitLab CI uses GHC from the image and does not download it again!

## Hints

* You may use [cmdargs](https://hackage.haskell.org/package/cmdargs) for parsing arguments. You may support different order in the CLI invocations (e.g. `debt-diary add -f debt-diary.json`) or slightly different syntax (e.g. `debt-diary add --file=debt-diary.json`) but it must work as described in tge requirements.
* Start with loading the JSON (which will be common for all invocations except `create`).
* For JSON, use [aeson](https://hackage.haskell.org/package/aeson), you will need to have two data types and define corresponding instances for `FromJSON` and `ToJSON`. Use `Maybe` and `UTCTime` where suitable. You need to take a look at the [documentation](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html), or some [tutorial](https://www.fpcomplete.com/haskell/library/aeson/) which is essential skill for programmers.
* If you like lenses, use [lens-aeson](https://hackage.haskell.org/package/lens-aeson) and make lenses for your data types.
* The efficiency is not critical for this kind of application, don't worry about *O(N)* for accessing or changing n-th record.
* You can think of this task as a preparation for semester project, where you might develop a Haskell app from scratch based on specification (however, more complex than this).
* The file handling errors can be caught and pretty-printed
* If you get stuck, ask using MR or issue.

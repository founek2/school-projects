# MI-AFP Homework Elm #02

_Homework to practice The Elm Architecture._

Your goal is to create a simple application for managing debts. You have server and views prepared; your task is to implement the application's business logic.

## Install dependencies

Install dependencies

```
$ yarn install
```

## Format

Format `js` code with [prettier](https://prettier.io/) and `ELM` code with [elm-format](https://github.com/avh4/elm-format):

```
$ yarn format
```

## Server

The server is implemented in the [Haskell](https://www.haskell.org/) and it's base url is `https://ccmi.fit.cvut.cz:8889/`.

See all available endpoints in [Debt diary swagger](https://ccmi.fit.cvut.cz:8889/swagger-ui/#/).

## Assignment

You have an application boilerplate prepared in `src/Main.elm`. There are some views with styles from `src/Styles.elm` so you don't have to bother with the look. Use `elm reactor` to open the application in the browser. It consists of 3 components: **Person List**, **Person Detail** and **Debt Record Form**. You should implement the following functionality for each component:

**Person List**

- should load a list of persons from the server when the application is started and show the name of each person,
  - make sure to handle error state as well
- when a user clicks on a person, it should open detail in the Person Detail component
- Persons should be sorted alphabetically

**Person Detail**

- should load and display a person name and all debts for this person - add view for debts and show total balance (debt).
  - Debt records should be sorted by from the newest to the oldest records
  - make sure to handle error state as well
  - should somehow display empty state when there is no debt record
- Debt records contains delete button, which will delete debt record. When debt record is deleted, data for the person should be updated and reloaded (show updated records and balance)

**Debt Record Form**

- user should be able to fill in Person (prepared dropdown, only fill in all existing persons), Debt Amount, TimeStamp and Debt Note
- when submitted, it should create a new debt record using the server's API
  - can be submitted only if all fields are filled
  - should be cleared after it is submitted
  - make sure to handle error response from the server
  - make sure the form cannot be submitted multiple times (e.g., by disabling the submit button while it is loading)

### Hint

When sending timestamp do BE, append manually ":00Z" (UTC zone, for this use case it's ok to use hardcoded data for time zone) to data provided by this input (see also comment in code)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for more details.

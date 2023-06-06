# Example of React & Elm integration

Todo application build with combination of React and Elm. [Online demo](https://elm.projects.iotdomu.cz)

## Architecture

Entry application is written in React. It is composed of following:

-   page container (App.jsx)
-   input for creating new todos and calling API to create todo item (TodoCreator.jsx)
-   interactive list of todos (TodoList.Elm)
-   selection of user (UserSelect.Elm)

React binds it all togather:

-   When UserSelect is initialized, it sends via port default userId, Reacts passes it to TodoList, which sends request to retrieve user's todos. The same thing goes again on select change.
-   When user inserts text into input and presses enter (component TodoCreator), React component sends API call to create todo and newly created todo object is passed into TodoList elm component, which adds it to todo list

### Elm components

TodoList.Elm

-   toogle completion of item
-   delete item
-   load items based on current user Id
-   recieve new todo via port from React

UserSelect.Elm

-   change current user id
-   send selected user id via ports to React
-   default user id is set via flags

### Used API

I have used this [dummy API](https://dummyjson.com/docs/todos), which is returning objects and api calls ends with success, but it actually does not change server status. That is the reason why after page refresh made changes will not be shown -> during operation in App, it is using in memory storage and after restart it downloads clean state from server. Also adding multiple new todos will create them, but all will share the same id, which is causing weird behavior and does not allow to toggle or delete them, since API calls will end with 404.

## Webpack setup

It took some time, but I was able to get it running with latest versions of all webpack loaders and packages, although it does not run on Mac with Arm chip. For some reason it installs elm binary build for x86 -> hence fails to start due to invalid architecture type. Any change made in dev mode, will automatically refresh the page to see the result - either in React or Elm.

```bash
# dev mode
yarn dev

# prod build -> output to build folder
yarn build
```

# Goal

I want to play with React & Elm interoperability. Find out how well it can integrate in order to get experience about possibility to slowly migrate existing React project into Elm.

## What I want to do?

TODO app, which will be build with combination of React and Elm.

## How it will look like

Container page will be build with React and it will contain one input, where user can write todo item. The rest will be build with Elm - integration with [API](https://dummyjson.com), holding data of all todos, rendering list of todos, ability to delete item.

## How will I achieve it?

I found interesting React package, which I want to try [react-elm-components](https://www.npmjs.com/package/react-elm-components). Communication will be done via ports - for start it will just send text from input, but later it will create todo via API and than pass TODO as object into Elm to render it.

## Roadmap

-   [x] Setup webpack for react
-   [x] Build cotainer page with React
-   [x] Setup elm build process
-   [x] Integrate elm component into React page
-   [x] Build UI of APP - React/Elm
-   [x] Integrate API - Elm

## Improvements

-   [ ] Add localization via [elm-i18next](https://github.com/ChristophP/elm-i18next)

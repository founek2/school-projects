# hw06

Homework to practice Foldable, Traversable, State Monad Transformer, and Lens

## Task

In this homework you will implement some functions to work with maps...

1. *Data.QuadTree* = make `QuadTree` data structure an instance of `Functor`, `Foldable`, and `Traversable` (think about delegation) and implement other prepared functions. You can use prepared lenses for data types `QuadTree` and `Quadrant` to make your code more readable. *Spoiler*: the most challenging part is `fromMatrix`.
2. *Data.Geography[.Styles]* = this module defined `Coords`, `Map`, and other related stuff. Few functions are not implemented and are waiting for you to be done. Try to make it as simple as possible. Recall syntactic sugar, point free style, and pattern matching. Again, lenses are ready for you...
3. *MapTraveler* = here is the interesting part - the `StateT`, you can rework it or just investigate and use as is. You have to implement 3 normal functions which are used in state manipulation and for the CLI.

When everything is done, you should be able to travel with:

```
% stack exec traveler
The Map Traveler
╔════════════════════════════════════════╗
║^         @            ~~~~~~   ░░░░░░░░║
║          ▓     ^     ~~~~~~~     ░░░░░░║
║   ^      ▓           ~~~~~~~     ^ ░░░░║
║  ███████ ▓    ^^     ~~~~~~~       ░░░░║
║  █     █ ▓             ~~~~~     ^ ░░░░║
║  ███████ ▓     ^         ~~            ║
║    ^^    ▓▓▓▓▓▓▓▓▓▓▓▓▓   ~~            ║
║  ^                   ▓   ~~            ║
║       ^        ^     ▓   ~~            ║
║^^ ^           ^^     ▓   ~~~~          ║
║^^       ^   ░░░ ░░░░ ▓     ~~~         ║
║^^^^         ░░░░░░░░ ▓▓       ~~       ║
╚════════════════════════════════════════╝
What direction do you want to go?
w
╔════════════════════════════════════════╗
║^         @            ~~~~~~   ░░░░░░░░║
║          ▓     ^     ~~~~~~~     ░░░░░░║
║   ^      ▓           ~~~~~~~     ^ ░░░░║
║  ███████ ▓    ^^     ~~~~~~~       ░░░░║
║  █     █ ▓             ~~~~~     ^ ░░░░║
║  ███████ ▓     ^         ~~            ║
║    ^^    ▓▓▓▓▓▓▓▓▓▓▓▓▓   ~~            ║
║  ^                   ▓   ~~            ║
║       ^        ^     ▓   ~~            ║
║^^ ^           ^^     ▓   ~~~~          ║
║^^       ^   ░░░ ░░░░ ▓     ~~~         ║
║^^^^         ░░░░░░░░ ▓▓       ~~       ║
╚════════════════════════════════════════╝
What direction do you want to go?
s
╔════════════════════════════════════════╗
║^         ▓            ~~~~~~   ░░░░░░░░║
║          @     ^     ~~~~~~~     ░░░░░░║
║   ^      ▓           ~~~~~~~     ^ ░░░░║
║  ███████ ▓    ^^     ~~~~~~~       ░░░░║
║  █     █ ▓             ~~~~~     ^ ░░░░║
║  ███████ ▓     ^         ~~            ║
║    ^^    ▓▓▓▓▓▓▓▓▓▓▓▓▓   ~~            ║
║  ^                   ▓   ~~            ║
║       ^        ^     ▓   ~~            ║
║^^ ^           ^^     ▓   ~~~~          ║
║^^       ^   ░░░ ░░░░ ▓     ~~~         ║
║^^^^         ░░░░░░░░ ▓▓       ~~       ║
╚════════════════════════════════════════╝
What direction do you want to go?
q
Byeee!
```

You should be able to move with *w a s d* step by step thru *walkable* places in the map.

The CLI and some other trivial parts are not tested so you can improve it if you want. Also you can create other maps, options, and so on. You can also build some game on top of this and use some *ncurses* library.

## Notes

  * Follow **DRY** principle, use syntactic sugar when suitable, make your code clean and understandable.
  * If you encounter some trouble, create an issue in your repository.
  * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.

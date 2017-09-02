# Multi-core on Haskell

This project examines Haskell's multi-core function.

# How to compile

```
$ stack build
```

# How to use

```
$ stack exec mc -- normal +RTS -s -N4
$ stack exec mc -- fork +RTS -s -N4
$ stack exec mc -- eval +RTS -s -N4
```


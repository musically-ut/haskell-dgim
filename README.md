# haskell-dgim

This is a Haskell implementation of the [Datar-Gionis-Indyk-Motwani algorithm](http://www-cs-students.stanford.edu/~datar/papers/sicomp_streams.pdf) for approximately counting occurences of certain elements in a (very large) prefix of a (possibly infinite) stream.

## Installation

This package is now available on Hackage with the name [dgim](https://hackage.haskell.org/package/dgim): 

    cabal install dgim
    
or by name in your `.cabal` file. However, it is in the alpha stage and the API is likely to change in the future.

### Bleeding edge

Install `cabalg`, if you do not already have it with `cabal install cabalg`.

    cabalg https://github.com/musically-ut/haskell-dgim

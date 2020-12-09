# braille-conway

A very simple implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) using Unicode Braille.

## Motivation

While this might sound a bit silly at first, it is the best way of experimenting
with Conway's Game of Life for people who are used to working with Braille.

Worlds can be constructed very easily by just writing Braille patterns.
And Braille users can directly feel the evolution of the world they provided.

Since most systems have a font for Unicode Braille these days, it
can of course also be used by sighted users.

## Usage

With `brltty-trtxt` from the `brltty` package, you can translate any
arbitrary text to Braille.  `braille-conway` will read from `STDIN` and
print successive generations as an animation to `STDOUT`.
Note that the coordinate system wraps around.

Some examples.

### Glider

```console
$ printf "   ?l   \n\n" | brltty-trtxt -i de | braille-conway
```

And here are the first five generations from the input above.

```
   ⠢⠇   

```

```
⠀⠀⠀⠨⠖⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀
```

```
⠀⠀⠀⠠⠵⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀
```

```
⠀⠀⠀⠐⡴⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀
```

```
⠀⠀⠀⠠⣰⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀
```

### R-pentomino

What Conway calls the R-pentomino (also called F-pentomino for its resemblance
of the letter `F`) is for Braille users the (capital) letter `P` or the
two letter combination `cd`.
```console
$ printf "\n\n    P    \n\n" | brltty-trtxt | braille-conway
```
### Evolution of random text

Another rather silly example would be the evolution of the source code of this program.

```console
$ head -$((LINES-1)) src/Braille/Conway.hs | brltty-trtxt | braille-conway
```

## Building

This is a Haskell program.  Use `cabal` or `stack` to build/install.

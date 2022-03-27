# chord_finder
A small tool to find chords from lists of music notes, written in A B C or in "Figured bass" notations

# build

```
dune build
```
# dependencies

- library unix
- library ANSITerminal

# use

run `help_chords.exe` and then type one main note, followed by other notes or numbers (interval from the first note). hit enter.
You will get a list of chords, ordered by maximum matching.

# Limitations

- this is for guitare: so difference between C# and Db is taken into account currently.
- only major minor, 7 and 7M chords are currently considered (baroque music rarely goes beyond that anyway).

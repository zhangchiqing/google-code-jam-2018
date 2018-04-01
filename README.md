# codejam

### Install
```
brew cask install haskell-platform
```

### Development
Develop in REPL
```
stack ghci
```

Run Tests in REPL
```
stack ghci src/GCJ2017QA.hs

ghci> defaultMain tests
```

### Run

Edit Lib.hs

```
stack build; stack exec codejam-exe < ~/Downloads/A-small-practice.in > ~/Downloads/output
```


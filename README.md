# Google Code Jam 2018

### Result

| Problem | File | Small Dataset | Large Dataset |
| --- | --- | --- | --- |
| 2018 Qualification Round - A | GCJ2018QA.hs | Passed | Failed |
| 2018 Qualification Round - A | GCJ2018QA2.hs | Passed | Passed |
| 2018 Qualification Round - B | GCJ2018QB.hs | Passed | Failed |
| 2018 Qualification Round - B | GCJ2018QB2.hs | Passed | Passed |
| 2018 Qualification Round - D | GCJ2018QD.hs | Passed | Failed |
| 2018 Qualification Round - D | GCJ2018QDD.hs | Passed | Passed |
| 2018 Round 1A - A | GCJ2018R1A.hs | Passed | Passed |
| 2018 Round 1A - B | GCJ2018R1B.hs | Passed | Passed |

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


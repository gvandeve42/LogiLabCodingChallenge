# LogiLab coding Challenge

For this coding challenge, I choose the [Mastermind](http://codingdojo.org/kata/Mastermind/) exercise on codingdojo.

## Getting Started
These instructions will get you a copy of the project up and running on your local machine.
### Prerequisites
For running this code, you will need to install the [Haskell plateform](https://www.haskell.org/platform/) on your machine.

On Debian:
```sh
$ sudo apt-get install haskell-platform
```
If you want to run the tests, tou can install the HUnit library (optional)
```sh
$ cabal new-install --lib HUnit
```
### Installing
First, clone the repository on your local machine
```sh
$ git clone https://github.com/gvandeve42/LogiLabCodingChallenge.git
```
then, at the root of this folder, launch ghci
```sh
$ ghci
```
while in the Haskell interpreter, you can load the project
```sh
Prelude> :l Mastermind/Core.hs
```
You can then run the Mastermind functions
```sh
*Mastermind.Core> let secret = map Color ["red", "blue", "green", "yellow"]
*Mastermind.Core> let guess = map Color ["red", "green", "blue", "yellow"]
*Mastermind.Core> getResult secret guess
Result {wellPlaced = 2, missPlaced = 2}
```
### Running the tests
for running the tests, make sure you have the HUnit library installed, then type
```sh
*Mastermind.Core> :l Mastermind/Test.hs 
[1 of 2] Compiling Mastermind.Core  ( Mastermind/Core.hs, interpreted )
[2 of 2] Compiling Mastermind.Test  ( Mastermind/Test.hs, interpreted )
Ok, two modules loaded.
*Mastermind.Test> runTestTT mastermindTests
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Counts {cases = 4, tried = 4, errors = 0, failures = 0}
```
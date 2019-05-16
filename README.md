# LogiLab coding Challenge

For this codding challenge, I choose the [Mastermind](http://codingdojo.org/kata/Mastermind/) exercise on codingdojo.

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
You can now compile the project
```sh
$ ghc -o mastermind Main.hs
```
### Running

You can test the project with the command
```sh
mastermind <secret_length> <number_of_colors>
```

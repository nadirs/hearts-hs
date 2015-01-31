# Hearts-hs
[![Build Status](https://travis-ci.org/nadirs/hearts-hs.svg?branch=master)](https://travis-ci.org/nadirs/hearts-hs)

The game we used to play on Windows 98, re-written in Haskell.

## Pipeline:
A very rough and vague plan of how the game flow could be unwound:
    1. World → User/Client → Action
    2. Action → Validator → Validation
    3. Hairy multi-path choice pattern-matching on Validation → 4. or 5.
    4. Validation → Server → World
    5. Validation → UI → World

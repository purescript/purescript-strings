# purescript-strings

[![Latest release](http://img.shields.io/github/release/purescript/purescript-strings.svg)](https://github.com/purescript/purescript-strings/releases)
[![Build status](https://travis-ci.org/purescript/purescript-strings.svg?branch=master)](https://travis-ci.org/purescript/purescript-strings)

This package contains string & char utility functions and regular expressions.

The PureScript language uses the UTF-16 character encoding for strings. Character encodings can be a source of pitfalls if you're not aware of the design and key details of UTF-16 and Unicode.

Following is some terminology and examples that may help you understand the design of this library.

- A character is a minimal unit of text that has semantic value. The Unicode term for this is an "(extended) grapheme cluster."
- A code point is a value that can be used in a coded character set, such as Unicode, and is expressed as a hexadecimal value, such as U+0000.
- The valid code point range for the Unicode standard is U+0000 to U+10FFFF, inclusive
- The set of characters from U+0000 to U+FFFF are sometimes referred to as the Basic Multilingual Plane (BMP).
- A plane is a continuous group of 65,536 (2^16) code points. There are 17 planes in the Unicode standard.
- To support supplementary characters without changing the char primitive data type and causing incompatibility, supplementary characters are defined by a pair of code point values that are called surrogates.
- A code unit is a 16-bit char value, and a code point can consist of multiple code units.

Now, consider the string "𝐀bc". It has contains 3 characters, 3 code points, but 4 code units. The first character is the U+1D400, the mathematical bold capital "A", which requires two code points to specify. As another example, consider "Pok\x00E9mon" and "Poke\x0301mon". The U+00E9 code point defines 'e' with acute as a single value, but you can also write it as the plain 'e' followed by U+0301, which is the combining acute accent. Both consist of 7 code points, but the former is 7 code units and the latter is 8 code units.

The most intuitive length of a Unicode string is the number of code points, so the set of functions exports by `Data.String`, such as `length`, consider code points. There are situations in which a PureScript program will need access to a string's code units, however, so those functions are also included in this package in the `Data.String.CodeUnits` module.


## Installation

```
bower install purescript-strings
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-strings).

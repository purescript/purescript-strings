# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v6.0.1](https://github.com/purescript/purescript-strings/releases/tag/v6.0.1) - 2022-08-16

Bugfixes:
- Fix `Char`'s `toEnum` implementation (#163 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript/purescript-strings/releases/tag/v6.0.0) - 2022-04-27

Breaking changes:
- Migrate FFI to ES modules (#158 by @kl0tl and @JordanMartinez)
- Replaced polymorphic proxies with monomorphic `Proxy` (#158 by @JordanMartinez)
- In `slice`, drop bounds checking and `Maybe` return type (#145 by Quelklef)

New features:

Bugfixes:

Other improvements:
- Surround code with backticks in documentation (#148)
- Make `RegexFlags` a `newtype` and a `Newtype` instance for it(#159 by @mhmdanas)

## [v5.0.0](https://github.com/purescript/purescript-strings/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#129)
- Updated `replace'` to reflect the existence of optional capturing groups (#126)

New features:
- Replaced `unsafeCoerce` with `coerce` where appropriate (#130)
- Replaced monomorphic proxies with `Type.Proxy.Proxy` and polymorphic variables (#134)
- Added a dotAll regexp flag (#133)

Bugfixes:
- Removed the bounds check from the foreign implementation of `lastIndexOf'` (#137)

Other improvements:
- Fix line endings to match overall project style (#132)
- Removed references to `codePointToInt`, which no longer exists (#135)
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#136)
- Added a changelog and pull request template (#140, #141)

## [v4.0.2](https://github.com/purescript/purescript-strings/releases/tag/v4.0.2) - 2020-05-13

- Improved performance for `stripPrefix` / `stripSuffix` (#123, @michaelficarra)

## [v4.0.1](https://github.com/purescript/purescript-strings/releases/tag/v4.0.1) - 2018-11-11

- Fixed out of bounds access in `unsafeCodePointAt0Fallback` (@zyla)
- Fixed `slice` when end index equals string length (@abaco)

## [v4.0.0](https://github.com/purescript/purescript-strings/releases/tag/v4.0.0) - 2018-05-23

- Updated for PureScript 0.12
- `splitAt` now always returns a value (#78, @MonoidMusician)
- Added `slice` (@themattchan)
- Added more `String` `Gen`s to correspond with `Char` `Gen`s (@matthewleon)
- `Regex` `match` now returns `NonEmptyArray`
- All string functions now operate on code points now rather than code units. The old functions are available via the `.CodeUnits` modules
- `fromCharCode` can return `Nothing` now if given a value out of range

## [v3.5.0](https://github.com/purescript/purescript-strings/releases/tag/v3.5.0) - 2018-02-12

- Added `Data.String.NonEmpty`

## [v3.4.0](https://github.com/purescript/purescript-strings/releases/tag/v3.4.0) - 2017-12-28

- Add `Show CodePoint` instance (@csicar)
- Add `codePointFromChar` (@csicar)
- Expanded docs for most functions in `Data.String` and `Data.String.CodePoints` (@csicar)

## [v3.3.2](https://github.com/purescript/purescript-strings/releases/tag/v3.3.2) - 2017-11-19

- Performance improvement in `Data.String.Regex.match` (@fehrenbach)

## [v3.3.1](https://github.com/purescript/purescript-strings/releases/tag/v3.3.1) - 2017-08-06

- Fix some `Show` instances (@Rufflewind)

## [v3.3.0](https://github.com/purescript/purescript-strings/releases/tag/v3.3.0) - 2017-07-10

- Add a new module `Data.String.CodePoints`, which treats strings as sequences of Unicode code points rather than sequences of UTF-16 code units. In the future we may swap this module with `Data.String`. (@michaelficarra)
- Fix a typo in the documentation (@ijks)

## [v3.2.1](https://github.com/purescript/purescript-strings/releases/tag/v3.2.1) - 2017-06-06

- Ensure `genString` behaves the same regardless of the `MonadGen` implementation of `chooseInt` when `max < min`

## [v3.2.0](https://github.com/purescript/purescript-strings/releases/tag/v3.2.0) - 2017-06-05

- Generated strings from `genString` now vary in length
- Added additional `Char` generators

## [v3.1.0](https://github.com/purescript/purescript-strings/releases/tag/v3.1.0) - 2017-04-28

- Added some generator functions - introduced `Data.String.Gen` and `Data.Char.Gen`

## [v3.0.0](https://github.com/purescript/purescript-strings/releases/tag/v3.0.0) - 2017-03-26

- Updated for PureScript 0.11

## [v2.1.0](https://github.com/purescript/purescript-strings/releases/tag/v2.1.0) - 2016-12-25

- Added `unsafeRegex` (@rightfold)

## [v2.0.2](https://github.com/purescript/purescript-strings/releases/tag/v2.0.2) - 2016-10-26

- Documentation fix for `split` #70 (@leighman)

## [v2.0.1](https://github.com/purescript/purescript-strings/releases/tag/v2.0.1) - 2016-10-08

- Improved `null` check implementation (@Risto-Stevcev)

## [v2.0.0](https://github.com/purescript/purescript-strings/releases/tag/v2.0.0) - 2016-10-08

- Updated dependencies
- `Pattern` and `Replacement` newtypes are now used to distinguish between arguments when a function accepts multiple strings
- `RegexFlags` have been reworked as a monoid  (@Risto-Stevcev)

## [v1.1.0](https://github.com/purescript/purescript-strings/releases/tag/v1.1.0) - 2016-07-20

- Restored export of the `count` function.

## [v1.0.0](https://github.com/purescript/purescript-strings/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v0.7.1](https://github.com/purescript/purescript-strings/releases/tag/v0.7.1) - 2015-11-20

- Removed unused imports (@tfausak)

## [v0.7.0](https://github.com/purescript/purescript-strings/releases/tag/v0.7.0) - 2015-08-13

- Removed orphan (and incorrect) `Bounded Char` instance

## [v0.6.0](https://github.com/purescript/purescript-strings/releases/tag/v0.6.0) - 2015-08-02

- Added `toLower` and `toUpper` to `Data.Char`
- `search` in `Data.String.Regex` now returns `Maybe` result rather than using -1 for failure
- Added test suite

All updates by @LiamGoodacre

## [v0.5.5](https://github.com/purescript/purescript-strings/releases/tag/v0.5.5) - 2015-07-28

Add `stripSuffix`.

## [v0.5.4](https://github.com/purescript/purescript-strings/releases/tag/v0.5.4) - 2015-07-18

- Removed duplicate `Show` instance for `Char` (@anttih)

## [v0.5.3](https://github.com/purescript/purescript-strings/releases/tag/v0.5.3) - 2015-07-10

Add `stripPrefix` (@hdgarrood)

## [v0.5.2](https://github.com/purescript/purescript-strings/releases/tag/v0.5.2) - 2015-07-07

- Fixed `char` and `charCodeAt` in `Data.String.Unsafe` #36 (@stkb)

## [v0.5.1](https://github.com/purescript/purescript-strings/releases/tag/v0.5.1) - 2015-07-06

- Fixed missing `count` implementation (@qxjit)

## [v0.5.0](https://github.com/purescript/purescript-strings/releases/tag/v0.5.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

- Fixed various FFI exports (@sharkdp)
- Fixed `localeCompare`

## [v0.4.5](https://github.com/purescript/purescript-strings/releases/tag/v0.4.5) - 2015-03-23

- Added `char` to `Data.String.Unsafe` (@brainrape)
- Functions in `Data.String.Unsafe` now throw errors immediately when given unacceptable inputs (@brainrape)

## [v0.4.4](https://github.com/purescript/purescript-strings/releases/tag/v0.4.4) - 2015-03-22

- Updated docs

## [v0.4.3](https://github.com/purescript/purescript-strings/releases/tag/v0.4.3) - 2015-02-18

- Added `noFlags` record for default regex flags (@fresheyeball)

## [v0.4.2](https://github.com/purescript/purescript-strings/releases/tag/v0.4.2) - 2014-11-28

- Added `null`, `singleton`, `uncons`, `takeWhile`, and `dropWhile` to `Data.String` (@NightRa)

## [v0.4.1](https://github.com/purescript/purescript-strings/releases/tag/v0.4.1) - 2014-11-06

- Use ternary operator in JavaScript output (@davidchambers)

## [v0.4.0](https://github.com/purescript/purescript-strings/releases/tag/v0.4.0) - 2014-10-27

- Made `charCodeAt` safe, added unsafe versions of `charAt`, `charCodeAt` (@garyb)

## [v0.3.3](https://github.com/purescript/purescript-strings/releases/tag/v0.3.3) - 2014-10-24

- Added `split` to `Data.String.Regex` (@davidchambers)

## [v0.3.2](https://github.com/purescript/purescript-strings/releases/tag/v0.3.2) - 2014-10-16

- Added essential instances for `Char` (@jdegoes)

## [v0.3.1](https://github.com/purescript/purescript-strings/releases/tag/v0.3.1) - 2014-10-15

- Fixed typo in `fromCharArray` FFI implementation (@jdegoes)

## [v0.3.0](https://github.com/purescript/purescript-strings/releases/tag/v0.3.0) - 2014-10-14

- Introduced `Char` newtype and corresponding functions (@jdegoes)
- Made `charAt` safe - breaking change (@jdegoes)

## [v0.2.1](https://github.com/purescript/purescript-strings/releases/tag/v0.2.1) - 2014-07-21

- Fix typo in FFI definition for `flags` (@garyb)

## [v0.2.0](https://github.com/purescript/purescript-strings/releases/tag/v0.2.0) - 2014-07-20

- `Show` instance for `Regex` (@michaelficarra)
- `Regex` now has `RegexFlags` rather than a string for options (@michaelficarra)

## [v0.1.3](https://github.com/purescript/purescript-strings/releases/tag/v0.1.3) - 2014-05-04

- Renamed `Data.String.Regex.replaceR` to `replace`, added `replace'` which uses a function to construct replacements for matches.

## [v0.1.2](https://github.com/purescript/purescript-strings/releases/tag/v0.1.2) - 2014-04-30

- Added `indexOf'` and `lastIndexOf'` (paf31)

## [v0.1.1](https://github.com/purescript/purescript-strings/releases/tag/v0.1.1) - 2014-04-27

- Swapped `joinWith` arguments for better style

## [v0.1.0](https://github.com/purescript/purescript-strings/releases/tag/v0.1.0) - 2014-04-25

- Initial release

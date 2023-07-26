# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v9.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v9.0.0) - 2022-07-26

Breaking changes:
- Correct type signature for `pipeline` (#55 by @JordanMartinez)

Bugfixes:
- Fix FFI for `isPaused` and `pause` (#55 by @JordanMartinez)

## [v8.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v8.0.0) - 2022-07-19

Breaking changes:
- Update `node-buffer` to `v9.0.0` (#48 by @JordanMartinez)
- Reimplement event handlers using `eventNameH`-style API (#49 by @JordanMartinez)

  Previously, one would write something like the following, and be unable to remove
  the resulting listener.
  ```purs
  Stream.onData stream \buffer -> do
    ...
  ```

  Now, one writes such a thing via `on` (or a similar function) from `node-event-emitter`:
  ```purs
  -- if the listener should be removed later, use `on`.
  removeListener <- stream # on dataH \buffer -> do
    ...
  -- if it doesn't need to be removed, use `on_`.
  stream # on_ dataH \buffer -> do
    ...
  ```
- Renamed functions to better adhere to naming consistency (#50 by @JordanMartinez)

  All functions that take an optional callback are now 
  named using the following schema:
  - no callback: `functionName`
  - with callback: `functionName'`

  Thus, the following were renamed:
  - `write` was renamed to `write'`
  - `writeString` was renamed to `writeString'`
  - `end` was renamed to `end'`
  - `destroyWithError` was renamed to `destroy'`

  `write`, `writeString`, and `end` now refer to their non-callback versions.


New features:
- Added event handlers for `Writeable` streams (#49 by @JordanMartinez)
- Added missing APIs (#51, #53 by @JordanMartinez)

  - readable, readableEnded, readableFlowing, readableHighWaterMark, readableLength
  - pipe'
  - writeable, writeableEnded, writeableCorked, errored, writeableFinished, writeableHighWaterMark, writeableLength, writeableNeedDrain
  - closed, destroyed
  - allowHalfOpen
  - pipeline
  - readableFromString, readableFromBuffer
  - newPassThrough
- Integrated `node-streams-aff` into library (#52 by @JordanMartinez)

  Convenience APIs added for readable streams in "paused" mode:
  - readSome
  - readAll
  - readN

  Convenience APIs for writeable streams:
  - write
  - end
  
  Convenience APIs for converting `String`s from/to `Array Buffer`
  - toStringUTF8
  - fromStringUTF8

  The only APIs from the library not added were `newReadable` and `push`.
- Added convenience API for converting `Readable` to `String` or `Buffer` (#53 by @JordanMartinez)

  - `readableToStringUtf8`
  - `readableToString`
  - `readableToBuffers`

Bugfixes:
- Drop misleading comment for `setEncoding` (#51 by @JordanMartinez)

  For context, see #37.

Other improvements:
- Bumped CI's node version to `lts/*` (#48 by @JordanMartinez)
- Updated CI `actions/checkout` and `actions/setup-nodee` to `v3` (#48 by @JordanMartinez)
- Format code via purs-tidy; enforce formatting via CI (#48 by @JordanMartinez)
- Refactor tests using `passThrough` streams (#49 by @JordanMartinez)
- Updated FFI to use uncurried functions (#50 by @JordanMartinez)
- Relocated `setEncoding`, `Read`, and `Write` for better locality in docs (#51 by @JordanMartinez)
- Added `node-streams-aff` tests (#52 by @JordanMartinez)
- Updated `spec` to `v7.5.3` to address `bower` dep issue (#54 by @JordanMartinez)

  See https://github.com/purescript-spec/purescript-spec/pull/142 for more context.

## [v7.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v7.0.0) - 2022-04-29

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#39 by @nwolverson, @JordanMartinez, @sigma-andex)
- Update `write`/`writeString`/`end` callbacks to include `Maybe Error` arg (#40 and #43 by @JordanMartinez)

New features:

Bugfixes:
- Exported `destroyWithError` (#43 by @JordanMartinez)

Other improvements:
- Fix `Gzip` example (#17, #36 by @matthewleon and @JordanMartinez)

## [v6.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v6.0.0) - 2022-04-27

Due to an incorrectly-made breaking change, please use `v7.0.0` instead.

## [v5.0.0](https://github.com/purescript-node/purescript-posix-types/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
  - Added support for PureScript 0.14 and dropped support for all previous versions (#31)

Other improvements:
  - Migrated CI to GitHub Actions, updated installation instructions to use Spago, and switched from `jshint` to `eslint` (#30)
  - Added a changelog and pull request template (#32)

## [v4.0.1](https://github.com/purescript-node/purescript-node-streams/releases/tag/v4.0.1) - 2019-07-24

- Relax upper bound on `purescript-node-buffer`

## [v4.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v4.0.0) - 2018-05-27

- Updated for PureScript 0.12

## [v3.3.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v3.3.0) - 2017-11-29

- Added `unpipe` and `destroy` functions (@matthewleon)

## [v3.2.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v3.2.0) - 2017-11-26

- Allow `onClose` to be used on writable streams (@matthewleon)

## [v3.1.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v3.1.0) - 2017-04-20

- Added `onFinish` event (@felixSchl)

## [v3.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v3.0.0) - 2017-04-04

- Updated for 0.11 (@anilanar)

## [v2.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v2.0.0) - 2016-10-17

- Updated dependencies

## [v1.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v1.0.0) - 2016-06-07

- Compatibility with 0.9.x of the PureScript compiler.

## [v0.6.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.6.0) - 2016-05-25

- Made the `size` parameter available to the `read` functions (@felixSchl).

## [v0.5.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.5.0) - 2016-05-20

- Added `read`, `readString`, `readEither`, and `onReadable` (@felixSchl)

## [v0.4.1](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.4.1) - 2016-05-02

- Fixed license in bower.json for Pursuit.

## [v0.4.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.4.0) - 2016-03-31

- Made the `Error` value available to the `onError` callback.

## [v0.3.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.3.0) - 2015-12-14

- Removed the last type parameter to `Stream` (#4)
- Added `onDataEither`, for reading from a stream that might or might not have an encoding set. (#4)

## [v0.2.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.2.0) - 2015-12-09

- Updated dependencies

## [v0.1.4](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.1.4) - 2015-12-05

- Fixed warnings (@thimoteus)

## [v0.1.3](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.1.3) - 2015-10-02

- Fixed an issue in the FFI.

## [v0.1.2](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.1.2) - 2015-09-24

- Fixed some JS errors.

## [v0.1.1](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.1.1) - 2015-09-23

- Released to Pursuit

## [v0.1.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v0.1.0) - 2015-09-23

- Initial release.

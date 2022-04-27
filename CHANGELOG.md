# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v6.0.0](https://github.com/purescript-node/purescript-node-streams/releases/tag/v6.0.0) - 2022-04-27

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#39 by @nwolverson, @JordanMartinez, @sigma-andex)
- Update `write` callback to include `Error` arg (#40 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:
- Fix `Gzip` example (#17, #36 by @matthewleon and @JordanMartinez)

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

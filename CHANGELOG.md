# Changelog

All notable changes to the Daraja HTTP Framework are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Releases are tagged `vMAJOR.MINOR.PATCH` and published at
<https://github.com/michaelJustin/daraja-framework/releases>.

## [Unreleased]

_Work tracked under the [3.2.0 milestone](https://github.com/michaelJustin/daraja-framework/milestone/17)._

## [3.1.2] - 2026-09-08

### Fixed

- `IWebFilterConfig.GetFilterName` now returns the filter name instead of an
  empty string. `SetName` was added to `IWriteableConfig`; `TdjWebFilterHolder`
  passes its `Name` into the config before `Init`. (#415)
- `TdjWebFilterHolder` and `TdjWebComponentHolder` clear their instance field
  after freeing it (in `DoStop`, and on a failed filter init), removing a
  dangling-pointer / repeated-stop hazard. (#415)
- `TdjPathMap` establishes its sort order when URL patterns are added rather than
  as a side effect of every `GetMatches` lookup. (#415)
- `djNCSALogFilter`: correct timezone suffix for UTC offsets west of Greenwich
  (`DecodeTime` was called on a negative `TDateTime`). (#416)

### Changed

- `djStatisticsFilter.RequestsActive` widened to `Int64` to match the backing
  counter and the other accessors. (#417)

### Internal

- The FPC `Console` test build is compiled with heap tracing (`-gh`) and writes
  `heaptrace.log`; `UNIT-TESTS.md` documents checking it on every run. A leaked
  `TTestFilter` in `djWebFilterTests` was fixed. (#448)
- Version constant set to `3.1.2`. (#418)

## [3.1.1] - 2026-05-20

### Changed

- Use parametrized logging. (#406)
- Comment out deprecated methods. (#408)

## [3.1.0] - 2026-05-17

### Changed

- Requires slf4p 1.0.8.
- `TdjLoggerFactory.GetLogger` calls use a class reference. (#396)

[Unreleased]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.2...HEAD
[3.1.2]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.1...v3.1.2
[3.1.1]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.0...v3.1.1
[3.1.0]: https://github.com/michaelJustin/daraja-framework/compare/v3.0.6...v3.1.0

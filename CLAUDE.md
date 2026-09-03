# Daraja HTTP Framework

Compact HTTP server application framework for Object Pascal. Targets **Delphi
2009+** and **Lazarus 4.x / FPC 3.2.x**. Keep every change compiling on both.

## Layout

- `source/` — the framework. `source/optional/` holds unsupported helper units.
- `test/unittests/` — the test suite (FPCUnit + DUnit). See `UNIT-TESTS.md`.
- `demo/` — example servers.
- API doc-comment conventions: see `DOC-COMMENTS.md`.

## Dependencies

Expected as **siblings of this repository directory**:

- `Indy/Lib/{Core,Protocols,System}` — Indy 10.6.2 / 10.6.3 (use this checkout,
  not the one bundled with Delphi — older bundled versions lack `hcPATCH`).
- `slf4p/src/main` — Simple Logging Facade for Pascal.

## Running the tests

From `test/unittests/`:

- FPC: `run-fpc.cmd` / `run-fpc.sh` (builds the `Console` build mode, runs headless)
- Delphi: `run-delphi.cmd` (builds with `dcc32`, runs the DUnit text runner)

Both exit non-zero on failure. Full details, including how to run a single suite
and the GUI runners, are in `UNIT-TESTS.md`.

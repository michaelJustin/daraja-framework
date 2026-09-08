# Building and running the unit tests

The tests live in [`test/unittests/`](test/unittests/). One set of test source
files feeds two project files:

| Project | Compiler | Runner |
|---------|----------|--------|
| `Unittests.lpi` | Free Pascal / Lazarus | FPCUnit |
| `Unittests.dpr` | Delphi | DUnit |

## Dependencies

Both builds expect these checkouts as **siblings of the repository directory**
(the project search paths use `..\..\..\Indy\...` and `..\..\..\slf4p\src\main`):

- `Indy/Lib/{Core,Protocols,System}` — Indy 10.6.2 / 10.6.3
- `slf4p/src/main` — Simple Logging Facade for Pascal

For Delphi, use this Indy checkout, **not** the one bundled with the IDE — older
bundled Indy versions lack `hcPATCH` and will not compile the framework.

## Quick start

From `test/unittests/`:

```
run-fpc.cmd          # Windows, Free Pascal / Lazarus
./run-fpc.sh         # POSIX, Free Pascal / Lazarus
run-delphi.cmd       # Windows, Delphi
```

Each script builds from scratch, runs the **whole** suite headless, prints a
plain-text report, and exits non-zero if anything failed. No IDE, no
interaction, no environment setup beyond the dependencies above.

- `run-fpc` needs `lazbuild` on `PATH`, or set `LAZBUILD` to its full path.
- `run-delphi` defaults to RAD Studio 2009 at
  `C:\Program Files (x86)\CodeGear\RAD Studio\6.0`; override `BDS` and `INDY` if
  your layout differs.

Extra arguments are forwarded to the runner, e.g.
`run-fpc.cmd --suite=TdjPathMapTests` or
`run-fpc.cmd --format=xml --file=results.xml`.

## What the scripts do

### Free Pascal

```
lazbuild -B --build-mode=Console Unittests.lpi
UnittestsConsole.exe --all --format=plain
```

The project has two build modes:

- **Default** — GUI test runner (`Unittests.exe`), for interactive use.
- **Console** — console-subsystem binary (`UnittestsConsole.exe`) whose stdout
  works under redirection and which never waits for input. This is the one used
  for scripted / CI runs.

Exit code: bit 0 set on failures, bit 1 set on errors.

#### Always check `heaptrace.log`

The **Console** build mode is compiled with heap tracing (`-gh`), and the runner
calls `SetHeapTraceOutput('heaptrace.log')`, so every `run-fpc` run rewrites
`test/unittests/heaptrace.log` with the FPC leak report.

A green test run is **not** enough — also open `heaptrace.log` and confirm the
`unfreed memory blocks` count has not grown. A handful of blocks allocated from
Indy unit initialisation (`IdThread`/`IdStack` `..._init$` frames) are expected
and can be ignored; any block whose call trace points into `source/` or a test
unit is a regression and must be fixed before merging.

The heap report does not affect the process exit code, so scripts and CI that
only check the exit status will miss leaks.

### Delphi

```
dcc32 -B -Q -U<...;Indy Lib;source;slf4p> -I<Indy Core;Indy System> -E. Unittests.dpr
Unittests.exe -text-mode
```

`Unittests.dpr` runs the DUnit **text** runner with `-text-mode` and the DUnit
**GUI** runner otherwise. The text runner uses `rxbHaltOnFailures`, so the
process exits with `ErrorCount + FailureCount`.

The `-I` include path is required for `IdCompilerDefines.inc`. Two harmless Indy
warnings are expected (W1036 `LCloseConnection`, W1035 SSPI).

## Running a subset

- FPCUnit: `--suite=<TestCaseClass>` (optionally `.<TestMethod>`), or `--list`.
- DUnit text runner: registered-test selection is not available on the command
  line; use the GUI runner, or temporarily narrow `RegisterUnitTests`.

## Optional / opt-out suites

- The **HTTPS** suite (`THttpsTests`) is compiled only when `DARAJA_TEST_HTTPS`
  is defined (it needs the OpenSSL DLLs).
- The integration suites that start a loopback HTTP server (`TSessionTests`,
  `TAPIConfigTests`) run everywhere by default. Define `DARAJA_SKIP_SERVER_TESTS`
  to exclude them where binding a listening socket is not possible or not
  wanted. The **`ConsoleCI`** build mode is the `Console` mode with that define
  baked in (`lazbuild -B --build-mode=ConsoleCI Unittests.lpi` &rarr;
  `UnittestsConsoleCI`); CI builds it. A `lazbuild --opt=-d...` flag would also
  work with a recent Lazarus, but Lazarus 3.0's `lazbuild` rejects `--opt`.

## GUI runners

Build the default mode (`lazbuild -B Unittests.lpi`, or the Delphi project
without `-text-mode`) and run the executable with no arguments.

## Continuous integration

[`.github/workflows/tests.yml`](.github/workflows/tests.yml) checks out the two
dependencies as siblings and runs the Free Pascal suite headless on both
`windows-latest` and `ubuntu-latest` for every push and pull request that
touches `source/` or `test/`. Windows installs Lazarus via `setup-lazarus`;
Linux installs it from the Ubuntu archive (the action's SourceForge download
stalls on the hosted Linux runners) and runs the console runner under `xvfb`
(the runner links the LCL). Delphi is not covered in CI.

CI builds the `ConsoleCI` mode, so the loopback-server integration suites
(`TSessionTests`, `TAPIConfigTests`) do **not** run in CI. Run `run-fpc` /
`run-delphi` locally before merging to exercise them.

## Notes

- The runners `SetCurrentDir` to the executable's own folder at startup, so the
  `.\resources\` paths used by the upload tests resolve regardless of where the
  runner is launched from.
- `TdjDefaultWebComponent` resolves `webapps\` relative to
  `ExtractFilePath(ParamStr(0))`, so the test executable must be built into
  `test/unittests/` (next to `webapps/`). All project files and scripts already
  do this.

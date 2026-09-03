# Building and running the unit tests

The tests live in [`test/unittests/`](test/unittests/). One set of test source
files feeds two project files:

| Project | Compiler | Runner |
|---------|----------|--------|
| `Unittests.lpi` | Free Pascal / Lazarus | FPCUnit (console or GUI) |
| `Unittests.dpr` | Delphi | DUnit (text or GUI) |

## Dependencies

Both builds expect these checkouts as **siblings of the repository directory**
(the project search paths use `..\..\..\Indy\...` and `..\..\..\slf4p\src\main`):

- `Indy/Lib/{Core,Protocols,System}` — Indy 10.6.2 / 10.6.3
- `slf4p/src/main` — Simple Logging Facade for Pascal

For Delphi, use this Indy checkout, **not** the one bundled with the IDE — older
bundled Indy versions lack `hcPATCH` and will not compile the framework.

## Free Pascal / Lazarus

```
lazbuild -B test/unittests/Unittests.lpi
cd test/unittests
Unittests.exe --all --format=plain
```

Any command-line argument selects the console runner; with no arguments the GUI
runner opens.

Note: the `.lpi` sets `GraphicApplication=True`, so a run whose output is piped
or redirected produces **no stdout**. To capture console output, temporarily set
`<GraphicApplication Value="False"/>` in the `.lpi`, rebuild, then revert.

## Delphi

Build from the `test/unittests/` directory (adjust the two paths for your
machine):

```
set RS=C:\Program Files (x86)\CodeGear\RAD Studio\6.0
set I=..\..\..\Indy\Lib
"%RS%\bin\dcc32.exe" -B -Q ^
  "-U%RS%\lib;%I%\Core;%I%\Protocols;%I%\System;..\..\source;..\..\source\optional;..\..\..\slf4p\src\main" ^
  "-I%I%\Core;%I%\System" -N0<dcu-output-dir> -E. Unittests.dpr
Unittests.exe -text-mode
```

- `-I` (include search path) is required for `IdCompilerDefines.inc`.
- `-E.` puts the exe in `test/unittests/` — see the working-directory note below.
- Without `-text-mode` the DUnit GUI runner opens.
- Two harmless Indy warnings are expected (W1036 `LCloseConnection`, W1035 SSPI).

## Running the full suite

`TestHelper.RegisterUnitTests` only registers the session, HTTPS and API-config
suites when `not UseConsoleTestRunner` — i.e. when **no** command-line argument
is passed. Every CLI invocation passes an argument, so the console / text runner
sees only the small subset (~16–17 tests).

To run the full suite from the command line, temporarily remove the
`if not UseConsoleTestRunner` guard in `TestHelper.RegisterUnitTests` (there is
one in the FPC branch and one in the `{$ELSE}` Delphi branch), rebuild, run, then
revert. Alternatively, run the GUI runner, which always registers everything.

Full suite size: **67 tests** (FPC) / **66 tests** (Delphi — the FPC-only
`TdjWebFilterTests` is not in the Delphi project).

## Working directory

`TdjDefaultWebComponent` resolves the `webapps\` folder relative to
`ExtractFilePath(ParamStr(0))` (the executable's location), not the current
directory. The test executable must therefore sit in `test/unittests/` (next to
`webapps/`), or `TdjDefaultWebComponentTests` will get 404 responses.

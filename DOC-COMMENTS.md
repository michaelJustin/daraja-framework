# API documentation conventions for `source/`

The public API reference at <https://michaeljustin.github.io/daraja-framework/>
is generated from the doc comments in the `source/` unit files. This document
describes how those comments are written and how the site is built, so that new
and edited comments stay consistent with the existing ones.

## Toolchain

| Piece | Location | Purpose |
|-------|----------|---------|
| [Doxygen](https://www.doxygen.nl/) | external | documentation generator |
| [pas2dox](https://sourceforge.net/projects/pas2dox/) | `make/pas2dox-0.50rc1.exe` | pre-processor that turns Object Pascal into something Doxygen understands |
| Doxygen config | [make/doxygen.cfg](make/doxygen.cfg) | project settings (`Doxyfile`) |
| Local build script | [make/makedox.cmd](make/makedox.cmd) | runs Doxygen, prints `warnings.log`, opens the result |
| Single-file preview | [make/d.cmd](make/d.cmd) | pipes one unit through pas2dox to inspect the generated C++ |
| CI workflow | [.github/workflows/doxygen.yml](.github/workflows/doxygen.yml) | regenerates and deploys the site to the `gh-pages` branch |

pas2dox is wired in through `FILTER_PATTERNS = "*.pas=make\pas2dox-0.50rc1 -s"`
and `EXTENSION_MAPPING = pas=C`. Doxygen therefore parses the Pascal sources
*as if they were C/C++*, which is why Doxygen's C-style command set (see below)
is what actually works.

### Building the docs locally

```
cd make
makedox.cmd
```

Output goes to `docs/html/` (`OUTPUT_DIRECTORY = docs/html`); the entry point is
`docs/html/index.html`. Warnings are written to `warnings.log` in the repo root.
The CI job runs on every push to `master` and on every `v*` tag; it appends
`PROJECT_NUMBER` from `DWF_SERVER_VERSION` in `source/djGlobal.pas` so the site
always shows the version being built (the `PROJECT_NUMBER` in `doxygen.cfg` is
only used for local builds).

### What gets documented

* `INPUT = source` with `RECURSIVE = NO` and `FILE_PATTERNS = *.pas` — only the
  `.pas` files **directly** in `source/` are processed. `source/optional/` and
  `source/backup/` are **not** part of the published reference.
* `EXTRACT_ALL = YES` — every entity appears even when undocumented, so a missing
  comment is silent in the output but shows up in `warnings.log`.
* `EXTRACT_PRIVATE = NO` — `private` / `strict private` members (e.g. the
  `Logger` field) are omitted from the site. Do not rely on doc comments there.
* `INLINE_INHERITED_MEMB = YES` and `INHERIT_DOCS = YES` — inherited members are
  shown on the descendant, and an undocumented override inherits the ancestor's
  text. Only re-document an override when its behaviour genuinely differs.
* `SORT_MEMBER_DOCS = YES` — members are listed alphabetically in the output, so
  declaration order in the unit does not matter for the reader.

## Comment format

### The documentation block

Use a Qt-style block that starts with `{*` and ends with `*}`, with a leading
`*` on every interior line, placed immediately **before** the entity:

```pascal
{*
 * Add a new context.
 *
 * @param Context the context handler.
 * @throws EWebComponentException if an error occurs that interferes with the component's normal operation.
 *}
procedure Add(Context: TdjWebComponentContextHandler);
```

`JAVADOC_AUTOBRIEF = YES`: the text up to the first `.`, `?` or `!` becomes the
**brief** description used in summary lists; everything after it is the detailed
description. Write the first sentence so it stands alone.

Document, in the `interface` section:

* the unit's main page / overview (currently in [source/djServer.pas](source/djServer.pas)),
* every `type` — classes, interfaces, enumerations, class-reference types,
* every method, property and standalone routine,
* constants that are part of the API.

### Enumerations

Give the type a block, and document each value with a trailing `{*< ... }`
member comment:

```pascal
{*
 * @enum TSpecType
 * Declares the path specification types.
 *}
TSpecType = (stUnknown,  {*< Unknown type }
             stExact,    {*< Exact match  }
             stPrefix,   {*< Longest prefix match }
             stSuffix,   {*< Longest suffix match }
             stDefault   {*< Default }
            );
```

### Interfaces

Interfaces carry an explicit `@interface <Name>` line so pas2dox/Doxygen render
them correctly:

```pascal
{*
 * Interface for components with a lifecycle (start/stop capabilities).
 * @interface ILifeCycle
 *}
ILifeCycle = interface(IInterface)
```

## Commands in use

Doxygen commands are written with the `@` prefix (not `\`), except for the
conditional-section markers which use `\`. The vocabulary actually used in
`source/` is small — stick to it:

| Command | Use |
|---------|-----|
| `@param <Name> <text>` | one per parameter; `WARN_IF_INCOMPLETE_DOC` flags any that are missing |
| `@return <text>` | return value. `@returns` and `@result` also appear in the tree — **prefer `@return`** for new comments |
| `@throws <Exception> <when>` | exceptions the caller may see (typically `EWebComponentException`) |
| `@note <text>` | caveats, e.g. "if this method is overridden, the overriding code must also call inherited Init" |
| `@li <text>` | bullet list item (used in the overview and class summaries) |
| `@sa <ref>` | "see also"; may point at a URL (e.g. an RFC) |
| `@mainpage`, `@section <id> <title>`, `@subsection` | structure of the overview page in `djServer.pas` |
| `@enum <Name>`, `@interface <Name>` | tell the C-mode parser what the following entity is |
| `\link <target> <text> \endlink` | cross-reference with custom link text |

`MARKDOWN_SUPPORT = YES` (with `MARKDOWN_STRICT = YES`), so Markdown formatting
is available inside comment blocks. Raw HTML is also accepted — the overview uses
`<a target="_blank" href="...">` for external links.

## Hiding implementation detail

* The `implementation` section is fenced off so its symbols never reach the
  site. Every unit ends the interface line and the unit like this:

  ```pascal
  implementation /// \cond

  // ...

  end. /// \endcond
  ```

* Individual internal members are marked `/// \private` (for example the
  `TdjLifeCycle` overrides `DoStart` / `DoStop`).

* Internal type aliases are wrapped in `/// \cond` … `/// \endcond`.

## What is *not* a doc comment

* The `{*** ... ***}` licence banner at the top of every unit. `JAVADOC_BANNER`
  is `NO`, so Doxygen ignores it — leave it exactly as-is.
* `{ TdjClassName }` lines above a class are Lazarus code-folding markers, not
  documentation. Keep them, but real documentation goes in the `{* ... *}` block
  that follows.
* Ordinary `//` and `{ ... }` comments inside method bodies are just code
  comments; `HIDE_IN_BODY_DOCS = NO` but the bodies themselves are inside the
  `\cond` region, so they never surface.

## Checklist for a new or changed comment

1. Block is `{* ... *}`, immediately before the entity, in the `interface`
   section.
2. First sentence works as a standalone brief description.
3. Every parameter has an `@param`; a return value has `@return`; raised
   exceptions have `@throws`.
4. New cross-references use `\link ... \endlink` or a bare type name.
5. Run `make/makedox.cmd` and confirm `warnings.log` has no new entries for the
   files you touched.

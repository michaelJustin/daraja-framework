![Object Pascal](https://img.shields.io/badge/language-Object%20Pascal-blue.svg)
![Delphi 2009+](https://img.shields.io/badge/Delphi-2009+-blue.svg)
![Lazarus 4.x](https://img.shields.io/badge/Lazarus-4.x-blue.svg)
![Windows: Delphi and FPC](https://img.shields.io/badge/Windows-Delphi%20%7C%20FPC%2FLazarus-blue)
![Linux: FPC only](https://img.shields.io/badge/Linux-FPC%2FLazarus-blue)
![GitHub last commit](https://img.shields.io/github/last-commit/michaelJustin/daraja-framework)
[![Unit tests](https://github.com/michaelJustin/daraja-framework/actions/workflows/tests.yml/badge.svg)](https://github.com/michaelJustin/daraja-framework/actions/workflows/tests.yml)
[![Doxygen Docs](https://github.com/michaelJustin/daraja-framework/actions/workflows/doxygen.yml/badge.svg)](https://github.com/michaelJustin/daraja-framework/actions/workflows/doxygen.yml)
[![pages-build-deployment](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment)
![GitHub Repo stars](https://img.shields.io/github/stars/michaelJustin/daraja-framework?style=flat)

![Daraja HTTP Framework](daraja_logo_landscape.png)

In short, Daraja enables Object Pascal developers to write *well-structured HTTP server applications*.

## Contents

- [About](#about)
- [Features](#features)
- [Usage](#usage)
- [Example](#example)
- [Documentation](#documentation)
- [Licensing](#licensing)
- [Credits](#credits)

## About

__Daraja__ is a compact and flexible HTTP server application framework for Object Pascal, based on the HTTP server included in [Indy - Internet Direct](https://github.com/IndySockets/Indy). The framework uses URL patterns to match requests to your resource handler code, and optional request filtering for pre- and post-processing. It enables developers to create well-structured HTTP server applications, written with 100% open source code.

## Features

 - **URL-pattern routing** &mdash; map requests to handler classes by exact, prefix, suffix or default patterns
 - **Web components** &mdash; handle requests by overriding per-method hooks (`OnGet`, `OnPost`, `OnPut`, ...)
 - **Filter chains** &mdash; pluggable pre- and post-processing of requests and responses
 - **Contexts** &mdash; group resources under a base path with their own init parameters
 - **HTTP sessions** &mdash; server-side session state with configurable timeout
 - **Static content** &mdash; serve files from a directory with path-traversal protection
 - **Optional helpers** &mdash; NCSA access logging and request-statistics filters
 - **Dual compiler support** &mdash; one codebase for Delphi 2009+ and Lazarus 4.x / FPC 3.2.x
 - **AGPL or commercial** &mdash; 100% open source, with a commercial license available

## Usage

<details>
<summary>Prerequisites</summary>

The minimum requirements are:

 - Delphi 2009 or higher _or_
 - Lazarus 4.x / Free Pascal 3.2.x
 - [Indy - Internet Direct](https://github.com/IndySockets/Indy) 10.6.2 or 10.6.3
 - [slf4p - Simple Logging Facade for Pascal](https://github.com/michaelJustin/slf4p)

  Optional dependencies for some code examples and logging:
 - [JsonDataObjects](https://github.com/ahausladen/JsonDataObjects)
 - [Log4D](http://sourceforge.net/projects/log4d/)
  
</details>

<details>
<summary>Get the source</summary>

Daraja and its dependencies are expected to sit **side by side in the same parent
directory**:

```
Projects/
├── daraja-framework/      this repository
├── Indy/                  https://github.com/IndySockets/Indy  (10.6.2 or 10.6.3)
└── slf4p/                 https://github.com/michaelJustin/slf4p
```

```Console
mkdir Projects && cd Projects
git clone https://github.com/michaelJustin/daraja-framework.git
git clone https://github.com/IndySockets/Indy.git
git clone https://github.com/michaelJustin/slf4p.git
```

The demo and test project files use relative paths (`..\..\..\Indy\...`,
`..\..\..\slf4p\src\main`) that rely on this layout.

</details>

<details>
<summary>IDE configuration guide</summary>

To make Daraja HTTP Framework and Internet Direct (Indy) available for a project,

 - add the Daraja HTTP Framework `<Install>/source` folder to the project search path
 - add the folders `<Indy>/Lib/Core`, `<Indy>/Lib/System` and `<Indy>/Lib/Protocols` to
the project search path
 
</details>

## Example

These are the basic steps to configure a simple "Hello, World!" application. A simple resource will be defined in a TdjWebComponent which has only one method, `OnGet`. The web component will then be installed in the server.

### Resource definition

A Daraja Web Component defines the request handling and response building, but it does not specify the actual location (HTTP address) of a resource.
The web component in this example handles HTTP GET requests by overriding the OnGet method. The method sets the response content text and content type.

```pascal
type
  THelloWorldResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure THelloWorldResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'Hello, World!';
  Response.ContentType := 'text/plain';
end;
```

### Context and resource registration

We want to place the web component in the context `tutorial` and the absolute path `/hello`. We also want to use port 80. 
The full URL of our resource is `http://127.0.0.1/tutorial/hello`

```pascal
procedure Demo;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create(80);
  try
    Context := TdjWebAppContext.Create('tutorial');
    Context.Add(THelloWorldResource, '/hello');
    Server.Add(Context);
    Server.Start;
    WriteLn('Server is running, please open http://127.0.0.1/tutorial/hello');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
  end;
end;
```

Full source: [demo/01_helloworld/MainUnit.pas](../demo/01_helloworld/MainUnit.pas).
More runnable examples &mdash; sessions, filters, static content, server-sent
events, OpenID Connect &mdash; are in [demo/](../demo/).

#### Test with curl:

```Console
curl -i http://127.0.0.1/tutorial/hello
HTTP/1.1 200 OK
Connection: keep-alive
Content-Type: text/plain; charset=ISO-8859-1
Content-Length: 13
Date: Wed, 22 Jan 2025 19:07:14 GMT

Hello, World!
```

(The `charset=ISO-8859-1` is appended automatically by Indy; the example code only sets `text/plain`.)

## Documentation

### API docs

[API reference (Doxygen)](https://michaeljustin.github.io/daraja-framework/)

### Getting started with Daraja

[DarajaFrameworkGettingStarted.pdf](https://www.habarisoft.com/daraja_framework/3.1.0/DarajaFrameworkGettingStarted.pdf) (version 3.1.0)

### Changelog

See [CHANGELOG.md](../CHANGELOG.md) for the release history.

## Licensing

Daraja HTTP Framework is dual licensed under the GNU Affero General Public License and a commercial license. The GNU Affero General Public License is a free, copyleft license for software and other kinds of works, specifically designed to ensure cooperation with the community in the case of network server software.

### AGPL licensing FAQ

<details>
<summary>Can I use it in my commercial Project?</summary>
Yes, if you open source your whole project (thus also AGPL it) otherwise no.
</details>

<details>
<summary>Is it enough to ship the licence texts or do I need to ship the source code (from Daraja) too?</summary>
You have to supply the whole sourcecode of everything - but a download link should suffice.
</details>

<details>
<summary>Do I need to mention the use of Daraja inside my program (like a info message or something)?</summary>
No, this is not required.
</details>

### Commercial license

You can be released from the requirements of the AGPL license by purchasing a commercial license. The commercial license can be obtained from https://www.habarisoft.com/daraja_framework.html

## Credits

This software uses the following open source packages:

- [Indy - Internet Direct](https://github.com/IndySockets/Indy)

For example code, unit testing, and documentation, it uses the following open source packages:

- [JsonDataObjects](https://github.com/ahausladen/JsonDataObjects) &mdash; example code
- [Log4D](https://sourceforge.net/projects/log4d/) and [slf4p](https://github.com/michaelJustin/slf4p/) &mdash; logging
- [DUnit](https://dunit.sourceforge.net/) and [FPCUnit](https://wiki.freepascal.org/fpcunit) &mdash; unit testing
- [Doxygen](https://www.doxygen.nl/) and [pas2dox](https://sourceforge.net/projects/pas2dox/) &mdash; API documentation generation

## Origins

> "Daraja" means "bridge" in Swahili. The Daraja Framework serves as a bridge between incoming HTTP requests and the Object Pascal code that handles them, enabling seamless integration between web traffic and application logic.
> — ChatGPT, OpenAI (May 2025)

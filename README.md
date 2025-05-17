[![AGPL License](https://img.shields.io/badge/license-AGPL-blue.svg)](http://www.gnu.org/licenses/agpl-3.0)
![GitHub top language](https://img.shields.io/github/languages/top/michaelJustin/daraja-framework)
![Static Badge](https://img.shields.io/badge/OS-Windows-blue)
![GitHub Release](https://img.shields.io/github/v/release/michaelJustin/daraja-framework)
![GitHub Release Date - Published_At](https://img.shields.io/github/release-date/michaelJustin/daraja-framework)
![GitHub last commit](https://img.shields.io/github/last-commit/michaelJustin/daraja-framework)
[![Doxygen Docs](https://github.com/michaelJustin/daraja-framework/actions/workflows/doxygen.yml/badge.svg)](https://github.com/michaelJustin/daraja-framework/actions/workflows/doxygen.yml)
[![pages-build-deployment](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment)
![GitHub Repo stars](https://img.shields.io/github/stars/michaelJustin/daraja-framework?style=flat)

![](https://www.habarisoft.com/images/daraja_logo_landscape_2016_3.png)

In short, Daraja enables Object Pascal developers to write *well-structured HTTP server applications*.

## Example

A simple "Hello, World!" application.

### Resource definition
A Daraja Web Component defines the request handling and response building, but it does not specifiy the actual location (HTTP address) of a resource.

The web component in this example handles HTTP GET requests by overriding the OnGet method. The method sets the response content text and content type.

```Pascal
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

```Pascal
procedure Demo;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create(80);
  try
    Context := TdjWebAppContext.Create('demo');
    Context.Add(THelloWorldResource, '/hello.txt');
    Server.Add(Context);
    Server.Start;
    WriteLn('Server is running, please open http://127.0.0.1/demo/hello.txt');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
  end;
end;
```

Tested with curl:

```Console
curl -i http://127.0.0.1/demo/hello.txt
HTTP/1.1 200 OK
Connection: keep-alive
Content-Type: text/plain; charset=ISO-8859-1
Content-Length: 13
Date: Wed, 22 Jan 2025 19:07:14 GMT

Hello, World!
```

## Documentation

### API docs

https://michaeljustin.github.io/daraja-framework/

### Getting started with Daraja

https://www.habarisoft.com/daraja_framework/3.0.3/docs/DarajaFrameworkGettingStarted.pdf

## Project home page

https://www.habarisoft.com/daraja_framework.html

## Credits

This software uses the following open source packages:

- [Indy - Internet Direct](https://github.com/IndySockets/Indy)

For example code, unit testing, and documentation, it uses the following open source packages:

- [JsonDataObjects](https://github.com/ahausladen/JsonDataObjects) for example code
- [Log4D](http://sourceforge.net/projects/log4d/) for logging
- [DUnit](https://dunit.sourceforge.net/) and [FPCUnit](https://wiki.freepascal.org/fpcunit) for unit testing
- [Doxygen](https://www.doxygen.nl/) Doxygen is a widely-used documentation generator tool in software development.

## Origins

> "Daraja" means "bridge" in Swahili. The Daraja Framework serves as a bridge between incoming HTTP requests and the Object Pascal code that handles them, enabling seamless integration between web traffic and application logic.
> — ChatGPT, OpenAI (May 2025)

## Licensing

Daraja HTTP Framework is dual licensed under the GNU Affero General Public License and a commercial license. The GNU Affero General Public License is a free, copyleft license for software and other kinds of works, specifically designed to ensure cooperation with the community in the case of network server software.

## AGPL Licensing FAQ

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
No.
</details>

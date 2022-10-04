![](https://www.habarisoft.com/images/daraja_logo_landscape_2016_2.png)

# Daraja HTTP Framework

Daraja is a flexible HTTP server framework for Object Pascal, based on the stand-alone HTTP server in the free open source library Internet Direct (Indy).

Daraja provides the core foundation for serving HTTP resources of all content-types such as HTML pages, images, scripts, web service responses etc. by mapping resource paths to your own code. Your code then can create the response content, or let the framework serve a static file. 

## Optional Extensions

### daraja-restful

A RESTful framework extension, version 1.0 is compatible with Delphi 2009 and newer. Version 2.0 introduces support for Free Pascal, using a slightly different configuration of RESTful resource handlers.

https://github.com/michaelJustin/daraja-restful

### slf4p

A simple logging facade with support for LazLogger, Log4D, and other logging frameworks.

https://github.com/michaelJustin/slf4p

You can find this project at https://github.com/michaelJustin/daraja-framework

## Documentation

### API generated with doxygen

http://michaeljustin.github.io/daraja-framework/


### Getting Started PDF

A Getting Started document (PDF) is available at https://www.habarisoft.com/daraja_framework/2.0/docs/DarajaFrameworkGettingStarted.pdf

### Project home page

Visit https://www.habarisoft.com/daraja_framework.html for more information.

## IDE configuration

### Required paths

* The project search path must include the Indy and Daraja source directories.

Example:

`<daraja-home>\source;<indy-home>\Lib\Core\;<indy-home>\Lib\Protocols\;<indy-home>\Lib\System\`

* The project search path for Include files must include the Indy Core path.

Example:

`<indy-home>\Lib\Core\`

### Optional source

Some useful (but optional) units are located in the `optional` subfolder. Include it when needed:

`<daraja-home>\source;<daraja-home>\source\optional;<indy-home>\Lib\Core\;<indy-home>\Lib\Protocols\;<indy-home>\Lib\System\`

## Usage example: contexts

In the Daraja Framework, contexts are used for the high-level separation of HTTP resources, depending on their first path segment. Here is an example which uses two contexts, 'context1' and 'context2': 

    http://example.com/context1/index.html
    http://example.com/context2/other.html

This example uses 'news', 'files' and 'admin' contexts:

    http://example.com/news/index.html
    http://example.com/files/doc1.pdf
    http://example.com/admin/login.html

### Code
In the Daraja Framework, creating a context only requires the context name as the parameter of the TdjWebAppContext constructor: 

```pascal
  Server := TdjServer.Create;
  try
    Context1 := TdjWebAppContext.Create('context1');
    Server.AddContext(Context1); 
    Context2 := TdjWebAppContext.Create('context2');
    Server.AddContext(Context2); 
    Server.Start;
    ... 
```        

## Dynamic resource handlers

Contexts need resource handlers to process requests. A **resource handler** is responsible for the generation of the HTTP response matching a specific client request.

However, the routing between the actual HTTP request and the resource handler is performed via 'mapping' rules.

For example, a resource handler which returns a HTML document could be mapped to the `/context1/index.html` resource path with this **absolute path** resource handler mapping:

```pascal
  Context1.Add(TIndexPageResource, '/index.html');
```

Alternatively, a more general **suffix mapping** resource handler may be used, which should handle requests to any resources with the extension `*.html`:

```pascal
  Context1.Add(TCatchAllHtmlResource, '*.html');
```

This resource handler will be invoked for all requests for *.html resources - independent of their actual document name, and also for resources in sub-paths like `/context1/this/works/with_any_page.html`. (But note: the resource handler will _not_ receive requests for other context, such as `/context2/index.html`!)

![](https://www.habarisoft.com/images/daraja_logo_landscape_2016_2.png)

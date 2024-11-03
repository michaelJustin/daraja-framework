[![pages-build-deployment](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment)
[![AGPL License](https://img.shields.io/badge/license-AGPL-blue.svg)](http://www.gnu.org/licenses/agpl-3.0) ![GitHub Release Date - Published_At](https://img.shields.io/github/release-date/michaelJustin/daraja-framework)


![](https://www.habarisoft.com/images/daraja_logo_landscape_2016_2.png)




# Daraja HTTP Server Framework

Daraja is a flexible HTTP server framework for Object Pascal, based on the non-visual HTTP server component in the free open source library Internet Direct (Indy).

Daraja provides the core foundation for serving HTTP resources of all content-types such as HTML pages, images, scripts, web service responses etc. by mapping resource paths to your own code. Your code then can create the response content, or let the framework serve a static file.

## Documentation

### API generated with doxygen

http://michaeljustin.github.io/daraja-framework/


### Getting Started PDF

A Getting Started document (PDF) is available at https://www.habarisoft.com/daraja_framework/3.0-MS1/docs/DarajaFrameworkGettingStarted.pdf

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

## Dynamic resource handlers

Resource handlers are the central framework parts which process requests. A **resource handler** is responsible for the generation of the HTTP response matching a specific client request.
The routing between the actual HTTP request and the resource handler is performed via 'mapping' rules.
For example, a resource handler could be mapped to the `/context1/index.html` resource path with this **absolute path** resource handler mapping:

```pascal
  Context1.Add(TIndexPageResource, '/index.html');
```

Alternatively, a more general **suffix mapping** resource handler may be used, which should handle requests to any resources with the extension `*.html`:

```pascal
  Context1.Add(TCatchAllHtmlResource, '*.html');
```

This resource handler will be invoked for all requests for *.html resources - independent of their actual document name, and also for resources in sub-paths like `/context1/this/works/with_any_page.html`. But note: the resource handler will _not_ receive requests for other context, such as `/context2/index.html`! (more about contexts can be found in the full documentation)

## Resource filters

In addition to resource handlers, the application may also include resource filters. With filters, HTTP traffic can be modified in many ways, such as:
* Detect missing authorization, and forward to a login page.
* Set or remove HTTP headers depending on conditions.
* Pre- or postprocess the request body content, to remove or insert data.

![](https://www.habarisoft.com/images/daraja_logo_landscape_2016_2.png)



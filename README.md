[![pages-build-deployment](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/michaelJustin/daraja-framework/actions/workflows/pages/pages-build-deployment)
[![AGPL License](https://img.shields.io/badge/license-AGPL-blue.svg)](http://www.gnu.org/licenses/agpl-3.0) ![GitHub Release Date - Published_At](https://img.shields.io/github/release-date/michaelJustin/daraja-framework)

![](https://www.habarisoft.com/images/daraja_logo_landscape_2016_2.png)

# Daraja HTTP Server Framework

Daraja is a flexible HTTP server framework for Object Pascal, based on the non-visual HTTP server component in the free open source library Internet Direct (Indy).

Daraja provides the core foundation for serving HTTP resources of all content-types such as HTML pages, images, scripts, web service responses etc. by mapping resource paths to your own code. Your code then can create the response content, or let the framework serve a static file.

Included example projects show its usefulness for advanced uses case, such as OAuth 2.0 and OpenID Connect for Microsoft Entra ID and Google Identity platform.
- https://github.com/michaelJustin/daraja-framework/tree/master/demo/16_entra_refresh_token
- https://github.com/michaelJustin/daraja-framework/tree/master/demo/17_google_refresh_token

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

* Note: the project search path for Include files must include the Indy Core path.


## Resource handlers (TdjWebComponent)

A **resource handler** is responsible for the generation of the HTTP response matching a specific client request. The routing between the actual HTTP request and the resource handler is defined by 'mapping' rules.
For example, a resource handler could be mapped to `/context1/index.html` with an **absolute path** mapping:

```pascal
  Context1.Add(TIndexPageResource, '/index.html');
```

There are two other supported mapping types: **prefix mapping** ('/sub1/*', '/sub2/*' ...) and **suffix mapping** ('*.html', '*.pdf' ...). All mappings are checked in a defined order to find the responsible resource handler.

## Resource filters (TdjWebFilter)

In addition to resource handlers, the application may also include resource filters. With filters, HTTP traffic can be modified in many ways, such as:
* Detect missing authorization, and forward to a login page.
* Set or remove HTTP headers depending on conditions.
* Pre- or postprocess the request body content, to remove or insert data.

All mapping types may also be used for resource filters. 

![](https://www.habarisoft.com/images/daraja_logo_landscape_2016_2.png)



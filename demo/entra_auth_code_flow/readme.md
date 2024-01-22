# OAuth 2.0 authorization code flow example for Daraja HTTP Framework

This project implements the ["Microsoft identity platform and OAuth 2.0 authorization code flow"](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-auth-code-flow).

This application launches a local web server, requests an authorization code from Microsoft Entra, and uses it to request an OAuth bearer token. The access token then is used for two invocations of the Graph API to retrieve user profile data, and to send an email.

The content and recipient of the email is configured in a JSON document in unit [RootResource](RootResource.pas). The same unit also renders the index.html page after successful authentication.

## Requirements
* Daraja HTTP Framework (https://github.com/michaelJustin/daraja-framework)
* Indy (https://github.com/IndySockets)
* OpenSSL DLLs for Indy (https://github.com/IndySockets/OpenSSL-Binaries)
* JsonDataObjects (https://github.com/ahausladen/JsonDataObjects)

Note: the source code contains the configuration for an existing Microsoft Entra App registration. 
You may configure it to use a different App, by modifying the constants in unit [MainUnit](MainUnit.pas).

## Security considerations
The example code uses response_mode=form_post to receive the access token. Unlike with response_mode=fragment (or query), the browser does not receive the access_token parameter in the redirect request URI. Therefore, the access_token is not accessible within the browser's memory.


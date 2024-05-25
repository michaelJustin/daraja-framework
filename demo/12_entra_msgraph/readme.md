# Read user profile data and send mail using Microsoft Graph API

This application launches a local web server and requests an access token from Microsoft Entra. The access token then is used to retrieve user profile data and to send an email. The content and recipient of the email is configured in a JSON document in unit [RootResource](RootResource.pas). The same unit also renders the index.html page after successful authentication.

## Requirements
* Daraja HTTP Framework source and source/optional units.
* Indy 10.6.2 (https://github.com/IndySockets)
* OpenSSL DLLs for Indy (https://github.com/IndySockets/OpenSSL-Binaries)
* Delphi 2009+ or Lazarus / FPC 3.2

Note: the source code contains the configuration for an existing Microsoft Entra App registration. 
You may configure it to use a different App, by modifying the constants in unit [MainUnit](MainUnit.pas).


## Security considerations
The example code uses response_mode=form_post to receive the access token. Unlike with response_mode=fragment (or query), the browser does not receive the access_token parameter in the redirect request URI. Therefore, the access_token is not accessible within the browser's memory. ("Implicit Flow with Form Post")


# Demonstration of refresh token usage with Microsoft Entra ID / OpenID Connect

This application launches a local web server and requests an access token from Microsoft Entra ID (formerly known as Azure Active Directory) and a refresh token. The access token then is shown in a web page together with a button, which allows to request a new access token. 

## Requirements
* Daraja HTTP Server Framework source and source/optional units.
* Indy 10.6.3 (https://github.com/IndySockets)
* OpenSSL DLLs for Indy (https://github.com/IndySockets/OpenSSL-Binaries)
* JsonDataObjects (https://github.com/ahausladen/JsonDataObjects)
* Delphi 2009+ or Lazarus / FPC 3.2

Note: the example code contains the configuration for an existing Microsoft Entra App registration. 
You may configure it to use a different App registration, by modifying the constants in unit [MainUnit](MainUnit.pas).
Please note that the App registration must be configured as "Mobile and desktop application" (Public Client).   

## Security considerations
* The example code uses response_mode=form_post to receive the access token. Unlike with response_mode=fragment (or query), the browser does not receive the access_token parameter in the redirect request URI. Therefore, the access_token is not accessible within the browser's memory. ("Implicit Flow with Form Post")
* The example code uses PKCE, which stands for "Proof of Key Code Exchange", an extension of the OAuth 2.0 protocol that helps prevent code interception attacks.



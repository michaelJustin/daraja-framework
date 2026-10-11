# OAuth 2.0 authorization code flow example for Daraja HTTP Framework

This project implements the ["Microsoft identity platform and OAuth 2.0 authorization code flow"](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-auth-code-flow).

This application launches a local web server, requests an authorization code from Microsoft Entra, and uses it to request an OAuth bearer token. The access token then is used for two invocations of the Graph API to retrieve user profile data, and to send an email.

The content and recipient of the email is configured in a JSON document in unit [RootResource](RootResource.pas). The same unit also renders the index.html page after successful authentication.

## Requirements
* Daraja HTTP Framework (https://github.com/michaelJustin/daraja-framework)
* Indy (https://github.com/IndySockets)
* OpenSSL DLLs for Indy (https://github.com/IndySockets/OpenSSL-Binaries)
* (Delphi only) JsonDataObjects (https://github.com/ahausladen/JsonDataObjects)

Note: the source code contains the configuration for an existing Microsoft Entra App registration. 
You may configure it to use a different App, by modifying the constants in unit [MainUnit](MainUnit.pas).

## Security considerations
The example code uses response_mode=form_post to receive the access token. Unlike with response_mode=fragment (or query), the browser does not receive the access_token parameter in the redirect request URI. Therefore, the access_token is not accessible within the browser's memory.

## Configure your own Microsoft Entra app registration

1. Sign in to the [Microsoft Entra admin center](https://entra.microsoft.com) and go to
   **Entra ID > App registrations > New registration**.
2. Enter a name (for example `Daraja OAuth demo`). The demo uses the `consumers`
   endpoints, so choose **Personal Microsoft accounts only** as the supported account type.
3. Under **Redirect URI**, select the platform **Mobile and desktop applications**
   (public client) and enter `http://127.0.0.1/auth-response`.
   This must match `REDIRECT_URI` in `MainUnit.pas`. Click **Register**.
4. On the **Overview** page, copy the **Application (client) ID**.
5. Open **Authentication** and make sure **Allow public client flows** is enabled.
   No client secret is needed: public clients can't keep secrets, so the app does not send one.
6. Go to **API permissions > Add a permission > Microsoft Graph > Delegated permissions**
   and add `User.Read` and `Mail.Send`.
7. In `MainUnit.pas`, replace `CLIENT_ID` with your Application (client) ID.
8. Run the demo, open the local URL in a browser, sign in with a personal Microsoft
   account and accept the consent prompt.


File upload demo.

This demo presents a HTML upload form, which allows to send one or multiple files to the server.
The server side code stores the received files in the current directory.

Please note: the example uses a first draft of a subclass of the Indy component TIdMessageDecoderMIME, which assumes that the uploaded files are always unendcoded. Therefore, the current version of the demo may store corrupt files, if the client has sent encoded files.

Also, this demo has not been tested with other form input fields (text, checkboxes etc.).


File upload demo.

This demo presents a HTML upload form, which allows to send one or multiple files to the server.
The server side code stores the received files in the current directory.

Please note: the example uses the Indy component TIdMessageDecoderMIME, which currently decodes the multipart/form-data according to the content-transfer-encoding. If this header is not present, the Indy component uses several fall-back strategies.

Therefore, the current version of the demo may store corrupt files.


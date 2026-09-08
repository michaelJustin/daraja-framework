# Server-sent events

`TPingResource` streams a `text/event-stream` response, pushing a `ping` event
with a timestamp and the client's peer address; `THomeResource` serves a page
that subscribes with an `EventSource`.

Run and open <http://127.0.0.1/>. (Lazarus / FPC project only.)

# Custom error handler

Shows `TdjContextHandler.ErrorHandler`: a per-context extension point invoked
in place of the framework's default, generic 500 response when a Web
Component or Web Filter registered in that context raises an unhandled
exception (see [issue #528](https://github.com/michaelJustin/daraja-framework/issues/528)).

Two contexts register the same `TFailingResource` (its `OnGet` always
raises), differing only in whether `ErrorHandler` is set:

- `/plain/fail` has no `ErrorHandler` — the framework's default, generic
  500 page (introduced in #520/#523; discloses nothing about the exception).
- `/custom/fail` has `TCustomErrorPage` (a minimal custom `IHandler`, built
  on `TdjAbstractHandler`) set as its `ErrorHandler` — it reads
  `Context.LastErrorExceptionClass`/`.LastErrorExceptionMessage` (populated
  by the framework right before the handler is invoked) to render its own
  page.

Run and open <http://127.0.0.1:8080/plain/fail> and
<http://127.0.0.1:8080/custom/fail> to compare the two.

## Try it

```
curl http://127.0.0.1:8080/plain/fail
curl http://127.0.0.1:8080/custom/fail
```

## Note

`ErrorHandler` is a single handler per context — it is not a Servlet-style
`<error-page>` mapping table keyed by status code or exception type. If a
custom `ErrorHandler` itself raises, the framework falls back to its default
generic response rather than failing the request.

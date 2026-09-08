# Form-based authentication

Session-backed form login. `TFormAuthFilter` guards `/admin` and redirects
anonymous users to `/login`; `TLoginResource` / `TLogoutResource` manage the
`auth:username` session value, and `TdjNCSALogFilter` logs all requests.

Run and open <http://127.0.0.1/index.html>.

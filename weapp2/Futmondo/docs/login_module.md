# Login and account isolation

`login_UI(id)` returns an empty email/password form, login button, logout button and connection status. `login_Server(id,user=NULL,password=NULL)` returns a reactive authentication value: NULL when logged out, otherwise a named token/user-ID/login-name vector. UI inputs are not populated from server credentials. The submitted password input is cleared after authentication and never logged. Every failed attempt renders a generic accessible error below the form, including a first failure when the token was already NULL; transport details are not exposed.

`login(user_name=NULL,password=NULL)` calls the official authentication endpoint only with explicitly supplied values. Authenticated tokens are used in server-side requests; no token is printed or rendered. Optional unattended sessions are stored encrypted only after the user connects the worker, as documented in [automation](automation.md).

Clicking Login immediately changes its label to “Logging in…” and displays an accessible “Connecting to Futmondo…” status in the browser before the synchronous authentication call completes. Login and Logout remain disabled while authentication is pending, and repeated clicks or Enter presses cannot queue duplicate submissions. The prior result is hidden during that attempt. A namespaced `login_state` custom message with payload `list(busy = FALSE)` restores the form on success, invalid credentials, transport failure, or missing fields. Empty credentials produce a local validation message without calling the authentication endpoint; each attempt clears the password input.

Pressing Enter while either credential input is focused triggers the same namespaced Login button click as a mouse or touch click. The handler prevents a browser form submission and ignores IME composition events; it does not create another authentication route.

Usage: include `login_UI("login")` in the UI and call `auth <- login_Server("login")` in the server; read `auth()` reactively for the authentication value. Module IDs also namespace the browser loading indicator and completion message, so independent module instances do not share login state.

Run `Rscript test/test_login_feedback.R` for focused missing-field, error, invalid-response, success, completion-message and password-clearing checks. Run `Rscript test/test_login_feedback.R --browser` to additionally exercise the rendered module in Chromium, including immediate feedback, duplicate suppression, completion reset, Enter submission and IME handling. Browser checks use the existing Playwright test tooling location, overridable with `FUTMONDO_TEST_NODE_MODULES`; they do not authenticate or contact Futmondo.

On login, root server loads the account's memberships and offers a league selector. Switching a league binds roster, financial rules, rivals, forecasts and action context to its championship/team IDs. Logout clears the authentication value and account cache. Selected-player confirmations cannot survive account/league/team changes.

Every administrative handler checks `is_authorized_admin()` against authenticated identity and the configured server administrator. Menu visibility alone is not authorization. See [multi-league design](multi_league.md) and the offline lifecycle tests.

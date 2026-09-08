# Login and account isolation

`login_UI(id)` returns an empty email/password form, login button, logout button and connection status. `login_Server(id,user=NULL,password=NULL)` returns a reactive authentication value: NULL when logged out, otherwise a named token/user-ID/login-name vector. UI inputs are not populated from server credentials. The submitted password input is cleared after authentication and never logged. Every failed attempt renders a generic accessible error below the form, including a first failure when the token was already NULL; transport details are not exposed.

`login(user_name=NULL,password=NULL)` calls the official authentication endpoint only with explicitly supplied values. Authenticated tokens are used in server-side requests; no token is printed or rendered. Optional unattended sessions are stored encrypted only after the user connects the worker, as documented in [automation](automation.md).

Pressing Enter while the password input is focused triggers the same namespaced Login button click as a mouse or touch click. The handler prevents a browser form submission and ignores IME composition events; it does not create another authentication route.

On login, root server loads the account's memberships and offers a league selector. Switching a league binds roster, financial rules, rivals, forecasts and action context to its championship/team IDs. Logout clears the authentication value and account cache. Selected-player confirmations cannot survive account/league/team changes.

Every administrative handler checks `is_authorized_admin()` against authenticated identity and the configured server administrator. Menu visibility alone is not authorization. See [multi-league design](multi_league.md) and the offline lifecycle tests.

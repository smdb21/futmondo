# Login and session context

`login(user_name=NULL, password=NULL)` uses credentials explicitly entered by the user. Tokens stay in the R server session and are not printed or rendered.

The server lists account memberships and binds each selected league to its championship/team IDs. Logout clears authentication, private cached state, and session-owned pending work. Account or league changes invalidate selected-player action confirmations. No login credentials or encrypted sessions are retained for unattended work.

Focused coverage belongs in the offline login tests and application lifecycle harness.

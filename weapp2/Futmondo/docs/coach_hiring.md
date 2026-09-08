# Automatic coach preference

The Your Team page includes **Automatically hire the coach each round**, selected by default. This is a preference pending integration: the available captures do not contain a verified hiring endpoint. The UI explicitly directs users to hire in Futmondo while integration is unavailable. Checking the box saves the preference when storage is available; it never sends a hiring request, creates an automation job or claims that a coach has been hired.

## UI and server contracts

Both functions are defined in `Modules/Players_in_Teams_Module.R` and mounted by the existing Your Team UI/server.

- `squad_coach_UI(id)` returns responsive Shiny tags containing a heading, dynamic checkbox and status/help text. `id` is the module namespace.
- `squad_coach_Server(id, login_token, championship_id, user_team_id)` accepts reactive authentication and league/team IDs. It returns a reactive list with `account_id`, `championship_id`, `user_team_id`, `enabled`, `execution_available=FALSE` and `reason="hiring_endpoint_unverified"`; unauthenticated or incomplete context returns NULL.

Each account/league/team combination has a distinct checkbox input identity. Choices persist in `league_preferences` after successful acknowledgement and restore across sessions. Without configured storage, the UI explicitly reports session-only state; new contexts default to checked. Delayed events from another league cannot change the current preference. This is not an executable worker policy. See [persistence contracts](insights_runtime.md).

```r
# Inside a parent module:
squad_coach_UI(ns("coach"))
# Inside its server, using reactive authentication and membership values:
coach_preference <- squad_coach_Server("coach", login_token, championship_id, user_team_id)
# coach_preference()$enabled is a preference, not permission to call an unknown endpoint.
```

## Evidence and remaining capture

The HAR audit covered the supplied captures and all recorded API versions:

- `POST /2/userteam/getdtconfig` reads `{championshipId,userteamId}` and returns `answer={date,pro}`. No mutation or active coach contract is established by these fields.
- `/1/userteam/information` exposes `configuration.enableCoach`, `staff.coach` and `pro`. The meaning and per-round lifecycle of `staff.coach` require before/after hiring evidence; configured bench availability is not proof of a hired coach.
- No successful coach-hiring request/acknowledgement is recorded.

[Official automatic-coach rules](https://help.futmondo.com/article/159-entrenador-automatico) say the contract lasts one round, must be obtained before kickoff, and costs 500 mondos unless the championship is PRO. A successful future integration must verify the charge, round identity and existing contract before attempting a hire.

Next round, capture opening Lineup, clicking the coach-hiring button and confirming, and refreshing afterward. Retain the successful request URL/method/body and response, plus the information/lineup responses before and after. Keep the HAR local because it may include account credentials. An already-hired message with no network request cannot establish the mutation contract.

## Verification

`Rscript test/test_coach_preference.R`: **8 passed, 0 failed**, covering Your Team placement, checked default, explicit pending/off status, account/league/team isolation, delayed input, session fallback, acknowledged restoration across sessions, and zero HTTP requests.

`Rscript test/test_ui_reliability.R`: **14 checks passed**. The persistence follow-up also passed the full offline application lifecycle. Database writes use the existing bounded, defensive connector; failed acknowledgements retain an explicit session-only status.

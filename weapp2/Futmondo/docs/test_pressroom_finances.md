# Pressroom Finances Test Script Documentation

This document describes the standalone test script `test/test_team_pressroom_finances.R`, which validates a user team's pressroom transaction history against the API-reported budget.

---

## 1. Overview

The script performs a full end-to-end validation of a single user team's finances by:

1. Authenticating with the Futmondo API via email/password login.
2. Retrieving the active championship and the user's team ID.
3. Fetching **all** pressroom transactions using cursor-based pagination.
4. Filtering transactions that involve the user's own team (as buyer or seller).
5. Computing the remaining budget from first principles.
6. Cross-checking the computed budget against the API-reported value from `/1/userteam/information`.

The initial budget constant is `300,000,000` EUR (integer format). A live run against a real account produced an API-reported budget of `14,323,915` EUR, confirming the formula holds against real-world data.

---

## 2. API Endpoints Queried

The script interacts with four Futmondo API endpoints. All requests use `POST` with a JSON body and the header `Content-Type: application/json; charset=utf-8`.

### 2.1 `POST /5/login/with_mail`

Full URL: `https://api.futmondo.com/5/login/with_mail`

**Purpose**: Authenticate the user and obtain a session token.

**Request Payload**:
```json
{
  "header": {
    "token": "",
    "userid": ""
  },
  "query": {
    "email": "<user_name>",
    "password": "<password>"
  },
  "answer": {}
}
```

**Response**: Returns a named character vector (via the `login()` helper) containing at minimum:
- `token`: String session token for subsequent authenticated calls.
- `userid`: String user identifier.

### 2.2 `POST /2/user/activechampionships`

Full URL: `https://api.futmondo.com/2/user/activechampionships`

**Purpose**: Retrieve the list of active championships for the logged-in user, including the user's team ID within each championship.

**Request Payload**:
```json
{
  "header": {
    "token": "<token>",
    "userid": "<userid>"
  },
  "query": {},
  "answer": {}
}
```

**Response**: Returns an unlisted named character vector. Relevant fields:
- `id`: The championship ID (top-level field).
- `userteam.id` (or `userTeamId` variant): The user's team ID within the championship.
- `userteam.name`: The user's team name.

### 2.3 `POST /1/locker/pressroom`

Full URL: `https://api.futmondo.com/1/locker/pressroom`

**Purpose**: Fetch the complete transfer feed (all player transactions) for a given championship. Supports cursor-based pagination via the `from` parameter.

**Request Payload**:
```json
{
  "header": {
    "token": "<token>",
    "userid": "<userid>"
  },
  "query": {
    "championshipId": "<championship_id>",
    "from": "<cursor>"
  },
  "answer": {}
}
```

- `from`: On the first request, set to an empty string (`""`). On subsequent requests, set to the `_id` of the last item returned on the previous page.

**Response**:
```json
{
  "answer": {
    "news": [
      {
        "_id": "<transaction_id>",
        "created": "<ISO 8601 timestamp>",
        "_player": { "_id": "<player_id>", "name": "<player_name>" },
        "_buyer":  { "_id": "<buyer_team_id>", "name": "<buyer_team_name>" },
        "_seller": { "_id": "<seller_team_id>", "name": "<seller_team_name>" },
        "price": "<integer price in EUR>"
      },
      ...
    ]
  }
}
```

Each element in the `news` array represents a single pressroom transaction. The API returns results in newest-first order.

### 2.4 `POST /1/userteam/information`

Full URL: `https://api.futmondo.com/1/userteam/information`

**Purpose**: Retrieve detailed information about a specific user team, including the API-reported budget.

**Request Payload**:
```json
{
  "header": {
    "token": "<token>",
    "userid": "<userid>"
  },
  "query": {
    "championshipId": "<championship_id>",
    "userteamId": "<user_team_id>",
    "type": "market"
  },
  "answer": {}
}
```

**Response**: Returns the `answer` object (via the `get_user_team_info()` helper). Relevant fields:
- `budget`: Numeric value representing the current API-reported budget in EUR.
- `teamValue`: Numeric squad valuation.
- `position`: Integer league standing position.

---

## 3. Cursor-Based Pagination Mechanism

The pressroom feed (`/1/locker/pressroom`) uses a cursor-based pagination scheme via the `from` query parameter.

### How It Works

1. **Initial Request**: Set `from` to an empty string (`""`). The API returns the first page of results (newest first).

2. **Extract Cursor**: Take the `_id` field of the **last item** in the returned `news` array. This `_id` becomes the cursor for the next page.

3. **Subsequent Requests**: Set `from` to the extracted cursor value. The API returns the next page of results, starting from items that follow the cursor.

4. **Termination**: The loop stops when:
   - The `news` array is empty (all pages fetched).
   - The last item's `_id` equals the current cursor (no new data).
   - The maximum page limit (50 pages) is reached.

### Pseudocode

```
cursor <- ""
while (page_count < 50) {
  response <- POST(PRESSROOM_URL, query = { championshipId, from = cursor })
  news <- response$answer$news
  if (is_empty(news)) break

  append(news, all_transactions)

  last_id <- news[last_item]$_id
  if (last_id == cursor) break
  cursor <- last_id
}
```

### Usage as Feed Cursor

The `_id` of the **most recent transaction overall** (i.e., the first item in the API response, since results are newest-first) serves as the feed cursor for future incremental polling. To fetch only new transactions since a previous check, set `from` to that `_id`.

---

## 4. Money Left Formula and Validation

### 4.1 Formula

The remaining budget (Money Left) is computed from first principles:

```
Money Left = Initial Budget - Total Purchases + Total Sales
```

Where:
- **Initial Budget**: `300,000,000` EUR (hardcoded constant `INITIAL_BUDGET`).
- **Total Purchases**: Sum of `price` for all transactions where the user's team is the buyer (`buyer_team_id == user_team_id`).
- **Total Sales**: Sum of `price` for all transactions where the user's team is the seller (`seller_team_id == user_team_id`).

### 4.2 Validation Against API-Reported Budget

After computing the Money Left from transaction history, the script queries `/1/userteam/information` to obtain the API-reported `budget` value. It then compares the two:

- **MATCH**: If `abs(calculated - api_budget) == 0`, the script reports a match.
- **MISMATCH**: If they differ, the script reports the absolute difference in EUR.

### 4.3 Live Validation Result

A live run against a real account confirmed:
- **API-reported budget**: `14,323,915` EUR
- **Calculated budget** (from full pressroom history): matches the API value exactly (difference of `0` EUR).

This confirms that the pressroom feed contains a complete and accurate transaction history, and the formula correctly derives the remaining budget.

---

## 5. Functional Parameters

### Environment Variables

The script reads credentials from the `.Renviron` file (or system environment):

| Variable   | Type   | Description                          |
|------------|--------|--------------------------------------|
| `user_name`| String | Futmondo account email address       |
| `password` | String | Futmondo account password            |

If either variable is empty, the script terminates with an error.

### Internal Constants

| Constant         | Value        | Description                          |
|------------------|--------------|--------------------------------------|
| `INITIAL_BUDGET` | `300000000L` | Starting budget in EUR (integer)     |
| `max_pages`      | `50`         | Maximum pagination pages to fetch    |

### Helper Functions

| Function     | Signature                              | Description                                       |
|--------------|----------------------------------------|---------------------------------------------------|
| `fmt_num()`  | `fmt_num(x)`                           | Formats a numeric value with dot thousands separator and comma decimal separator. Returns `"N/A"` for NA. |
| `get_val()`  | `get_val(obj, key, default = "")`      | Safely extracts a value from a list by key, returning `default` if missing. |
| `parse_item()`| `parse_item(item)`                    | Parses a single pressroom transaction JSON object into a one-row data frame. |

### Parse Item Return Schema

`parse_item()` returns a one-row data frame with these columns:

| Column           | Type    | Description                           |
|------------------|---------|---------------------------------------|
| `id`             | Character | Transaction `_id`                   |
| `created`        | Character | ISO 8601 timestamp of transaction   |
| `player_id`      | Character | Player `_id`                        |
| `player_name`    | Character | Player display name                 |
| `buyer_team_id`  | Character | Buyer team `_id`                    |
| `buyer_team_name`| Character | Buyer team display name             |
| `seller_team_id` | Character | Seller team `_id`                   |
| `seller_team_name`| Character | Seller team display name            |
| `price`          | Numeric   | Transaction price in EUR            |

---

## 6. Execution Steps

The script executes in nine sequential steps:

| Step | Description                                                    |
|------|----------------------------------------------------------------|
| 1/9  | Read `user_name` and `password` from `.Renviron`.               |
| 2/9  | Call `login()` via `/5/login/with_mail` to obtain token and userid. |
| 3/9  | Call `get_championships()` via `/2/user/activechampionships` to extract championship ID and user team ID. Falls back to `get_teams()` if team ID is not in the championships response. |
| 4/9  | Fetch all pressroom transactions via `/1/locker/pressroom` using cursor pagination. |
| 5/9  | Parse raw JSON transactions into a data frame via `parse_item()`. |
| 6/9  | Filter to transactions where the user's team is buyer or seller. |
| 7/9  | Identify today's transactions, the most recent overall, and the most recent for the user's team. |
| 8/9  | Calculate Money Left and validate against the API-reported budget from `/1/userteam/information`. |
| 9/9  | Print a formatted summary of all findings.                     |

---

## 7. CLI Usage Examples

### 7.1 Prerequisites

Ensure the `.Renviron` file (located at the project root or in the user's home directory) contains:

```
user_name=your_email@example.com
password=your_password
```

### 7.2 Running the Test

From the project root directory (`/home/rstudio/workspace/futmondo/weapp2/Futmondo/`):

```bash
# Run from the test/ directory (the script sources futmondo_functions.R relative to CWD)
Rscript test/test_team_pressroom_finances.R
```

Or explicitly set the working directory:

```bash
Rscript --vanilla test/test_team_pressroom_finances.R
```

### 7.3 Running with Explicit Environment Variables

```bash
user_name=your_email@example.com password=your_password Rscript test/test_team_pressroom_finances.R
```

---

## 8. Output Structure

The script produces structured console output organized into nine sections matching the execution steps, followed by a summary block.

### 8.1 Step Output

Each step prints a header line (e.g., `[4/9] Fetching all pressroom transactions (cursor pagination)...`) followed by indented details. During pagination, each page emits:

```
    Page N (cursor: <cursor_value>)...
```

### 8.2 Summary Block

The final summary is printed between separator lines:

```
=================================================================
           PRESSROOM FINANCES SUMMARY
=================================================================
  User:              <user_name>
  User Team ID:      <user_team_id>
  Championship ID:   <championship_id>
  Today's Date:      <YYYY-MM-DD>
-----------------------------------------------------------------
  Initial Budget:    300.000.000 EUR
  Total Purchases:   <formatted_value> EUR
  Total Sales:       <formatted_value> EUR
  Calculated Left:   <formatted_value> EUR
  API Budget:        <formatted_value> EUR
-----------------------------------------------------------------
  Today's Txns:      <count>
  Latest Txn ID:     <transaction_id>
=================================================================
```

### 8.3 Most Recent Transaction Overall (Feed Cursor)

```
--- Most Recent Transaction Overall (Feed Cursor) ---
  _id:               <transaction_id>
  created:           <ISO 8601 timestamp>
  player:            <player_name>
  buyer (team):      <team_name> (id=<team_id>)
  seller (team):     <team_name> (id=<team_id>)
  price:             <formatted_value> EUR
  >> Use this _id as `from` cursor for next poll: <transaction_id>
```

### 8.4 Most Recent Transaction for User's Team

```
--- Most Recent Transaction for User's Team ---
  _id:               <transaction_id>
  created:           <ISO 8601 timestamp>
  direction:         BUY or SELL
  player:            <player_name>
  counterparty:      <team_name> (id=<team_id>)
  price:             <formatted_value> EUR
```

### 8.5 Today's Transactions Detail

Each transaction of the current day is printed in tabular form:

```
--- Today's Transactions Detail (User's Team) ---
  [YYYY-MM-DD HH:MM:SS] BUY  Player Name             | Counterparty Team | 1.234.567 EUR | ID: <txn_id>
```

If no transactions occurred today, the output is `(no transactions today)`.

---

## 9. Dependencies

The script requires the following R packages (loaded via `suppressPackageStartupMessages`):

| Package    | Purpose                                    |
|------------|--------------------------------------------|
| `httr`     | HTTP client for API requests (POST)        |
| `jsonlite` | JSON serialization (`toJSON`) and parsing  |
| `dplyr`    | Data manipulation (loaded but not directly used in this script; available if extended) |

The script also sources `futmondo_functions.R` from the current working directory, which provides `login()`, `get_championships()`, `get_teams()`, `get_user_team_info()`, `clear_api_cache()`, and the `PRESSROOM_URL` constant.

---

## 10. Error Handling

| Condition                                          | Behavior                                              |
|----------------------------------------------------|-------------------------------------------------------|
| Missing `user_name` or `password` in `.Renviron`   | `stop()` with descriptive message                     |
| Cannot extract `championship_id`                   | `stop()` with descriptive message                     |
| No teams found via `get_teams()` fallback          | `stop()` with descriptive message                     |
| Pressroom response missing `answer.news`           | Breaks pagination loop gracefully                     |
| Empty `news` array                                 | Breaks pagination loop gracefully                     |
| `api_budget` is NA                                 | Skips diff comparison; prints calculated value only   |

---

## 11. Notes

- The script clears the API cache (`clear_api_cache()`) before fetching championships to ensure fresh data.
- The `fmt_num()` helper uses a dot (`.`) as the thousands separator and a comma (`,`) as the decimal separator, consistent with European number formatting.
- The `type = "market"` parameter in the `/1/userteam/information` query is required to retrieve the budget field.
- The pagination loop caps at 50 pages to prevent runaway execution in edge cases.
- **Counterparty Rule -- Futmondo System**: In any pressroom transaction, if the `_buyer` ID is missing, `NULL`, or empty (`""`), the buyer is the **Futmondo System / Market** (the player was sold to the market/computer). Likewise, if the `_seller` ID is missing, `NULL`, or empty (`""`), the seller is the **Futmondo System / Market** (the player was bought from the market/computer). Agents and code must treat missing/empty counterparty IDs as the Futmondo system, not as corrupted or missing data. When displaying or reporting transactions, label an empty buyer or seller as `"Futmondo / Mercado"` or `"Futmondo (System)"`.
# Futmondo API Endpoint Documentation

All requests to the Futmondo API (`api.futmondo.com`) are **stateless POST requests**. Sessions are validated by embedding the token and userId in the JSON payload body wrapper:

```json
{
  "header": {
    "token": "YOUR_SESSION_TOKEN",
    "userid": "YOUR_USER_ID"
  },
  "query": { ... },
  "answer": {}
}
```

---

## 1. Authentication
* **Endpoint**: `POST https://api.futmondo.com/5/login/with_mail`
* **Query Payload**:
  ```json
  { "mail": "user@example.com", "pwd": "your_password" }
  ```
* **Response**: Contains nested tokens under `response$answer$mobile$token` and `response$answer$mobile$userid`.

---

## 2. Active Championships
* **Endpoint**: `POST https://api.futmondo.com/2/user/activechampionships`
* **Query Payload**:
  ```json
  { "excludeGeneral": false, "includeProphets": true }
  ```
* **Response**: A nested object containing a list of championships. Each championship details the logged-in user's team under `championship$userteam`.

---

## 3. Real Club Teams List
* **Endpoint**: `POST https://api.futmondo.com/1/league/championshipteams`
* **Query Payload**:
  ```json
  { "championshipId": "CHAMPIONSHIP_ID" }
  ```
* **Response**: Returns a direct JSON array under `response$answer` containing the real-world club teams (Alaves, Athletic de Bilbao, Real Madrid, etc.) in the league.

---

## 4. User Team Information & Rules
* **Endpoint**: `POST https://api.futmondo.com/1/userteam/information`
* **Query Payload**:
  ```json
  { "championshipId": "CHAMPIONSHIP_ID", "userteamId": "USER_TEAM_ID", "type": "market" }
  ```
* **Response**: Contains detailed team budget, points, league position, total squad value, and critical configurations (clause limits, bid duration).

---

## 5. User Team Roster (Squad)
* **Endpoint**: `POST https://api.futmondo.com/1/userteam/roster`
* **Query Payload**:
  ```json
  { "championshipId": "CHAMPIONSHIP_ID", "userteamId": "USER_TEAM_ID" }
  ```
* **Response**: Returns an array of players currently on the user's squad. **Note**: Returns an empty array `[]` when the squad is empty before the season starts.

---

## 6. Market Players
* **Endpoint**: `POST https://api.futmondo.com/1/market/players`
* **Query Payload**:
  ```json
  { "championshipId": "CHAMPIONSHIP_ID", "userteamId": "USER_TEAM_ID", "type": "market" }
  ```
* **Response**: List of active transfer market players with valuation change data, bid logs, and remaining listing duration.

---

## 7. Submit Bids or Purchase Release Clauses
* **Endpoint**: `POST https://api.futmondo.com/1/market/bid`
* **Query Payload**:
  ```json
  {
    "championshipId": "CHAMPIONSHIP_ID",
    "userteamId": "USER_TEAM_ID",
    "player_id": "PLAYER_ID",
    "player_slug": "PLAYER_SLUG",
    "price": 10000000,
    "isClause": true
  }
  ```
* **Response**: Returns operation success code under `response$answer$code`.

---

## 🏷️ Transfer Market Bidding vs. Release Clause Purchases

This section documents the underlying game mechanics and transaction rules that players encounter when trading in Futmondo championships:

### 1. Bidding (`isClause: false`)
* **Context**: Used when a player is listed on the active transfer market (offered by the system "computer" or list by another user).
* **Consent**: Standard auction mechanism. If offered by a user, the seller must manually accept or reject the offer. If offered by the computer, the system automatically sells the player to the highest bidder after the listing countdown expires.
* **Cost**: Custom offer value (minimum is usually the player's value or the seller's asking price).

### 2. Release Clause Purchases (`isClause: true`)
* **Context**: Used to steal a player directly from another active user's roster.
* **Consent**: Completely bypasses the owner's consent. If the buyer pays the release clause price, the player is instantly transferred to their squad.
* **Cost**: Highly expensive. Calculated automatically by the system based on the player's performance, valuation, and league settings.

### ⚠️ Player Transfer Cooldown & Protection Rules
To prevent teams from hoarding players or repeatedly stealing players in rapid succession, Futmondo implements a systemic protection rule:
* **Release Clause Cooldown**: Immediately after a player is acquired by a user (via bidding or release clause), they are temporarily protected. They **cannot be bought by release clause** until the league-specific system cooldown duration expires.
* **Custom Offers Protected Window**: During this protection window, other users are still permitted to submit custom transfer bids to the new owner. However, because release clauses are locked, the new owner holds absolute veto power—they can choose to manually accept or decline any received bids.
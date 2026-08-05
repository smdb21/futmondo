# Futmondo API Reference (HAR File Extraction)

## Overview

This document details all 24 unique API endpoints extracted from four HAR (HTTP Archive) files capturing network traffic from the Futmondo web application (Flutter-based SPA hosted at `https://app.futmondo.com`).

### Base URL

All API requests are sent to:

```
https://api.futmondo.com
```

### Authentication

Every request includes a JSON envelope with a `header` object containing:

| Field    | Type   | Description                              |
|----------|--------|------------------------------------------|
| token    | string | User authentication token                |
| userid   | string | Unique user identifier (MongoDB ObjectId)|

### Request Envelope

All POST requests use the following JSON structure:

```json
{
  "header": {
    "token": "<auth_token>",
    "userid": "<user_id>"
  },
  "query": "<endpoint_specific_payload>",
  "answer": {}
}
```

- **Content-Type**: `application/json; charset=utf-8`
- **HTTP Method**: POST (all 24 endpoints)

### Response Envelope

All responses follow a consistent envelope:

```json
{
  "answer": "<endpoint_specific_data>",
  "query": "<echoed_query_payload>",
  "header": {
    "token": "<auth_token>",
    "userid": "<user_id>"
  }
}
```

### Common Response Headers

| Header                       | Value                      |
|------------------------------|----------------------------|
| access-control-allow-origin  | *                          |
| content-type                 | application/json; charset=utf-8 |
| x-content-type-options       | nosniff                    |
| x-dns-prefetch-control       | off                        |
| x-download-options           | noopen                     |
| x-xss-protection             | 1; mode=block              |
| keep-alive                   | timeout=5                  |

---

## Endpoint Summary Table

| # | Endpoint | HAR File(s) | Method | Key Query Parameters | Response Type |
|---|----------|-------------|--------|---------------------|---------------|
| 1 | `/1/conversation/unreaditems` | All 4 | POST | None | Integer (count) |
| 2 | `/1/league/championshipteams` | app.futmondo.com.har | POST | championshipId | Array of team objects |
| 3 | `/1/locker/pressroom` | app.futmondo.com.plantillas_y_vestuario.har | POST | championshipId, from | Object with news array |
| 4 | `/1/market/bid` | app.futmondo.com.har | POST | championshipId, userteamId, player_slug, player_id, price, isClause | Object with code |
| 5 | `/1/market/myplayers` | app.futmondo.com.har | POST | championshipId, userteamId, type | Array |
| 6 | `/1/market/players` | app.futmondo.com.har | POST | championshipId, userteamId, type | Array of player objects |
| 7 | `/1/market/rosterbids` | app.futmondo.com.har | POST | championshipId, userteamId, type | Array |
| 8 | `/1/notification/unread` | All 4 | POST | None | Integer (count) |
| 9 | `/1/player/summary` | app.futmondo.com.har | POST | championshipId, userteamId, playerId | Object with data and prices |
| 10 | `/1/user/information` | app.futmondo.com.har | POST | None | User profile object |
| 11 | `/1/userteam/dreamteam` | app.futmondo.com.har | POST | championshipId, type, round | Object (error or team data) |
| 12 | `/1/userteam/information` | All 4 | POST | championshipId, userteamId, type | Team information object |
| 13 | `/1/userteam/moneymovements` | app.futmondo.com.finanzas.har | POST | championshipId, userteamId | Array of money movements |
| 14 | `/1/userteam/nightmareteam` | app.futmondo.com.har | POST | championshipId, type, round | Object (error or team data) |
| 15 | `/1/userteam/roster` | app.futmondo.com.har, app.futmondo.com.other_users.har | POST | championshipId, userteamId | Array of roster players |
| 16 | `/1/userteam/rounds` | app.futmondo.com.har | POST | championshipId, userteamId | Array of round data |
| 17 | `/2/championship/teams` | app.futmondo.com.other_users.har | POST | championshipId | Object with teams array |
| 18 | `/2/league/list` | app.futmondo.com.har | POST | None | Array of league objects |
| 19 | `/2/locker/news` | app.futmondo.com.plantillas_y_vestuario.har | POST | championshipId, from | Object with news array |
| 20 | `/2/user/activechampionships` | app.futmondo.com.har | POST | excludeGeneral, includeProphets | Object with championships, rounds, leagues |
| 21 | `/2/userteam/getdtconfig` | app.futmondo.com.har | POST | championshipId, userteamId | Configuration object |
| 22 | `/5/announcement/list` | app.futmondo.com.har | POST | onlyIds | Array of announcement objects |
| 23 | `/5/league/championshipplayers` | app.futmondo.com.har | POST | championshipId | Object with players array |
| 24 | `/5/prize/unread` | All 4 | POST | None | Integer (count) |

---

## Detailed Endpoint Specifications

---

### 1. POST `/1/conversation/unreaditems`

**Origin HAR File(s):** `app.futmondo.com.har`, `app.futmondo.com.finanzas.har`, `app.futmondo.com.plantillas_y_vestuario.har`, `app.futmondo.com.other_users.har`

**Purpose & Functional Description:**

Returns the count of unread conversation items for the authenticated user. Used to display notification badges on the conversation/messages tab.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`
- `Accept: */*`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string: auth token>",
    "userid": "<string: user ObjectId>"
  },
  "query": "",
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": 0,
  "query": {},
  "header": {
    "token": "<string: auth token>",
    "userid": "<string: user ObjectId>"
  }
}
```

**Key Return Fields & Data Types:**

| Field  | Type    | Description                     |
|--------|---------|---------------------------------|
| answer | integer | Count of unread conversation items |

---

### 2. POST `/1/league/championshipteams`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the list of football teams participating in a specific championship/league. Used to populate team selectors and display team rosters.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string: championship ObjectId>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [
    {
      "id": "<string: team ObjectId>",
      "name": "<string: team name>",
      "logo": "<string: logo filename>",
      "league": {
        "name": "<string: league name>",
        "slug": "<string: league slug>",
        "dsc": "<string: country code>"
      }
    }
  ],
  "query": {
    "championshipId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer[].id | string | Team MongoDB ObjectId |
| answer[].name | string | Team display name |
| answer[].logo | string | Logo image filename |
| answer[].league.name | string | League name |
| answer[].league.slug | string | League URL slug |
| answer[].league.dsc | string | Country descriptor |

---

### 3. POST `/1/locker/pressroom`

**Origin HAR File(s):** `app.futmondo.com.plantillas_y_vestuario.har`

**Purpose & Functional Description:**

Returns market news and player transfer activity for a championship. Shows recently listed or sold players with bid history. Used in the pressroom/market news feed.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string: championship ObjectId>",
    "from": "<string: pagination cursor, empty for first page>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "news": [
      {
        "_id": "<string: news item ObjectId>",
        "created": "<string: ISO 8601 datetime>",
        "_playerTeam": {
          "_id": "<string: team ObjectId>",
          "name": "<string: team name>",
          "slug": "<string: team slug>"
        },
        "_player": {
          "_id": "<string: player ObjectId>",
          "slug": "<string: player slug>",
          "name": "<string: player name>"
        },
        "_seller": {
          "_id": "<string: seller ObjectId>",
          "name": "<string: seller team name>"
        },
        "_buyer": {
          "_id": "<string: buyer ObjectId>",
          "name": "<string: buyer team name>"
        },
        "bids": [
          {
            "u": {
              "_id": "<string: bidder ObjectId>",
              "name": "<string: bidder name>"
            },
            "bid": "<integer: bid amount>",
            "_id": "<string: bid ObjectId>"
          }
        ],
        "price": "<integer: current price>"
      }
    ]
  },
  "query": {
    "championshipId": "<string>",
    "from": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.news[].\u005f_id | string | News item ObjectId |
| answer.news[].created | string | ISO 8601 timestamp |
| answer.news[\]._playerTeam | object | Player's club info |
| answer.news[].\u005f_player | object | Player details |
| answer.news[].\u005f_seller | object | Selling team (for listings) |
| answer.news[].\u005f_buyer | object | Buying team (for purchases) |
| answer.news[].bids | array | List of bids on the player |
| answer.news[].price | integer | Current listed price |

---

### 4. POST `/1/market/bid`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Places a bid on a player in the market. This is a write operation that submits a purchase offer for a specific player at a given price.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string: championship ObjectId>",
    "userteamId": "<string: user team ObjectId>",
    "player_slug": "<string: player slug>",
    "player_id": "<string: player ObjectId>",
    "price": "<integer: bid amount>",
    "isClause": "<boolean: whether this is a release clause bid>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "code": "api.general.ok"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "player_slug": "<string>",
    "player_id": "<string>",
    "price": "<integer>",
    "isClause": "<boolean>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.code | string | Result code (e.g., "api.general.ok" on success) |
| query.championshipId | string | Echoed championship ID |
| query.userteamId | string | Echoed user team ID |
| query.player_slug | string | Echoed player slug |
| query.player_id | string | Echoed player ID |
| query.price | integer | Echoed bid price |
| query.isClause | boolean | Echoed clause flag |

---

### 5. POST `/1/market/myplayers`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the list of players currently owned by the user's team that are available for sale on the market.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "market"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [],
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "market"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer | array | Array of player objects (empty if no players for sale) |
| query.type | string | Always "market" |

---

### 6. POST `/1/market/players`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the full list of players available for purchase in the market for a given championship. Includes pricing, stats, and bid counts.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "market"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [
    {
      "id": "<string: player ObjectId>",
      "name": "<string: player name>",
      "slug": "<string: player slug>",
      "role": "<string: primary position>",
      "role2": "<string: secondary position>",
      "photo": "<string: photo filename>",
      "points": "<integer: total points>",
      "value": "<integer: current market value>",
      "team": "<string: club name>",
      "logo": "<string: club logo filename>",
      "status": "<string: injury/suspension status>",
      "creationDate": "<string: ISO 8601 listing date>",
      "expirationDate": "<string: ISO 8601 expiry date>",
      "price": "<integer: asking price>",
      "isClause": "<boolean: release clause>",
      "computer": "<boolean: AI-controlled>",
      "type": "<string: player type (normal, etc.)>",
      "change": "<integer: price change>",
      "average": {
        "average": "<float: overall average>",
        "homeAverage": "<string: home average>",
        "awayAverage": "<string: away average>",
        "averageLastFive": "<string: last 5 matches average>",
        "matches": "<integer: matches played>",
        "fitness": "<array: fitness data>",
        "total": "<string: total score>"
      },
      "numberOfBids": "<integer: current bid count>"
    }
  ],
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "market"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer[].id | string | Player ObjectId |
| answer[].name | string | Player name |
| answer[].slug | string | Player URL slug |
| answer[].role | string | Primary position (portero, defensa, centrocampista, delantero) |
| answer[].role2 | string | Secondary position |
| answer[].value | integer | Current market value |
| answer[].price | integer | Asking price |
| answer[].numberOfBids | integer | Number of active bids |
| answer[].average | object | Performance statistics |

---

### 7. POST `/1/market/rosterbids`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the list of active bids placed by the user's team on players in the market.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "roster"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [],
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "roster"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer | array | Array of bid objects (empty if no active bids) |
| query.type | string | Always "roster" |

---

### 8. POST `/1/notification/unread`

**Origin HAR File(s):** `app.futmondo.com.har`, `app.futmondo.com.finanzas.har`, `app.futmondo.com.plantillas_y_vestuario.har`, `app.futmondo.com.other_users.har`

**Purpose & Functional Description:**

Returns the count of unread notifications for the authenticated user. Used for the notification bell badge across all app screens.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": "",
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": 9,
  "query": {},
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer | integer | Count of unread notifications |

---

### 9. POST `/1/player/summary`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns detailed summary information for a specific player, including current stats, historical price data, and performance metrics.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "playerId": "<string: player ObjectId>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "data": {
      "id": "<string: player ObjectId>",
      "name": "<string: player name>",
      "slug": "<string: player slug>",
      "role": "<string: primary position>",
      "role2": "<string: secondary position>",
      "photo": "<string: photo filename>",
      "points": "<integer: total points>",
      "value": "<integer: current market value>",
      "team": "<string: club name>",
      "logo": "<string: club logo filename>",
      "teamId": "<string: team ObjectId>",
      "status": "<string: injury/suspension status>",
      "rating": "<integer: player rating>",
      "average": {
        "average": "<float: overall average>",
        "homeAverage": "<string>",
        "awayAverage": "<string>",
        "averageLastFive": "<string>",
        "matches": "<integer>",
        "fitness": "<array>",
        "total": "<string>"
      },
      "x": "<string: sponsor tag>",
      "change": "<integer: value change>",
      "computer": "<boolean>",
      "total": {
        "points": "<integer>",
        "played": "<integer>"
      }
    },
    "prices": [
      {
        "_id": "<string: price record ObjectId>",
        "c": "<integer>",
        "s": "<integer>",
        "date": "<string: ISO 8601>",
        "price": "<integer: historical price>"
      }
    ]
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "playerId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.data | object | Current player data |
| answer.data.rating | integer | Player rating (1-5) |
| answer.data.value | integer | Current market value |
| answer.data.change | integer | Value change amount |
| answer.prices | array | Historical price records |
| answer.prices[].price | integer | Price at given date |
| answer.prices[].date | string | ISO 8601 timestamp |

---

### 10. POST `/1/user/information`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the authenticated user's profile information including screen name, avatar, notification preferences, and account details.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": "",
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "id": "<string: user ObjectId>",
    "screenName": "<string: display name>",
    "photo": "<string: avatar URL>",
    "subscribe": "<boolean: subscription status>",
    "mondos": {
      "availables": "<integer: available Mondos currency>",
      "atStake": "<integer: Mondos currently at stake>"
    },
    "language": "<string: locale code>",
    "notifications": {
      "conversation": "<boolean>",
      "championship": "<boolean>",
      "third_parties": "<boolean>",
      "news": "<boolean>",
      "private_messages": "<boolean>",
      "points": "<boolean>",
      "market": "<boolean>",
      "rounds": "<boolean>",
      "players": "<boolean>",
      "matches": "<boolean>",
      "locker": "<boolean>"
    },
    "devices": "<integer: registered device count>",
    "email": "<string: email address>"
  },
  "query": {},
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.id | string | User ObjectId |
| answer.screenName | string | Display name |
| answer.photo | string | Avatar image URL |
| answer.subscribe | boolean | Premium subscription flag |
| answer.mondos.availables | integer | Available virtual currency |
| answer.mondos.atStake | integer | Currency in active bets |
| answer.language | string | Locale (e.g., "es") |
| answer.notifications | object | Per-category notification toggles |
| answer.devices | integer | Number of registered devices |
| answer.email | string | User email address |

---

### 11. POST `/1/userteam/dreamteam`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the Dream Team (best-performing lineup) for a given championship and round. Returns an error when no valid round is available.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "type": "dreamteam",
    "round": "<string or null: round identifier>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload (Error Case):**

```json
{
  "answer": {
    "error": true,
    "code": "api.error.invalid"
  },
  "query": {
    "championshipId": "<string>",
    "type": "dreamteam",
    "round": null
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.error | boolean | Error flag |
| answer.code | string | Error code (e.g., "api.error.invalid") |
| query.type | string | Always "dreamteam" |
| query.round | string/null | Round identifier |

---

### 12. POST `/1/userteam/information`

**Origin HAR File(s):** `app.futmondo.com.har`, `app.futmondo.com.finanzas.har`, `app.futmondo.com.plantillas_y_vestuario.har`, `app.futmondo.com.other_users.har`

**Purpose & Functional Description:**

Returns comprehensive information about a specific user team in a championship, including budget, points, position, team value, and championship configuration.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "<string: e.g., 'market'>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "id": "<string: userteam ObjectId>",
    "name": "<string: team name>",
    "championship": "<string: championship name>",
    "championshipId": "<string>",
    "type": "<string: 'normal'>",
    "championshipLevel": "<integer>",
    "championshipGroup": "<integer>",
    "championshipMode": "<string: 'social'>",
    "isPublic": "<boolean>",
    "championshipFinished": "<boolean>",
    "points": "<integer>",
    "budget": "<integer: remaining budget>",
    "withheld": "<integer: withheld funds>",
    "teamValue": "<integer: total team value>",
    "latestLockerNews": "<null or object>",
    "latestMarketMove": "<null or object>",
    "position": "<integer: league position>",
    "trend": "<string: 'equal', 'up', 'down'>",
    "mondos": {
      "enabled": "<boolean>",
      "entry": "<integer>",
      "pot": "<integer>",
      "payout": "<string>",
      "fee": "<integer>",
      "jackpotFee": "<integer>",
      "minPot": "<integer>"
    },
    "cts": "<boolean>",
    "pro": "<boolean>",
    "sponsor": {
      "banner_link": "<string>",
      "banner": "<string>"
    },
    "sport": "<string: 'soccer'>",
    "initialRosterEmpty": "<boolean>",
    "league": {
      "_id": "<string>",
      "name": "<string>",
      "sponsor": {
        "r_18_b": "<string>",
        "r_18_c": "<string>",
        "l_18_b": "<string>",
        "l_18_c": "<string>"
      },
      "dataSourceChamp": "<string>"
    },
    "configuration": {
      "budget": "<integer>",
      "numberOfPlayers": "<integer>",
      "moneyPerPoint": "<integer>",
      "moneyPerRanking": "<integer>",
      "rankingMode": "<string>",
      "usersToRank": "<integer>",
      "dreamTeamPlayer": "<integer>",
      "mvpPlayer": "<integer>",
      "extraPay": "<integer>",
      "marketPlayers": "<integer>",
      "bidDuration": "<integer>",
      "marketTimes": "<integer>",
      "maxPlayersInRoster": "<integer>",
      "maxUserteams": "<integer>",
      "playerRetention": "<integer>",
      "enableAutomaticClauses": "<boolean>",
      "enablingClause": "<integer>",
      "playerMoveInDays": "<integer>",
      "numberOfRounds": "<integer>",
      "remainingNumberOfRounds": "<integer>",
      "fullSeason": "<boolean>"
    }
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>",
    "type": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.id | string | User team ObjectId |
| answer.name | string | Team display name |
| answer.budget | integer | Remaining budget |
| answer.teamValue | integer | Total squad value |
| answer.points | integer | Championship points |
| answer.position | integer | League standing position |
| answer.trend | string | Position trend (equal/up/down) |
| answer.pro | boolean | PRO league flag |
| answer.configuration | object | Championship rules config |

---

### 13. POST `/1/userteam/moneymovements`

**Origin HAR File(s):** `app.futmondo.com.finanzas.har`

**Purpose & Functional Description:**

Returns the financial transaction history for a user team, including budget allocations, player purchases, bonuses, and market transactions.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [
    {
      "_id": "<string: transaction ObjectId>",
      "concept": "<string: player name or description>",
      "type": "<string: 'budget', 'buy', 'sell', 'bonus'>",
      "category": "<string: 'bonus', 'market', 'round', etc.>",
      "money": "<integer: amount (positive for income, negative for支出)>",
      "date": "<string: ISO 8601 timestamp>"
    }
  ],
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer[].\u005f_id | string | Transaction ObjectId |
| answer[].concept | string | Description (player name or category) |
| answer[].type | string | Transaction type (budget, buy, sell, bonus) |
| answer[].category | string | Category (bonus, market, round) |
| answer[].money | integer | Amount (positive=income, negative=expense) |
| answer[].date | string | ISO 8601 timestamp |

---

### 14. POST `/1/userteam/nightmareteam`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the Nightmare Team (worst-performing lineup) for a given championship and round. Returns an error when no valid round is available.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "type": "nightmare",
    "round": "<string or null>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload (Error Case):**

```json
{
  "answer": {
    "error": true,
    "code": "api.error.invalid"
  },
  "query": {
    "championshipId": "<string>",
    "type": "nightmare",
    "round": null
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.error | boolean | Error flag |
| answer.code | string | Error code |
| query.type | string | Always "nightmare" |
| query.round | string/null | Round identifier |

---

### 15. POST `/1/userteam/roster`

**Origin HAR File(s):** `app.futmondo.com.har`, `app.futmondo.com.other_users.har`

**Purpose & Functional Description:**

Returns the current player roster for a specific user team in a championship.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [],
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer | array | Array of player roster objects |
| query.championshipId | string | Echoed championship ID |
| query.userteamId | string | Echoed user team ID |

---

### 16. POST `/1/userteam/rounds`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the round-by-round performance data for a user team in a championship, including points earned per round.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [],
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer | array | Array of round performance objects |

---

### 17. POST `/2/championship/teams`

**Origin HAR File(s):** `app.futmondo.com.other_users.har`

**Purpose & Functional Description:**

Returns all user teams registered in a championship with standings, team values, and clause information. Response body is base64-encoded in the HAR.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload (decoded):**

```json
{
  "answer": {
    "isAdmin": "<boolean>",
    "teams": [
      {
        "id": "<string: userteam ObjectId>",
        "userid": "<string: user ObjectId>",
        "name": "<string: user display name>",
        "photo": "<string: avatar URL>",
        "teamname": "<string: team name>",
        "teamslug": "<string: team URL slug>",
        "teamid": "<string: team ObjectId>",
        "points": "<integer>",
        "teamValue": "<integer>",
        "lastAccess": "<string: ISO 8601>",
        "isAdmin": "<boolean>",
        "nln": "<integer>",
        "awards": "<object>",
        "clauses": {
          "w": {
            "in": "<integer>",
            "out": "<integer>"
          },
          "t": {
            "in": "<integer>",
            "out": "<integer>"
          }
        },
        "cw": {
          "l": "<boolean>",
          "c": "<boolean>",
          "o": "<boolean>"
        }
      }
    ]
  },
  "query": {
    "championshipId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.isAdmin | boolean | Whether requester is admin |
| answer.teams[].id | string | User team ObjectId |
| answer.teams[].name | string | User display name |
| answer.teams[].teamname | string | Team name |
| answer.teams[].points | integer | Championship points |
| answer.teams[].teamValue | integer | Total squad value |
| answer.teams[].clauses | object | Release clause counts |
| answer.teams[].cw | object | Championship week flags |

---

### 18. POST `/2/league/list`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the list of all available leagues (football competitions) in the system, including their rounds, modes, and supported media sources.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": "",
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": [
    {
      "_id": "<string: league ObjectId>",
      "name": "<string: league name>",
      "slug": "<string: URL slug>",
      "sport": "<string: 'soccer'>",
      "modes": ["<string>"],
      "media": ["<string>"],
      "ps": ["<string>"],
      "multi": {
        "is": "<boolean>",
        "leagues": "<array>"
      },
      "weight": "<integer>",
      "divider": "<integer>",
      "country": "<string: country code>",
      "dataSourceChamp": "<string>",
      "rounds": [
        {
          "_id": "<string: round ObjectId>",
          "status": "<string: 'next', 'future', 'done'>"
        }
      ]
    }
  ],
  "query": {},
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer[].\u005f_id | string | League ObjectId |
| answer[].name | string | League name (e.g., "Liga EA Sports") |
| answer[].slug | string | URL slug |
| answer[].sport | string | Sport type |
| answer[].modes | array | Supported game modes |
| answer[].media | array | Supported media sources |
| answer[].country | string | Country code |
| answer[].rounds | array | Round schedule with status |

---

### 19. POST `/2/locker/news`

**Origin HAR File(s):** `app.futmondo.com.plantillas_y_vestuario.har`

**Purpose & Functional Description:**

Returns the locker news feed for a championship, including administrative actions, sponsor promotions, and system announcements.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "from": "<string: pagination cursor, empty for first page>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "news": [
      {
        "_id": "<string: news ObjectId>",
        "created": "<string: ISO 8601>",
        "typ": "<string: 'a' for admin, 's' for sponsor>",
        "styp": "<string: 'info', 'warning', etc.>",
        "t": "<string: translation key or title>",
        "txt": "<string: HTML-formatted message>",
        "u": {
          "p": "<string: user avatar URL>",
          "n": "<string: user name>",
          "id": "<string: user ObjectId>"
        },
        "data": {
          "<contextual_key>": "<string: contextual value>"
        }
      }
    ]
  },
  "query": {
    "championshipId": "<string>",
    "from": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.news[].\u005f_id | string | News item ObjectId |
| answer.news[].created | string | ISO 8601 timestamp |
| answer.news[].typ | string | Type ("a"=admin, "s"=sponsor) |
| answer.news[].styp | string | Sub-type (info, warning) |
| answer.news[].t | string | Title/translation key |
| answer.news[].txt | string | HTML-formatted body text |
| answer.news[].u | object | Associated user info |
| answer.news[].data | object | Contextual data payload |

---

### 20. POST `/2/user/activechampionships`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns all active championships for the authenticated user, including championship details, associated rounds, and league settings.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "excludeGeneral": "<boolean>",
    "includeProphets": "<boolean>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "championships": [
      {
        "id": "<string: championship ObjectId>",
        "name": "<string: championship name>",
        "members": "<integer: member count>",
        "isAdmin": "<boolean>",
        "country": "<string>",
        "status": "<string: 'ready', 'active', 'finished'>",
        "type": "<string: 'normal'>",
        "level": "<integer>",
        "group": "<integer>",
        "userteam": {
          "id": "<string>",
          "name": "<string>",
          "budget": "<integer>",
          "points": "<integer>",
          "trend": "<string>",
          "position": "<integer>",
          "status": "<string>"
        },
        "mode": "<string>",
        "official": "<boolean>",
        "finished": "<boolean>",
        "started": "<boolean>",
        "version": "<integer>",
        "pro": "<boolean>",
        "lea": "<boolean>",
        "league": "<string: league ObjectId>",
        "sport": "<string>",
        "ps": "<string>",
        "isPublic": "<boolean>",
        "custom": {
          "ps": "<null or string>",
          "media": [
            {
              "media": "<string>",
              "pct": "<integer>"
            }
          ],
          "stats": "<string>"
        },
        "sponsorName": "<string>",
        "sponsor": "<string>",
        "entity": "<string>"
      }
    ],
    "rounds": [
      {
        "_id": "<string>",
        "number": "<integer>",
        "championshipId": "<string>",
        "beginProcess": "<string: ISO 8601>",
        "name": "<string>"
      }
    ],
    "leagues": [
      {
        "_id": "<string>",
        "name": "<string>",
        "generalSettings": "<object>"
      }
    ]
  },
  "query": {
    "excludeGeneral": "<boolean>",
    "includeProphets": "<boolean>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.championships | array | User's active championships |
| answer.championships[].id | string | Championship ObjectId |
| answer.championships[].members | integer | Member count |
| answer.championships[].userteam | object | User's team in this championship |
| answer.championships[].mode | string | Game mode (social, classic, picas) |
| answer.rounds | array | Upcoming round schedule |
| answer.leagues | array | League settings |

---

### 21. POST `/2/userteam/getdtconfig`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the Dream Team configuration for a user team, including the last update date and PRO status.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "date": "<string: ISO 8601 last update>",
    "pro": "<boolean>"
  },
  "query": {
    "championshipId": "<string>",
    "userteamId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.date | string | ISO 8601 timestamp of last DT update |
| answer.pro | boolean | PRO league flag |

---

### 22. POST `/5/announcement/list`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the list of platform-wide announcements, including promotional content, news, and event information. Response body is base64-encoded in the HAR.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "onlyIds": "<boolean>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload (decoded):**

```json
{
  "answer": [
    {
      "_id": "<string: announcement ObjectId>",
      "uAt": "<string: ISO 8601 update time>",
      "cAt": "<string: ISO 8601 creation time>",
      "img": "<string: image URL>",
      "__v": "<integer: version>",
      "sites": ["<string>"],
      "text": [
        {
          "lng": "<string: language code>",
          "t": "<string: title>",
          "summary": "<string: summary text>",
          "body": "<string: full body text>",
          "_id": "<string>"
        }
      ]
    }
  ],
  "query": {
    "onlyIds": "<boolean>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer[].\u005f_id | string | Announcement ObjectId |
| answer[].uAt | string | Last updated timestamp |
| answer[].cAt | string | Creation timestamp |
| answer[].img | string | Announcement image URL |
| answer[].sites | array | Target sites (e.g., "futmondo") |
| answer[].text | array | Localized text content |
| answer[].text[].lng | string | Language code (e.g., "es") |
| answer[].text[].t | string | Title |
| answer[].text[].summary | string | Summary |
| answer[].text[].body | string | Full body HTML |

---

### 23. POST `/5/league/championshipplayers`

**Origin HAR File(s):** `app.futmondo.com.har`

**Purpose & Functional Description:**

Returns the complete player database for a championship, including all available players with their stats, values, and team assignments. This is the largest response in the API.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": {
    "championshipId": "<string>"
  },
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": {
    "players": [
      {
        "id": "<string: player ObjectId>",
        "name": "<string: player name>",
        "slug": "<string: player slug>",
        "role": "<string: primary position>",
        "role2": "<string: secondary position>",
        "photo": "<string: photo filename>",
        "points": "<integer>",
        "value": "<integer: market value>",
        "status": "<string>",
        "rating": "<integer: 1-5>",
        "computer": "<boolean>",
        "dbp": "<boolean>",
        "average": {
          "average": "<float>",
          "homeAverage": "<string>",
          "awayAverage": "<string>",
          "averageLastFive": "<string>",
          "matches": "<integer>",
          "fitness": "<array>",
          "total": "<string>"
        },
        "change": "<integer: value change>",
        "teamId": "<string: team ObjectId>",
        "fav": "<boolean: user favorite>"
      }
    ]
  },
  "query": {
    "championshipId": "<string>"
  },
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer.players | array | Full player roster |
| answer.players[].id | string | Player ObjectId |
| answer.players[].name | string | Player name |
| answer.players[].role | string | Primary position |
| answer.players[].value | integer | Market value |
| answer.players[].rating | integer | Player rating (1-5) |
| answer.players[].dbp | boolean | Database player flag |
| answer.players[].change | integer | Value change |
| answer.players[].fav | boolean | User's favorite flag |

---

### 24. POST `/5/prize/unread`

**Origin HAR File(s):** `app.futmondo.com.har`, `app.futmondo.com.finanzas.har`, `app.futmondo.com.plantillas_y_vestuario.har`, `app.futmondo.com.other_users.har`

**Purpose & Functional Description:**

Returns the count of unread prize/promotional notifications for the authenticated user. Used for the prize badge indicator.

**Request Headers:**

- `Content-Type: application/json; charset=utf-8`

**Request JSON Schema & Example Payload:**

```json
{
  "header": {
    "token": "<string>",
    "userid": "<string>"
  },
  "query": "",
  "answer": {}
}
```

**Response JSON Schema & Example Payload:**

```json
{
  "answer": 0,
  "query": {},
  "header": {
    "token": "<string>",
    "userid": "<string>"
  }
}
```

**Key Return Fields & Data Types:**

| Field | Type | Description |
|-------|------|-------------|
| answer | integer | Count of unread prize notifications |

---

## Notes

- All endpoints use POST with JSON bodies. No GET endpoints were observed in the HAR files.
- The API uses a versioned path structure: `/1/`, `/2/`, `/5/` prefix segments denote API versioning or module grouping.
- CORS preflight (OPTIONS) requests are handled by the server with `access-control-allow-origin: *` and `x-powered-by: Express`.
- Some responses are base64-encoded in the HAR files (specifically `/2/championship/teams` and `/5/announcement/list`); the decoded JSON is shown above.
- The `query` field in the request can be either an empty string (`""`) for endpoints with no parameters, or an object containing endpoint-specific parameters.
- Error responses follow the pattern `{"error": true, "code": "<error_code>"}` within the `answer` field.
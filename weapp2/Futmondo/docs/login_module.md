# Login Module Documentation

This document describes the `Login_Module.R` Shiny module, which handles user authentication with the Futmondo API and presents the connection status.

---

## 1. Overview

The Login Module provides two exported functions:
* `login_UI(id)` -- Renders the login input form box and the authentication result box.
* `login_Server(id, user, password)` -- Drives user authentication via `login()`, updates connection state reactively, and displays status.

---

## 2. Authentication UI & Status

Upon successful login with Futmondo API (`POST https://api.futmondo.com/5/login/with_mail`):
1. **Security Clean-Up**: Raw API tokens are hidden from the user interface.
2. **Welcome Card**: Renders a clean success card featuring a green checkmark icon (`fa-circle-check`), personalized greeting (`"Welcome back, [user_name]!"`), and active connection indicator.
3. **Status Update**: Updates the result box status to `"success"`.

---

## 3. Security Notice

A lock icon badge (`fa-lock`) and the caption `"Your password is encrypted and is never saved or stored anywhere."` are displayed on the login form to reassure users that their credentials are used strictly for session authentication with the Futmondo API and are never saved to disk or database.
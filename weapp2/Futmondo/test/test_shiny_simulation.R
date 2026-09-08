#!/usr/bin/env Rscript
# Safe default: actual application lifecycle with deterministic API fixtures.
# All HTTP is blocked; this script never reads account credentials for login,
# sends real trades, resets a database or writes live snapshots.
source("test/test_application_offline.R", local = .GlobalEnv)

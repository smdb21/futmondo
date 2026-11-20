#!/usr/bin/env Rscript
# Successful legacy worker capacity is complete historical evidence, not a
# failed acquisition. Real codec/card; deterministic reads, no accounts/writes.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
now <- as.POSIXct('2026-11-15 12:00:00', tz = 'UTC')
Sys.time <- function() now
get_sb_url <- function() ''
get_sb_key <- function() ''
info <- list(
  budget = 1000,
  withheld = 0,
  teamValue = 2000,
  maxBid = 2000,
  configuration = list(maxPlayersInRoster = 20, mnmp = .5, mbmv = TRUE)
)
history <- data.frame(
  id = character(),
  player_id = character(),
  buyer_team_id = character(),
  seller_team_id = character(),
  is_clause = logical(),
  created = character()
)
capacity <- function(at = now) {
  calculate_team_acquisition_capacity(
    info,
    data.frame(id = 'owned', value = 2000),
    history,
    data.frame(id = character()),
    list(),
    'mine',
    now = at
  )
}
value <- capacity(now - 300)
stopifnot(
  isTRUE(attr(value, 'complete')),
  acquisition_capacity_observation_complete(value),
  !evaluate_acquisition_preflight(value, 'offer')$ok
)
cat(
  'PASS source capacity publishes completeness independently of its original expired action deadline\n'
)
auth <- c(userid = 'capacity-completeness-fixture', token = 'synthetic')
ctx <- resource_context(auth, 'league', 'mine')
descriptor <- fm_worker_descriptor(
  'get_team_acquisition_capacity',
  list(login = auth, championship_id = 'league', user_team_id = 'mine'),
  ctx
)
legacy <- value
attr(legacy, 'complete') <- NULL
restore <- function(
  data = legacy,
  completeness = 'unknown',
  status = 'ok',
  stamp = now - 300
) {
  raw <- charToRaw(fm_worker_json(fm_worker_encode(data)))
  compressed <- history_baseline_compress(raw)
  manifest <- list(
    account_id = ctx$account_id,
    resource_key = descriptor$resource_key,
    handler = descriptor$handler,
    context = descriptor$context,
    version = 'legacy-capacity',
    json_bytes = length(raw),
    parts = 1L,
    sha256 = paste0(openssl::sha256(compressed)),
    observed_at = rival_finance_stamp(stamp),
    checked_at = rival_finance_stamp(stamp),
    valid_until = rival_finance_stamp(stamp + 60),
    status = status,
    completeness = completeness
  )
  fm_worker_restore(
    list(
      key = descriptor$resource_key,
      manifest = manifest,
      chunks = list(jsonlite::base64_enc(compressed))
    ),
    ctx$account_id
  )
}
restored <- restore()
stopifnot(
  attr(restored$data, 'worker_completeness') == 'complete',
  attr(restored$data, 'worker_completeness_source') == 'capacity_contract',
  restored$data$valid_until == legacy$valid_until,
  attr(restored$data, 'valid_until') == legacy$valid_until,
  attr(restored$data, 'observed_at') == now - 300,
  !resource_snapshot_unexpired(restored$data),
  !evaluate_acquisition_preflight(restored$data, 'offer')$ok
)
cat(
  'PASS actual legacy codec reconstructs completeness without renewing timestamps or enabling an expired purchase\n'
)
for (kind in c(
  'partial',
  'roster',
  'funds',
  'identities',
  'count',
  'bid',
  'diagnostics',
  'deadline'
)) {
  bad <- legacy
  switch(
    kind,
    partial = {
      bad$status <- 'partial'
    },
    roster = {
      bad$roster$status <- 'unavailable'
    },
    funds = {
      bad$funds$withheld <- NA_real_
    },
    identities = {
      bad$roster$pending_inbound_ids <- NULL
    },
    count = {
      bad$roster$effective_count <- 2L
    },
    bid = {
      bad$outstanding$completeness <- 'partial'
    },
    diagnostics = {
      bad$diagnostics <- 'reported budget unavailable'
    },
    deadline = {
      bad$valid_until <- NA_real_
    }
  )
  stopifnot(
    !acquisition_capacity_observation_complete(bad),
    attr(restore(bad)$data, 'worker_completeness') == 'unknown'
  )
}
for (field in c(
  'complete',
  'worker_completeness',
  'worker_acquisition_status'
)) {
  bad <- legacy
  attr(bad, field) <- if (field == 'complete') FALSE else 'partial'
  stopifnot(
    !acquisition_capacity_observation_complete(bad),
    attr(restore(bad)$data, 'worker_completeness') == 'unknown'
  )
}
stopifnot(
  attr(restore(completeness = 'partial')$data, 'worker_completeness') ==
    'partial',
  attr(restore(status = 'unavailable')$data, 'worker_completeness') ==
    'unknown',
  attr(restore(stamp = now + 60)$data, 'worker_completeness') == 'unknown'
)
cat(
  'PASS partial/error/future envelopes and missing funds, IDs, counts, offers or deadlines cannot gain complete proof\n'
)
fm_worker_configured <- function() TRUE
account <- fm_worker_account_state(ctx$account_id)
account$known <- TRUE
account$bootstrap_complete <- TRUE # saved snapshot already restored (since 76dec9b)
account$account <- list(enabled = TRUE, auth_status = 'active')
fm_worker_put(ctx$account_id, descriptor$resource_key, restored)
rpc <- 0L
live <- 0L
writes <- 0L
# Since 76dec9b an opened card asks the database for newer saved observations
# (a 'read' RPC). That is allowed and answered with "nothing newer"; any other
# RPC (enqueueing acquisition, writes) still fails the test.
reads <- 0L
fm_worker_rpc <- function(name, ...) {
  if (identical(name, 'read')) {
    reads <<- reads + 1L
    return(promises::promise_resolve(list(account = account$account, resources = list(), jobs = list(), health = account$health)))
  }
  rpc <<- rpc + 1L
  stop('Unexpected database/source request')
}
real_async <- async_adapter_fetch
async_adapter_fetch <- function(
  fn,
  args = list(),
  context,
  worker_bypass = FALSE,
  intent = NULL,
  ...
) {
  # Since 76dec9b manual checks pass intent='manual_action' instead of worker_bypass.
  worker_bypass <- isTRUE(worker_bypass) || identical(intent, 'manual_action')
  if (fn == 'get_team_acquisition_capacity') {
    if (worker_bypass) {
      live <<- live + 1L
      return(promises::promise_resolve(capacity()))
    }
    return(real_async(fn, args, context, worker_bypass = FALSE, ...))
  }
  promises::promise_resolve(switch(
    fn,
    get_player_summary = list(
      data = list(id = 'target'),
      championship = list(owner = list(id = 'rival', computer = FALSE)),
      market = list(p = 150, bids = list()),
      points = list(),
      prices = list()
    ),
    get_lineup_from_team = list(pointSystem = 'fixture'),
    get_player_full_profile = list(profile = list()),
    get_player_matches = list(matches = list()),
    read_card_round_history = data.frame(),
    read_card_price_history = data.frame(),
    stop(fn)
  ))
}
buy_clause <- function(...) {
  writes <<- writes + 1L
  stop('Unexpected transaction')
}
player <- data.frame(
  id = 'target',
  name = 'Fixture',
  role = 'delantero',
  team = '',
  value = 200,
  price = 150,
  change = 0,
  points = 10,
  user_team_id = 'rival',
  market_inMarket = TRUE,
  clause_price = 400,
  clause_transferred = FALSE
)
settle <- function(session) {
  for (i in 1:30) {
    later::run_now(0)
    session$flushReact()
  }
}
shiny::testServer(
  selected_player_Server,
  args = list(
    selected_player = reactive(player),
    login_token = reactive(auth),
    championship_id = reactive('league'),
    user_team_id = reactive('mine')
  ),
  {
    settle(session)
    stopifnot(
      live == 0L,
      !grepl(
        'Checking availability|Retry availability',
        output$action_buttons$html
      )
    )
    session$setInputs(btn_bid_market = 1L)
    settle(session)
    html <- output$action_buttons$html
    stopifnot(
      live == 1L,
      rpc == 0L,
      writes == 0L,
      evaluate_acquisition_preflight(
        card_states$capacity()$data,
        'bid',
        200
      )$ok,
      !grepl(
        'Availability checks expired before they completed',
        html,
        fixed = TRUE
      ),
      !grepl('btn_verify_availability', html, fixed = TRUE)
    )
    cat(
      'PASS selection skips expired financial evidence; explicit editor demand verifies it without a transaction\n'
    )
  }
)
elapsed <- system.time(
  for (i in 1:1000) {
    acquisition_capacity_observation_complete(legacy)
  }
)[['elapsed']]
cat(sprintf(
  'METRIC 1000 completeness validations %.3fs; pure validation loop; no extra I/O\n',
  elapsed
))

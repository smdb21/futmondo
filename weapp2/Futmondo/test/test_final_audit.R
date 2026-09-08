#!/usr/bin/env Rscript
# Static migration guards; no database connection, SQL execution or live writes.
sql <- paste(readLines('scripts/migrations/20260907_private_history.sql', warn=FALSE), collapse='\n')
checks <- 0L
check <- function(name, expr) { force(expr); checks <<- checks+1L; cat('[PASS]',name,'\n') }
check('legacy private league/history tables are restricted to server role', {
  for (table in c('championships','user_teams','user_team_history','player_history',
      'market_transactions','round_dream_team','player_daily_snapshots','manager_dna_profiles','decision_log'))
    stopifnot(grepl(paste0("'",table,"'"), sql, fixed=TRUE))
  stopifnot(grepl('ENABLE ROW LEVEL SECURITY',sql,fixed=TRUE),
            grepl('REVOKE ALL ON TABLE public.%I FROM anon, authenticated',sql,fixed=TRUE),
            grepl('GRANT ALL ON TABLE public.%I TO service_role',sql,fixed=TRUE))
})
check('team history is scoped, backfilled, and rejects mismatched league writes', {
  stopifnot(grepl('user_team_history ADD COLUMN IF NOT EXISTS championship_id',sql,fixed=TRUE),
            grepl('SET championship_id = team.championship_id',sql,fixed=TRUE),
            grepl('CHECK (championship_id IS NOT NULL) NOT VALID',sql,fixed=TRUE),
            grepl('NEW.championship_id IS DISTINCT FROM owner_championship',sql,fixed=TRUE),
            grepl('BEFORE INSERT OR UPDATE ON public.user_team_history',sql,fixed=TRUE))
})
check('league player roles and ratings use composite-scoped daily snapshots', {
  for (column in c('role','role2','primary_role','rating'))
    stopifnot(grepl(paste('player_daily_snapshots ADD COLUMN IF NOT EXISTS',column),sql,fixed=TRUE))
  stopifnot(!grepl('ALTER TABLE public.players ADD COLUMN',sql,fixed=TRUE),
            !grepl('ALTER TABLE public.real_clubs ADD COLUMN',sql,fixed=TRUE))
})
check('serial sequence privileges are scoped to owned history sequences', {
  stopifnot(grepl("'user_smart_alerts', 'transfer_scenarios'",sql,fixed=TRUE),
            grepl('pg_get_serial_sequence',sql,fixed=TRUE),
            grepl('GRANT USAGE, SELECT ON SEQUENCE',sql,fixed=TRUE),
            !grepl('ALL SEQUENCES IN SCHEMA',sql,fixed=TRUE))
})
check('legacy adoption matches every semantic field and requires one exact candidate', {
  for (column in c('championship_id','player_id','buyer_team_id','seller_team_id','price','is_clause','transaction_date'))
    stopifnot(grepl(paste0(column,' IS NOT DISTINCT FROM NEW.',column),sql,fixed=TRUE))
  stopifnot(grepl("source_event_id IS NULL AND provenance = 'legacy'",sql,fixed=TRUE),
            grepl('cardinality(candidate_ids) = 1',sql,fixed=TRUE),
            grepl('FOR UPDATE',sql,fixed=TRUE),
            grepl('RETURN NULL;',sql,fixed=TRUE))
})
check('identified events and ambiguous history retain provenance without deletion', {
  stopifnot(grepl('source_event_id = NEW.source_event_id;',sql,fixed=TRUE),
            grepl('NEW.provenance := existing_provenance',sql,fixed=TRUE),
            grepl(':legacy_adopted',sql,fixed=TRUE),grepl(':legacy_ambiguous',sql,fixed=TRUE),
            !grepl('DELETE FROM',sql,ignore.case=TRUE),
            grepl('SECURITY INVOKER',sql,fixed=TRUE))
})
cat('Final audit migration:', checks, 'static checks passed. SQL execution still requires a disposable PostgreSQL validation.\n')

-- Additive retained-history query indexes and explicit server-only grants.
-- No historical rows or provenance timestamps are rewritten.
BEGIN;
CREATE INDEX IF NOT EXISTS player_history_context_page ON public.player_history(championship_id,player_id,recorded_at,id);
CREATE INDEX IF NOT EXISTS team_history_context_page ON public.user_team_history(championship_id,recorded_at,id);
CREATE INDEX IF NOT EXISTS player_round_context_revision ON public.player_match_observations(championship_id,player_id,season,scoring_version,round,observed_at DESC);
CREATE INDEX IF NOT EXISTS market_transactions_buyer ON public.market_transactions(buyer_team_id);
CREATE INDEX IF NOT EXISTS market_transactions_seller ON public.market_transactions(seller_team_id);
CREATE INDEX IF NOT EXISTS players_club ON public.players(real_club_id);
CREATE INDEX IF NOT EXISTS user_teams_championship ON public.user_teams(championship_id);

DO $$
DECLARE tbl TEXT; seq TEXT;
BEGIN
  FOREACH tbl IN ARRAY ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history','market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations','market_observations','player_match_observations','league_rule_snapshots','observation_revisions'] LOOP
    IF to_regclass(format('public.%I',tbl)) IS NOT NULL THEN
      EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',tbl);
      EXECUTE format('REVOKE ALL ON TABLE public.%I FROM PUBLIC, anon, authenticated',tbl);
      EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE public.%I TO service_role',tbl);
      IF EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name=tbl AND column_name='id') THEN
        seq := pg_get_serial_sequence(format('public.%I',tbl),'id');
        IF seq IS NOT NULL THEN
          EXECUTE format('REVOKE ALL ON SEQUENCE %s FROM PUBLIC, anon, authenticated',seq);
          EXECUTE format('GRANT USAGE, SELECT ON SEQUENCE %s TO service_role',seq);
        END IF;
      END IF;
    END IF;
  END LOOP;
END $$;

COMMIT;

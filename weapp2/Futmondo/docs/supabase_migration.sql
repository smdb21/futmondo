-- Futmondo Insights - Supabase Migration Script
-- Run these statements in your Supabase project's SQL Editor:

-- 1. Add is_active column to user_teams (defaults to TRUE)
ALTER TABLE user_teams 
ADD COLUMN IF NOT EXISTS is_active BOOLEAN DEFAULT TRUE;

-- 2. Add round_number and active_teams_count columns to user_team_history
ALTER TABLE user_team_history 
ADD COLUMN IF NOT EXISTS round_number INTEGER,
ADD COLUMN IF NOT EXISTS active_teams_count INTEGER;
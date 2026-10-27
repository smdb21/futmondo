# League data isolation

A championship identifies a distinct Futmondo game with its own members, roster ownership, cash, points, rules, and bonuses. Shared player/club identities do not imply shared league values or scores.

Every private cache resource carries explicit account and relevant championship/team context. Historical rounds additionally carry season and scoring-version context. UI action selections belong to the same context as the displayed player and must be discarded when it changes.

Database reads are scoped to the selected league and required entity/window. Team-history writes validate ownership against the team row. Source timestamps and uncertainty remain attached to observations.

Unknown season/scoring context must not be inferred from another league. See [session state](multileague_state.md) and [database schema](database_schema.md).

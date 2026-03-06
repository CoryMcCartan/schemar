# Example: Election data schema (enightmodels)
#
# Based on the tidy election data format documented in:
#   /Users/cmccartan/Documents/Consulting/cbs/enightmodels/inst/schema.md
#   /Users/cmccartan/Documents/Consulting/cbs/enightmodels/R/format.R
#
# Nesting structure (from check_elec_assump() in format.R):
#
#   Outer (RACE_ID): state × election × jurisdiction × contest × district
#     └── geo: geographic unit (county, precinct, etc.) within each race
#           └── (candidate, party) × time × method: fully crossed within (race, geo)
#                 stage, votes: outcome columns
#
# The sch_nest(.keys = c("candidate", "party", "time", "method")) verifies that
# the same (candidate, party, time, method) combinations appear across all geos.
# NOTE: the current schemar key check is GLOBAL (same set across ALL outer groups),
# not hierarchical (same set within each race). This means data with different
# time snapshots per race will trigger false positives. See SUGGESTIONS.md.
#
# Additional columns are permitted (sch_others()).

devtools::load_all()

# Constants (from enightmodels) ------------------------------------------------

STATES <- c(state.abb, "DC")

CONTESTS <- c(
    "pres", "sen", "house", "gov", "ag", "ltgov", "stsen", "sthouse",
    "mayor", "prop", "pres_dem", "pres_rep", "sen_dem", "sen_rep",
    "house_dem", "house_rep", "gov_dem", "gov_rep", "ag_dem", "ag_rep",
    "ltgov_dem", "ltgov_rep", "stsen_dem", "stsen_rep", "sthouse_dem",
    "sthouse_rep", "mayor_dem", "mayor_rep"
)

PARTIES <- c("dem", "rep", "ind", "oth", "all", "yes", "no")

VOTE_METHODS <- c("abs", "adv", "mail", "early", "eday", "prov", "all")

# Schema -----------------------------------------------------------------------

# stage can be integer (editorial stage code: 0, 2, 4, 6, 10) or
# character/factor (e.g. "partial", "final", "not reported")
sch_stage <- sch_custom(
    name = "stage",
    desc = "Reporting stage",
    check = function(x, type) is.integer(x) || is.character(x) || is.factor(x),
    msg = function(type) "integer stage code or character/factor stage label",
    coerce = function(x, type) x
)

schema_elec <- sch_schema(
    .desc = "Tidy election data",
    # --- Race identifier (outer key, shared across all geos/candidates/times) ---
    state = sch_factor("State USPS abbreviation", levels = STATES, strict = FALSE, missing = FALSE),
    election = sch_date("Election date", missing = FALSE),
    jurisdiction = sch_character("Jurisdiction identifier", missing = FALSE),
    contest = sch_factor("Contest type", levels = CONTESTS, strict = FALSE, missing = FALSE),
    district = sch_integer("District number", bounds = c(1, Inf)), # NA OK for statewide
    # --- Within-race: geo varies, then (candidate, party, time, method) fully crossed ---
    sch_nest(
        geo = sch_character("Geographic unit", missing = FALSE),
        sch_nest(
            # The same (candidate, party, time, method) set must appear in every geo.
            # See SUGGESTIONS.md for limitations of this global key check.
            .keys = c("candidate", "party", "time", "method"),
            candidate = sch_character("Candidate name"), # NA OK (e.g. total rows)
            party = sch_factor("Party abbreviation", levels = PARTIES, strict = FALSE, missing = FALSE),
            time = sch_datetime("Data record timestamp", missing = FALSE),
            stage = sch_stage,
            method = sch_factor("Vote method", levels = VOTE_METHODS, strict = FALSE, missing = FALSE),
            votes = sch_numeric("Vote count", bounds = c(0, Inf), missing = FALSE)
        )
    ),
    sch_others()
)

print(schema_elec)

# Compliant data ---------------------------------------------------------------
#
# Structure:
#   2 races: NJ pres (dist 1), PA pres (dist 1)
#   2 geos per race: NJ → 34001 (Atlantic Co.), 34003 (Bergen Co.)
#                    PA → 42001 (Adams Co.),    42003 (Allegheny Co.)
#   2 candidates × 2 (time, stage, method) snapshots = 4 rows per (race, geo)
#   Total: 4 × 2 geos × 2 races = 16 rows

elec_date <- as.Date("2024-11-05")
t1 <- as.POSIXct("2024-11-05 23:00:00", tz = "America/New_York") # partial eday
t2 <- as.POSIXct("2024-11-05 23:30:00", tz = "America/New_York") # certified all

make_race_geo <- function(state, geo, votes) {
    data.frame(
        state = state,
        election = elec_date,
        jurisdiction = "US",
        contest = "pres",
        district = 1L,
        geo = geo,
        candidate = c(
            "Harris, Kamala D.", "Trump, Donald J.",
            "Harris, Kamala D.", "Trump, Donald J."
        ),
        party = c("dem", "rep", "dem", "rep"),
        time = c(t1, t1, t2, t2),
        stage = c(4L, 4L, 10L, 10L),
        method = c("eday", "eday", "all", "all"),
        votes = votes,
        stringsAsFactors = FALSE
    )
}

df <- rbind(
    make_race_geo("NJ", "34001", c(12345, 9876, 18000, 15000)),
    make_race_geo("NJ", "34003", c(22000, 19000, 31000, 28000)),
    make_race_geo("PA", "42001", c(8765, 11234, 14000, 17000)),
    make_race_geo("PA", "42003", c(55000, 48000, 81000, 72000))
)

cat("\nCompliant data (", nrow(df), "rows):\n")
print(df)
cat("\nValidating compliant data...\n")
sch_validate(schema_elec, df)
cat("OK\n")

# Compliant data with character stage (alternative stage format) ---------------

df_char_stage <- df
df_char_stage$stage <- ifelse(df$stage == 4L, "partial", "certified")
cat("\nCompliant data with character stage:\n")
sch_validate(schema_elec, df_char_stage)
cat("OK\n")

# Type-level corruptions -------------------------------------------------------

cat("\n--- Corruption 1: invalid state abbreviation ---\n")
bad1 <- df
bad1$state[1] <- "XX"
tryCatch(sch_validate(schema_elec, bad1), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 2: invalid party value ---\n")
bad2 <- df
bad2$party[1] <- "lib"
tryCatch(sch_validate(schema_elec, bad2), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 3: negative vote count ---\n")
bad3 <- df
bad3$votes[1] <- -500
tryCatch(sch_validate(schema_elec, bad3), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 4: election column is character, not Date ---\n")
bad4 <- df
bad4$election <- as.character(bad4$election)
tryCatch(sch_validate(schema_elec, bad4), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 5: missing required 'votes' column ---\n")
bad5 <- df[, setdiff(names(df), "votes")]
tryCatch(sch_validate(schema_elec, bad5), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 6: NA in votes (not permitted) ---\n")
bad6 <- df
bad6$votes[1] <- NA_real_
tryCatch(sch_validate(schema_elec, bad6), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 7: NA in party (not permitted) ---\n")
bad7 <- df
bad7$party[1] <- NA_character_
tryCatch(sch_validate(schema_elec, bad7), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 8: stage is numeric (double), not integer or character ---\n")
bad8 <- df
bad8$stage <- as.numeric(bad8$stage)
tryCatch(sch_validate(schema_elec, bad8), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 9: district = 0 (below lower bound of 1) ---\n")
bad9 <- df
bad9$district[1] <- 0L
tryCatch(sch_validate(schema_elec, bad9), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 10: invalid contest value ---\n")
bad10 <- df
bad10$contest[1] <- "treas"
tryCatch(sch_validate(schema_elec, bad10), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 11: time column is Date, not POSIXct ---\n")
bad11 <- df
bad11$time <- as.Date(bad11$time)
tryCatch(sch_validate(schema_elec, bad11), error = function(e) message(conditionMessage(e)))

# Nesting-structure corruptions ------------------------------------------------
#
# These expose violations of the within-race key invariant:
#   every geo in a race must carry the exact same (candidate, party, time, method) set.

cat("\n--- Nesting 1: one geo missing a candidate (Trump absent from geo 34003) ---\n")
# Drop all Trump rows in NJ/34003 → that geo's key set differs from the others
bad_n1 <- df[!(df$geo == "34003" & df$party == "rep"), ]
tryCatch(sch_validate(schema_elec, bad_n1), error = function(e) message(conditionMessage(e)))

cat("\n--- Nesting 2: one geo has an extra time snapshot (a third reporting time) ---\n")
extra_rows <- make_race_geo("NJ", "34001", c(18500, 16000, 18500, 16000))
t3 <- as.POSIXct("2024-11-06 02:00:00", tz = "America/New_York")
extra_rows$time <- c(t3, t3, t3, t3)
extra_rows$stage <- c(10L, 10L, 10L, 10L)
extra_rows$method <- c("all", "all", "all", "all")
bad_n2 <- rbind(df, extra_rows)
tryCatch(sch_validate(schema_elec, bad_n2), error = function(e) message(conditionMessage(e)))

cat("\n--- Nesting 3: one geo missing a time snapshot (no partial/eday in geo 34003) ---\n")
# Drop all t1 rows in NJ/34003
bad_n3 <- df[!(df$geo == "34003" & df$time == t1), ]
tryCatch(sch_validate(schema_elec, bad_n3), error = function(e) message(conditionMessage(e)))

cat("\n--- Nesting 4 (LIMITATION - NOT caught): duplicate row in one geo ---\n")
# Duplicate one row: (NJ, 34001, Harris, dem, t1, eday) appears twice.
# This violates the uniqueness of (candidate, party, time, method) within a
# (race, geo), but schemar only checks single-column distinctness.
# A compound distinct check would be needed to catch this. See SUGGESTIONS.md.
bad_n4 <- rbind(df, df[1, ])
result <- tryCatch(
    {
        sch_validate(schema_elec, bad_n4)
        "OK (not caught -- see SUGGESTIONS.md for compound distinct proposal)"
    },
    error = function(e) conditionMessage(e)
)
message(result)

cat("\n--- check='names' only: catches missing columns without full validation ---\n")
bad_names_only <- df[, setdiff(names(df), c("geo", "votes"))]
tryCatch(
    sch_validate(schema_elec, bad_names_only, check = "names"),
    error = function(e) message(conditionMessage(e))
)

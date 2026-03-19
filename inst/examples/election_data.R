# Example: Election data schema (enightmodels)
#
# Based on the tidy election data format documented in:
#   /Users/cmccartan/Documents/Consulting/cbs/enightmodels/inst/schema.md
#   /Users/cmccartan/Documents/Consulting/cbs/enightmodels/R/format.R
#
# Relationship structure:
#   (state + election + jurisdiction + contest + district) / (geo * (party / candidate)) / (time * method)
#
# The RACE_ID is a compound key of (state, election, jurisdiction, contest, district).
# Within each race, geo × party × time × method are fully crossed.
# Candidate is nested within party. Stage and votes are outcome columns.

devtools::load_all()

# Constants (from enightmodels) ------------------------------------------------

STATES <- c(state.abb, "DC")

# fmt: skip
CONTESTS <- c("pres", "sen", "house", "gov", "ag", "ltgov", "stsen", "sthouse",
    "mayor", "prop", "pres_dem", "pres_rep", "sen_dem", "sen_rep", "house_dem",
    "house_rep", "gov_dem", "gov_rep", "ag_dem", "ag_rep", "ltgov_dem", "ltgov_rep",
    "stsen_dem", "stsen_rep", "sthouse_dem", "sthouse_rep", "mayor_dem", "mayor_rep")

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
    .relationships = ~ (state + election + jurisdiction + contest + district) /
        (geo * (party / candidate)) /
        (time * method),
    # --- Race identifier columns ---
    state = sch_factor("State USPS abbreviation", levels = STATES, strict = FALSE, missing = FALSE),
    election = sch_date("Election date", missing = FALSE),
    jurisdiction = sch_factor("Jurisdiction identifier", strict = FALSE, missing = FALSE),
    contest = sch_factor("Contest type", levels = CONTESTS, strict = FALSE, missing = FALSE),
    district = sch_integer("District number", bounds = c(1, Inf)), # NA OK for statewide
    # --- Within-race columns ---
    geo = sch_factor("Geographic unit", strict = FALSE, missing = FALSE),
    party = sch_factor("Party abbreviation", levels = PARTIES, strict = FALSE, missing = FALSE),
    candidate = sch_factor("Candidate name", strict = FALSE), # NA OK (e.g. total rows)
    time = sch_datetime("Data record timestamp", missing = FALSE),
    method = sch_factor("Vote method", levels = VOTE_METHODS, strict = FALSE, missing = FALSE),
    # --- Outcome columns (not in relationship formula) ---
    stage = sch_stage,
    votes = sch_numeric("Vote count", bounds = c(0, Inf), missing = FALSE),
    past = sch_numeric(
        "Past election vote baseline",
        bounds = c(0, Inf),
        missing = FALSE,
        required = FALSE
    ),
    sch_others()
)

print(schema_elec)

# Compliant data ---------------------------------------------------------------
#
# Structure:
#   2 races: NJ pres (dist 1), PA pres (dist 1)
#   2 geos per race: NJ → 34001, 34003; PA → 42001, 42003
#   2 candidates (nested within party): Harris/dem, Trump/rep
#   2 time snapshots × 2 methods = 4 rows per (race, geo, candidate)
#   Total: 2 races × 2 geos × 2 candidates × 2 times × 2 methods = 32 rows

elec_date <- as.Date("2024-11-05")
t1 <- as.POSIXct("2024-11-05 23:00:00", tz = "America/New_York")
t2 <- as.POSIXct("2024-11-05 23:30:00", tz = "America/New_York")

make_race_geo <- function(state, geo, base_votes) {
    n_t = 1L + rpois(1, 0.5)
    expand.grid(
        candidate = c("Harris, Kamala D.", "Trump, Donald J."),
        time = rep(c(t1, t2), n_t) + rpois(2 * n_t, 1000),
        method = c("eday", "all"),
        stringsAsFactors = FALSE
    ) |>
        transform(
            state = state,
            election = elec_date,
            jurisdiction = "US",
            contest = "pres",
            district = 1L,
            geo = geo,
            party = ifelse(candidate == "Harris, Kamala D.", "dem", "rep"),
            stage = ifelse(time == t1, 4L, 10L),
            votes = base_votes + seq_len(8) * 100
        )
}


df <- rbind(
    make_race_geo("NJ", "34001", 12000),
    make_race_geo("NJ", "34003", 22000),
    make_race_geo("PA", "42001", 8000),
    make_race_geo("PA", "42003", 55000)
)
rownames(df) <- NULL

cat("\nCompliant data (", nrow(df), "rows):\n")
print(head(df, 12))
cat("\nValidating compliant data...\n")
sch_validate(schema_elec, df)
cat("OK\n")

d_nj = new_sch_df(enightmodels::nj, schema_elec, class = "elec_data")
validate_sch_df(d_nj)
sch_validate(schema_elec, d_nj)

dplyr::filter(d_nj, stage == 2 | party == "dem") |> validate_sch_df()
dplyr::filter(d_nj, stage == 2 | party == "dem" | geo != "34039_020") |> validate_sch_df()

# Corruptions ------------------------------------------------------------------

cat("\n--- Corruption 1: invalid party value ---\n")
bad1 <- df
bad1$party[1] <- "lib"
tryCatch(sch_validate(schema_elec, bad1), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 2: negative vote count ---\n")
bad2 <- df
bad2$votes[1] <- -500
tryCatch(sch_validate(schema_elec, bad2), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 3: missing required 'votes' column ---\n")
bad3 <- df[, setdiff(names(df), "votes")]
tryCatch(sch_validate(schema_elec, bad3), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 4: NA in votes (not permitted) ---\n")
bad4 <- df
bad4$votes[1] <- NA_real_
tryCatch(sch_validate(schema_elec, bad4), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 5: duplicate primary key (duplicate row) ---\n")
bad5 <- rbind(df, df[1, ])
tryCatch(sch_validate(schema_elec, bad5), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 6: incomplete crossing (missing candidate in one geo) ---\n")
# Drop all Trump rows in NJ/34003 → party × candidate × time × method incomplete
bad6 <- df[!(df$geo == "34003" & df$party == "rep"), ]
tryCatch(sch_validate(schema_elec, bad6), error = function(e) message(conditionMessage(e)))
tryCatch(sch_validate(schema_elec, d_nj[-3, ]), error = function(e) message(conditionMessage(e)))

cat("\n--- Corruption 7: incomplete crossing (one geo has extra time snapshot) ---\n")
extra <- make_race_geo("NJ", "34001", 18000)
t3 <- as.POSIXct("2024-11-06 02:00:00", tz = "America/New_York")
extra$time <- t3
bad7 <- rbind(df, extra)
tryCatch(sch_validate(schema_elec, bad7), error = function(e) message(conditionMessage(e)))

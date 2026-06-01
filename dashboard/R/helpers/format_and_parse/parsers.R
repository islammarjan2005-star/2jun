

# -----------------------------------------------------------------------------
# SQL fragments for date parsing
# -----------------------------------------------------------------------------

# Quarter range like "Oct–Dec 2024" (em/en dashes normalized to "-"):
#   - split on "-", take the *second* part ("Dec 2024")
#   - normalize whitespace/case
#   - to_date('Mon YYYY')
.sql_parse_quarter_range <- function(col_name) {
  dbplyr::sql(sprintf("
    to_date(
      initcap(substr(
        btrim(split_part(
          regexp_replace(
            btrim(regexp_replace(%s::text, '\\\\s+', ' ', 'g')),
            '[\\u2013\\u2014]', '-', 'g'
          ),
          '-', 2
        )),
        1, 8
      )),
      'Mon YYYY'
    )::date
  ", col_name))
}

# Monthly "MMM YYYY" using your CAST approach (as in get_awe_table)
.sql_parse_month_yyyy_cast <- function(col_name) {
  dbplyr::sql(sprintf("CAST(%s AS DATE)", col_name))
}

# Monthly "MMM YY" using to_date with a 2-digit year mask
.sql_parse_month_yy <- function(col_name) {
  dbplyr::sql(sprintf("
    to_date(
      initcap(btrim(%s::text)),
      'Mon YY'
    )::date
  ", col_name))
}

# Monthly "MMM YY" carrying a trailing provisional/revised flag, e.g.
# "Dec 25 (p)" or "Mar 25 (r)". Strips a trailing " (x)" marker (any letters,
# any case) before the 2-digit-year parse. Character classes are used instead
# of backslash escapes so the pattern is independent of standard_conforming_strings.
.sql_parse_month_yy_flagged <- function(col_name) {
  dbplyr::sql(sprintf("
    to_date(
      initcap(btrim(
        regexp_replace(%s::text, '[[:space:]]*[(][[:alpha:]]+[)][[:space:]]*$', '', 'g')
      )),
      'Mon YY'
    )::date
  ", col_name))
}

.sql_cast_date <- function(col_name) {
  dbplyr::sql(sprintf("CAST(%s AS DATE)", col_name))
}


# Router by descriptive parse_mode
.date_sql_for <- function(parse_mode, col_name) {
  parse_mode <- match.arg(parse_mode, c(
                                          "none",
                                          "CAST_DATE",
                                          "MMM-MMM YYYY",
                                          "MMM YYYY",
                                          "MMM YY",
                                          "MMM YY (P/R)"
                                        ))

  switch(
    parse_mode,
    "none"         = NULL,
    "MMM-MMM YYYY" = .sql_parse_quarter_range(col_name),
    "MMM YYYY"     = .sql_parse_month_yyyy_cast(col_name),
    "MMM YY"       = .sql_parse_month_yy(col_name),
    "MMM YY (P/R)" = .sql_parse_month_yy_flagged(col_name),
    "CAST_DATE"    = .sql_cast_date(col_name)
  )
}

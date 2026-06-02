

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

# Monthly "MMM YY" (e.g. "Mar 88", "Dec 25", "Dec 25 (p)") -> date.
#
# Built WITHOUT to_date(), which guesses leniently and returns nonsense on
# messy inputs. Two real-world quirks in the ONS workforce-jobs source are
# handled here:
#   * provisional "(p)" / revised "(r)" flags, e.g. "Dec 25 (p)"
#   * a footnote digit appended to the period, e.g. "Mar 203" (= "Mar 20")
#     or "Mar 194" (= "Mar 19") -- the trailing 3/4 is a flattened superscript
#     footnote marker, NOT part of the year.
# Approach:
#   - normalise whitespace
#   - token 1 = month name -> map first 3 letters to a month number
#   - token 2 = year -> keep only digits; if 4+ digits use as-is, otherwise
#     take the FIRST TWO digits (drops the stray footnote digit) and apply the
#     century pivot (<= 50 -> 20xx, else 19xx)
#   - assemble with make_date(year, month, 1)
# Because only the first two tokens are read and non-digits/extra year digits
# are dropped, "(p)"/"(r)" flags and footnote digits are both ignored.
.sql_parse_month_yy <- function(col_name) {
  clean <- sprintf("btrim(regexp_replace(%s::text, '[[:space:]]+', ' ', 'g'))", col_name)
  mon   <- sprintf("lower(left(split_part(%s, ' ', 1), 3))", clean)
  yrd   <- sprintf("regexp_replace(split_part(%s, ' ', 2), '[^0-9]', '', 'g')", clean)
  y2    <- sprintf("nullif(left(%s, 2), '')", yrd)
  y4    <- sprintf("nullif(left(%s, 4), '')", yrd)
  lenx  <- sprintf("length(%s)", yrd)

  dbplyr::sql(sprintf("
    make_date(
      CASE
        WHEN %s >= 4          THEN (%s)::int
        WHEN (%s)::int <= 50  THEN 2000 + (%s)::int
        ELSE                       1900 + (%s)::int
      END,
      CASE %s
        WHEN 'jan' THEN 1  WHEN 'feb' THEN 2  WHEN 'mar' THEN 3
        WHEN 'apr' THEN 4  WHEN 'may' THEN 5  WHEN 'jun' THEN 6
        WHEN 'jul' THEN 7  WHEN 'aug' THEN 8  WHEN 'sep' THEN 9
        WHEN 'oct' THEN 10 WHEN 'nov' THEN 11 WHEN 'dec' THEN 12
      END,
      1
    )
  ", lenx, y4, y2, y2, y2, mon))
}

# Same as .sql_parse_month_yy (kept as a named mode for clarity); both strip
# any provisional/revised "(x)" marker before the 2-digit-year parse.
.sql_parse_month_yy_flagged <- function(col_name) {
  .sql_parse_month_yy(col_name)
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

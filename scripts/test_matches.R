
# Test av matchschema
# SHL och HockeyAllsvenskan

library(fastRhockey)

today <- as.Date(
  format(Sys.time(), tz = "Europe/Stockholm", format = "%Y-%m-%d")
)

end_date <- today + 7

current_year <- as.integer(format(today, "%Y"))
current_month <- as.integer(format(today, "%m"))

season <- current_year + ifelse(current_month >= 7, 1, 0)

cat("Dagens datum:", as.character(today), "\n")
cat("Säsong:", season, "\n")

# =====================================
# HOCKEYALLSVENSKAN
# =====================================

cat("\n===== HOCKEYALLSVENSKAN =====\n")

ha <- ha_schedule()

ha_time <- as.POSIXct(
  ha$scheduled_date_time,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC"
)

ha_date <- as.Date(
  format(ha_time, tz = "Europe/Stockholm", format = "%Y-%m-%d")
)

ha_upcoming <- ha[
  !is.na(ha_date) &
  ha_date >= today &
  ha_date <= end_date,
]

cat("Matcher kommande 7 dagar:", nrow(ha_upcoming), "\n")

if (nrow(ha_upcoming) > 0) {

  print(
    data.frame(
      Datum = ha_date[!is.na(ha_date) &
        ha_date >= today & ha_date <= end_date],
      Hemmalag = ha_upcoming$home_team_name,
      Bortalag = ha_upcoming$away_team_name
    )
  )
}

# =====================================
# SHL
# =====================================

cat("\n===== SHL =====\n")

shl <- shl_schedule(
  season = season,
  game_type = "regular"
)

shl_date <- as.Date(
  substr(shl$start_date_time, 1, 10)
)

shl_upcoming <- shl[
  !is.na(shl_date) &
  shl_date >= today &
  shl_date <= end_date,
]

cat("Matcher kommande 7 dagar:", nrow(shl_upcoming), "\n")

if (nrow(shl_upcoming) > 0) {

  print(
    data.frame(
      Datum = shl_date[!is.na(shl_date) &
        shl_date >= today & shl_date <= end_date],
      Hemmalag = shl_upcoming$home_team_names_short,
      Bortalag = shl_upcoming$away_team_names_short
    )
  )
}

cat("\n===== TEST KLART =====\n")

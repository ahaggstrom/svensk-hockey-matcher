
# Test av spelarporträtt från HockeyAllsvenskan
# Vi använder medium-storlek.

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install("sportsdataverse/fastRhockey")

players <- fastRhockey::ha_players(
  team = "AIK"
)

# Visa spelare som har medium-porträtt
result <- players[
  !is.na(players$headshots_medium) &
  nzchar(players$headshots_medium),
  c("first_name", "family_name", "headshots_medium")
]

print(result)

# Spara listan med bildadresser
dir.create("players/HA/AIK", recursive = TRUE,
           showWarnings = FALSE)

write.csv(
  result,
  "players/HA/AIK/portraits.csv",
  row.names = FALSE
)

cat("Test klart! Antal porträtt:",
    nrow(result), "\n")

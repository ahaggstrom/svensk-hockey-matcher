
# Test av spelarporträtt
# HockeyAllsvenskan - AIK

library(fastRhockey)

cat("Hämtar AIK:s spelartrupp...\n")

players <- ha_players(team = "AIK")

cat("Antal spelare:", nrow(players), "\n")

cat("\nTillgängliga kolumner:\n")
print(names(players))

cat("\nExempel på spelarporträtt:\n")

print(
  players[, c(
    "first_name",
    "family_name",
    "headshots_large",
    "team_logo_url"
  )]
)

cat("\nTestet är klart!\n")

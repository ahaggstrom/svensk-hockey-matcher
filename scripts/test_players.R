
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

# Testa om bildservern levererar bilder

urls <- unique(na.omit(players$headshots_large))

cat("\nAntal tillgängliga bildadresser:", length(urls), "\n")
cat("\nTestar bildservern...\n")

if (length(urls) == 0) {

  cat("Inga bildadresser hittades!\n")

} else {

  for (i in seq_len(min(5, length(urls)))) {

    tryCatch({

      response <- curl::curl_fetch_memory(urls[i])

      cat(
        "Bild", i,
        "| HTTP:", response$status_code,
        "| Typ:", response$type,
        "| Storlek:", length(response$content), "bytes\n"
      )

    }, error = function(e) {

      cat(
        "Bild", i,
        "| FEL:", conditionMessage(e), "\n"
      )

    })
  }
}

cat("\nBildtestet är klart!\n")

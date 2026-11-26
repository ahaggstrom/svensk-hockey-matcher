
# Ladda ner spelarporträtt och laglogotyp
# HockeyAllsvenskan - AIK

library(fastRhockey)
library(curl)
library(magick)

team <- "AIK"

cat("Hämtar spelartrupp:", team, "\n")

players <- ha_players(team = team)

folder <- file.path("players", "HA", team)

dir.create(
  folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# Funktion för att ladda ner och konvertera bilder till PNG

download_png <- function(url, destination) {

  if (is.na(url) || url == "") {
    return(FALSE)
  }

  tryCatch({

    response <- curl::curl_fetch_memory(url)

    if (response$status_code != 200) {
      return(FALSE)
    }

    img <- magick::image_read(response$content)

    magick::image_write(
      img,
      path = destination,
      format = "png"
    )

    TRUE

  }, error = function(e) {

    cat("Kunde inte ladda ner:", destination, "\n")
    FALSE

  })
}

# Hämta lagets logotyp

logo_url <- players$team_logo_url[!is.na(players$team_logo_url)][1]

cat("Hämtar laglogotyp...\n")

download_png(
  logo_url,
  file.path(folder, "team-logo.png")
)

# Hämta spelarporträtt

downloaded <- 0

for (i in seq_len(nrow(players))) {

  player <- players[i, ]

  url <- player$headshots_large

  if (is.na(url) || url == "") {
    next
  }

  player_id <- as.character(player$player_cms_id)

  if (is.na(player_id) || player_id == "") {
    player_id <- as.character(i)
  }

  filename <- paste0(player_id, ".png")

  destination <- file.path(folder, filename)

  cat(
    "Hämtar:",
    player$first_name,
    player$family_name,
    "\n"
  )

  if (download_png(url, destination)) {
    downloaded <- downloaded + 1
  }
}

cat("\n-------------------------\n")
cat("KLART!\n")
cat("Lag:", team, "\n")
cat("Antal spelare:", nrow(players), "\n")
cat("Nedladdade porträtt:", downloaded, "\n")
cat("Mapp:", folder, "\n")
cat("-------------------------\n")

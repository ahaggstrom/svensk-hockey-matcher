
# Automatisk hämtning av spelarporträtt
# SHL och HockeyAllsvenskan

library(fastRhockey)
library(curl)
library(magick)

# Aktuellt säsongsår
current_year <- as.integer(format(Sys.Date(), "%Y"))
current_month <- as.integer(format(Sys.Date(), "%m"))

season <- current_year + ifelse(current_month >= 7, 1, 0)

cat("Säsong:", season, "\n")

# Funktion för att ladda ner och konvertera bilder

download_png <- function(url, destination) {

  if (length(url) == 0 || is.na(url) || url == "") {
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
    cat("Bildfel:", conditionMessage(e), "\n")
    FALSE
  })
}

# Skapa säker mappnamn

safe_folder <- function(name) {
  gsub("[^[:alnum:]_-]", "_", name)
}

# =====================================
# HOCKEYALLSVENSKAN
# =====================================

cat("\n============================\n")
cat("HOCKEYALLSVENSKAN\n")
cat("============================\n")

ha_schedule_data <- ha_schedule()

ha_teams <- unique(c(
  as.character(ha_schedule_data$home_team_code),
  as.character(ha_schedule_data$away_team_code)
))

ha_teams <- ha_teams[
  !is.na(ha_teams) & nzchar(ha_teams)
]

cat("Antal lag:", length(ha_teams), "\n")

for (team in ha_teams) {

  cat("\nHämtar:", team, "\n")

  tryCatch({

    players <- ha_players(team = team)

    folder <- file.path(
      "players",
      "HA",
      safe_folder(team)
    )

    dir.create(
      folder,
      recursive = TRUE,
      showWarnings = FALSE
    )

    # Laglogotyp

    logo_url <- players$team_logo_url[
      !is.na(players$team_logo_url) &
      nzchar(players$team_logo_url)
    ]

    if (length(logo_url) > 0) {
      download_png(
        logo_url[1],
        file.path(folder, "team-logo.png")
      )
    }

    # Spelarporträtt

    downloaded <- 0

    for (i in seq_len(nrow(players))) {

      url <- players$headshots_medium[i]

      if (is.na(url) || url == "") {
        next
      }

      player_id <- as.character(players$player_cms_id[i])

      if (is.na(player_id) || player_id == "") {
        player_id <- as.character(i)
      }

      destination <- file.path(
        folder,
        paste0(player_id, ".png")
      )

      if (download_png(url, destination)) {
        downloaded <- downloaded + 1
      }
    }

    cat(
      "Klart:", team,
      "| Spelare:", nrow(players),
      "| Bilder:", downloaded, "\n"
    )

  }, error = function(e) {

    cat("Fel för", team, ":", conditionMessage(e), "\n")

  })
}

# =====================================
# SHL
# =====================================

cat("\n============================\n")
cat("SHL\n")
cat("============================\n")

shl_schedule_data <- shl_schedule(season = season)

shl_teams <- unique(rbind(
  data.frame(
    team_uuid = shl_schedule_data$home_team_uuid,
    team_code = shl_schedule_data$home_team_names_code
  ),
  data.frame(
    team_uuid = shl_schedule_data$away_team_uuid,
    team_code = shl_schedule_data$away_team_names_code
  )
))

shl_teams <- shl_teams[
  !is.na(shl_teams$team_uuid) &
  !is.na(shl_teams$team_code),
]

shl_teams <- shl_teams[
  !duplicated(shl_teams$team_uuid),
]

cat("Antal lag:", nrow(shl_teams), "\n")

for (i in seq_len(nrow(shl_teams))) {

  team_uuid <- shl_teams$team_uuid[i]
  team_code <- as.character(shl_teams$team_code[i])

  cat("\nHämtar:", team_code, "\n")

  tryCatch({

    players <- shl_roster(team_uuid = team_uuid)

    folder <- file.path(
      "players",
      "SHL",
      safe_folder(team_code)
    )

    dir.create(
      folder,
      recursive = TRUE,
      showWarnings = FALSE
    )

    downloaded <- 0

    for (j in seq_len(nrow(players))) {

      url <- players$portrait_url[j]

      if (is.na(url) || url == "") {
        next
      }

      player_id <- as.character(players$uuid[j])

      destination <- file.path(
        folder,
        paste0(player_id, ".png")
      )

      if (download_png(url, destination)) {
        downloaded <- downloaded + 1
      }
    }

    cat(
      "Klart:", team_code,
      "| Spelare:", nrow(players),
      "| Bilder:", downloaded, "\n"
    )

  }, error = function(e) {

    cat(
      "Fel för", team_code,
      ":", conditionMessage(e), "\n"
    )

  })
}

cat("\n============================\n")
cat("ALLA LAG BEHANDLADE!\n")
cat("============================\n")

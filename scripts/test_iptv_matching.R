
library(curl)
library(jsonlite)

host <- sub("/+$", "", Sys.getenv("IPTV_HOST"))
username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

get_api <- function(action) {
  url <- paste0(
    host,
    "/player_api.php?username=",
    curl_escape(username),
    "&password=",
    curl_escape(password),
    "&action=",
    action
  )

  response <- curl_fetch_memory(url)

  if (response$status_code != 200) {
    stop("API-fel: HTTP ", response$status_code)
  }

  fromJSON(rawToChar(response$content), simplifyVector = FALSE)
}

normalize <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

cat("Hämtar IPTV-kanaler...\n")

channels <- get_api("get_live_streams")

target_ids <- c("462", "1416")

teams <- read.csv(
  "scripts/teams.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

teams$normalized <- normalize(teams$team)

cat("Läser befintliga matchbilder...\n")

image_files <- list.files(
  "players/generated",
  pattern = "\\.png$",
  recursive = TRUE,
  full.names = TRUE
)

cat("\n====================================\n")
cat("TEST AV IPTV-MATCHNING\n")
cat("====================================\n")

for (id in target_ids) {

  matching_channels <- Filter(
    function(x) identical(x$category_id, id),
    channels
  )

  cat("\nKATEGORI:", id, "\n")

  for (channel in matching_channels) {

    channel_name <- channel$name
    normalized_name <- normalize(channel_name)

    matched_teams <- teams[
      vapply(
        teams$normalized,
        function(team) grepl(team, normalized_name, fixed = TRUE),
        logical(1)
      ),
    ]

    if (nrow(matched_teams) == 2) {

      cat("\nKANAL:", channel_name, "\n")
      cat(
        "LAG:",
        paste(matched_teams$team, collapse = " - "),
        "\n"
      )

      home <- normalize(matched_teams$team[1])
      away <- normalize(matched_teams$team[2])

      candidates <- image_files[
        grepl(home, normalize(basename(image_files)), fixed = TRUE) &
        grepl(away, normalize(basename(image_files)), fixed = TRUE)
      ]

      if (length(candidates) > 0) {
        cat("MATCHBILD:", candidates[1], "\n")
      } else {
        cat("MATCHBILD: Hittades inte\n")
      }

    } else if (nrow(matched_teams) > 0) {
      cat("\nKANAL:", channel_name, "\n")
      cat("Hittade lag:", paste(matched_teams$team, collapse = ", "), "\n")
      cat("MATCHNING: Ofullständig\n")
    }
  }
}

cat("\nTEST KLART\n")

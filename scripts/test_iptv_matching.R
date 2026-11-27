
library(curl)
library(jsonlite)

# IPTV-inställningar
host <- sub("/+$", "", Sys.getenv("IPTV_HOST"))
username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

if (host == "" || username == "" || password == "") {
  stop("IPTV Secrets saknas.")
}

# Hämta data från Strong 8K
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

# Normalisera namn: å, ä och ö blir a och o
normalize <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Skapa alternativa namnformer för lagen
team_aliases <- function(team) {
  full <- normalize(team)

  core <- full
  core <- sub("^(IF|IK) ", "", core)
  core <- sub(" (IF|IK|HC|BK|SK|HF|HK|IS|HOCKEY)$", "", core)

  aliases <- unique(c(full, core))

  if (full == "MODO HOCKEY") {
    aliases <- c(aliases, "MODO")
  }

  aliases[nchar(aliases) > 0]
}

# Kontrollera att ett lagnamn finns som separat fras
contains_phrase <- function(text, phrase) {
  padded_text <- paste0(" ", text, " ")
  padded_phrase <- paste0(" ", phrase, " ")

  grepl(padded_phrase, padded_text, fixed = TRUE)
}

# Matcha lag i kanalnamnet
find_teams <- function(channel_name, teams) {
  normalized_channel <- normalize(channel_name)

  matched <- which(vapply(seq_len(nrow(teams)), function(i) {
    any(vapply(
      team_aliases(teams$team[i]),
      function(alias) contains_phrase(normalized_channel, alias),
      logical(1)
    ))
  }, logical(1)))

  # Ta bort kortare lagnamn som ingår i ett längre lagnamn.
  # Exempel: AIK ska inte räknas separat i Skellefteå AIK.
  if (length(matched) > 1) {
    remove <- logical(length(matched))

    for (i in seq_along(matched)) {
      aliases_i <- team_aliases(teams$team[matched[i]])

      for (j in seq_along(matched)) {
        if (i == j) next

        aliases_j <- team_aliases(teams$team[matched[j]])

        if (any(vapply(aliases_i, function(a) {
          any(vapply(aliases_j, function(b) {
            nchar(b) > nchar(a) &&
              contains_phrase(b, a)
          }, logical(1)))
        }, logical(1)))) {
          remove[i] <- TRUE
        }
      }
    }

    matched <- matched[!remove]
  }

  teams$team[matched]
}

# Hitta datum i kanal- eller bildnamn
get_date_key <- function(x) {
  x <- normalize(x)

  months <- c(
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  )

  for (month in months) {
    pattern <- paste0("(?:^| )([0-9]{1,2}) ", month, "(?: |$)|(?:^| )", month, " ([0-9]{1,2})(?: |$)")
    match <- regmatches(x, regexpr(pattern, x))

    if (length(match) && nchar(match) > 0) {
      numbers <- regmatches(match, gregexpr("[0-9]{1,2}", match))[[1]]

      if (length(numbers) > 0) {
        return(paste(month, as.integer(numbers[1])))
      }
    }
  }

  NA_character_
}

# Gör kanalnamnet säkert som filnamn
safe_filename <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[<>:\"/\\\\|?*]", " ", x)
  x <- gsub("[^A-Za-z0-9._ -]", " ", x)
  x <- trimws(gsub("[[:space:]]+", "_", x))
  x <- gsub("_+", "_", x)

  # Begränsa längden på filnamnet
  substr(x, 1, 180)
}

cat("Hämtar IPTV-kanaler...\n", flush = TRUE)
channels <- get_api("get_live_streams")

cat("Läser laglistan...\n", flush = TRUE)
teams <- read.csv(
  "scripts/teams.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

if (!all(c("league", "team") %in% names(teams))) {
  stop("teams.csv måste innehålla kolumnerna league och team.")
}

cat("Letar efter befintliga matchbilder...\n", flush = TRUE)

image_files <- list.files(
  "players/generated",
  pattern = "\\.png$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(image_files) == 0) {
  stop("Inga befintliga matchbilder hittades.")
}

# Endast dessa två IPTV-kategorier
target_ids <- c("462", "1416")

created <- 0
skipped <- 0

cat("\n====================================\n")
cat("SKAPAR IPTV-BILDKOPIOR\n")
cat("====================================\n")

for (id in target_ids) {

  matching_channels <- Filter(
    function(x) identical(as.character(x$category_id), id),
    channels
  )

  cat("\nKategori:", id, "\n")

  for (channel in matching_channels) {

    channel_name <- channel$name
    matched_teams <- find_teams(channel_name, teams)

    # Vi kräver exakt två identifierade lag
    if (length(matched_teams) != 2) {
      next
    }

    # Hitta bildfiler som innehåller båda lagen
    candidates <- image_files[vapply(image_files, function(file) {
      filename <- normalize(basename(file))

      all(vapply(matched_teams, function(team) {
        any(vapply(
          team_aliases(team),
          function(alias) contains_phrase(filename, alias),
          logical(1)
        ))
      }, logical(1)))
    }, logical(1))]

    # Om flera bilder finns, använd kanalens datum
    if (length(candidates) > 1) {
      channel_date <- get_date_key(channel_name)

      if (!is.na(channel_date)) {
        date_matches <- candidates[vapply(candidates, function(file) {
          identical(get_date_key(basename(file)), channel_date)
        }, logical(1))]

        if (length(date_matches) > 0) {
          candidates <- date_matches
        }
      }
    }

    if (length(candidates) != 1) {
      cat("\nHOPPAR ÖVER:", channel_name, "\n")
      cat("Identifierade lag:", paste(matched_teams, collapse = " - "), "\n")
      cat("Möjliga matchbilder:", length(candidates), "\n")
      skipped <- skipped + 1
      next
    }

    source_file <- candidates[1]

    # Skapa kopian i samma mapp som originalbilden
    target_file <- file.path(
      dirname(source_file),
      paste0(safe_filename(channel_name), ".png")
    )

    if (file.exists(target_file)) {
      cat("\nFINNS REDAN:", basename(target_file), "\n")
      skipped <- skipped + 1
      next
    }

    success <- file.copy(
      source_file,
      target_file,
      overwrite = FALSE
    )

    if (success) {
      cat("\nSKAPAD:", basename(target_file), "\n")
      cat("Original:", basename(source_file), "\n")
      created <- created + 1
    } else {
      cat("\nFEL VID KOPIERING:", channel_name, "\n")
      skipped <- skipped + 1
    }
  }
}

cat("\n====================================\n")
cat("KLART\n")
cat("Nya bildkopior:", created, "\n")
cat("Överhoppade kanaler:", skipped, "\n")
cat("====================================\n")

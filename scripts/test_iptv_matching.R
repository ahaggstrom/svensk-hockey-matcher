
library(curl)
library(jsonlite)

# --------------------------------------------------
# INSTÄLLNINGAR
# --------------------------------------------------

host <- sub("/+$", "", Sys.getenv("IPTV_HOST"))
username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

if (host == "" || username == "" || password == "") {
  stop("IPTV Secrets saknas.")
}

generated_dir <- "players/generated"
target_categories <- c("462", "1416")

# --------------------------------------------------
# API
# --------------------------------------------------

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

# --------------------------------------------------
# NORMALISERING
# --------------------------------------------------

normalize <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

safe_filename <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[<>:\"/\\\\|?*]", " ", x)
  x <- gsub("[^A-Za-z0-9._ -]", " ", x)
  x <- trimws(gsub("[[:space:]]+", "_", x))
  x <- gsub("_+", "_", x)
  substr(x, 1, 180)
}

contains_phrase <- function(text, phrase) {
  grepl(
    paste0(" ", phrase, " "),
    paste0(" ", text, " "),
    fixed = TRUE
  )
}

# --------------------------------------------------
# KOPPLING MELLAN LAG OCH BILDFILENS NAMN
# --------------------------------------------------

team_mapping <- list(
  "BJORKLOVEN" = c("BJORKLOVEN", "IF BJORKLOVEN"),
  "BRYNAS" = c("BRYNAS", "BRYNAS IF"),
  "DJURGARDEN" = c("DJURGARDEN", "DJURGARDENS", "DJURGARDENS IF"),
  "FROLUNDA" = c("FROLUNDA", "FROLUNDA HC"),
  "FARJESTAD" = c("FARJESTAD", "FARJESTAD BK"),
  "HV71" = c("HV71", "HV 71"),
  "MALMO" = c("MALMO", "MALMO REDHAWKS", "MALMO IF"),
  "LINKOPING" = c("LINKOPING", "LINKOPING HC"),
  "LULEA" = c("LULEA", "LULEA HF"),
  "ROGLE" = c("ROGLE", "ROGLE BK"),
  "SKELLEFTEA" = c("SKELLEFTEA AIK", "SKELLEFTEA"),
  "TIMRA" = c("TIMRA", "TIMRA IK"),
  "VAXJO" = c("VAXJO", "VAXJO LAKERS", "VAXJO LAKERS HC"),
  "OREBRO" = c("OREBRO", "OREBRO HK"),

  "AIK" = c("AIK"),
  "ALMTUNA" = c("ALMTUNA", "ALMTUNA IS"),
  "KARLSKOGA" = c("BIK KARLSKOGA", "KARLSKOGA"),
  "OSKARSHAMN" = c("IK OSKARSHAMN", "OSKARSHAMN"),
  "KALMAR" = c("KALMAR", "KALMAR HC"),
  "LEKSAND" = c("LEKSAND", "LEKSANDS", "LEKSANDS IF"),
  "MODO" = c("MODO", "MODO HOCKEY"),
  "MORA" = c("MORA", "MORA IK"),
  "NYBROVIKINGS" = c("NYBRO VIKINGS", "NYBRO VIKINGS IF", "NYBROVIKINGS"),
  "SODERTALJE" = c("SODERTALJE", "SODERTALJE SK"),
  "VIMMERBY" = c("VIMMERBY", "VIMMERBY HC"),
  "VISBY" = c("VISBY ROMA", "VISBY ROMA HC", "VISBY"),
  "VASTERAS" = c("VASTERAS", "VASTERAS IK"),
  "OSTERSUND" = c("OSTERSUND", "OSTERSUNDS", "OSTERSUNDS IK")
)

# --------------------------------------------------
# HITTA LAGEN I KANALNAMNET
# --------------------------------------------------

find_teams <- function(channel_name) {
  channel <- normalize(channel_name)

  matched <- names(team_mapping)[vapply(team_mapping, function(aliases) {
    any(vapply(aliases, function(alias) {
      contains_phrase(channel, normalize(alias))
    }, logical(1)))
  }, logical(1))]

  # Skellefteå AIK får inte räknas som Skellefteå + AIK.
  if ("SKELLEFTEA" %in% matched) {
    matched <- setdiff(matched, "AIK")
  }

  matched
}

# --------------------------------------------------
# DATUMHANTERING
# --------------------------------------------------

get_date_key <- function(x) {
  x <- normalize(x)

  months <- c(
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  )

  for (month in months) {
    pattern <- paste0(
      "(^| )", month, " ([0-9]{1,2})( |$)|",
      "(^| )([0-9]{1,2}) ", month, "( |$)"
    )

    match <- regmatches(x, regexpr(pattern, x))

    if (length(match) && nchar(match) > 0) {
      numbers <- regmatches(
        match,
        gregexpr("[0-9]{1,2}", match)
      )[[1]]

      if (length(numbers) > 0) {
        return(paste(month, as.integer(numbers[1])))
      }
    }
  }

  NA_character_
}

# --------------------------------------------------
# HÄMTA ORIGINALBILDER
# --------------------------------------------------

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

cat("Letar efter originalbilder...\n", flush = TRUE)

all_images <- list.files(
  generated_dir,
  pattern = "\\.png$",
  recursive = TRUE,
  full.names = TRUE
)

# Endast originalbilder med formatet:
# LAG1_LAG2_Mon_DD.png
month_pattern <- paste(
  c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  collapse = "|"
)

original_pattern <- paste0(
  "^[A-Z0-9]+_[A-Z0-9]+_(",
  month_pattern,
  ")_[0-9]{1,2}\\.png$"
)

image_files <- all_images[
  grepl(
    original_pattern,
    basename(all_images),
    ignore.case = TRUE
  )
]

if (length(image_files) == 0) {
  stop("Inga originalbilder hittades.")
}

cat("Hittade", length(image_files), "originalbilder.\n")

# --------------------------------------------------
# MATCHA KANALER OCH SKAPA BILDKOPIOR
# --------------------------------------------------

created <- 0
updated <- 0
skipped <- 0

cat("\n====================================\n")
cat("SKAPAR IPTV-BILDKOPIOR\n")
cat("====================================\n")

for (id in target_categories) {

  matching_channels <- Filter(
    function(x) identical(as.character(x$category_id), id),
    channels
  )

  cat("\nKategori:", id, "\n")

  for (channel in matching_channels) {

    channel_name <- channel$name
    matched_teams <- find_teams(channel_name)

    if (length(matched_teams) != 2) {
      next
    }

    # Hitta originalbilder där båda lagens filnamnstoken finns.
    candidates <- image_files[vapply(image_files, function(file) {

      filename <- toupper(basename(file))
      filename <- sub("\\.PNG$", "", filename)

      all(vapply(matched_teams, function(team) {
        grepl(
          paste0("(^|_)", team, "(_|$)"),
          filename
        )
      }, logical(1)))

    }, logical(1))]

    # Matcha även datumet från IPTV-kanalnamnet.
    channel_date <- get_date_key(channel_name)

    if (!is.na(channel_date)) {
      date_matches <- candidates[vapply(candidates, function(file) {
        identical(
          get_date_key(basename(file)),
          channel_date
        )
      }, logical(1))]

      if (length(date_matches) > 0) {
        candidates <- date_matches
      } else {
        candidates <- character(0)
      }
    }

    if (length(candidates) != 1) {
      cat("\nHOPPAR ÖVER:", channel_name, "\n")
      cat("Identifierade lag:", paste(matched_teams, collapse = " - "), "\n")
      cat("Möjliga originalbilder:", length(candidates), "\n")
      skipped <- skipped + 1
      next
    }

    source_file <- candidates[1]

    target_file <- file.path(
      dirname(source_file),
      paste0(safe_filename(channel_name), ".png")
    )

    existed <- file.exists(target_file)

    success <- file.copy(
      source_file,
      target_file,
      overwrite = TRUE
    )

    if (success) {

      if (existed) {
        updated <- updated + 1
        cat("\nUPPDATERAD:", basename(target_file), "\n")
      } else {
        created <- created + 1
        cat("\nSKAPAD:", basename(target_file), "\n")
      }

      cat("Original:", basename(source_file), "\n")

    } else {
      cat("\nFEL VID KOPIERING:", channel_name, "\n")
      skipped <- skipped + 1
    }
  }
}

# --------------------------------------------------
# RESULTAT
# --------------------------------------------------

cat("\n====================================\n")
cat("KLART\n")
cat("Nya bildkopior:", created, "\n")
cat("Uppdaterade bildkopior:", updated, "\n")
cat("Överhoppade kanaler:", skipped, "\n")
cat("====================================\n")

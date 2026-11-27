
library(fastRhockey)
library(magick)
library(curl)

# --------------------------------------------------
# INSTÄLLNINGAR
# --------------------------------------------------

today <- as.Date(format(
  Sys.time(),
  tz = "Europe/Stockholm",
  format = "%Y-%m-%d"
))

end_date <- today + 7

width <- 1280
height <- 720

generated_dir <- file.path("players", "generated")

dir.create(generated_dir, recursive = TRUE, showWarnings = FALSE)

# PNG-komprimering
png_defines <- c("png:compression-level" = "9")

# --------------------------------------------------
# RENSNING AV GAMLA BILDER
# Tar bort genererade bilder äldre än 7 dagar
# --------------------------------------------------

cleanup_old_images <- function() {

  if (!dir.exists(generated_dir)) return(invisible(NULL))

  files <- list.files(
    generated_dir,
    pattern = "\\.png$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) return(invisible(NULL))

  file_info <- file.info(files)

  old_files <- files[
    !is.na(file_info$mtime) &
      as.numeric(difftime(
        Sys.time(),
        file_info$mtime,
        units = "hours"
      )) > 168
  ]

  if (length(old_files) > 0) {
    file.remove(old_files)
    cat("Tog bort", length(old_files), "bilder äldre än 7 dagar.\n")
  } else {
    cat("Inga bilder äldre än 7 dagar att ta bort.\n")
  }

  # Ta bort tomma mappar
  directories <- list.dirs(
    generated_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  directories <- directories[order(nchar(directories), decreasing = TRUE)]

  for (dir in directories) {
    if (length(list.files(dir, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(dir, recursive = TRUE)
    }
  }

  invisible(NULL)
}

cleanup_old_images()

# --------------------------------------------------
# NAMNFORMATERING
# --------------------------------------------------

safe_folder <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  gsub("[^A-Z0-9_-]", "_", x)
}

safe_team_name <- function(x) {

  x <- safe_folder(x)
  x <- gsub("_+", "_", x)

  # Ta bort vanliga prefix och suffix
  x <- sub("^(IF|IK)_", "", x)
  x <- sub("_(HC|IF|IK|HF|HK|SK|IS|BK)$", "", x)

  # Specialnamn
  aliases <- c(
    "BIK_KARLSKOGA" = "KARLSKOGA",
    "MODO_HOCKEY" = "MODO",
    "MODOHOCKEY" = "MODO",
    "VAXJO_LAKERS" = "VAXJO",
    "VISBY_ROMA" = "VISBY",
    "MALMO_REDHAWKS" = "MALMO",
    "FARJESTAD" = "FARJESTAD",
    "DJURGARDENS" = "DJURGARDEN",
    "SKELLEFTEA_AIK" = "SKELLEFTEA"
  )

  if (x %in% names(aliases)) {
    x <- unname(aliases[x])
  }

  x <- gsub("[^A-Z0-9]", "", x)

  x
}

# Engelska månadsnamn oavsett systemets språk
english_months <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

make_match_filename <- function(home, away, match_datetime) {

  home <- safe_team_name(home)
  away <- safe_team_name(away)

  match_date <- as.Date(format(
    match_datetime,
    tz = "Europe/Stockholm",
    format = "%Y-%m-%d"
  ))

  month_number <- as.integer(format(match_date, "%m"))
  day_number <- format(match_date, "%d")

  paste0(
    home, "_",
    away, "_",
    english_months[month_number], "_",
    day_number,
    ".png"
  )
}

# --------------------------------------------------
# HÄMTA OCH CACHa BILDER
# --------------------------------------------------

get_image <- function(url, cache_path = NULL) {

  if (is.null(url) || length(url) == 0 || is.na(url) || url == "") {
    return(NULL)
  }

  # Använd befintlig cache
  if (!is.null(cache_path) && file.exists(cache_path)) {
    return(tryCatch(
      image_read(cache_path),
      error = function(e) NULL
    ))
  }

  # Lokal fil
  if (file.exists(url)) {
    return(tryCatch(
      image_read(url),
      error = function(e) NULL
    ))
  }

  temp_file <- tempfile()

  result <- tryCatch({

    curl_download(
      url,
      destfile = temp_file,
      quiet = TRUE
    )

    if (grepl("\\.svg($|\\?)", url, ignore.case = TRUE)) {
      img <- image_read_svg(temp_file, width = 600)
    } else {
      img <- image_read(temp_file)
    }

    if (!is.null(cache_path)) {

      dir.create(
        dirname(cache_path),
        recursive = TRUE,
        showWarnings = FALSE
      )

      image_write(
        img,
        path = cache_path,
        format = "png",
        compression = "zip",
        defines = png_defines
      )
    }

    img

  }, error = function(e) {

    cat("Kunde inte hämta bild:", url, "\n")
    cat("Fel:", conditionMessage(e), "\n")
    NULL

  })

  unlink(temp_file)

  result
}

# --------------------------------------------------
# LOGOTYPER
# --------------------------------------------------

make_white_logo <- function(img) {

  image_colorize(
    img,
    opacity = 100,
    color = "white"
  )
}

fit_logo <- function(img, max_width, max_height) {

  img <- image_trim(img)

  img <- image_resize(
    img,
    paste0(max_width, "x", max_height)
  )

  image_extent(
    img,
    geometry = paste0(max_width, "x", max_height),
    gravity = "center",
    color = "none"
  )
}

# --------------------------------------------------
# SKAPA MATCHBILD
# --------------------------------------------------

create_match_image <- function(
    league,
    home_code,
    away_code,
    home_name,
    away_name,
    match_datetime,
    home_logo_url = NULL,
    away_logo_url = NULL
) {

  cat(
    "\nSkapar bild:",
    league,
    home_name,
    "-",
    away_name,
    "\n"
  )

  # Bakgrund
  bg_path <- file.path("assets", "bg_hockey.jpg.webp")

  if (file.exists(bg_path)) {

    canvas <- image_read(bg_path) |>
      image_resize("1280x720^") |>
      image_extent("1280x720", gravity = "center")

  } else {

    canvas <- image_blank(
      width = width,
      height = height,
      color = "#101820"
    )

  }

  # ------------------------------------------------
  # LIGALOGO
  # ------------------------------------------------

  league_logo_path <- if (league == "HA") {
    file.path("assets", "HA_logo.png.png")
  } else {
    file.path("assets", "SHL_logo.svg.webp")
  }

  if (file.exists(league_logo_path)) {

    league_logo <- image_read(league_logo_path)

    # Endast SHL-logotypen ska vara vit
    if (league == "SHL") {
      league_logo <- make_white_logo(league_logo)
    }

    # HA-logotypen dubbelt så stor som tidigare
    if (league == "HA") {
      league_logo <- fit_logo(league_logo, 680, 240)
    } else {
      league_logo <- fit_logo(league_logo, 340, 120)
    }

    canvas <- image_composite(
      canvas,
      league_logo,
      operator = "over",
      gravity = "north",
      offset = "+0+65"
    )

  } else {

    cat("Hittade inte ligalogotypen:", league_logo_path, "\n")

  }

  # ------------------------------------------------
  # LAGLOGOTYPER
  # ------------------------------------------------

  home_cache <- file.path(
    "players", league, home_code, "team-logo.png"
  )

  away_cache <- file.path(
    "players", league, away_code, "team-logo.png"
  )

  home_logo <- get_image(home_logo_url, home_cache)
  away_logo <- get_image(away_logo_url, away_cache)

  if (!is.null(home_logo)) {

    home_logo <- fit_logo(home_logo, 360, 360)

    canvas <- image_composite(
      canvas,
      home_logo,
      operator = "over",
      gravity = "west",
      offset = "+140+55"
    )

  } else {

    cat("Saknar logotyp för:", home_name, "\n")

  }

  if (!is.null(away_logo)) {

    away_logo <- fit_logo(away_logo, 360, 360)

    canvas <- image_composite(
      canvas,
      away_logo,
      operator = "over",
      gravity = "east",
      offset = "+140+55"
    )

  } else {

    cat("Saknar logotyp för:", away_name, "\n")

  }

  # ------------------------------------------------
  # SPARA MATCHBILD
  # ------------------------------------------------

  match_date <- format(
    match_datetime,
    tz = "Europe/Stockholm",
    format = "%Y-%m-%d"
  )

  output_dir <- file.path(
    generated_dir,
    match_date
  )

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  filename <- make_match_filename(
    home_name,
    away_name,
    match_datetime
  )

  output_path <- file.path(output_dir, filename)

  image_write(
    canvas,
    path = output_path,
    format = "png",
    compression = "zip",
    defines = png_defines
  )

  cat("Sparad:", output_path, "\n")
}

# --------------------------------------------------
# SÄSONG
# --------------------------------------------------

year <- as.integer(format(today, "%Y"))
month <- as.integer(format(today, "%m"))

season <- year + ifelse(month >= 7, 1, 0)

cat("\nDatum:", as.character(today), "\n")
cat("Säsong:", season, "\n")

# --------------------------------------------------
# HOCKEYALLSVENSKAN
# --------------------------------------------------

cat("\nHämtar HockeyAllsvenskan...\n")

ha <- ha_schedule()

ha_datetime <- as.POSIXct(
  ha$scheduled_date_time,
  tz = "UTC"
)

ha_dates <- as.Date(format(
  ha_datetime,
  tz = "Europe/Stockholm",
  format = "%Y-%m-%d"
))

ha_matches <- which(
  !is.na(ha_dates) &
    ha_dates >= today &
    ha_dates <= end_date &
    (is.na(ha$is_completed) | !ha$is_completed)
)

if (length(ha_matches) > 0) {

  ha_matches <- ha_matches[
    order(ha_datetime[ha_matches])
  ]

  cat(
    "Hittade",
    length(ha_matches),
    "kommande HA-matcher.\n"
  )

  # TEST: endast första kommande matchen
  i <- ha_matches[1]

  create_match_image(
    league = "HA",
    home_code = safe_folder(ha$home_team_code[i]),
    away_code = safe_folder(ha$away_team_code[i]),
    home_name = ha$home_team_name[i],
    away_name = ha$away_team_name[i],
    match_datetime = ha_datetime[i],
    home_logo_url = ha$home_team_logo_url[i],
    away_logo_url = ha$away_team_logo_url[i]
  )

} else {

  cat("Inga kommande HA-matcher hittades.\n")

}

# --------------------------------------------------
# SHL
# --------------------------------------------------

cat("\nHämtar SHL...\n")

shl <- shl_schedule(
  season = season,
  game_type = "regular"
)

shl_datetime <- as.POSIXct(
  shl$start_date_time,
  tz = "Europe/Stockholm"
)

shl_dates <- as.Date(format(
  shl_datetime,
  tz = "Europe/Stockholm",
  format = "%Y-%m-%d"
))

shl_matches <- which(
  !is.na(shl_dates) &
    shl_dates >= today &
    shl_dates <= end_date &
    (is.na(shl$state) | shl$state != "post-game")
)

if (length(shl_matches) > 0) {

  shl_matches <- shl_matches[
    order(shl_datetime[shl_matches])
  ]

  cat(
    "Hittade",
    length(shl_matches),
    "kommande SHL-matcher.\n"
  )

  # TEST: endast första kommande matchen
  i <- shl_matches[1]

  create_match_image(
    league = "SHL",
    home_code = safe_folder(shl$home_team_names_code[i]),
    away_code = safe_folder(shl$away_team_names_code[i]),
    home_name = shl$home_team_names_short[i],
    away_name = shl$away_team_names_short[i],
    match_datetime = shl_datetime[i],
    home_logo_url = shl$home_team_icon[i],
    away_logo_url = shl$away_team_icon[i]
  )

} else {

  cat("Inga kommande SHL-matcher hittades.\n")

}

cat("\nKlart!\n")

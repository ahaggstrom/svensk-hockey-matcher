
library(fastRhockey)
library(magick)
library(curl)
library(rsvg)

# --------------------------------------------------
# Datum och säsong
# --------------------------------------------------

today <- as.Date(format(
  Sys.time(),
  tz = "Europe/Stockholm",
  format = "%Y-%m-%d"
))

end_date <- today + 7

year <- as.integer(format(today, "%Y"))
month <- as.integer(format(today, "%m"))

# Säsongen anges med det år den avslutas
season <- year + ifelse(month >= 7, 1, 0)

cat("Dagens datum:", as.character(today), "\n")
cat("Säsong:", season, "\n")


# --------------------------------------------------
# Hjälpfunktion för filnamn
# --------------------------------------------------

safe_folder <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  gsub("[^A-Z0-9_-]", "_", x)
}


# --------------------------------------------------
# Hämta bild
# Hanterar både lokala filer och webbadresser
# --------------------------------------------------

get_image <- function(url, cache_path = NULL) {

  if (is.null(url) || is.na(url) || !nzchar(as.character(url))) {
    return(NULL)
  }

  url <- as.character(url)

  # Om det redan är en lokal bild
  if (file.exists(url)) {
    return(tryCatch(
      image_read(url),
      error = function(e) {
        message("Kunde inte läsa lokal bild: ", url)
        NULL
      }
    ))
  }

  # Använd redan nedladdad logotyp
  if (!is.null(cache_path) && file.exists(cache_path)) {
    return(tryCatch(
      image_read(cache_path),
      error = function(e) NULL
    ))
  }

  # Hämta bild från internet
  temp_file <- tempfile()

  result <- tryCatch({

    curl_download(
      url = url,
      destfile = temp_file,
      quiet = TRUE
    )

    # SVG eller vanlig bild
    if (grepl("\\.svg($|\\?)", url, ignore.case = TRUE)) {
      img <- image_read_svg(temp_file)
    } else {
      img <- image_read(temp_file)
    }

    # Spara logotyp lokalt för framtida körningar
    if (!is.null(cache_path)) {
      dir.create(
        dirname(cache_path),
        recursive = TRUE,
        showWarnings = FALSE
      )

      image_write(
        img,
        path = cache_path,
        format = "png"
      )
    }

    img

  }, error = function(e) {
    message("Kunde inte hämta bild: ", url)
    message(conditionMessage(e))
    NULL
  })

  unlink(temp_file)

  result
}


# --------------------------------------------------
# Hämta första spelarbilden i lagets mapp
# --------------------------------------------------

get_player_image <- function(league, team_code) {

  team_folder <- file.path(
    "players",
    league,
    team_code
  )

  if (!dir.exists(team_folder)) {
    message("Saknar spelarmapp: ", team_folder)
    return(NULL)
  }

  files <- list.files(
    team_folder,
    pattern = "\\.(png|jpg|jpeg|webp)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  # Ta inte med lagets logotyp
  files <- files[
    basename(files) != "team-logo.png"
  ]

  if (length(files) == 0) {
    message("Hittade inga spelarporträtt i: ", team_folder)
    return(NULL)
  }

  get_image(files[1])
}


# --------------------------------------------------
# Anpassa bildstorlek
# --------------------------------------------------

prepare_image <- function(img, width, height) {

  if (is.null(img)) {
    return(NULL)
  }

  image_resize(
    img,
    paste0(width, "x", height, "^")
  ) |>
    image_extent(
      geometry = paste0(width, "x", height),
      gravity = "center",
      color = "none"
    )
}


# --------------------------------------------------
# Skapa matchbild
# --------------------------------------------------

create_match_image <- function(
    league,
    home_code,
    away_code,
    home_name,
    away_name,
    match_date,
    home_logo_url = NULL,
    away_logo_url = NULL
) {

  date_string <- format(match_date, "%Y-%m-%d")

  cat(
    "Skapar bild:",
    league,
    home_name,
    "-",
    away_name,
    date_string,
    "\n"
  )

  # Mappar för lagen
  home_logo_path <- file.path(
    "players", league, home_code, "team-logo.png"
  )

  away_logo_path <- file.path(
    "players", league, away_code, "team-logo.png"
  )

  # Hämta logotyper
  home_logo <- get_image(home_logo_url, home_logo_path)
  away_logo <- get_image(away_logo_url, away_logo_path)

  # Hämta spelarporträtt
  home_player <- get_player_image(league, home_code)
  away_player <- get_player_image(league, away_code)

  # Mörk bakgrund
  canvas <- image_blank(
    width = 512,
    height = 512,
    color = "#101820"
  )

  # Rubrik
  canvas <- image_annotate(
    canvas,
    text = paste(league, "MATCH"),
    gravity = "north",
    location = "+0+22",
    size = 25,
    color = "white",
    font = "DejaVu-Sans-Bold"
  )

  # Datum
  canvas <- image_annotate(
    canvas,
    text = date_string,
    gravity = "north",
    location = "+0+58",
    size = 19,
    color = "#BBBBBB"
  )

  # Logotyper
  home_logo <- prepare_image(home_logo, 110, 110)
  away_logo <- prepare_image(away_logo, 110, 110)

  if (!is.null(home_logo)) {
    canvas <- image_composite(
      canvas,
      home_logo,
      offset = "+91+112"
    )
  }

  if (!is.null(away_logo)) {
    canvas <- image_composite(
      canvas,
      away_logo,
      offset = "+311+112"
    )
  }

  # Spelarporträtt
  home_player <- prepare_image(home_player, 120, 150)
  away_player <- prepare_image(away_player, 120, 150)

  if (!is.null(home_player)) {
    canvas <- image_composite(
      canvas,
      home_player,
      offset = "+86+245"
    )
  }

  if (!is.null(away_player)) {
    canvas <- image_composite(
      canvas,
      away_player,
      offset = "+306+245"
    )
  }

  # Lagnamn
  canvas <- image_annotate(
    canvas,
    text = home_name,
    gravity = "northwest",
    location = "+35+415",
    size = 22,
    color = "white",
    font = "DejaVu-Sans-Bold"
  )

  canvas <- image_annotate(
    canvas,
    text = away_name,
    gravity = "northeast",
    location = "+35+415",
    size = 22,
    color = "white",
    font = "DejaVu-Sans-Bold"
  )

  # VS
  canvas <- image_annotate(
    canvas,
    text = "VS",
    gravity = "center",
    location = "+0+0",
    size = 25,
    color = "#F5C542",
    font = "DejaVu-Sans-Bold"
  )

  # Spara bilden i datumkatalog
  output_folder <- file.path(
    "players",
    "generated",
    date_string
  )

  dir.create(
    output_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )

  filename <- paste(
    safe_folder(home_name),
    safe_folder(away_name),
    date_string,
    sep = "_"
  )

  output_path <- file.path(
    output_folder,
    paste0(filename, ".png")
  )

  image_write(
    canvas,
    path = output_path,
    format = "png"
  )

  cat("Bild sparad:", output_path, "\n")
}


# --------------------------------------------------
# HOCKEYALLSVENSKAN
# --------------------------------------------------

cat("\nHämtar HockeyAllsvenskans spelschema...\n")

ha <- ha_schedule()

if (!is.null(ha) && nrow(ha) > 0) {

  ha_datetime <- as.POSIXct(
    ha$scheduled_date_time,
    tz = "UTC"
  )

  ha_dates <- as.Date(
    format(
      ha_datetime,
      tz = "Europe/Stockholm",
      format = "%Y-%m-%d"
    )
  )

  ha_matches <- which(
    ha_dates >= today &
    ha_dates <= end_date &
    !is.na(ha$is_completed) &
    !ha$is_completed
  )

  if (length(ha_matches) > 0) {

    i <- ha_matches[1]

    create_match_image(
      league = "HA",
      home_code = ha$home_team_code[i],
      away_code = ha$away_team_code[i],
      home_name = ha$home_team_name[i],
      away_name = ha$away_team_name[i],
      match_date = ha_dates[i],
      home_logo_url = ha$home_team_logo_url[i],
      away_logo_url = ha$away_team_logo_url[i]
    )

  } else {
    cat("Ingen kommande HA-match hittades inom perioden.\n")
  }

} else {
  cat("Kunde inte hämta HA-spelschemat.\n")
}


# --------------------------------------------------
# SHL
# --------------------------------------------------

cat("\nHämtar SHL:s spelschema...\n")

shl <- shl_schedule(
  season = season,
  game_type = "regular"
)

if (!is.null(shl) && nrow(shl) > 0) {

  shl_datetime <- as.POSIXct(
    shl$start_date_time,
    tz = "Europe/Stockholm"
  )

  shl_dates <- as.Date(
    format(
      shl_datetime,
      tz = "Europe/Stockholm",
      format = "%Y-%m-%d"
    )
  )

  shl_matches <- which(
    shl_dates >= today &
    shl_dates <= end_date &
    !is.na(shl$state) &
    shl$state != "post-game"
  )

  if (length(shl_matches) > 0) {

    i <- shl_matches[1]

    create_match_image(
      league = "SHL",

      # Lagkoder används för att hitta spelarbilder
      home_code = shl$home_team_names_code[i],
      away_code = shl$away_team_names_code[i],

      # Lagnamn används i bildtext och filnamn
      home_name = shl$home_team_names_short[i],
      away_name = shl$away_team_names_short[i],

      match_date = shl_dates[i],

      home_logo_url = shl$home_team_icon[i],
      away_logo_url = shl$away_team_icon[i]
    )

  } else {
    cat("Ingen kommande SHL-match hittades inom perioden.\n")
  }

} else {
  cat("Kunde inte hämta SHL-spelschemat.\n")
}

cat("\nKlart!\n")

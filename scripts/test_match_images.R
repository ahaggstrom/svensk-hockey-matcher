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
      img <- image_read_svg(temp_file, width = 600)
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
# Anpassa bildstorlek utan att beskära logotyper
# --------------------------------------------------

fit_logo <- function(img, max_width, max_height) {

  if (is.null(img)) {
    return(NULL)
  }

  # Trimmar bort eventuella genomskinliga marginaler runt loggan först
  img <- image_trim(img)

  # Skalar så att loggan ryms inom rutan med bibehållna proportioner
  image_resize(
    img,
    paste0(max_width, "x", max_height)
  ) |>
    image_extent(
      geometry = paste0(max_width, "x", max_height),
      gravity = "center",
      color = "none"
    )
}


# --------------------------------------------------
# Skapa matchbild (1280x720 widescreen)
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

  # Sökvägar för cachade lagloggor
  home_logo_path <- file.path(
    "players", league, home_code, "team-logo.png"
  )

  away_logo_path <- file.path(
    "players", league, away_code, "team-logo.png"
  )

  # Hämta laglogotyper
  home_logo <- get_image(home_logo_url, home_logo_path)
  away_logo <- get_image(away_logo_url, away_logo_path)

  # 1. Bakgrund (1280x720)
  bg_path <- file.path("assets", "bg_hockey.jpg")

  if (file.exists(bg_path)) {
    canvas <- image_read(bg_path) |>
      image_resize("1280x720^") |>
      image_extent("1280x720", gravity = "center")
  } else {
    message("Ingen bakgrundsbild hittades i ", bg_path, " – använder enfärgad bakgrund.")
    canvas <- image_blank(
      width = 1280,
      height = 720,
      color = "#101820"
    )
  }

  # 2. Ligalogotyp centrerad i toppen (t.ex. assets/SHL_logo.png)
  league_logo_path <- file.path("assets", paste0(league, "_logo.png"))

  if (file.exists(league_logo_path)) {
    league_logo <- image_read(league_logo_path) |>
      fit_logo(340, 120)

    canvas <- image_composite(
      canvas,
      league_logo,
      gravity = "north",
      offset = "+0+65"
    )
  } else {
    # Reserv om ligaloggan saknas: ren vit text i toppen
    canvas <- image_annotate(
      canvas,
      text = league,
      gravity = "north",
      location = "+0+70",
      size = 85,
      color = "white",
      font = "DejaVu-Sans-Bold"
    )
  }

  # 3. Stora lagloggor (360x360 px)
  home_logo <- fit_logo(home_logo, 360, 360)
  away_logo <- fit_logo(away_logo, 360, 360)

  if (!is.null(home_logo)) {
    canvas <- image_composite(
      canvas,
      home_logo,
      gravity = "west",
      offset = "+140+55"
    )
  }

  if (!is.null(away_logo)) {
    canvas <- image_composite(
      canvas,
      away_logo,
      gravity = "east",
      offset = "+140+55"
    )
  }

  # 4. Spara bilden i datumkatalog
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
      home_code = shl$home_team_names_code[i],
      away_code = shl$away_team_names_code[i],
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


# ==========================================
# TEST AV AUTOMATISK MATCHBILDGENERATOR
# SHL OCH HOCKEYALLSVENSKAN
# ==========================================

library(fastRhockey)
library(magick)
library(curl)
library(rsvg)

cat("\n===== STARTAR MATCHBILDGENERATOR =====\n")

# DATUM
today <- as.Date(format(
  Sys.time(),
  tz = "Europe/Stockholm",
  format = "%Y-%m-%d"
))

end_date <- today + 7

year <- as.integer(format(today, "%Y"))
month <- as.integer(format(today, "%m"))
season <- year + ifelse(month >= 7, 1, 0)

safe_folder <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  gsub("[^A-Z0-9_-]", "_", x)
}

# ==========================================
# HÄMTA BILD
# ==========================================

get_image <- function(url, path = NULL) {

  # Lokal bild
  if (length(url) == 1 && !is.na(url) && file.exists(url)) {
    return(tryCatch(image_read(url), error = function(e) NULL))
  }

  # Befintlig logotyp
  if (!is.null(path) && file.exists(path)) {
    result <- tryCatch(image_read(path), error = function(e) NULL)
    if (!is.null(result)) return(result)
  }

  if (length(url) == 0 || is.na(url) || url == "") return(NULL)

  tryCatch({

    response <- curl_fetch_memory(url)

    if (response$status_code != 200) return(NULL)

    is_svg <- grepl("\\.svg([?#].*)?$", url, ignore.case = TRUE)

    tmp <- tempfile(
      fileext = if (is_svg) ".svg" else ".img"
    )

    writeBin(response$content, tmp)

    if (is_svg) {
      img <- image_read_svg(tmp)
    } else {
      img <- image_read(tmp)
    }

    if (!is.null(path)) {
      image_write(img, path, format = "png")
    }

    img

  }, error = function(e) {

    cat("Kunde inte läsa bild:", conditionMessage(e), "\n")
    NULL

  })
}

# ==========================================
# SKAPA MATCHBILD
# ==========================================

create_match_image <- function(
  league,
  date,
  home_code,
  away_code,
  home_name,
  away_name,
  home_logo_url,
  away_logo_url
) {

  cat("\nSkapar:", home_name, "-", away_name, "\n")

  base_folder <- file.path("players", league)

  home_folder <- file.path(base_folder, safe_folder(home_code))
  away_folder <- file.path(base_folder, safe_folder(away_code))

  # Hämta spelarporträtt
  home_players <- list.files(
    home_folder,
    pattern = "\\.png$",
    full.names = TRUE
  )

  away_players <- list.files(
    away_folder,
    pattern = "\\.png$",
    full.names = TRUE
  )

  home_players <- home_players[
    basename(home_players) != "team-logo.png"
  ]

  away_players <- away_players[
    basename(away_players) != "team-logo.png"
  ]

  if (length(home_players) == 0 || length(away_players) == 0) {
    cat("Saknar spelarporträtt för något lag!\n")
    return(FALSE)
  }

  # Hämta logotyper
  home_logo <- get_image(
    home_logo_url,
    file.path(home_folder, "team-logo.png")
  )

  away_logo <- get_image(
    away_logo_url,
    file.path(away_folder, "team-logo.png")
  )

  # Hämta spelarporträtt
  home_player <- get_image(home_players[1])
  away_player <- get_image(away_players[1])

  if (is.null(home_player) || is.null(away_player)) {
    cat("Kunde inte läsa spelarporträtt!\n")
    return(FALSE)
  }

  # Skapa bakgrund
  canvas <- image_blank(
    width = 512,
    height = 512,
    color = "#101820"
  )

  # Ligatitel och datum
  canvas <- image_annotate(
    canvas,
    paste(league, format(date, "%Y-%m-%d")),
    gravity = "north",
    location = "+0+20",
    size = 23,
    color = "white",
    font = "DejaVu-Sans"
  )

  # Hemmalagets logotyp
  if (!is.null(home_logo)) {
    home_logo <- image_resize(home_logo, "115x115")

    canvas <- image_composite(
      canvas,
      home_logo,
      offset = "+90+75"
    )
  }

  # Bortalagets logotyp
  if (!is.null(away_logo)) {
    away_logo <- image_resize(away_logo, "115x115")

    canvas <- image_composite(
      canvas,
      away_logo,
      offset = "+307+75"
    )
  }

  # Spelarporträtt
  home_player <- image_resize(home_player, "180x230")
  away_player <- image_resize(away_player, "180x230")

  canvas <- image_composite(
    canvas,
    home_player,
    offset = "+35+210"
  )

  canvas <- image_composite(
    canvas,
    away_player,
    offset = "+297+210"
  )

  # Lagnamn
  canvas <- image_annotate(
    canvas,
    home_name,
    gravity = "southwest",
    location = "+25+15",
    size = 21,
    color = "white",
    font = "DejaVu-Sans"
  )

  canvas <- image_annotate(
    canvas,
    away_name,
    gravity = "southeast",
    location = "+25+15",
    size = 21,
    color = "white",
    font = "DejaVu-Sans"
  )

  # Spara bild
  output_folder <- file.path(
    "players",
    "generated",
    format(date, "%Y-%m-%d")
  )

  dir.create(
    output_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Filnamn: HEMMALAG_BORTALAG_DATUM
  filename <- paste(
    safe_folder(home_name),
    safe_folder(away_name),
    format(date, "%Y-%m-%d"),
    sep = "_"
  )

  output <- file.path(
    output_folder,
    paste0(filename, ".png")
  )

  image_write(
    canvas,
    path = output,
    format = "png"
  )

  cat("Bild skapad:", output, "\n")

  TRUE
}

# ==========================================
# HOCKEYALLSVENSKAN
# ==========================================

cat("\n===== HOCKEYALLSVENSKAN =====\n")

ha <- ha_schedule()

ha_time <- as.POSIXct(
  ha$scheduled_date_time,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC"
)

ha_date <- as.Date(
  format(
    ha_time,
    tz = "Europe/Stockholm",
    format = "%Y-%m-%d"
  )
)

ha_index <- which(
  !is.na(ha_date) &
    ha_date >= today &
    ha_date <= end_date &
    !is.na(ha$is_completed) &
    !ha$is_completed
)

if (length(ha_index) > 0) {

  ha_index <- ha_index[order(ha_date[ha_index])]

  i <- ha_index[1]

  create_match_image(
    "HA",
    ha_date[i],
    ha$home_team_code[i],
    ha$away_team_code[i],
    ha$home_team_name[i],
    ha$away_team_name[i],
    ha$home_team_logo_url[i],
    ha$away_team_logo_url[i]
  )

} else {

  cat("Inga kommande HA-matcher hittades.\n")

}

# ==========================================
# SHL
# ==========================================

cat("\n===== SHL =====\n")

shl <- shl_schedule(
  season = season,
  game_type = "regular"
)

shl_date <- as.Date(
  substr(shl$start_date_time, 1, 10)
)

shl_index <- which(
  !is.na(shl_date) &
    shl_date >= today &
    shl_date <= end_date &
    !is.na(shl$state) &
    shl$state != "post-game"
)

if (length(shl_index) > 0) {

  shl_index <- shl_index[order(shl_date[shl_index])]

  i <- shl_index[1]

  create_match_image(
    "SHL",
    shl_date[i],
    shl$home_team_names_code[i],
    shl$away_team_names_code[i],
    shl$home_team_names_code[i],
    shl$away_team_names_code[i],
    shl$home_team_icon[i],
    shl$away_team_icon[i]
  )

} else {

  cat("Inga kommande SHL-matcher hittades.\n")

}

# ==========================================
# KLART
# ==========================================

cat("\n===== BILDSKAPANDE KLART =====\n")

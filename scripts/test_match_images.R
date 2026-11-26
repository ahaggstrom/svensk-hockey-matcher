
# Test av automatisk matchbildsgenerator
# SHL och HockeyAllsvenskan

library(fastRhockey)
library(magick)
library(curl)

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
  gsub("[^[:alnum:]_-]", "_", as.character(x))
}

# =====================================
# HÄMTA BILD
# =====================================

get_image <- function(url, path = NULL) {

  if (!is.null(path) && file.exists(path)) {
    result <- tryCatch(
      image_read(path),
      error = function(e) NULL
    )

    if (!is.null(result)) return(result)
  }

  if (length(url) == 0 || is.na(url) || url == "") {
    return(NULL)
  }

  tryCatch({

    response <- curl_fetch_memory(url)

    if (response$status_code != 200) return(NULL)

    extension <- if (grepl("\\.svg", url, ignore.case = TRUE)) {
      ".svg"
    } else {
      ".img"
    }

    tmp <- tempfile(fileext = extension)
    writeBin(response$content, tmp)

    img <- image_read(tmp)

    if (!is.null(path)) {
      image_write(img, path, format = "png")
    }

    img

  }, error = function(e) {
    cat("Kunde inte läsa bild:", conditionMessage(e), "\n")
    NULL
  })
}

# =====================================
# SKAPA MATCHBILD
# =====================================

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

  # Hitta spelarporträtt

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

  if (length(home_players) == 0 ||
      length(away_players) == 0) {

    cat("Saknar spelarporträtt för något lag!\n")
    return(FALSE)
  }

  # Hämta laglogotyper

  home_logo <- get_image(
    home_logo_url,
    file.path(home_folder, "team-logo.png")
  )

  away_logo <- get_image(
    away_logo_url,
    file.path(away_folder, "team-logo.png")
  )

  # Hämta ett spelarporträtt per lag

  home_player <- get_image(home_players[1])
  away_player <- get_image(away_players[1])

  # Skapa bakgrund

  canvas <- image_blank(
    width = 512,
    height = 512,
    color = "#101820"
  )

  # Rubrik

  canvas <- image_annotate(
    canvas,
    paste(league, format(date, "%Y-%m-%d")),
    gravity = "north",
    location = "+0+20",
    size = 23,
    color = "white",
    font = "DejaVu-Sans"
  )

  # Logotyper

  if (!is.null(home_logo)) {
    home_logo <- image_resize(home_logo, "115x115")
    canvas <- image_composite(
      canvas, home_logo, offset = "+90+75"
    )
  }

  if (!is.null(away_logo)) {
    away_logo <- image_resize(away_logo, "115x115")
    canvas <- image_composite(
      canvas, away_logo, offset = "+307+75"
    )
  }

  # Spelarporträtt

  home_player <- image_resize(home_player, "180x230")
  away_player <- image_resize(away_player, "180x230")

  canvas <- image_composite(
    canvas, home_player, offset = "+35+210"
  )

  canvas <- image_composite(
    canvas, away_player, offset = "+297+210"
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

  filename <- paste(
    league,
    safe_folder(home_code),
    safe_folder(away_code),
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

# =====================================
# HOCKEYALLSVENSKAN
# =====================================

cat("\n===== HOCKEYALLSVENSKAN =====\n")

ha <- ha_schedule()

ha_time <- as.POSIXct(
  ha$scheduled_date_time,
  tz = "UTC"
)

ha_date <- as.Date(format(
  ha_time,
  tz = "Europe/Stockholm",
  format = "%Y-%m-%d"
))

ha_index <- which(
  !is.na(ha_date) &
  ha_date >= today &
  ha_date <= end_date &
  !ha$is_completed
)

if (length(ha_index) > 0) {

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

# =====================================
# SHL
# =====================================

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
  shl$state != "post-game"
)

if (length(shl_index) > 0) {

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

cat("\n===== BILDSKAPANDE KLART =====\n")


library(rvest)

shl_url <- "https://swehockey.se/foerening/sport/serier/shl/"
ha_url <- "https://swehockey.se/foerening/sport/serier/hockeyallsvenskan/"

get_teams <- function(url, heading_pattern, next_heading_pattern) {

  page <- read_html(url)

  headings <- html_elements(page, "h1, h2, h3, h4")
  heading_text <- html_text2(headings)

  start <- which(grepl(heading_pattern, heading_text, ignore.case = TRUE))[1]

  if (is.na(start)) {
    stop("Kunde inte hitta rubriken för laglistan.")
  }

  end <- which(
    seq_along(heading_text) > start &
      grepl(next_heading_pattern, heading_text, ignore.case = TRUE)
  )[1]

  if (is.na(end)) {
    end <- length(heading_text) + 1
  }

  container <- xml2::xml_parent(headings[[start]])

  elements <- xml_find_all(
    container,
    ".//li | .//a"
  )

  teams <- html_text2(elements)
  teams <- trimws(teams)
  teams <- unique(teams[teams != ""])

  teams <- teams[
    !grepl("Visa länkar|Lagen i|Historik", teams, ignore.case = TRUE)
  ]

  teams
}

cat("Hämtar SHL...\n")
shl <- get_teams(
  shl_url,
  "^Lagen i SHL",
  "SM herr|SHL Historik"
)

cat("Hämtar HockeyAllsvenskan...\n")
ha <- get_teams(
  ha_url,
  "^Lagen i HA",
  "Hockeyallsvenskan Historik"
)

if (length(shl) != 14 || length(ha) != 14) {
  cat("SHL:", length(shl), "lag\n")
  cat("HockeyAllsvenskan:", length(ha), "lag\n")
  stop("Fel antal lag. Uppdateringen avbryts.")
}

teams <- data.frame(
  league = c(
    rep("SHL", length(shl)),
    rep("HockeyAllsvenskan", length(ha))
  ),
  team = c(shl, ha)
)

write.csv(
  teams,
  "scripts/teams.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nUppdateringen lyckades!\n")
print(teams)

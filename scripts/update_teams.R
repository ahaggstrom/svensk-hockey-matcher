
library(rvest)
library(xml2)

# Officiella laglistor från Svenska Ishockeyförbundet
shl_url <- "https://swehockey.se/foerening/sport/serier/shl/"
ha_url <- "https://swehockey.se/foerening/sport/serier/hockeyallsvenskan/"

get_teams <- function(url, pattern) {

  page <- read_html(url)

  headings <- html_elements(page, "h1, h2, h3, h4")
  heading_text <- html_text2(headings)

  index <- which(grepl(pattern, heading_text, ignore.case = TRUE))[1]

  if (is.na(index)) {
    stop("Kunde inte hitta rubriken för laglistan.")
  }

  team_list <- xml_find_first(
    headings[[index]],
    "following-sibling::ul[1]"
  )

  if (inherits(team_list, "xml_missing")) {
    stop("Kunde inte hitta laglistan på sidan.")
  }

  teams <- html_elements(team_list, "a") |>
    html_text2()

  teams <- unique(trimws(teams))
  teams <- teams[teams != ""]

  teams
}

cat("Hämtar SHL-lag...\n")
shl <- get_teams(shl_url, "^Lagen i SHL")

cat("Hämtar HockeyAllsvenska lag...\n")
ha <- get_teams(ha_url, "^Lagen i HA")

# Kontrollera att båda serierna innehåller 14 lag
if (length(shl) != 14 || length(ha) != 14) {
  stop("Fel antal lag. Laglistan uppdateras inte.")
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
cat("SHL:", length(shl), "lag\n")
cat("HockeyAllsvenskan:", length(ha), "lag\n")

print(teams)

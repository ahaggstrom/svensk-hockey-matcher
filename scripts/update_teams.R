
library(rvest)
library(xml2)

shl_url <- "https://swehockey.se/foerening/sport/serier/shl/"
ha_url <- "https://swehockey.se/foerening/sport/serier/hockeyallsvenskan/"

get_teams <- function(url, heading_pattern) {

  page <- read_html(url)

  # Hämta rubriker och länkar i sidans faktiska ordning
  nodes <- xml2::xml_find_all(
    page,
    ".//*[self::h1 or self::h2 or self::h3 or self::h4 or self::a]"
  )

  node_types <- vapply(nodes, xml2::xml_name, character(1))
  node_text <- vapply(
    nodes,
    function(x) rvest::html_text2(x),
    character(1)
  )

  # Hitta rubriken för aktuell laglista
  start <- which(
    node_types %in% c("h1", "h2", "h3", "h4") &
      grepl(heading_pattern, node_text, ignore.case = TRUE)
  )[1]

  if (is.na(start)) {
    stop("Kunde inte hitta rubriken för laglistan.")
  }

  # Hitta nästa rubrik
  headings_after <- which(
    seq_along(nodes) > start &
      node_types %in% c("h1", "h2", "h3", "h4")
  )

  if (length(headings_after) == 0) {
    stop("Kunde inte hitta nästa rubrik.")
  }

  end <- headings_after[1]

  # Hämta endast länkar mellan rubrikerna
  team_indexes <- which(
    seq_along(nodes) > start &
      seq_along(nodes) < end &
      node_types == "a"
  )

  teams <- node_text[team_indexes]

  teams <- trimws(teams)
  teams <- unique(teams[teams != ""])

  if (length(teams) != 14) {
    cat("Hittade följande lag:\n")
    print(teams)
    stop("Förväntade 14 lag men hittade ", length(teams), ".")
  }

  teams
}

cat("Hämtar SHL...\n")
shl <- get_teams(shl_url, "^Lagen i SHL")

cat("Hämtar HockeyAllsvenskan...\n")
ha <- get_teams(ha_url, "^Lagen i HA")

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
cat("HockeyAllsvenskan:", length(ha), "lag\n\n")

print(teams)

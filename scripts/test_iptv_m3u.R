
library(curl)
library(jsonlite)

host <- sub("/+$", "", Sys.getenv("IPTV_HOST"))
username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

url <- paste0(
  host,
  "/player_api.php?username=",
  curl_escape(username),
  "&password=",
  curl_escape(password),
  "&action=get_live_categories"
)

response <- curl_fetch_memory(url)

if (response$status_code != 200) {
  stop("API-fel: HTTP ", response$status_code)
}

categories <- fromJSON(
  rawToChar(response$content),
  simplifyVector = FALSE
)

cat("Kategorier som innehåller TV 4 eller TV4:\n\n")

found <- 0

for (category in categories) {
  if (grepl("TV\\s*4", category$category_name, ignore.case = TRUE)) {
    cat(
      "ID: ", category$category_id,
      " | Namn: ", category$category_name,
      "\n",
      sep = ""
    )
    found <- found + 1
  }
}

cat("\nAntal träffar:", found, "\n")

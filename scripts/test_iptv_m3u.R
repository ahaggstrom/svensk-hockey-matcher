
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
categories <- fromJSON(rawToChar(response$content), simplifyVector = FALSE)

cat("Kategorier som innehåller SWEDEN:\n\n")

for (category in categories) {
  if (grepl("SWEDEN", category$category_name, ignore.case = TRUE)) {
    cat(
      "ID: ", category$category_id,
      " | Namn: ", category$category_name,
      "\n",
      sep = ""
    )
  }
}

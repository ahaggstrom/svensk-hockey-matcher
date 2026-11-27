
host <- sub("/+$", "", Sys.getenv("IPTV_HOST"))
username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

if (host == "" || username == "" || password == "") {
  stop("IPTV Secrets saknas.")
}

get_api <- function(action) {
  url <- paste0(
    host,
    "/player_api.php?username=",
    curl::curl_escape(username),
    "&password=",
    curl::curl_escape(password),
    "&action=",
    action
  )

  response <- curl::curl_fetch_memory(
    url,
    handle = curl::new_handle(
      followlocation = TRUE,
      connecttimeout = 20,
      timeout = 180,
      useragent = "VLC/3.0.18"
    )
  )

  if (response$status_code != 200) {
    stop("IPTV API misslyckades: ", action)
  }

  jsonlite::fromJSON(
    rawToChar(response$content),
    simplifyVector = TRUE
  )
}

cat("Hämtar kategorier från Strong 8K...\n")
categories <- get_api("get_live_categories")

cat("Hämtar kanaler från Strong 8K...\n")
channels <- get_api("get_live_streams")

if (!all(c("category_id", "category_name") %in% names(categories))) {
  stop("API-svaret saknar category_id eller category_name.")
}

if (!all(c("category_id", "name") %in% names(channels))) {
  stop("API-svaret saknar category_id eller name.")
}

categories$category_id <- as.character(categories$category_id)
channels$category_id <- as.character(channels$category_id)

channel_counts <- table(channels$category_id)

categories$channel_count <- as.integer(
  channel_counts[categories$category_id]
)

categories$channel_count[
  is.na(categories$channel_count)
] <- 0L

categories <- categories[
  order(categories$category_name),
]

cat("\n====================================\n")
cat("KATEGORIER I STRONG 8K\n")
cat("====================================\n\n")

for (i in seq_len(nrow(categories))) {
  cat(
    categories$category_name[i],
    " — ",
    categories$channel_count[i],
    " kanaler\n",
    sep = ""
  )
}

cat("\nTotalt antal kategorier:", nrow(categories), "\n")
cat("Totalt antal kanaler:", nrow(channels), "\n")

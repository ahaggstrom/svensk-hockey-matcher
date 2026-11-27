
library(curl)
library(jsonlite)

host <- sub("/+$", "", Sys.getenv("IPTV_HOST"))
username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

get_api <- function(action) {
  url <- paste0(
    host,
    "/player_api.php?username=",
    curl_escape(username),
    "&password=",
    curl_escape(password),
    "&action=",
    action
  )

  response <- curl_fetch_memory(url)

  if (response$status_code != 200) {
    stop("API-fel: HTTP ", response$status_code)
  }

  fromJSON(rawToChar(response$content), simplifyVector = FALSE)
}

cat("Hämtar kategorier...\n", flush = TRUE)
categories <- get_api("get_live_categories")

cat("Antal kategorier:", length(categories), "\n", flush = TRUE)

cat("Hämtar kanaler...\n", flush = TRUE)
channels <- get_api("get_live_streams")

cat("Antal kanaler:", length(channels), "\n", flush = TRUE)

cat("\nSöker efter PPV-kategorier...\n", flush = TRUE)

for (category in categories) {
  category_name <- category$category_name

  if (grepl("SWEDEN TV 4 PLAY PPV", category_name, ignore.case = TRUE)) {
    category_id <- category$category_id

    matching <- Filter(
      function(channel) identical(channel$category_id, category_id),
      channels
    )

    cat("\nKategori:", category_name, "\n")
    cat("Kategori-ID:", category_id, "\n")
    cat("Antal kanaler:", length(matching), "\n")

    for (channel in matching) {
      cat(" - ", channel$name, "\n", sep = "")
    }
  }
}

cat("\nDIAGNOSTIK KLAR\n", flush = TRUE)

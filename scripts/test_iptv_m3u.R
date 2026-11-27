
library(curl)
library(jsonlite)

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

cat("\nHÄMTAR KATEGORIER...\n")

categories <- get_api("get_live_categories")

cat("\nANTAL KATEGORIER:", length(categories), "\n")
cat("\nFÖRSTA 5 KATEGORIERNA – ALLA FÄLT:\n\n")

for (i in seq_len(min(5, length(categories)))) {
  cat("Kategori ", i, ":\n", sep = "")
  str(categories[[i]])
  cat("\n")
}

cat("\nHÄMTAR KANALER...\n")

channels <- get_api("get_live_streams")

cat("\nANTAL KANALER:", length(channels), "\n")
cat("\nFÖRSTA 3 KANALERNA – ALLA FÄLT:\n\n")

for (i in seq_len(min(3, length(channels)))) {
  cat("Kanal ", i, ":\n", sep = "")
  str(channels[[i]])
  cat("\n")
}

cat("\nDIAGNOSTIK KLAR\n")

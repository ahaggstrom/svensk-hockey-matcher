
# Testa Strong 8K via Xtream Codes API

host <- "http://enough62794.wd.business-cloud-8.ru"

username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

if (username == "" || password == "") {
  stop("IPTV_USERNAME eller IPTV_PASSWORD saknas.")
}

api_url <- paste0(
  host,
  "/player_api.php?username=",
  curl::curl_escape(username),
  "&password=",
  curl::curl_escape(password),
  "&action=get_live_streams"
)

cat("Ansluter till Strong 8K API...\n")

response <- tryCatch({
  handle <- curl::new_handle(
    followlocation = TRUE,
    connecttimeout = 20,
    timeout = 180,
    useragent = "VLC/3.0.18 LibVLC/3.0.18"
  )

  curl::curl_fetch_memory(api_url, handle = handle)

}, error = function(e) {
  stop("Kunde inte ansluta till IPTV API.")
})

if (response$status_code != 200) {
  stop(paste("API svarade med HTTP-status:", response$status_code))
}

data <- tryCatch({
  jsonlite::fromJSON(
    rawToChar(response$content),
    simplifyVector = TRUE
  )
}, error = function(e) {
  stop("API-svaret kunde inte läsas som JSON.")
})

if (!is.data.frame(data) || !"name" %in% names(data)) {
  stop("API-svaret innehöll inte förväntade kanaluppgifter.")
}

channel_names <- unique(data$name)
channel_names <- channel_names[!is.na(channel_names)]

cat("\nAnslutningen lyckades!\n")
cat("Antal kanaler:", length(channel_names), "\n\n")

cat("Exempel på kanalnamn:\n")

for (name in head(channel_names, 20)) {
  cat("-", name, "\n")
}

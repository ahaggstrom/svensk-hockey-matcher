
# Testa hämtning av Strong 8K M3U-playlist

host <- "http://enough62794.wd.business-cloud-8.ru"

username <- Sys.getenv("IPTV_USERNAME")
password <- Sys.getenv("IPTV_PASSWORD")

if (username == "" || password == "") {
  stop("IPTV_USERNAME eller IPTV_PASSWORD saknas i GitHub Secrets.")
}

# Bygg M3U-adressen utan att skriva ut den
playlist_url <- paste0(
  host,
  "/get.php?username=", curl::curl_escape(username),
  "&password=", curl::curl_escape(password),
  "&type=m3u_plus&output=ts"
)

cat("Testar anslutning till IPTV-servern...\n")

result <- tryCatch({

  handle <- curl::new_handle(
    followlocation = TRUE,
    connecttimeout = 20,
    timeout = 120,
    useragent = "Mozilla/5.0"
  )

  response <- curl::curl_fetch_memory(
    playlist_url,
    handle = handle
  )

  list(
    status = response$status_code,
    content = rawToChar(response$content)
  )

}, error = function(e) {
  stop("Kunde inte hämta playlisten. Kontrollera servern och inloggningen.")
})

if (result$status != 200) {
  stop(paste("Servern svarade med HTTP-status:", result$status))
}

playlist <- result$content

if (!grepl("#EXTM3U", playlist, fixed = TRUE)) {
  stop("Svaret verkar inte vara en giltig M3U-playlist.")
}

# Läs ut kanalnamnen
lines <- strsplit(playlist, "\n", fixed = TRUE)[[1]]

channel_lines <- grep("^#EXTINF", lines, value = TRUE)

channel_names <- sub(
  "^.*?,",
  "",
  channel_lines
)

channel_names <- trimws(channel_names)
channel_names <- channel_names[nzchar(channel_names)]

cat("\nM3U-hämtningen lyckades!\n")
cat("Antal kanaler:", length(channel_names), "\n\n")

cat("Exempel på kanalnamn:\n")

for (name in head(channel_names, 15)) {
  cat("-", name, "\n")
}

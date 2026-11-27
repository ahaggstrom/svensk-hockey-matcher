
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

categories <- get_api("get_live_categories")
channels <- get_api("get_live_streams")

target_ids <- c("462", "1416")

for (id in target_ids) {
  category <- Filter(
    function(x) identical(x$category_id, id),
    categories
  )

  matching <- Filter(
    function(x) identical(x$category_id, id),
    channels
  )

  cat("\n====================================\n")
  cat("KATEGORI:", category[[1]]$category_name, "\n")
  cat("ID:", id, "\n")
  cat("ANTAL KANALER:", length(matching), "\n")
  cat("====================================\n")

  for (channel in matching) {
    cat(channel$name, "\n")
  }
}

cat("\nTEST KLAR\n")

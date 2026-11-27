
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

cat("Hämtar kategorier...\n")
categories <- get_api("get_live_categories")

cat("Hämtar kanaler...\n")
channels <- get_api("get_live_streams")

# Gör om API-listorna till dataframes
category_df <- do.call(rbind, lapply(categories, function(x) {
  data.frame(
    category_id = x$category_id,
    category_name = x$category_name,
    stringsAsFactors = FALSE
  )
}))

channel_df <- do.call(rbind, lapply(channels, function(x) {
  data.frame(
    name = x$name,
    stream_id = x$stream_id,
    category_id = x$category_id,
    stringsAsFactors = FALSE
  )
}))

# Hitta de två aktuella kategorierna
target_categories <- category_df[
  grepl("SWEDEN TV 4 PLAY PPV", category_df$category_name, ignore.case = TRUE),
]

cat("\n====================================\n")
cat("MATCHANDE KATEGORIER\n")
cat("====================================\n")

if (nrow(target_categories) == 0) {
  cat("Inga matchande kategorier hittades.\n")
} else {
  for (i in seq_len(nrow(target_categories))) {
    id <- target_categories$category_id[i]
    category_name <- target_categories$category_name[i]

    matching_channels <- channel_df[
      channel_df$category_id == id,
    ]

    cat("\nKategori:", category_name, "\n")
    cat("Kategori-ID:", id, "\n")
    cat("Antal kanaler:", nrow(matching_channels), "\n")
    cat("------------------------------------\n")

    if (nrow(matching_channels) > 0) {
      cat(paste(matching_channels$name, collapse = "\n"), "\n")
    }
  }
}

cat("\nDIAGNOSTIK KLAR\n")

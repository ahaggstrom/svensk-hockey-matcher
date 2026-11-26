
# HÄMTA BILD
get_image <- function(url, path = NULL) {

  # Om bilden redan finns sparad lokalt
  if (length(url) == 1 && !is.na(url) && file.exists(url)) {
    return(tryCatch(image_read(url), error = function(e) NULL))
  }

  # Använd redan sparad logotyp om den finns
  if (!is.null(path) && file.exists(path)) {
    result <- tryCatch(image_read(path), error = function(e) NULL)
    if (!is.null(result)) return(result)
  }

  if (length(url) == 0 || is.na(url) || url == "") return(NULL)

  tryCatch({
    response <- curl_fetch_memory(url)

    if (response$status_code != 200) return(NULL)

    is_svg <- grepl("\\.svg", url, ignore.case = TRUE)
    extension <- if (is_svg) ".svg" else ".img"

    tmp <- tempfile(fileext = extension)
    writeBin(response$content, tmp)

    if (is_svg) {
      img <- image_read_svg(tmp)
    } else {
      img <- image_read(tmp)
    }

    if (!is.null(path)) {
      image_write(img, path, format = "png")
    }

    img

  }, error = function(e) {
    cat("Kunde inte läsa bild:", conditionMessage(e), "\n")
    NULL
  })
}

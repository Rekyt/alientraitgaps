download_invacost = function() {

  dir.create("inst/exdata/invacost/", showWarnings = FALSE, recursive = TRUE)

  download.file("https://figshare.com/ndownloader/articles/12668570/versions/4",
                "inst/exdata/invacost/Invacost.zip", mode = "wb")

  here::here("inst", "exdata", "invacost", "Invacost.zip")
}

unzip_invacost = function(invacost_data) {

  list_files = unzip(invacost_data, list = TRUE)

  invacost_data %>%
    unzip(exdir = here::here("inst", "exdata", "invacost"))

  list_files %>%
    mutate(Name = paste0(
      here::here("inst", "exdata", "invacost"), "/", Name
    )) %>%
    pull(Name)
}

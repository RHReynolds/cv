# Description: script to builds both the HTML and PDF versions of your CV

#---- Load libraries --------------------

library(rmarkdown)
library(here)
library(stringr)

#---- Main -----------------------------

# If you wanted to speed up rendering for googlesheets driven CVs you could use
# this script to cache a version of the CV_Printer class with data already
# loaded and load the cached version in the .Rmd instead of re-fetching it twice
# for the HTML and PDF rendering. This exercise is left to the reader.

rmds <-
  list.files(
    here::here(),
    pattern = "*.rmd"
  )

for (rmd in rmds) {
  name <- stringr::str_remove(rmd, "\\..*")

  print(
    stringr::str_c("Knitting ", name)
  )

  # Knit rmd to HTML version
  rmarkdown::render(
    stringr::str_c(name, ".rmd"),
    params = list(pdf_mode = FALSE),
    output_file =
      stringr::str_c("docs/", name, ".html")
  )

  # Knit the PDF version to temporary html location
  tmp_html_cv_loc <- fs::file_temp(ext = ".html")

  rmarkdown::render(
    stringr::str_c(name, ".rmd"),
    params = list(pdf_mode = TRUE),
    output_file = tmp_html_cv_loc
  )

  # Convert to PDF using Pagedown
  pagedown::chrome_print(
    input = tmp_html_cv_loc,
    output =
      stringr::str_c("docs/rhr_", name, ".pdf")
  )
}

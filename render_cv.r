# Description: script to builds both the HTML and PDF versions of your CV

#---- Load libraries --------------------

library(rmarkdown)
library(here)
library(stringr)

#---- Arguments -------------------------

args <-
  list(
    input_dir = here::here("templates"),
    output_dir = here::here("docs")
  )

#---- Main -----------------------------

# If you wanted to speed up rendering for googlesheets driven CVs you could use
# this script to cache a version of the CV_Printer class with data already
# loaded and load the cached version in the .Rmd instead of re-fetching it twice
# for the HTML and PDF rendering. This exercise is left to the reader.

rmds <-
  list.files(
    args$input_dir,
    pattern = "*.rmd"
  )

for (rmd in rmds) {
  name <- stringr::str_remove(rmd, "\\..*")

  print(
    stringr::str_c("Knitting ", name)
  )

  # Knit rmd to HTML version
  rmarkdown::render(
    input = file.path(args$input_dir, stringr::str_c(name, ".rmd")),
    params = list(pdf_mode = FALSE),
    output_file = file.path(args$output_dir, stringr::str_c(name, ".html"))
  )

  # Knit the PDF version to temporary html location
  tmp_html_cv_loc <-
    file.path(args$output_dir, "pdf", stringr::str_c("rhr_", name, ".html"))

  rmarkdown::render(
    input = file.path(args$input_dir, stringr::str_c(name, ".rmd")),
    params = list(pdf_mode = TRUE),
    output_file = tmp_html_cv_loc
  )

  # # Convert to PDF using Pagedown
  # pagedown::chrome_print(
  #   input = tmp_html_cv_loc,
  #   output = file.path(args$output_dir, stringr::str_c("rhr_", name, ".pdf"))
  # )
}

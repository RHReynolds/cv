# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!

# Main CV Constructor ----------------------------------------------------------------

#' Create a CV_Printer object
#'
#' Constructs a CV_Printer object from either a Google Sheet or a local Excel file.
#'
#' @param data_location Path to the data source. Either a URL to a Google Sheet or a directory
#'   containing a `.xlsx` file with the required sheets.
#' @param pdf_mode Logical. Should links be stripped for PDF rendering? Defaults to `FALSE`.
#' @param sheet_is_publicly_readable Logical. If using Google Sheets, is it publicly accessible?
#'   Defaults to `TRUE`.
#' @return A list (CV_Printer object) containing CV data and utilities.
#' @export
create_CV_object <- function(
  data_location,
  pdf_mode = FALSE,
  sheet_is_publicly_readable = TRUE
) {
  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(
    data_location,
    "docs\\.google\\.com"
  )

  if (is_google_sheets_location) {
    if (sheet_is_publicly_readable) {
      googlesheets4::sheets_deauth()
    } else {
      options(gargle_oauth_cache = ".secrets")
    }

    cv$entries_data <- read_gsheet(data_location, "entries")
    cv$skills <- read_gsheet(data_location, "language_skills")
    cv$text_blocks <- read_gsheet(data_location, "text_blocks")
    cv$contact_info <- read_gsheet(data_location, "contact_info")
  } else {
    xlsx_file <- list.files(
      path = data_location,
      pattern = ".xlsx",
      full.names = TRUE
    )
    cv$entries_data <- readxl::read_excel(
      xlsx_file,
      sheet = "entries",
      skip = 1
    )
    cv$skills <- readxl::read_excel(
      xlsx_file,
      sheet = "language_skills",
      skip = 1
    )
    cv$text_blocks <- readxl::read_excel(
      xlsx_file,
      sheet = "text_blocks",
      skip = 1
    )
    cv$contact_info <- readxl::read_excel(
      xlsx_file,
      sheet = "contact_info",
      skip = 1
    )
  }

  # Clean and enrich entries data
  cv$entries_data <-
    cv$entries_data |>
    tidyr::unite(
      tidyr::starts_with("description"),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) |>
    dplyr::mutate(
      description_bullets = ifelse(
        description_bullets != "",
        paste0("- ", description_bullets),
        ""
      ),
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = .extract_year(start),
      end_year = .extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start & no_end ~ "N/A",
        no_start & has_end ~ as.character(end),
        has_start & no_end ~ paste("Current", "-", start),
        TRUE ~ paste(end, "-", start)
      )
    ) |>
    dplyr::arrange(desc(.parse_dates(end))) |>
    dplyr::mutate_all(~ ifelse(is.na(.), "N/A", .))

  return(cv)
}


# Print Methods ----------------------------------------------------------------------

#' Print a section of entries from the CV
#'
#' @param cv CV_Printer object
#' @param section_id Character. Section ID as found in the `section` column of the entries table.
#' @param glue_template Optional string template for output formatting. Defaults to a markdown block.
#' @param resume Logical. If TRUE, filters entries where `in_resume` is TRUE.
#' @return Invisibly returns the modified CV object.
#' @export
print_section <- function(
  cv,
  section_id,
  glue_template = "default",
  resume = FALSE
) {
  if (resume) {
    cv$entries_data <- dplyr::filter(cv$entries_data, in_resume == TRUE)
  }

  if (glue_template == "default") {
    glue_template <- "
### {title}

{loc}

{institution}

{timeline}

{description_bullets}
\n\n\n"
  }

  section_data <- dplyr::filter(cv$entries_data, section == section_id)

  for (i in 1:nrow(section_data)) {
    for (col in c("title", "description_bullets")) {
      strip_res <- sanitize_links(cv, section_data[i, col])
      section_data[i, col] <- strip_res$text
      cv <- strip_res$cv
    }
  }

  print(glue::glue_data(section_data, glue_template))
  invisible(cv)
}


#' Print a text block by label
#'
#' @param cv CV_Printer object
#' @param label Character. Label in the `text_blocks` table to print.
#' @return Invisibly returns the modified CV object.
#' @export
print_text_block <- function(cv, label) {
  text_block <- dplyr::filter(cv$text_blocks, loc == label) |> dplyr::pull(text)
  strip_res <- sanitize_links(cv, text_block)
  cat(strip_res$text)
  invisible(strip_res$cv)
}


#' Print a list of skills
#'
#' @param cv CV_Printer object
#' @param type_of_interest Character. Type to filter for in the `skills` table.
#' @return Invisibly returns the CV object.
#' @export
print_skill <- function(cv, type_of_interest) {
  cv$skills |>
    dplyr::filter(type == type_of_interest) |>
    glue::glue_data(
      "- <i class='fa-{icon_type} fa-{icon}'></i> {skill}"
    ) |>
    print()
  invisible(cv)
}


#' Print all gathered links
#'
#' @param cv CV_Printer object
#' @return Invisibly returns the CV object.
#' @export
print_links <- function(cv) {
  n_links <- length(cv$links)
  if (n_links > 0) {
    cat(
      "
Links {data-icon=link}
--------------------------------------------------------------------------------

<br>
"
    )
    purrr::walk2(cv$links, 1:n_links, function(link, index) {
      print(glue::glue("{index}. {link}"))
    })
  }
  invisible(cv)
}


#' Print contact information
#'
#' @param cv CV_Printer object
#' @return Invisibly returns the CV object.
#' @export
print_contact_info <- function(cv) {
  glue::glue_data(
    cv$contact_info,
    "- <i class='fa fa-{icon}'></i> {contact}"
  ) |>
    print()
  invisible(cv)
}


#' Print HTML or PDF breaks
#'
#' @param html_breaks Number of breaks for HTML output.
#' @param pdf_breaks Number of breaks for PDF output.
#' @param pdf_mode Logical. Whether the output is for PDF.
#' @export
print_breaks <- function(html_breaks = 1, pdf_breaks = 1, pdf_mode) {
  cat(stringr::str_c(
    "\n",
    stringr::str_c(
      rep("<br>", ifelse(pdf_mode, pdf_breaks, html_breaks)),
      collapse = "\n"
    ),
    "\n"
  ))
}

# Helpers ---------------------------------------------------------------------------

#' Sanitize markdown links (for PDF output)
#'
#' Removes markdown-style links and replaces them with superscript references.
#'
#' @param cv CV_Printer object
#' @param text Character. Input text to sanitize.
#' @return List with modified CV and sanitized text.
sanitize_links <- function(cv, text) {
  if (cv$pdf_mode) {
    link_titles <- stringr::str_extract_all(text, "(?<=\\[).+?(?=\\])")[[1]]
    link_destinations <- stringr::str_extract_all(text, "(?<=\\().+?(?=\\))")[[
      1
    ]]

    n_links <- length(cv$links)
    n_new_links <- length(link_titles)

    if (n_new_links > 0) {
      cv$links <- c(cv$links, link_destinations)

      link_superscript_mappings <- purrr::set_names(
        paste0("<sup>", (1:n_new_links) + n_links, "</sup>"),
        paste0("(", link_destinations, ")")
      )

      text <- text |>
        stringr::str_replace_all(stringr::fixed(link_superscript_mappings)) |>
        stringr::str_replace_all("\\[(.+?)\\]", "\\1")
    }
  }
  list(cv = cv, text = text)
}

#' Read Google Sheet by sheet name
#' @param data_location Google Sheet URL
#' @param sheet_id Sheet name to read
#' @return A data.frame of the sheet contents
read_gsheet <- function(data_location, sheet_id) {
  googlesheets4::read_sheet(
    data_location,
    sheet = sheet_id,
    skip = 1,
    col_types = "c"
  )
}

#' Extract date from string
#' @param dates Vector of dates (character)
#' @return Date vector
.parse_dates <- function(dates) {
  date_month <- stringr::str_extract(
    dates,
    "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})"
  )
  date_month[is.na(date_month)] <- "1"
  paste("1", date_month, .extract_year(dates), sep = "-") |>
    lubridate::dmy()
}

#' Extract year from date string
#' @param dates Vector of date strings
#' @return Numeric vector of years
.extract_year <- function(dates) {
  date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
  date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) +
    10
  date_year
}

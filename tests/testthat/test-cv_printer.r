library(testthat)
library(dplyr)
library(stringr)
library(lubridate)

# Mock CV object for testing
mock_cv <- list(
    pdf_mode = TRUE,
    links = c(),
    text_blocks = tibble::tibble(
        loc = "intro",
        text = "This is a [link](http://example.com)."
    ),
    skills = tibble::tibble(
        type = "language",
        icon_type = "fab",
        icon = "r-project",
        skill = "R"
    ),
    contact_info = tibble::tibble(
        icon = "envelope",
        contact = "email@example.com"
    ),
    entries_data = tibble::tibble(
        section = "experience",
        title = "Data Scientist",
        description_1 = "Did stuff",
        description_2 = NA,
        loc = "Somewhere",
        institution = "Company",
        start = "2020",
        end = "2021",
        in_resume = TRUE
    )
)

# Test sanitize_links ----------------------------------------------------------
test_that("sanitize_links handles markdown links correctly", {
    result <- sanitize_links(mock_cv, "A [link](http://example.com)")
    expect_match(result$text, "A link<sup>1</sup>")
    expect_equal(result$cv$links, "http://example.com")
})

# Test .extract_year ----------------------------------------------------------
test_that(".extract_year extracts year correctly", {
    expect_equal(.extract_year("May 2020"), "2020")
    expect_equal(.extract_year("NULL"), as.character(year(Sys.Date()) + 10))
})

# Test .parse_dates -----------------------------------------------------------
test_that(".parse_dates parses valid dates", {
    parsed <- .parse_dates("May 2020")
    expect_true(inherits(parsed, "Date"))
})

# Test print_skill ------------------------------------------------------------
test_that("print_skill filters and prints skills", {
    local_edition(3)
    expect_snapshot_output(print_skill(mock_cv, "language"))
})

# Test print_contact_info -----------------------------------------------------
test_that("print_contact_info outputs contact data", {
    local_edition(3)
    expect_snapshot_output(print_contact_info(mock_cv))
})

# Test print_breaks -----------------------------------------------------------
test_that("print_breaks prints correct number of breaks", {
    local_edition(3)
    expect_snapshot_output(print_breaks(2, 3, TRUE))
    expect_snapshot_output(print_breaks(2, 3, FALSE))
})

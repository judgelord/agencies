#' @param text Character vector of text to search.
#' @param regextable A regex lookup table built by build_regextable().
#'
#' @return A tibble of matched agencies.
#' @export

extract_agencies <- function(text, regextable) {

  text <- as.character(text)

  data_tbl <- tibble::tibble(
    row_id = seq_along(text),
    text = text
  )

  results <- regextable::extract(
    data = data_tbl,
    regex_table = regextable,
    col_name = "text",
    pattern_col = "pattern",
    regex_return_cols = c("agency", "envirodatagov_url"),
    unique_match = FALSE,
    verbose = FALSE
  )

  results %>%
    dplyr::select(
      text_id = row_id,
      agency,
      envirodatagov_url
    ) %>%
    dplyr::distinct()
}

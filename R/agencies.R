#' Build agency regex lookup table
#'
#' @param sheet_url Google Sheets URL for the agency crosswalk
#'
#' @return A list with a regex table and duplicate aliases
#'
#' @export
#' @importFrom magrittr %>%

build_regextable <- function(sheet_url){

  crosswalk <- googlesheets4::read_sheet(sheet_url)

  crosswalk <- crosswalk %>%
    dplyr::mutate(agency_short = agency) %>%
    dplyr::select(department, department_acronym, agency, agency_short,
           department_agency_acronym, regulationsdotgov_agency, regulationsdotgov_acronym,
           ACUS_agency, department_agency_acronyms_prior, other_acronyms, other_names)

  web_xlsx <- system.file(
    "extdata",
    "Enviro Fed Web Tracker 2025-09-30.xlsx",
    package = "regextable.agencies"
  )

  if (web_xlsx == "") {
    stop("Enviro Fed Web Tracker Excel file not found in package.")
  }


  webtracker <- readxl::read_excel(web_xlsx, sheet = 1) %>%
    tibble::as_tibble()

  web_clean <- webtracker %>%
    dplyr::rename(
      acronym = Agency,
      url = URL
    ) %>%
    dplyr::mutate(
      original_url = url,
      url = dplyr::if_else(
        stringr::str_detect(original_url, "^https?://"),
        original_url,
        paste0("https://", original_url)
      ),
      root = paste0(urltools::scheme(url), "://", urltools::domain(url)),
      url_length = nchar(url)
    )

  landing_by_root <- web_clean %>%
    dplyr::group_by(acronym, root) %>%
    dplyr::summarize(
      n_urls = dplyr::n(),
      shortest_url = url[which.min(url_length)][1],
      .groups = "drop"
    ) %>%
    dplyr::group_by(acronym) %>%
    dplyr::arrange(acronym, desc(n_urls), nchar(shortest_url)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(acronym, envirodatagov_url = root)

  crosswalk2 <- crosswalk %>%
    dplyr::mutate(acronym = stringr::str_trim(toupper(department_agency_acronym)))

  landing_by_root2 <- landing_by_root %>%
    dplyr::mutate(acronym = stringr::str_trim(toupper(acronym)))

  crosswalk_with_enviro <- crosswalk2 %>%
    dplyr::left_join(landing_by_root2, by = "acronym")


  crosswalk <- crosswalk %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pattern = paste(
      dplyr::c_across(department:other_names) %>%
        stringr::str_trim() %>%
        purrr::discard(~ .x == "" | is.na(.x)),
      collapse = "|"
    )) %>%
    dplyr::ungroup()

  all_aliases <- crosswalk %>%
    dplyr::rowwise() %>%
    dplyr::mutate(alias_list = list(
      dplyr::c_across(department:other_names) %>%
        stringr::str_trim() %>%
        purrr::discard(~ .x == "" | is.na(.x))
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(agency_short, alias_list) %>%
    tidyr::unnest(alias_list) %>%
    dplyr::rename(alias = alias_list)

  duplicates <- all_aliases %>%
    dplyr::group_by(alias) %>%
    dplyr::summarize(n_agencies = dplyr::n_distinct(agency_short), .groups = "drop") %>%
    dplyr::filter(n_agencies > 1) %>%
    dplyr::pull(alias)

  aliases_unique <- all_aliases %>%
    dplyr::filter(!(alias %in% duplicates))

  patterns_unique <- aliases_unique %>%
    dplyr::group_by(agency_short) %>%
    dplyr::summarize(
      pattern = paste(unique(alias), collapse = "|"),
      .groups = "drop"
    )

  final_table <- crosswalk_with_enviro %>%
    dplyr::left_join(patterns_unique, by = "agency_short")

  return(list(
    regextable = final_table %>% dplyr::select(
      agency = agency_short,
      envirodatagov_url,
      pattern),
    duplicates = duplicates
  ))

}

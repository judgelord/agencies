regextable.agencies
===================

Description
-----------
The regextable.agencies package provides tools to identify and extract U.S. federal
agency names from text using a regex-based lookup table built from a curated agency
crosswalk. It is designed to link agency mentions in unstructured text to standardized
agency identifiers and associated metadata, such as official web URLs tracked by
EnviroData.gov.

The package exposes two primary functions:

  - build_regextable(): Constructs the regex lookup table from a Google Sheets
    crosswalk and a bundled Excel file of agency web URLs.
  - extract_agencies(): Searches a character vector for agency mentions using the
    lookup table produced by build_regextable().


Installation
------------
devtools::install_github("judgelord/agencies")

library(regextable.agencies)


Authentication
--------------
build_regextable() reads the agency crosswalk from Google Sheets using the
googlesheets4 package, which requires authentication with a Google account that has
read access to the sheet.

Interactive authentication (default):
  googlesheets4::gs4_auth()

This opens a browser window prompting you to log in and grant access. Credentials are
cached locally for subsequent sessions.

Non-interactive / scripted authentication:
For automated workflows (e.g., CI pipelines, scheduled scripts), use a service account
or a pre-authorized token:

  # Authenticate with a service account JSON key file
  googlesheets4::gs4_auth(path = "path/to/service-account-key.json")

  # Or, suppress interactive prompts and use a cached token
  googlesheets4::gs4_auth(email = "your-email@example.com")

If the sheet is publicly readable (shared with "Anyone with the link"), you can bypass
authentication entirely:

  googlesheets4::gs4_deauth()

See the googlesheets4 documentation for full details:
https://googlesheets4.tidyverse.org/reference/gs4_auth.html


Data
----
The package relies on two data sources:

1. A Google Sheets agency crosswalk (supplied by the user as a URL) containing
   columns for department names, agency names, acronyms, and alternate names from
   sources including regulations.gov, ACUS, and other known aliases.

2. A bundled Excel file ("Enviro Fed Web Tracker 2025-09-30.xlsx") included in the
   package under inst/extdata/, containing agency acronyms and associated URLs
   tracked by EnviroData.gov.


build_regextable()
------------------
Reads the agency crosswalk from Google Sheets and the bundled web tracker Excel file,
resolves duplicate aliases across agencies, and returns a regex lookup table suitable
for use with extract_agencies().

Parameters:
  sheet_url   A Google Sheets URL pointing to the agency crosswalk table.

Returns a list with two elements:
  regextable   A tibble with one row per agency, containing:
                 - agency: the standardized short agency name
                 - envirodatagov_url: the root URL from the EnviroData.gov web tracker
                 - pattern: a regex pattern of the form \bALIAS\b|\bALIAS\b|... for
                   all non-duplicate aliases associated with that agency
  duplicates   A character vector of alias strings that matched more than one agency
               and were therefore excluded from the regex patterns (except where the
               alias exactly matches the agency's primary department_agency_acronym,
               in which case it is retained for that agency).

Example:
  texts <- c(
    "The EPA issued new guidance on water quality.",
    "Both the Secret Service and DOJ are investigating."
  )

  results <- extract_agencies(texts, table$regextable)
  print(results)

  #> # A tibble: 3 × 3
  #>   text_id agency          envirodatagov_url
  #>     <int> <chr>           <chr>
  #> 1       1 EPA             https://www.epa.gov
  #> 2       2 Secret Service  https://www.secretservice.gov
  #> 3       2 DOJ             https://www.justice.gov


Duplicate Alias Resolution
--------------------------
Because many agency acronyms and short names overlap across departments, aliases that
appear for more than one agency are flagged as duplicates and removed from all regex
patterns by default. The one exception is when a duplicate alias exactly matches an
agency's department_agency_acronym field, in which case it is resolved in favor of
that agency and retained.

Users can inspect table$duplicates to review which aliases were excluded and decide
whether to handle them manually.


extract_agencies()
------------------
Searches a character vector for mentions of federal agencies using a regex lookup
table produced by build_regextable(). For each match, returns the originating text's
index, the matched agency name, and the associated EnviroData.gov URL.

Parameters:
  text          A character vector of text to search.
  regextable    A regex lookup table (the $regextable element) returned by
                build_regextable().

Returns a tibble with one row per match, including:
  text_id             The index position of the matched text in the input vector.
  agency              The standardized short agency name.
  envirodatagov_url   The root URL for the agency from the EnviroData.gov tracker.

If the same agency matches multiple times in the same input string, only distinct
(text_id, agency, envirodatagov_url) combinations are returned.

Example:
  texts <- c(
    "The EPA issued new guidance on water quality.",
    "Both the Forest Service and BLM manage federal lands."
  )

  results <- extract_agencies(texts, table$regextable)
  print(results)


Dependencies
------------
  googlesheets4   Reading the agency crosswalk from Google Sheets
  readxl          Reading the bundled web tracker Excel file
  dplyr           Data manipulation
  tidyr           Reshaping alias lists
  stringr         String cleaning and trimming
  purrr           Filtering missing values across columns
  tibble          Tibble construction
  urltools        Parsing URL scheme and domain
  magrittr        Pipe operator (%>%)
  regextable      Core regex extraction engine (extract())

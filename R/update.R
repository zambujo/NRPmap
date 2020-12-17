#' URL validator
#'
#' @param url A url to validate.
#'
#' @return A boolean TRUE if the response header status is 200.
#' @export
#'
#' @examples valid_url("https://github.com/zambujo")
valid_url <- function(url) {
  valid <- url %>%
    httr::HEAD() %>%
    purrr::pluck("status_code") %>%
    identical(200L)
  return(valid)
}

#' Updates data from p3.snf.ch
#'
#' @param filename a character matching filenames in p3.snf.ch/P3Export/
#'
#' @return a dataframe with the data (does not handle errors)
#'
#' @examples data_update("P3_PersonExport.csv")
data_update <- function(filename) {
  p3_url <- "http://p3.snf.ch/P3Export/{ filename }" %>%
    glue::glue()

  p3_url %>%
    valid_url() %>%
    stopifnot()

  "updating data { p3_url }" %>%
    glue::glue() %>%
    usethis::ui_info()

  p3_url %>%
    readr::read_csv2(col_types = readr::cols(.default = "c")) %>%
    janitor::clean_names()
}


#' Extracts descriptions from NRP landing pages
#'
#' @param nrp_number the NRP number
#'
#' @return a character with the NRP description (does not handle errors)
#'
#' @examples extract_description(77)
extract_description <- function(nrp_number) {
  nrp_url <- "http://www.nfp{ nrp_number }.ch/en" %>%
    glue::glue()

  nrp_url %>%
    valid_url() %>%
    stopifnot()

  "extracting description from { nrp_url }" %>%
    glue::glue() %>%
    usethis::ui_info()

  nrp_url %>%
    xml2::read_html() %>%
    rvest::html_nodes("#ctl00_PlaceHolderMain_Content__ControlWrapper_RichHtmlField p") %>%
    rvest::html_text() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    stringr::str_c(collapse = "  ")
}


# main --------------------------------------------------------------------

p3data <- c("P3_GrantExport.csv", "P3_PersonExport.csv") %>%
  purrr::map(data_update)

nrp_selection <-
  p3data %>%
  purrr::pluck(1) %>%
  dplyr::select(start_date, end_date, funding_instrument) %>%
  dplyr::mutate_all(stringr::str_replace_all,
                    pattern = "data not included in P3",
                    replacement = NA_character_) %>%
  dplyr::filter(stringr::str_detect(funding_instrument,
                                    stringr::str_c("NRP"))) %>%
  dplyr::mutate(
    nrp_number = stringr::str_extract(funding_instrument, "\\d+"),
    nrp_number = as.integer(nrp_number)) %>%
  dplyr::filter(nrp_number > 65, nrp_number < 78) %>%
  dplyr::count(funding_instrument)

grants <-
  p3data %>%
  purrr::pluck(1) %>%
  dplyr::filter(funding_instrument %in% dplyr::pull(nrp_selection, funding_instrument)) %>%
  dplyr::select(
    project_number,
    project_title,
    funding_instrument,
    university,
    approved_amount,
    responsible_applicant
  ) %>%
  dplyr::mutate(
    university = stringr::str_extract(university, "(?<=- )[[:upper:]]+$"),
    approved_amount = readr::parse_number(approved_amount),
    grant_link = glue::glue("http://p3.snf.ch/project-{ project_number }"),
    nrp_number = readr::parse_number(funding_instrument)
  )
usethis::ui_done("--- done tidying grants dataset ---")

people <-
  p3data %>%
  purrr::pluck(2) %>%
  dplyr::select(person_id_snsf,
                last_name,
                first_name,
                dplyr::starts_with("projects")) %>%
  # separate_rows(starts_with("projects")) %>% # FIXME: rlang error
  tidyr::separate_rows(projects_as_responsible_applicant) %>%
  tidyr::separate_rows(projects_as_applicant) %>%
  tidyr::separate_rows(projects_as_partner) %>%
  tidyr::separate_rows(projects_as_employee) %>%
  tidyr::separate_rows(projects_as_contact_person) %>%
  tidyr::gather(
    key = "role",
    value = "project_number",
    dplyr::starts_with("projects"),
    na.rm = TRUE
  ) %>%
  dplyr::semi_join(grants, by = "project_number") %>%
  dplyr::mutate(
    role = stringr::str_remove(role, "projects_as_"),
    last_name = iconv(last_name, to = "ASCII//TRANSLIT"),
    first_name = iconv(first_name, to = "ASCII//TRANSLIT"),
    person_link = glue::glue(
      "http://p3.snf.ch/person-{ person_id_snsf }-{ last_name }-{ first_name }"
    ),
    person_link = stringr::str_replace_all(person_link, " ", "-")
  ) %>%
  dplyr::distinct()
usethis::ui_done("--- done tidying poeple dataset ---")

nrp_summary <- grants %>%
  dplyr::distinct(nrp_number, funding_instrument) %>%
  dplyr::rename(number = nrp_number) %>%
  dplyr::arrange(number) %>%
  dplyr::mutate(description = purrr::map_chr(number, extract_description))
usethis::ui_done("--- done summary ---")


# save to
if (!dir.exists(here::here("Data"))) {
  usethis::ui_done("creating a new 'Data/' folder...")
  here::here("Data") %>%
    dir.create()
}
# TODO: write json instead?

grants %>%
  readr::write_rds(here::here("Data", "grants.rds.xz"), compress = "xz")
people %>%
  readr::write_rds(here::here("Data", "people.rds.xz"), compress = "xz")
nrp_summary %>%
  readr::write_rds(here::here("Data", "summary.rds.xz"), compress = "xz")
usethis::ui_done("update saved...")

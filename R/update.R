#' Updates data from p3.snf.ch
#'
#' @param filename a character matching filenames in p3.snf.ch/P3Export/
#'
#' @return a dataframe with the data (does not handle errors)
#'
#' @examples data_update("P3_PersonExport.csv")
data_update <- function(filename) {
  "updating data from http://p3.snf.ch/P3Export/{ filename }" %>%
    glue() %>%
    ui_info()
  "http://p3.snf.ch/P3Export/{ filename }" %>%
    glue() %>%
    read_csv2(col_types = cols(.default = "c")) %>%
    clean_names()
}

#' Extracts descriptions from NRP landing pages
#'
#' @param nrp_number the NRP number
#'
#' @return a character with the NRP description (does not handle errors)
#'
#' @examples extract_description(77)
extract_description <- function(nrp_number) {
  "extracting description from http://www.nfp{ nrp_number }.ch/en" %>%
    glue() %>%
    ui_info()
  "http://www.nfp{ nrp_number }.ch/en" %>%
    glue() %>%
    read_html() %>%
    html_nodes("#ctl00_PlaceHolderMain_Content__ControlWrapper_RichHtmlField p") %>%
    html_text() %>%
    str_squish() %>%
    str_c(collapse = "  ")
}

# main --------------------------------------------------------------------

p3data <- c("P3_GrantExport.csv", "P3_PersonExport.csv") %>%
  map(data_update)

# NRPs with running grants today
nrp_active <- p3data %>%
  pluck(1) %>%
  select(start_date, end_date, funding_instrument) %>%
  mutate_all(str_replace_all,
             pattern = "data not included in P3",
             replacement = NA_character_) %>%
  filter(
    str_detect(funding_instrument, str_c("NRP"))) %>%
  mutate(
    start_date = parse_date(start_date, format = "%d.%m.%Y"),
    end_date = parse_date(end_date, format = "%d.%m.%Y")) %>%
  filter(
    start_date <= today(),
    end_date >= today()) %>%
  count(funding_instrument) %>%
  rename(active_grants = n)

# extract descriptions from NRP landing pages
nrp_descriptions <-
  nrp_active %>%
  pull(funding_instrument) %>%
  parse_number() %>%
  map_chr(extract_description)

# tidy the data
grants <-
  p3data %>%
  pluck(1) %>%
  filter(funding_instrument %in% pull(nrp_active, funding_instrument)) %>%
  select(
    project_number,
    project_title,
    funding_instrument,
    university,
    approved_amount,
    responsible_applicant
  ) %>%
  mutate(
    university = str_extract(university, "(?<=- )[[:upper:]]+$"),
    approved_amount = parse_number(approved_amount),
    grant_link = glue("http://p3.snf.ch/project-{ project_number }"),
    nrp_number = parse_number(funding_instrument))

people <-
  p3data %>%
  pluck(2) %>%
  select(person_id_snsf, last_name, first_name, starts_with("projects")) %>%
  # separate_rows(starts_with("projects")) %>% # FIXME: rlang error
  separate_rows(projects_as_responsible_applicant) %>%
  separate_rows(projects_as_applicant) %>%
  separate_rows(projects_as_partner) %>%
  separate_rows(projects_as_employee) %>%
  separate_rows(projects_as_contact_person) %>%
  gather(key = "role", value = "project_number", starts_with("projects"), na.rm = TRUE) %>%
  semi_join(grants, by = "project_number") %>%
  mutate(
    role = str_remove(role, "projects_as_"),
    last_name = iconv(last_name, to = "ASCII//TRANSLIT"),
    first_name = iconv(first_name, to = "ASCII//TRANSLIT"),
    person_link = glue(
      "http://p3.snf.ch/person-{ person_id_snsf }-{ last_name }-{ first_name }"),
    person_link = str_replace_all(person_link, " ", "-")) %>% # multiple names
  distinct()

# save to
if (!dir.exists(here("Data"))) {
  ui_done("creating a new 'Data/' folder...")
  here("Data") %>%
    dir.create()
}

grants %>%
  write_rds(here("Data", "grants.rds.xz"), compress = "xz")
people %>%
  write_rds(here("Data", "people.rds.xz"), compress = "xz")
ui_done("update saved...")

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
    iconv(to = "ASCII//TRANSLIT") %>%
    str_c(collapse = "  ")
}


#' Dashboard block-builder
#'
#' @param n NRP number
#' @param proj project dataframe
#' @param ppl people dataframe
#'
#' @return a visNetwork widget
#'
#' @examples viz_build(75)
viz_build <- function(n, proj = grants, ppl = people) {
  proj <- filter(proj, nrp_number == n)
  ppl <- inner_join(ppl, proj, by = "project_number")

  proj_html <- proj %>%
    mutate(
      kchf = round(approved_amount / 1000),
      title_wrap = str_wrap(project_title, width = 35),
      title_wrap = str_replace_all(title_wrap, "\\n", "<br>"),
      title_html = glue(
        "<font size=\"1\">",
        "<a href=\"{ grant_link }\" ",
        "target=\"_blank\">{ title_wrap }</a>",
        "</font><br>",
        "<font size=\"1\" color=\"#202020\">",
        "{ kchf } kCHF</font>"))

  ppl_html <- ppl %>%
    mutate(
      full_name = glue("{ first_name } { last_name }"),
      title_html = glue(
        "<font size=\"1\">",
        "<a href=\"{ person_link }\" ",
        "target=\"_blank\">",
        "{ full_name }</a></font>"))

  nodes_grants <- proj_html %>%
    mutate(
      id0 = project_number,
      label = project_number,
      group = "Grants",
      institute = university,
      title = title_html,
      shadow = FALSE,
      shape = "icon",
      icon.code = "f15c",
      icon.color = "#118df0",
      icon.size = log(approved_amount) * 12) %>%
    select(id0,
           label,
           group,
           institute,
           title,
           shadow,
           shape,
           starts_with("icon"))

  nodes_people <- ppl_html %>%
    group_by(person_id_snsf) %>%
    summarise(
      label = head(full_name, 1),
      group = "People",
      institute = head(university, 1),
      title = head(title_html, 1),
      shadow = FALSE,
      shape = "icon",
      icon.code = "f007",
      icon.color = ifelse(
        "responsible_applicant" %in% role,
        "#ff304f",
        "#0e2f56"),
      icon.size = 120) %>%
    ungroup() %>%
    rename(id0 = person_id_snsf)

  # build nodes & edges -----------------------------------------------------
  nodes <- rbind(nodes_grants, nodes_people) %>%
    rownames_to_column("id") %>%
    ungroup()

  edges <- ppl %>%
    distinct() %>%
    mutate(width = 12, color = "#808080") %>%
    left_join(nodes, by = c("person_id_snsf" = "id0")) %>%
    rename(from = id) %>%
    left_join(nodes, by = c("project_number" = "id0")) %>%
    rename(to = id) %>%
    mutate(color = ifelse(
      role == "responsible_applicant",
      "#ff304f",
      color)) %>%
    select(from, to, width, color) %>%
    mutate(from = as.numeric(from), to = as.numeric(to))

  # visNetwork() output -----------------------------------------------------
  visNetwork(nodes, edges) %>%
    visIgraphLayout() %>%
    visPhysics(stabilization = FALSE) %>%
    visOptions(
      selectedBy = "institute",
      highlightNearest = list(enabled = TRUE, hover = TRUE)) %>%
    visInteraction(navigationButtons = TRUE)
}

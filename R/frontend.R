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
  proj <- dplyr::filter(proj, nrp_number == n)
  ppl <- dplyr::inner_join(ppl, proj, by = "project_number")

  proj_html <- proj %>%
    dplyr::mutate(
      kchf = round(approved_amount / 1000),
      title_wrap = stringr::str_wrap(project_title, width = 35),
      title_wrap = stringr::str_replace_all(title_wrap, "\\n", "<br>"),
      title_html = glue::glue(
        "<font size=\"1\">",
        "<a href=\"{ grant_link }\" ",
        "target=\"_blank\">{ title_wrap }</a>",
        "</font><br>",
        "<font size=\"1\" color=\"#202020\">",
        "{ kchf } kCHF</font>"
      )
    )

  ppl_html <- ppl %>%
    dplyr::mutate(
      full_name = glue::glue("{ first_name } { last_name }"),
      title_html = glue::glue(
        "<font size=\"1\">",
        "<a href=\"{ person_link }\" ",
        "target=\"_blank\">",
        "{ full_name }</a></font>"
      )
    )

  nodes_grants <- proj_html %>%
    dplyr::mutate(
      id0 = project_number,
      label = project_number,
      group = "Grants",
      institute = university,
      title = title_html,
      shadow = FALSE,
      shape = "icon",
      icon.code = "f15c",
      icon.color = "#118df0",
      icon.size = log(approved_amount) * 12
    ) %>%
    dplyr::select(id0,
                  label,
                  group,
                  institute,
                  title,
                  shadow,
                  shape,
                  dplyr::starts_with("icon"))

  nodes_people <- ppl_html %>%
    dplyr::group_by(person_id_snsf) %>%
    dplyr::summarise(
      label = head(full_name, 1),
      group = "People",
      institute = head(university, 1),
      title = head(title_html, 1),
      shadow = FALSE,
      shape = "icon",
      icon.code = "f007",
      icon.color = ifelse("responsible_applicant" %in% role,
                          "#ff304f",
                          "#0e2f56"),
      icon.size = 120
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(id0 = person_id_snsf)

  # build nodes & edges ------------------------------------------
  nodes <- dplyr::bind_rows(nodes_grants, nodes_people) %>%
    tibble::rownames_to_column("id") %>%
    dplyr::ungroup()

  edges <- ppl %>%
    dplyr::distinct() %>%
    dplyr::mutate(width = 12, color = "#808080") %>%
    dplyr::left_join(nodes, by = c("person_id_snsf" = "id0")) %>%
    dplyr::rename(from = id) %>%
    dplyr::left_join(nodes, by = c("project_number" = "id0")) %>%
    dplyr::rename(to = id) %>%
    dplyr::mutate(color = ifelse(role == "responsible_applicant",
                                 "#ff304f",
                                 color)) %>%
    dplyr::select(from, to, width, color) %>%
    dplyr::mutate(from = as.numeric(from), to = as.numeric(to))

  # output -------------------------------------------------------
  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visIgraphLayout() %>%
    visNetwork::visPhysics(stabilization = FALSE) %>%
    visNetwork::visOptions(selectedBy = "institute",
                           highlightNearest = list(enabled = TRUE,
                                                   hover = TRUE)) %>%
    visNetwork::visInteraction(navigationButtons = TRUE)
}

if (!require(pacman)) install.packages("pacman")
if (!require(janitor)) install.packages("janitor")
p_load("tidyverse", "lubridate", "stringr", "magrittr", "rmarkdown")
p_load("RCurl", "xml2")

p_load_gh("leeper/rio") # for dev version
# update and save the data ------------------------------------------------
grants <- rio::import("http://p3.snf.ch/P3Export/P3_GrantExport.csv") %>%
  janitor::clean_names()
write_rds(grants, "grants.rds.bz2", compress = "bz2")
ppl <- rio::import("http://p3.snf.ch/P3Export/P3_PersonExport.csv") %>%
  janitor::clean_names()
write_rds(ppl, "ppl.rds.bz2", compress = "bz2")

# NRPs with running grants ------------------------------------------------
nrp_active <- grants %>%
  select(start_date, end_date, funding_instrument) %>%
  filter(str_detect(funding_instrument, str_c("^NRP "))) %>%
  mutate(start_date = dmy(start_date), end_date = dmy(end_date)) %>%
  filter(start_date <= today(), end_date >= today()) %$%
  unique(funding_instrument) %>%
  sort()

nrp_numbers <- nrp_active %>%
  str_extract("[[:digit:]]+") %>%
  as.numeric()

parse_description <- function(nrp_number) {
  sprintf("http://www.nfp%d.ch/en", nrp_number) %>%
    getURL(.encoding = "UTF-8") %>%
    read_html() %>%
    xml_find_all("//*[@id=\"ctl00_PlaceHolderMain_Content__ControlWrapper_RichHtmlField\"]") %>%
    xml_text() %>%
    iconv(to = "ASCII//TRANSLIT")
}

nrp_descriptions <- map_chr(nrp_numbers, parse_description)

# build and deploy storyboard ---------------------------------------------

story_snippet <- function(nrp_name, nrp_description) {
  sprintf(" \n### %s\n \n```{r}\nviz_build(\"%s\")\n```\n  \n  \n***\n  \n  \n  - **Description**: %s \n  \n  \n - **Data**: [http://p3.snf.ch/](http://p3.snf.ch/Pages/DataAndDocumentation.aspx)\n  \n - **Credits**: [flexdashboard](https://github.com/rstudio/flexdashboard), [visNetwork](https://datastorm-open.github.io/visNetwork/)\n  \n - **More**: [SNSF National Research programmes](http://www.snf.ch/en/researchinFocus/nrp/Pages/default.aspx)\n  \n<hr>\n  \nLast updated: `r Sys.time()`.  \nCompiled with `r version$version.string`.\n  \n  \n", nrp_name, nrp_name, nrp_description)
}

rmd_head <- readLines("head.rmd") %>% str_c(collapse = "\n")
rmd_body <- map2_chr(nrp_active, nrp_descriptions, story_snippet) %>%
  str_c(collapse = "\n")

writeLines(str_c(rmd_head, rmd_body, collapse = "\n"), "index.rmd")
rmarkdown::render("index.rmd", output_file = "index.html")
file.remove("index.rmd")

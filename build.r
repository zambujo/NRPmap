# boilerplate -------------------------------------------------------------
packages <- c(
  "tidyverse",
  "janitor",
  "rmarkdown",
  "lubridate",
  "knitr",
  "rvest",
  "usethis",
  "glue",
  "here",
  "curl",
  "conflicted"
)
sapply(packages, library, character.only = TRUE)
conflict_prefer("filter", "dplyr")
conflict_prefer("parse_date", "readr")
conflict_prefer("pluck", "purrr")
conflict_prefer("here", "here")
rm(packages)




## TODO : continue refactoring from here

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

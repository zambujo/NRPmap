package_list <- c(
  "tidyverse",
  "janitor",
  "rmarkdown",
  "lubridate",
  "knitr",
  "rvest",
  "usethis",
  "glue",
  "curl",
  "emojifont",
  "flexdashboard",
  "visNetwork",
  "knitr",
  "conflicted")
sapply(package_list,
       library,
       character.only = TRUE)
ui_info("--- packages loaded ---")

conflict_prefer("filter", "dplyr")
conflict_prefer("parse_date", "readr")
conflict_prefer("pluck", "purrr")
conflict_prefer("here", "here")
ui_info("--- conflicts sorted ---")
rm(package_list)

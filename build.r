rmd_head <- readLines("head.rmd") %>% str_c(collapse = "\n")
rmd_body <- map2_chr(nrp_active, nrp_descriptions, story_snippet) %>%
  str_c(collapse = "\n")

writeLines(str_c(rmd_head, rmd_body, collapse = "\n"), "index.rmd")
rmarkdown::render("index.rmd", output_file = "index.html")
file.remove("index.rmd")

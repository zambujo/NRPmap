usethis::ui_info("updating data ....")
here::here("R", "update.R") %>%
  source()

usethis::ui_done("rendering the storyboard .....")
here::here("Rmd", "storyboard.Rmd") %>%
  rmarkdown::render(
    output_format = "flexdashboard::flex_dashboard",
    output_file = "index.html",
    output_dir = here("docs"),
    quiet = FALSE
  )

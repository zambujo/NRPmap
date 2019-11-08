library(here)
source(here("R", "boilerplate.R"))
source(here("R", "local.R"))

# update the data ----
source(here("R", "update.R"))

# update the storyboard ----
render(
  here("Rmd", "storyboard.Rmd"),
  output_format = "flexdashboard::flex_dashboard",
  output_file = "index.html",
  output_dir = here("docs"),
  quiet = FALSE)

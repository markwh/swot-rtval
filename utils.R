# Utility functions

rtload <- function() {
  pkgloc <- "~/Documents/rivertile"
  devtools::document(pkg = pkgloc)
  devtools::load_all(pkgloc, export_all = FALSE)
}


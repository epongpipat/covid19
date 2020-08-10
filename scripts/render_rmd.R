#!/usr/local/bin/Rscript --vanilla
rmarkdown::render(input = "scripts/covid19.Rmd", output_file = "docs/index.html")
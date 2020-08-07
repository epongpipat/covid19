#!/usr/local/bin/Rscript --vanilla

packages <- c("rmarkdown", "glue")
xfun::pkg_attach2(packages, message = F)

render(input = glue("scripts/covid19.Rmd"), 
       output_file = glue("docs/covid19.html"))

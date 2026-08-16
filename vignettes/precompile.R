# vignettes/precompile.R
#
# Precompute the Monte-Carlo-heavy vignettes. Each `*.Rmd.orig` source actually
# runs its simulations/fits; this script knits it to a static `*.Rmd` whose
# code is shown but NOT executed at package-build time, with the real outputs
# (and any figures) baked in. That keeps `R CMD build` fast and within CRAN's
# vignette-build budget while the rendered vignette still shows genuine
# results.
#
# Run from the repo root:
#   Rscript vignettes/precompile.R
#
# Commit the generated `*.Rmd`, the `*.Rmd.orig` sources, and any figure files
# this writes into vignettes/. The `.orig` sources and this script are listed
# in .Rbuildignore so they do not ship in the built package.

# load_all() the dev package first (at the repo root), then knit from the
# vignettes directory so relative fig.path values resolve beside the output.
pkgload::load_all(".", quiet = TRUE)

vig_dir <- "vignettes"
origs <- list.files(vig_dir, pattern = "\\.Rmd\\.orig$")

old_wd <- setwd(vig_dir)
on.exit(setwd(old_wd), add = TRUE)

for (f in origs) {
  out <- sub("\\.orig$", "", f)
  message("Precompiling ", f, " -> ", out)
  knitr::knit(input = f, output = out)
}

message("Done. Generated: ", paste(sub("\\.orig$", "", origs), collapse = ", "))

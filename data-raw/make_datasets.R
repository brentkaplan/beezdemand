## This script builds package datasets (.rda) from raw CSVs.
## Run locally with: source("data-raw/make_datasets.R")

op <- options(stringsAsFactors = FALSE)
on.exit(options(op), add = TRUE)

dir.create("data", showWarnings = FALSE)

# lowNicClean --------------------------------------------------------------
if (file.exists("data/lowNicClean.csv")) {
  lowNicClean <- tryCatch(read.csv("data/lowNicClean.csv"), error = identity)
  if (!inherits(lowNicClean, "error")) {
    save(lowNicClean, file = "data/lowNicClean.rda", compress = "xz")
  }
}

# cannabisCigarettes -------------------------------------------------------
# The source CSV is kept outside the repository (internal_docs/ is not tracked)
# and already holds the sequential integer ids; no id conversion happens here.
if (file.exists("internal_docs/cannabisCigarettes.csv")) {
  cannabisCigarettes <- tryCatch(read.csv("internal_docs/cannabisCigarettes.csv"), error = identity)
  if (!inherits(cannabisCigarettes, "error")) {
    cannabisCigarettes$Q1035 <- NULL
    stopifnot(is.integer(cannabisCigarettes$id), !anyNA(cannabisCigarettes$id))
    save(cannabisCigarettes, file = "data/cannabisCigarettes.rda", compress = "xz")
  }
}

# ongoingETM ---------------------------------------------------------------
if (file.exists("data/ongoingETM.csv")) {
  ongoingETM <- tryCatch(read.csv("data/ongoingETM.csv"), error = identity)
  if (!inherits(ongoingETM, "error")) {
    save(ongoingETM, file = "data/ongoingETM.rda", compress = "xz")
  }
}

message("Datasets built (if source CSVs were found). Run devtools::document() next.")


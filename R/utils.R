##' Manufacture an output directory that is unique
##'
##' Creates out directory and if proposed new directory already
##' exists, then a new name is proposed, and checked, until a unique
##' new directory name is found.
##'
##' A more detailed description here.
##' @param basedir Where to place the output file? (slash on end needed)
##' @param basename Proposed name of output directory (slash on end needed)
##' @param timestring Leave as default, will use MMDD to give unique directory name.
##' @return A string with a directory name
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
createOutdir <- function(basedir = "../", basename = "/output/",
                         timestring = format(Sys.time(), "%m%d"))
{
    outdir <- paste0(basedir, basename, timestring)
    j <- 1
    outcandidate <- outdir
    while(file.exists(outcandidate)){
        j <- j + 1
        outcandidate <- paste0(outdir, "-", j)
        outcandidate <- gsub("//", "/", outcandidate)
    }
    outdir <- paste0(outcandidate, "/")
    if(!file.exists(outdir)) res <- dir.create(outdir, recursive = TRUE)
    if (!res) stop("output directory could not be created")
    list(outdir, basename)
}

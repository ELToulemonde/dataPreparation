# data_preparation_news
# -------------
#' Show the NEWS file
#'
#'Show the NEWS file of the dataPreparation package.
#' @export
data_preparation_news <- function() {
    newsfile <- system.file("NEWS", package = "dataPreparation")
    file.show(newsfile)
}

# .onAttach
# ----------
.onAttach <- function(libname, pkgname) {
    dpver <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
    fields = "Version")
    packageStartupMessage(paste(pkgname, dpver))
    packageStartupMessage("Type data_preparation_news() to see new features/changes/bug fixes.")
}
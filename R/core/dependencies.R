swms_required_packages <- function() {
  c(
    "curl",
    "fs",
    "future.apply",
    "here",
    "httr",
    "jsonlite",
    "lubridate",
    "png",
    "progress",
    "progressr",
    "stringr",
    "terra",
    "xml2",
    "yaml"
  )
}

install_and_load_dependencies <- function(packages = swms_required_packages()) {
  if (isTRUE(getOption("swms.dependencies.loaded", FALSE))) {
    return(invisible(TRUE))
  }

  repos <- getOption("repos")
  cran_repo <- unname(repos["CRAN"])
  if (is.null(repos) || is.na(cran_repo) || identical(cran_repo, "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  missing_packages <- packages[!vapply(
    packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )]

  if (length(missing_packages) > 0) {
    message(
      "Installing missing R packages: ",
      paste(missing_packages, collapse = ", ")
    )
    install.packages(
      missing_packages,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }

  unavailable_packages <- packages[!vapply(
    packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )]

  if (length(unavailable_packages) > 0) {
    stop(
      "The following R packages could not be installed or loaded: ",
      paste(unavailable_packages, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
  options(swms.dependencies.loaded = TRUE)
  invisible(TRUE)
}

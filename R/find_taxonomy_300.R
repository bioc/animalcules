#' Find the taxonomy for maximum 300 tids
#'
#' @param tids Given taxonomy ids
#' @return taxondata Data with the taxonomy information
#' @import rentrez
#' @import XML

#' @examples
#' \donttest{
#' taxonLevels <- find_taxonomy_300(tids = 1200)
#' }
#'
#' @export
find_taxonomy_300 <- function(tids) {
    if (is.null(tids)) {
        return(NULL)
    }
    na.vec <- c()
    for (i in seq_len(length(tids))) {
        if (is.na(tids[i])) {
            na.vec <- c(na.vec, i)
        }
    }
    r_fetch <- fetch_taxonomy_xml(tids)
    dat <- xmlToList(r_fetch)
    taxonLevels <- lapply(dat, function(x) x$LineageEx)
    if (!is.null(na.vec)) {
        for (i in seq_len(length(na.vec))) {
            taxonLevels <- append(taxonLevels, list(NA), na.vec[i] - 1)
        }
    }
    return(taxonLevels)
}

#' Fetch taxonomy XML records from NCBI Entrez
#'
#' Wraps \code{rentrez::entrez_fetch()} so that transient failures (network
#' errors, or NCBI returning an HTML error page when rate-limited) are
#' retried instead of being passed to the XML parser.
#'
#' @param tids Given taxonomy ids
#' @param max_tries Maximum number of attempts
#' @param wait Base number of seconds to wait between attempts
#' @return A character string containing the taxonomy XML
#' @noRd
fetch_taxonomy_xml <- function(tids, max_tries = 3, wait = 2) {
    last_err <- NULL
    for (attempt in seq_len(max_tries)) {
        res <- tryCatch(
            entrez_fetch(db = "taxonomy", id = tids, rettype = "xml"),
            error = function(e) e
        )
        if (inherits(res, "error")) {
            last_err <- res
        } else if (is.character(res) && length(res) == 1 &&
            grepl("^\\s*<\\?xml", res) && grepl("<TaxaSet", res, fixed = TRUE)) {
            return(res)
        } else {
            last_err <- simpleError(paste(
                "NCBI Entrez returned a non-XML response",
                "(likely an HTML error page)"
            ))
        }
        if (attempt < max_tries) {
            Sys.sleep(wait * attempt)
        }
    }
    stop(
        "Unable to retrieve taxonomy information from NCBI Entrez after ",
        max_tries, " attempts: ", conditionMessage(last_err),
        call. = FALSE
    )
}

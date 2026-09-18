#' Find the Taxonomy Information Matrix
#'
#' @param names Row names of the taxonomy matrix
#' @param taxonLevels Taxon Levels of all tids
#' @return taxmat Taxonomy Information Matrix
#'
#' @examples
#' # Lineage information in the format returned by find_taxonomy(),
#' # constructed here so the example does not require network access
#' taxonLevels <- list(list(
#'     Taxon = list(TaxId = "2", ScientificName = "Bacteria", Rank = "domain"),
#'     Taxon = list(TaxId = "1224", ScientificName = "Pseudomonadota",
#'         Rank = "phylum"),
#'     Taxon = list(TaxId = "570", ScientificName = "Klebsiella", Rank = "genus")
#' ))
#' tax_table <- find_taxon_mat("ti|573", taxonLevels)
#'
#' \donttest{
#' ids <- c("ti|54005", "ti|73001", "ti|573", "ti|228277", "ti|53458")
#' tids <- c("54005", "73001", "573", "228277", "53458")
#' taxonLevels <- find_taxonomy(tids)
#' tax_table <- find_taxon_mat(ids, taxonLevels)
#' }
#'
#' @export

find_taxon_mat <- function(names, taxonLevels) {
    # tax.name <- c('superkingdom', 'kingdom', 'phylum', 'class', 'order',
    #     'suborder', 'family', 'subfamily', 'genus', 'subgenus', 'species',
    #     'subspecies', 'no rank')
    tax.name <- c(
        "superkingdom", "kingdom", "phylum", "class", "order",
        "family", "genus", "species"
    )
    tl <- c()
    for (i in seq_len(length(tax.name))) {
        tl[tax.name[i]] <- "others"
    }
    taxmat <- NULL
    for (i in seq_len(length(taxonLevels))) {
        taxrow <- tl
        tLineageEx <- taxonLevels[[i]]
        for (j in seq_len(length(tLineageEx))) {
            rank <- tLineageEx[[j]]["Rank"]
            # taxid <- tLineageEx[[j]]["TaxId"]
            scientificName <- tLineageEx[[j]]["ScientificName"]
            # NCBI Taxonomy renamed the 'superkingdom' rank to 'domain'
            if (!is.null(rank) && identical(as.character(rank), "domain")) {
                rank <- "superkingdom"
            }
            if (!is.null(rank) && !is.na(rank) && rank %in% tax.name) {
                # taxrow[as.character(rank)] <- as.character(taxid)
                taxrow[as.character(rank)] <- as.character(scientificName)
            }
        }
        taxmat <- rbind(taxmat, taxrow)
    }
    rownames(taxmat) <- names
    return(taxmat)
}

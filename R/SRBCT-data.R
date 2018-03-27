#' Small round blue cell tumors (SRBCTs) microarray data
#'
#' The SRBCT dataset looks at classifying 4 classes of different childhood tumours sharing
#' similar visual features during routine histology. These classes include Ewing's family of tumours (EWS), neuroblastoma (NB),
#' Burkitt's lymphoma (BL), and rhabdomyosarcoma (RMS). Data was collected from 83 cDNA microarrays, with 1586 features present after
#' filtering for genes with zero Median Absolute Deviation. Further, we standardised the dataset to obtain the final data set uploaded.
#' @docType data
#'
#' @usage data(SRBCT)
#'
#' @format An list containing class labels \code{vy} and corresponding microarray measurements \code{mX}
#'
#' @keywords datasets
#'
#' @references Khan et al. (2001) Nature Medicine 7:673 EP
#' (\href{https://www.nature.com/articles/nm0601_673}{Nature Medicine})
#'
#' @source \href{http://www.biolab.si/supp/bi-cancer/projections/info/SRBCT.html}{Biolab}
#'
#' @examples
#' data(SRBCT)
#' vy <- SRBCT$vy
#' mX <- SRBCT$mX

"SRBCT"

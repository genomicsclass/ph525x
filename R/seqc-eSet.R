##library(seqc)

##' Creates an ExpressionSet of counts for a given subset of the samples
##' provided by the seqc data package.
##'
##' Currently this only works for \code{feature == 'gene'}.
##'
##' @param feature The type of features you want to build an ExpressionSet
##'   out of (gene, junction, taqman
##' @param annotation If \code{feature == 'gene'}, then this determines which
##'   set of gene features you want (refseq, aceview)
##' @param platform Subset the data based on the platform it was sequenced on?
##'   (ILM, ROC, LIF)
##' @param center Subset the data based on the center that sequenced the
##'   libraries?
##'
##' @return An ExpressionSet with the counts from the samples that satisfy the
##'   critera set by the function parameters.
seqc.eSet <- function(feature=c('gene', 'junction', 'taqman'),
                      annotation=c('refseq', 'aceview'),
                      platform=NULL,
                      center=NULL) {
  feature <- match.arg(feature)
  annotation <- match.arg(annotation)

  if (feature == 'taqman') {
    out <- .create.SEQC.taqman.eSet()
  } else if (feature == 'gene') {
    out <- .create.SEQC.gene.eSet(annotation, platform, center)
  } else {
    stop("Not dealing with junctions for now")
    out <- .create.SEQC.junction.eSet(platform, center)
  }

  out
}

##' Returns a character vector of the available SEQC samples
##'
##' @param feature Limit samples to be of "gene" or "junction"
##' @param annotation Limit samples to a particular annotation source (refseq or
##'   aceview)
##' @param platform Limit samples to be of a particular platform (ILM, LIF, ROC)
##' @param center Limit samples to only include those of a particulr center
##'
##' @return a character vector of the sample names/objects in the package:seqc
##'   namespace
seqc.samples <- function(feature=NULL, annotation=NULL, platform=NULL,
                         center=NULL) {
  ls.idx <- which(search() == 'package:seqc')
  fns.all <- ls(ls.idx)
  fns <- fns.all

  any.taqman <- c(feature == 'taqman', annotation == 'taqman',
                  platform == 'taqman', center == 'taqman')
  if (any(any.taqman)) {
    return('taqman')
  }

  if (is.character(feature)) {
    features <- unique(.parse.feature(fns.all))
    feature <- match.arg(feature, features)
    fns <- fns[.parse.feature(fns) == feature]
  }

  if (is.character(annotation)) {
    annotation <- match.arg(annotation, c('refseq', 'aceview'))
    ## annotation plays no role in the junction files
    annotation <- c(annotation, 'junction')
    annotations <- .parse.annotation(fns)
    fns <- fns[annotations %in% annotation]
  }

  if (is.character(platform)) {
    platforms <- .parse.platform(fns.all)
    platform <- match.arg(platform, platforms, several.ok=TRUE)
    fns <- fns[.parse.platform(fns) %in% platform]
  }

  if (is.character(center)) {
    centers <- setdiff(.parse.center(fns.all), 'taqman')
    center <- match.arg(center, centers, several.ok=TRUE)
    fns <- fns[.parse.center(fns) %in% center]
  }

  fns
}

## -----------------------------------------------------------------------------
## eSet generation helper functions

##' Create eSet for all samples from given platform and site
.create.SEQC.gene.eSet <- function(annotation, platform, center) {
  ## Ensure that all rows are the same across every data.frame in x
  seqc.idx <- which(search() == 'package:seqc')

  fns <- seqc.samples(feature='gene', annotation=annotation, platform=platform,
                      center=center)
  if (length(fns) == 0) {
    stop("No samples fit the selection criteria to build a gene eSet")
  }

  ## Check that the meta data for each dataset is the same
  meta.cols <- c('EntrezID', 'Symbol', 'GeneLength', 'IsERCC')
  meta <- lapply(fns, function(xx) get(xx, seqc.idx)[, meta.cols])
  kosher <- sapply(meta, function(m) all.equal(m, meta[[1]]))
  if (!all(kosher)) {
    stop("Features across each data gene-level data file are not the same")
  }

  ## Build the counts matrix
  platform <- c()
  counts <- do.call(cbind, lapply(fns, function(xx) {
    cnts <- get(xx, seqc.idx)
    out <- as.matrix(cnts[, setdiff(names(cnts), meta.cols)])
    colnames(out) <- paste(colnames(out), .parse.center(xx), sep='_')
    platform <<- c(platform, rep(.parse.platform(xx), ncol(out)))
    out
  }))

  ## Parse the names of the columns in this matrix to something useful
  s.info <- strsplit(colnames(counts), '_')
  pd <- data.frame(
    platform=factor(platform),
    sample=factor(sapply(s.info, '[[', 1L)),
    replicate=factor(sapply(s.info, '[[', 2L)),
    lane=factor(sapply(s.info, '[[', 3L)), ## or region for 454
    flowcell=factor(sapply(s.info, function(x) {
      if (length(x) == 5) x[4L] else '454'
    })),
    center=factor(sapply(s.info, tail, 1L)))

  out <- ExpressionSet(counts)
  pData(out) <- pd
  fData(out) <- meta[[1]]
  out
}

.create.SEQC.junction.eSet <- function(x) {
  stop("Junction stuff not yet implemented")
}

.create.SEQC.taqman.eSet <- function(x) {
  stop("Taqman generation not yet implemented")
}

## -----------------------------------------------------------------------------
## Utility functions to parse information out of the objects loaded up by the
## seqc package
.parse.platform <- function(x) {
  sapply(strsplit(x, '_'), '[[', 1L)
}

## either aceview, refseq, junction, or taqman
.parse.annotation <- function(x) {
  x <- sapply(strsplit(x, '_'), '[', 2L)
  ifelse(is.na(x), 'taqman', x)
}

## either gene or junction
.parse.feature <- function(x) {
  out <- .parse.annotation(x)
  ifelse(out == 'taqman', 'gene', ifelse(out == 'junction', out, 'gene'))
}

.parse.center <- function(x) {
  pieces <- strsplit(x, '_')
  anno <- .parse.annotation(x)
  ifelse(anno == 'junction', sapply(pieces, '[', 3), sapply(pieces, tail, 1))
}

.parse.sample <- function(x) {
  stop("This is only for junction stuff, which we ignore for now")
}

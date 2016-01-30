\name{ph525x-package}
\alias{ph525x-package}
\alias{ph525x}
\docType{package}
\title{
ph525x, extra software
}
\description{
Software that can be used with the ph525x course that is not
yet in a CRAN or Bioconductor package.

}
\details{

\preformatted{
The DESCRIPTION file: 

Package: ph525x
Title: support functions for edX course PH525
Version: 0.0.12
Author: VJ Carey <stvjc@channing.harvard.edu> and others
Description: basic functions not yet in released software
Suggests: Homo.sapiens, TxDb.Hsapiens.UCSC.hg19.knownGene,        
	org.Hs.eg.db, GO.db, RSQLite, DBI,
	OrganismDbi, GenomicFeatures, AnnotationDbi,
	Biobase, grid, GenomicRanges,
	GenomeInfoDb, IRanges, S4Vectors,
	BiocGenerics, seqc,
	rmarkdown, knitr, weaver, BiocInstaller
Depends: png, grid, Biobase
Maintainer: VJ Carey <stvjc@channing.harvard.edu>
License: Artistic-2.0
LazyLoad: yes
}


Software that can be used with the ph525x course that is not
yet in a CRAN or Bioconductor package.  An illustrative vignette
is available.

}
\author{
VJ Carey


}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
ph525x() # verify installation
viewHreads14()
}

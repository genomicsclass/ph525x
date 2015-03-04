\name{modPlot}
\alias{modPlot}
\alias{modTracks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
visualize a gene model 
}
\description{
visualize a gene model from a resource that can be
processed via GeneRegionTrack in Gviz
}
\usage{
modPlot(symbol, genome = "hg19", annoResource = Homo.sapiens, getter = exonsBy, byattr = "gene", expansion = 0, collapseTranscripts = "meta", useGeneSym = TRUE, plot.it = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbol}{
gene symbol resolvable in \code{annoResource}
}
  \item{genome}{
character tag
}
  \item{annoResource}{
OrganismDb or TxDb instance
}
  \item{getter}{
exon GRanges generator
}
  \item{byattr}{
value of "by" argument to \code{getter}
}
  \item{expansion}{
numeric, number of bp upstream and downstream of gene
region to include
}
  \item{collapseTranscripts}{
if TRUE a single gene model is provided; if FALSE all
transcripts displayed separately
}
  \item{useGeneSym}{
if TRUE each transcript labeled with gene symbol, if FALSE,
UCSC labels are used
}
  \item{plot.it}{
if TRUE plotTracks is run, if FALSE, Gviz track object is returned
}
  \item{\dots}{
passed to \code{\link[Gviz]{plotTracks}}
}
}
\details{
In cases of overlapping gene regions, transcript information
from another gene may appear in the plot.
}
\value{
depends on value of plot.it
}
%\references{
%%% ~put references to the literature/web site here ~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
modPlot("NISCH", collapse=FALSE, useGeneSym=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

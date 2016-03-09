\name{rainfall}
\alias{rainfall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
produce an embellished rainfall plot using TCGA mutation data
}
\description{
produce an embellished rainfall plot using TCGA mutation data
}
\usage{
rainfall(mut, colmap = kataColors(), oind = 1, 
   ptcex = 0.1, dodensity = TRUE, inbw = 1000, 
   dvscale = 2, ..., splitter = "Tumor_Sample_Barcode", 
   legcex = 0.8, legy = 7, legxdenom = 3, xaxgen = rainxax, ymax = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mut}{
data frame with mutations on multiple samples, as returned by
RTCGAToolbox, for example
}
  \item{colmap}{
A vector of color names; names of vector elements are nucleotide
substitution symbols
}
  \item{oind}{
index into the set of samples.  The function will
split the data by sample identifier and sort by number
of mutations present (decreasing).  oind=1 will select
sample with greatest number of mutations; oind=2 will
select sample with second greatest, ...
}
  \item{ptcex}{
cex value for plotting points
}
  \item{dodensity}{
if TRUE a density estimate will be computed and plotted
}
  \item{inbw}{
bw setting for stats::density
}
  \item{dvscale}{
density estimate will be scaled to have maximum value dvscale
}
  \item{\dots}{
additional arguments to plot
}
  \item{splitter}{
name of variable used to split the mutation data.frame into different samples
}
  \item{legcex}{
cex setting for the legend call
}
  \item{legy}{
y value for positioning legend
}
  \item{legxdenom}{
fraction along x axis to at which to center legend
}
  \item{xaxgen}{
function that generates a call to axis(1, ...)
}
  \item{ymax}{
ylim setting for plot call
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
invisibly returns a list with elements mutGR, density, and thecall
}
\references{
\url{http://www.pubmedcentral.nih.gov/articlerender.fcgi?artid=3776390&tool=pmcentrez&rendertype=abstract}
}
\author{
VJ Carey <stvjc@channing.harvard.edu>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(brcaMuts)
par(mfrow=c(3,1), mar=c(4,6,2,2))
for (i in 1:3) rainfall(brcaMuts, oind=i)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

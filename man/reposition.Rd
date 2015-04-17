\name{reposition}
\alias{reposition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
move all ranges in a GRanges to randomly chosen chromosomes with new
starting positions
}
\description{
move all ranges in a GRanges to randomly chosen chromosomes with new
starting positions
}
\usage{
reposition(gr, backoff = -20000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr}{
a \code{\link[GenomicRanges]{GRanges-class}} instance
}
  \item{backoff}{
an integer in bases to move a range whose end hangs over
end of chromosome
}
}
\details{
Purpose is to obtain a set of ranges with widths identical
to \code{gr}, but randomly relocated over the genome
}
\value{
A \code{GRanges}
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
\note{
It would be nice to be able to reposition to locations
that are remote but that share certain characteristics
like GC proportion ...

Currently if a chromosome is not represented in \code{gr}
it is not a candidate for hosting a repositioned range.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
set.seed(1234)
gr = GRanges(c("chr1", "chr4"), IRanges(c(50000, 100000), c(100000, 200000)))
reposition(gr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

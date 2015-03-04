\name{genemodel}
\alias{genemodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
obtain GRanges for exons of named gene
}
\description{
obtain GRanges for exons of named gene
}
\usage{
genemodel(sym, genome = "hg19", 
  annoResource = Homo.sapiens, getter = exonsBy, byattr = "gene")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sym}{
symbol that can be resolved to ENTREZID for \code{annoResource}
}
  \item{genome}{
tag for genome build in use
}
  \item{annoResource}{
OrganismDbi or TxDb instance
}
  \item{getter}{
function to call to get exonRanges
}
  \item{byattr}{
attribute to use with \code{getter}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
GRanges instance
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
genemodel("NISCH")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

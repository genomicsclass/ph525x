
getilgf = function() {
suppressPackageStartupMessages({
 library(ph525x)
 library(seqc)
 library(Biobase)
})
 seqc.eSet("gene", "refseq", "ILM")
}

showGeneBySite = function(gene="ORMDL3", samptype="A", builder=getilgf,
  delta = NULL) {
#
# objective: illustrate variability in level and spread of seqc data
# on a single gene, either the Universal Human Reference RNA (delta==NULL)
# or the difference between UHRR and Human Brain Reference RNA (delta=="B")
#
 es = builder()  # get seqc data
 stopifnot(is(es, "ExpressionSet"))
 ind = grep(gene, fData(es)$Symbol)  # find the gene
 stopifnot(length(ind)==1)
 myg = es[ ind, es$sample == samptype ]  # limit to gene
 ex = exprs(myg)[1,]
 theylab = paste("read counts for", gene, "sample type", samptype)
 if (!is.null(delta)) {
    stopifnot(delta %in% es$sample)
    myg2 = es[ ind, es$sample == delta ]
    ex = ex - exprs(myg2)[1,]
    theylab = paste("difference in read counts for", gene, ",", samptype,
     "-", delta )
    }
 boxplot(ex~myg$center, ylab=theylab, xlab="SEQC site")
}
 

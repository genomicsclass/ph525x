
viewHreads14 = function( bamind=1, gene14="BCL2L2" ) {
 require(RNAseqData.HNRNPC.bam.chr14)
 require(Gviz)
 gnmod = try(genemodel(gene14))  # sloppy
 if (inherits(gnmod, "try-error")) stop("can't make gene model")
 stopifnot(as.character(seqnames(gnmod))=="chr14")
 ps <- RNAseqData.HNRNPC.bam.chr14_BAMFILES
 A1 = AlignmentsTrack( ps[bamind], chromosome="chr14")
 bm = modPlot(gene14,
    useGeneSym=FALSE, collapseTranscripts=FALSE, plot.it=FALSE)
 plotTracks(list("bam1"=A1, bm[[1]], GenomeAxisTrack()), from=start(range(gnmod))-1000, to=end(range(gnmod))+1000, sizes=c(4,2,1))
}


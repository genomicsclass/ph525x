multScan = function (sym = "ESRRA", listOfBindingGRs, radius = 500000) 
{
    stopifnot(is.list(listOfBindingGRs))
    require(ERBS)
    require(Gviz)
    require(Homo.sapiens)
    require(TxDb.Hsapiens.UCSC.hg19.knownGene)
    txdb = TxDb.Hsapiens.UCSC.hg19.knownGene
    eid = select(Homo.sapiens, keys = sym, keytype = "SYMBOL", 
        columns = "ENTREZID")
    allg = genes(txdb)
    must_concat = FALSE
    curgAddr = genes(txdb, single.strand = FALSE, vals = list(gene_id = eid$ENTREZID))[[1]]
    if (length(curgAddr) > 1) {
        must_concat = TRUE
        curgAddr$gene_id = eid$ENTREZID
        curgAddr = curgAddr[which(as.character(seqnames(curgAddr)) %in% 
            paste0("chr", c(1:22, "X", "Y")))[1]]
    }
    curgNeigh = subsetByOverlaps(allg, curgAddr + radius)
    if (must_concat) 
        curgNeigh = c(curgNeigh, curgAddr)
    curgNeigh$symbol = mapIds(Homo.sapiens, keys = curgNeigh$gene_id, 
        keytype = "ENTREZID", column = "SYMBOL")
    DTL = lapply(listOfBindingGRs, function(x) DataTrack(subsetByOverlaps(x, 
        range(curgNeigh))[,7]))
    message("getting ideogram...")
    idxTrack = IdeogramTrack(genome = "hg19", chr = as.character(seqnames(curgNeigh[1])))
    message("done.")
    plotTracks(c(idxTrack, GenomeAxisTrack(), DTL, GeneRegionTrack(curgNeigh, 
        showId = TRUE, name = paste("genes near", sym)), GenomeAxisTrack()))
}

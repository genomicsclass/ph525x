esrraScan = function(sym = "ESRRA", bindingGR, radius=500000) {
require(ERBS)
require(Gviz)
require(Homo.sapiens)
require(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb = TxDb.Hsapiens.UCSC.hg19.knownGene
eid = select(Homo.sapiens, keys=sym, keytype="SYMBOL", columns="ENTREZID")
allg = genes(txdb) # multistrand are dropped
must_concat = FALSE
curgAddr = genes(txdb, single.strand=FALSE, filter=list(gene_id=eid$ENTREZID) )[[1]]
if (length(curgAddr)>1) {
   must_concat = TRUE
   curgAddr$gene_id = eid$ENTREZID
   curgAddr = curgAddr[ which(as.character(seqnames(curgAddr)) %in%
         paste0("chr", c(1:22,"X", "Y")))[1] ]
   }
curgNeigh = subsetByOverlaps(allg, curgAddr+radius) 
if (must_concat) curgNeigh = c(curgNeigh, curgAddr)
curgNeigh$symbol = mapIds(Homo.sapiens, keys=curgNeigh$gene_id, keytype="ENTREZID",
  column="SYMBOL")
sc = subsetByOverlaps(bindingGR, range(curgNeigh))
message("getting ideogram...")
idxTrack = IdeogramTrack(genome="hg19", chr=as.character(seqnames(curgNeigh[1])))
message("done.")
plotTracks(list(idxTrack, GenomeAxisTrack(), 
   DataTrack(sc[,7], name="ESRRA peak values"), 
   GeneRegionTrack(curgNeigh, showId=TRUE,
         name=paste("genes near", sym)), GenomeAxisTrack()))
}



genemodel = function(sym, genome="hg19", annoResource=Homo.sapiens,
   getter=exonsBy, byattr="gene") {
 stopifnot(is.atomic(sym) && (length(sym)==1))
 require(deparse(substitute(annoResource)), character.only=TRUE)
 num = select(annoResource, keys=sym, keytype="SYMBOL", 
    columns=c("ENTREZID", "SYMBOL"))$ENTREZID
 getter(annoResource, by=byattr)[[num]]
}

getTxDb = function(x) {
   require(deparse(substitute(x)), character.only=TRUE)
   curtxdb = get(deparse(substitute(x)))
   get(slot(curtxdb, "keys")[2,2]) #get(x@matrix[2,2])
}
   #  TxDb.Hsapiens.UCSC.hg19.knownGene # PHONY ...
   # GeneRegionTrack fails for Homo.sapiens

modTrack = function(symbol,
   genome="hg19", annoResource=Homo.sapiens,
   getter=exonsBy, byattr="gene", expansion=0,
   collapseTranscripts="meta") {
 require(Gviz)
 m1 = genemodel(symbol, annoResource=annoResource)
 thechr = as.character(seqnames(m1)[1])
 st = min(start(m1)) - expansion
 en = max(end(m1)) + expansion
 GeneRegionTrack(getTxDb(annoResource), chromosome=thechr, start=st,
   end=en, showId=TRUE, collapseTranscripts=collapseTranscripts,
   geneSymbols=TRUE)
}

modPlotOLD = function(symbol,
   genome="hg19", annoResource=Homo.sapiens,
   getter=exonsBy, byattr="gene", expansion=0,
   collapseTranscripts="meta") {
  tr = modTrack(symbol, genome, annoResource, getter, byattr,
       expansion, collapseTranscripts)
  entrez = tr@range$gene
  require(deparse(substitute(annoResource)), character.only=TRUE)
  syms = select(annoResource, keys=entrez, keytype="ENTREZID",
     columns="SYMBOL")$SYMBOL
  tr@range$symbol = syms
  plotTracks( list(tr, GenomeAxisTrack()) )
}

modPlot = function (symbol, genome = "hg19", annoResource = Homo.sapiens, 
    getter = exonsBy, byattr = "gene", 
    expansion = 0, collapseTranscripts = "meta", useGeneSym = 
       TRUE, plot.it=TRUE, ... ) {
    tr = modTrack(symbol, genome, annoResource, getter, byattr, 
        expansion, collapseTranscripts)
    entrez = tr@range$gene
    require(deparse(substitute(annoResource)), character.only = TRUE)
  if (useGeneSym) {
    syms = select(annoResource, keys = entrez, keytype = "ENTREZID", 
        columns = "SYMBOL")$SYMBOL
    tr@range$symbol = syms
    }
    fin = list(tr, GenomeAxisTrack())
    curc = as.character(seqnames(tr@range)[1])
    fin[[1]]@name = paste0(symbol, " (", curc, ")")
    if (plot.it) plotTracks(fin, ...)
    invisible(fin)
}


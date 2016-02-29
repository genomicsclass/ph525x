geneLabelDF = function(syms) {
 require(Homo.sapiens)
 chrs = mapIds(Homo.sapiens, keys=syms, keytype="SYMBOL", column="TXCHROM")
 locs = mapIds(Homo.sapiens, keys=syms, keytype="SYMBOL", column="TXSTART")
 ans = data.frame(chrs, as.numeric(locs), as.numeric(locs), syms)
 ans = na.omit(ans)
 colnames(ans) = c("chr", "start", "start2", "sym")
 ans
}

 getglo = function(sym) mapIds(Homo.sapiens, keys=sym, keytype="SYMBOL",
    column="TXSTART")
 getgchr = function(sym) mapIds(Homo.sapiens, keys=sym, keytype="SYMBOL",
   column="TXCHROM")

l2df = function(x, coln=c("snpid", "geneloc")) {
  nrep = sapply(x,length)
  nn = rep(names(x), nrep)
  data=unlist(x)
  ans = data.frame(nn, data, stringsAsFactors=FALSE)
  names(ans) = coln
  ans
}


sglistToDF = function(sgl, slocpack=SNPlocs.Hsapiens.dbSNP144.GRCh37) {
 sn = names(sgl)
 snlocs = snpsById( slocpack, sn, ifnotfound="drop" )
 avail = snlocs$RefSNP_id
 sgl = sgl[avail]  # reorder list if necessary
 require(GenomeInfoDb)
 seqlevelsStyle(snlocs) = "UCSC"
# snpchr = gsub("ch", "chr", names(snlocs))
 sldf = data.frame(snpid=names(sgl), snpchr=as.character(seqnames(snlocs)), snploc=pos(snlocs), stringsAsFactors=FALSE)
 genechr = l2df(lapply(sgl, getgchr), c("snpid", "genechr"))
 geneloc = l2df(lapply(sgl, getglo), c("snpid", "geneloc"))
 gdf = cbind(genechr, geneloc[,-1])
 names(gdf) = c("snpid", "gchr", "gloc")
 merge(sldf, gdf, by="snpid")
}

sglToCircos = function(sgl) {
require(SNPlocs.Hsapiens.dbSNP144.GRCh37)
require(Homo.sapiens)

# Given a manually constructed SNP->GENE list, construct circos plot

sglDF = sglistToDF(sgl)
keeps = unique(sglDF$snpid)
myd = data.frame(sglDF[,2], sglDF[,3], sglDF[,3]+1, sglDF[,4], sglDF[,5], sglDF[,5]+1)

require(RCircos)
data(UCSC.HG19.Human.CytoBandIdeogram);
cyto.info <- UCSC.HG19.Human.CytoBandIdeogram;
RCircos.Set.Core.Components(cyto.info, chr.exclude=NULL, tracks.inside=4,
   tracks.outside=0)
RCircos.Set.Plot.Area();
RCircos.Chromosome.Ideogram.Plot();
RCircos.Link.Plot(myd, track.num=4)
RCircos.Env$RCircos.PlotPar$text.size = .6

normlocs = sglDF[-which(duplicated(sglDF$snpid)),1:3]

snplab = data.frame(normlocs[,2], normlocs[,3], normlocs[,3], normlocs[,1], stringsAsFactors=FALSE)
snplab[,1] = as.character(snplab[,1])
snplab[,4] = as.character(snplab[,4])

RCircos.Gene.Name.Plot(snplab,name.col=4,track.num=2,"in")

genes = na.omit(unlist(sgl[keeps]))
gdf = geneLabelDF(genes)
RCircos.Gene.Name.Plot(gdf,4,2,"in")
}

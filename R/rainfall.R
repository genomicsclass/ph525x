rainxax = function() {
 xloc = c(76690339, 380007736, 580350465, 789821899, 976392502, 1141356109, 
  1307757306, 1467876229, 1600781548, 1744253164, 1884533327, 2028666234, 
  2133236628, 2244693085, 2355833271, 2445323332, 2535618444, 2625611662, 
  2693322214, 2756561857, 2812282897, 2861117875, 2944368789, 3.089e+09
  )
 axis(1, at=xloc, labels=c(1:22, "X", "Y"))
}

kataColors = function() {
cmap = c("blue", "black", "red", "purple",
  "yellow", "green")
names(cmap) = c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
cmap
}

totalgd = function (gr) 
{
    an = as.numeric
    off = c(0, cumsum(an(seqlengths(gr)[-length(seqlengths(gr))])))
    names(off) = names(seqlengths(gr))
    gr = trim(gr)
    gr$totalgd = GenomicRanges::start(gr) + off[as.character(seqnames(gr))]
    gr
}
rainfall = function (mut, colmap = kataColors(), oind=1, ptcex=.3, 
  dodensity=TRUE, inbw=1000, dvscale=2, ...,
  splitter="Tumor_Sample_Barcode", legcex=.8, legy=8.5, legxdenom=3,
  xaxgen=rainxax, ymax=9, na.rm=TRUE)
{
#
# takes complete TCGA mutation structure, splits by id, sorts
# by number of mutations descending, displays results for oind element
#
    thecall = match.call()
    require("GenomicRanges")
    names(mut) = gsub("position", "Position", names(mut))
    smut = split(mut[[splitter]], mut[[splitter]])
    nrs = sapply(smut, length)
    o = order(nrs, decreasing=TRUE)
    oids = names(smut)[o]
    id2keep = oids[oind]
    mut = mut[which(mut[[splitter]]==id2keep),]
    an = as.numeric
    gg = with(mut, GRanges(Chromosome, IRanges(an(Start_Position), 
        an(End_Position))))
    require(Homo.sapiens)
    require(GenomeInfoDb)
    seqlevelsStyle(gg) = "UCSC"
  suppressWarnings({
    seqinfo(gg) = seqinfo(Homo.sapiens)[seqlevels(gg)]
    seqlevels(gg) = sortSeqlevels(seqlevels(gg))
  })
    gg = trim(gg)
    mcols(gg) = mut
    gg = sort(gg)
#
# from COSMIC web site
#
# The profile of each signature is displayed using the 
# six substitution subtypes: C>A, C>G, C>T, T>A, T>C, 
# and T>G (all substitutions are referred to by the 
# pyrimidine of the mutated Watson–Crick base pair).
    subt = function(ref, a1, a2) {
        alt = ifelse(a1 != ref, a1, a2)
        tmp = ref
        needsw = which(alt %in% c("C", "T"))
        ref[needsw] = alt[needsw]
        alt[needsw] = tmp[needsw]
        paste(ref, alt, sep = ">")
     }

    WW = with(mcols(gg[mcols(gg)$Variant_Type == "SNP", ]), subt(Reference_Allele, 
        Tumor_Seq_Allele1, Tumor_Seq_Allele2))
    gg$subst = rep(NA_character_, length(gg))
    mcols(gg[which(gg$Variant_Type == "SNP")])$subst = WW
    gg3 = gg
    gg3 = gg3[which(gg3$subst %in% names(colmap))]
    ncolor = length(unique(WW))
    gg3 = totalgd(gg3)
    di = c(1, log10(diff(gg3$totalgd) + 1))
    plot(x = gg3$totalgd, y = di, col = colmap[gg3$subst], pch = 19, axes=FALSE,
        ylab="log10 dbp", cex=ptcex, ylim=c(0,ymax), xlab="chromosome", ...)
    axis(2)
    box()
    glen = sum(as.numeric(seqlengths(Homo.sapiens))[1:24])
    legend(glen/legxdenom,legy, horiz=TRUE, col=colmap, legend=names(colmap), pch=19, cex=legcex)
    abline(v=cumsum(as.numeric(seqlengths(gg3))), lty=2)
    title(mcols(gg3)[["Tumor_Sample_Barcode"]][1])
    if (dodensity) {
       resp = gg3$totalgd
       if (na.rm) resp = na.omit(resp)
       myd = density(resp, bw=inbw)
       lines(myd$x, dvscale*(myd$y/max(myd$y)))
       }
    if (!is.null(xaxgen)) xaxgen()
    invisible(list(mutGR=gg3, density=myd, thecall=thecall))
}


OnehnRBamPath = function(ind=1){
# NB: first four are control, 5-8 are knockdown of hnRNP C
         require(RNAseqData.HNRNPC.bam.chr14)
         RNAseqData.HNRNPC.bam.chr14_BAMFILES[ind]
}

readOnehnR = function(ind=1) {
         require(RNAseqData.HNRNPC.bam.chr14)
         bamfile <- RNAseqData.HNRNPC.bam.chr14_BAMFILES[ind]
         param <- ScanBamParam(tag="NM")
         readGAlignmentPairs(bamfile, use.names=TRUE, param=param)
}
 
opsOnehnR = function(ind=1, pick=first) {
         one = readOnehnR(ind)
         colSums(cigarOpTable(cigar(pick(one))))
}

countOneJuncshnR = function(ind=1, pick=first) {
         one = readOnehnR(ind)
         table(njunc(pick(one)))
}

tableOneEditDisthnR = function(ind=1, pick=first) {
         galp = readOnehnR(ind)
         table(mcols(pick(galp))$NM)
}
     
setActiveSeq = function(txdb, picks) {
   ii = isActiveSeq(txdb)
   ii[] = FALSE
   ii[picks] = TRUE
   isActiveSeq(txdb) = ii
   txdb
}
   

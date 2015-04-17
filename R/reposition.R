
reposition = function(gr, backoff=-20000) {
#
#objective is to obtain a set of intervals with
# same widths as gr but distributed randomly
# over chromosomes with randomly chosen starts
#
 require(Homo.sapiens)
 num = length(gr)
 wid = width(gr)
 sn = seqnames(gr)
# obtain random range reassignment
 ranord = sample(1:num, size=num, replace=FALSE)
 sl = seqlengths(Homo.sapiens)
 plens = sl[as.character(sn)[ranord]] # bounds for reassigned ranges
# obtain random starting position
 ini = round( runif(num, 1, plens), 0 )
 ranges(gr) = IRanges(rep(1,num),width=1) # wipe out
 seqnames(gr) = sn[ranord]  # assign the scrambled seqnames to ranges
 attempt = IRanges(ini, width=wid[ranord]) # use scrambled widths
    # now if any scrambled width hangs over the end of chromosome
    # back it off -- this is ad hoc
 bad = which(end(attempt) > sl[as.character(seqnames(gr))])
 if (length(bad)>0)
    attempt[bad] = shift(attempt[bad], backoff)
 ranges(gr) = attempt
 gr
}


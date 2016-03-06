showz = function(sym="ESRRA", radius=1e5) {
 require(ggbio)
 require(erma)
 require(ERBS)
 es = genemodel(sym)
 data(HepG2)
 data(GM12878)
 hsub = subsetByOverlaps(HepG2, es+radius)
 gsub = subsetByOverlaps(GM12878, es+radius)
 tracks( gene=es, hepNarr=autoplot(hsub), gmNarr=autoplot(gsub), title=sym ) 
}

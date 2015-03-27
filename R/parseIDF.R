
setClass("AEidf", representation(atts="character", vals="character",
  basis="character"))
setMethod("show", "AEidf", function(object) {
  require(Biobase, quietly=TRUE)
  cat("instance of ArrayExpress IDF protocol metadata, from \n")
  cat("   ", object@basis, "\n")
  cat("selected attributes: \n")
  kp = c(2,5,7)   #ugh
  vtoshow = object@vals[kp]
  names(vtoshow) = object@atts[kp]
  show(substr(vtoshow, 1, 50))
  cat("use @atts, @vals, @basis for additional info.\n")
})

parseIDF = function(fn) {
  zz = readLines(fn)
  zzs = strsplit(zz, "\t")
  zzss = lapply(zzs, function(x) x[which(nchar(x)>0)])
  atts = sapply(zzss, "[", 1)
  vals = sapply(lapply(zzss, "[", -1), function(x) paste0(x, collapse="::"))
  new("AEidf", atts=atts,  vals=vals, basis=fn)
}

parseSDRF = function(fn) {
 xx = readLines(fn)
 xxx = strsplit(xx, "\t")
 facs = lapply(xxx, "[", c(1:5,26))
 tmp = t(sapply(facs, force))
 tmp = data.frame(tmp[-1,], stringsAsFactors=FALSE)
 names(tmp) = tmp[1,]
 tmp
}

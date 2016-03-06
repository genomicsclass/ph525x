#
# this is rapidly improvised code for benchmarking out-of-memory
# strategies, will write in current folder without checking
# for clobbering
#

.h5RoundTrip = function(x, chunkIn=c(1000,10), inLevel=0, intimes=1) {
system("rm -rf ex_hdf5file.h5")
require(rhdf5)
h5createFile("ex_hdf5file.h5")
h5createDataset("ex_hdf5file.h5", "x", c(nrow(x),ncol(x)), 
   storage.mode = "double", chunk=chunkIn, level=inLevel)
mw = microbenchmark(h5write(x, "ex_hdf5file.h5", name="x"), times=intimes)
mr= microbenchmark(h5read("ex_hdf5file.h5", name="x"), times=intimes)
msel= microbenchmark(ysel <- h5read("ex_hdf5file.h5", name="x", index=list(4001:5000, 1:100)), times=intimes)
stopifnot(all.equal(ysel, x[4001:5000,]))
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="hdf5")
}

.ffRoundTrip = function(x, chunkIn=c(5000,10), inLevel=0, intimes=1) {
system("rm -rf ex_ff.ff")
require(ff)
dx = dim(x)
mw = microbenchmark({
  xff <- ff(vmode="double", dim=dx, filename="ex_ff.ff")
  xff[,] = x
  }, times=intimes)
mr= microbenchmark({
  suppressWarnings({
   yff <- as.ram(xff)
                  })}, times=intimes)
msel= microbenchmark({
  suppressWarnings({
   yff <- xff[4001:5000,]
                  })}, times=intimes)
stopifnot(all.equal(yff, x[4001:5000,]))
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="ff")
}

.bmRoundTrip = function(x, intimes=1) {
system("rm -rf ex_bm.bm ex_bm.desc")
require(bigmemory)
dx = dim(x)
mw = microbenchmark({
  xbm = big.matrix(dx[1], dx[2], init=NA, backingfile="ex_bm.bm",
   descriptorfile="ex_bm.bm.desc")
  xbm[,] = x
  }, times=intimes)
mr = microbenchmark(xin <- xbm[,], times=intimes)
msel = microbenchmark({xin2 <- xbm[4001:5000,]}, times=intimes)
stopifnot(all.equal(xin2, x[4001:5000,]))
indl = list(3001:4000, 4001:5000)
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="bigmemory")
}


.slRoundTrip = function(x, intimes=1) {
system("rm -rf ex_sqlite.sqlite")
Sys.sleep(1)
stopifnot(!file.exists("ex_sqlite.sqlite"))
library(RSQLite)
con = dbConnect(SQLite(), "ex_sqlite.sqlite")
mw = microbenchmark({
  dbWriteTable(con, "x", data.frame(ind=1:nrow(x), x), overwrite=TRUE)
  }, times=intimes)
mr= microbenchmark(yff <- dbReadTable(con, "x"), times=intimes)
msel = microbenchmark( 
   {tmp <- dbGetQuery(con, "select * from x where ind >= 4001 and ind <= 5000")},
   times=intimes
   )
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="sqlite")
}

.dtRoundTrip = function(x, intimes=1) {
system("rm -rf ex_dt.rda")
Sys.sleep(1)
library(data.table)
mw = microbenchmark({
  dtx = data.table(x)
  save(dtx, file="ex_dt.rda", compress=FALSE)
  }, times=intimes)
mr= microbenchmark(load("ex_dt.rda"), times=intimes)
# at this point dtx is available
msel = microbenchmark( tmp <- dtx[4001:5000,], times=intimes )
oo = as.matrix(tmp)
dimnames(oo) = NULL
stopifnot(all.equal(oo, x[4001:5000,]))
list(mwrite=mw, ingFull=mr, ing1K=msel, times=intimes, method="data.table")
}

getStats = function(times, ..., summstat = mean, rtfun=.h5RoundTrip) {
 a1 = lapply(1:times, function(z) rtfun(...))
 w = lapply(a1, "[[", "mwrite")
 r = lapply(a1, "[[", "ingFull")
 rsel = lapply(a1, "[[", "ing1K")
 ans = list(
   meth=a1[[1]]$method,
   wr=summstat(sapply(w, function(x)x[,"time"]/10^6)),
   ingFull=summstat(sapply(r, function(x)x[,"time"]/10^6)),
   ing1K=summstat(sapply(rsel, function(x)x[,"time"]/10^6))
   )
  data.frame(ans)
}

benchOOM = function(NR=5000, NC=100, times=5, inseed=1234,
  methods = list(.h5RoundTrip, .ffRoundTrip, .slRoundTrip, .dtRoundTrip, .bmRoundTrip)) {
require(microbenchmark)
require(parallel)
options(mc.cores=2)
nel = NR * NC
set.seed(inseed)
x = array(rnorm(nel), dim=c(NR,NC))
cbind(NR=NR, NC=NC, times=times, do.call(rbind, 
    lapply(methods, function(z) getStats(times, x, rtfun=z))))
}

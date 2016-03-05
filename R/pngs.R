
#seqcAbst = function() {
# f = readPNG(system.file("pngs/seqc.png", package="ph525x"))
# grid.raster(f)
#}

prender = function(pref) {
 newf = sub("%%PREF%%", pref, "pngs/%%PREF%%.png")
 f = readPNG(system.file(newf, package="ph525x"))
# if (!(dev.cur()==1)) dev.off()
 grid.raster(f)
}

seqcAbst = function() prender("seqc")
seqcDesign = function() prender("seqcFreeH")
obesTitle = function() prender("obesTitle")
obesManh = function() prender("obesManh")
esetDet1 = function() prender("esetFreeH")
esetDet2 = function() prender("esetDet2")
sydhTop = function() prender("SYDHenc")
summex = function() prender("summex")
ggshot = function() prender("ggshot")
firehose = function() prender("firehose")

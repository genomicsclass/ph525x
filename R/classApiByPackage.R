setClass("S4ClassAPI", representation(classname="character",
   methods="character", signatures="character",
   packages="character"))

.checkClassArg = function(x)
 stopifnot(is(x, "character") && is.atomic(x) && length(x)==1)

.checkPackName = function(package) {
  stopifnot(length(grep("^package:", package))>0)
}
.cleanPackName = function(package) {
  sub("^package:", "", package)
}
.dropNoMeth = function(x) {
  dr = sapply(x, function(x) isTRUE(x[2] == "<No methods>"))
  if (!any(dr)) return(x)
  x[-which(dr)]
}
.cleanMref = function(x)
  sub("Function: (.*) \\(.*", "\\1", x[1])
.cleanPackRef = function(x) gsub("package:", "", x)

S4APIByPackage = function(class, package) {
  require(S4Vectors)
  .checkClassArg(class)
  .checkPackName(package)
  require(.cleanPackRef(package), character.only=TRUE, quietly=TRUE)
  gs = getGenerics(package) 
  methods = gs@.Data
  packages = gs@package
  gInPack = methods[ which(packages==.cleanPackName(package)) ]
  stopifnot(length(gInPack)>0)
  #ans = .dropNoMeth(lapply(gInPack, function(x) showMethods(x, classes=class, where=package,
#      printTo=FALSE)))
  ans = lapply(gInPack, methods)
#  meths = sapply(ans, .cleanMref)
#  DataFrame(methods=meths, sigs = sapply(ans, "[", 2), package=.cleanPackRef(package))
  ans
}

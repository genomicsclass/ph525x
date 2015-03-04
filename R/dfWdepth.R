dfWdepth = function(koStruct) {
#
# takes a k00001.keg read and converts to data frame with order reflecting
# concept hierarchy
#
 al = koStruct$fullList
 n1 = length(al)
 n2 = lapply(al, length)  # number of level 2 entities
 n3 = lapply(al, function(x) lapply(x, length)) # number of level 3
 df = data.frame(lev1=NA, lev2=NA, depth=NA, term=NA, tag=NA)
 for (i in 1:n1) {
   df = rbind(df, c(lev1=i, lev2 = i, depth=1, term=koStruct$Anames[i], tag=koStruct$Atags[i]))
   for (j in 1:n2[[i]]) {
      df = rbind(df, c(lev1 = i, lev2 = paste(i, j, sep="."), depth=2, term=koStruct$Bnames[[i]][j], tag=koStruct$Btags[[i]][j]))
      for (k in 1:n3[[i]][[j]]) {
           df = rbind(df, c(lev1 = i, lev2 = paste(i, j, sep="."), depth=3, term=koStruct$fullList[[i]][[j]][k], tag=koStruct$Ctags[[i]][[j]][k]))
           }
      }
 }
 df[-1,]
}

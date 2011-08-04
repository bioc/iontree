saveMSnRaw <-
function(dataFolder="D:/Data/Raw"){
  setwd(dataFolder)
  fn=list.files("./", pattern=".mzXML$")
  if(length(fn)==0) stop("mzXML files not found")
  MS2RAW = list()
  MS3RAW = list()
  N=length(fn)
  for (i in 1:N){
   msdf = .jcall("XCMS", "Ljava/util/ArrayList;", "getMSData", fn[i])
   ms2f = getMSnRaw(msdf, msLevel = 2)
   attr(ms2f, "filename")=fn[i]
   MS2RAW[[i]] = ms2f

   ms3f = getMSnRaw(msdf, msLevel = 3)
   attr(ms3f, "filename")=fn[i]
   MS3RAW[[i]] = ms3f

   cat(i, "/", N, " files complete\n", sep="")
   flush.console()
  }

  save(MS2RAW, file = "MS2RAW.Rdata")
  save(MS3RAW, file = "MS3RAW.Rdata")
}#>>>


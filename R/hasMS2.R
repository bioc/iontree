hasMS2 <-
function(MS2RAW, mzRange=c(1854, 1854.5), rtRange=c(280,400)){
  n=length(MS2RAW)
  Found=rep(FALSE, n)
  for(i in 1:n){
    ind.mz=which(MS2RAW[[i]]$premz>=mzRange[1]& MS2RAW[[i]]$premz<=mzRange[2])
    if(is.null(rtRange)){
     if(length(ind.mz)!=0) Found[i]=TRUE          
    }else{
      #rt must be for the mzRange
      rtMZ=MS2RAW[[i]]$rt[ind.mz]
      ind.rt=which(rtMZ>=rtRange[1]& rtMZ<=rtRange[2])
      if(length(ind.rt)!=0) Found[i]=TRUE    
    }
  }
  #cat("MS2 derived from mz:", mzRange, "and RT:", rtRange, "found in the following samples:\n")
  sampleIdx=which(Found==TRUE)
  #print to console
  for(f in sampleIdx) cat(f, attributes(MS2RAW[[f]])$filename, "\n")
  #return(sampleIdx)
  sampleIdx 
}#>>>


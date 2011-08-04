buildIonTree <-
function(mzRange=c(340.5, 341.5), rtRange=c(270, 282), ms2, ms3){
  if(!all(names(ms2)==c("premz",  "rt", "msn.sp")))
    stop("ms2 data must have components: premz, rt, msn.sp")

  #ms2$premz type of matrix
  idx.pa=which(ms2$premz[,1]>=mzRange[1] & ms2$premz[,1]<=mzRange[2])

  if(length(idx.pa)!=0){
    cat(length(idx.pa), " ms2 spectra found\n")
  }else{
    cat("No MS2 found\n")
    return(NULL)
  }

  spectra.a=ms2$msn.sp[idx.pa];
  rtMzs=ms2$rt[idx.pa]
  cat("Raw MS2 rtRange: [", range(rtMzs), "]\n")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #so which.peak is all true for DIMS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  which.peak=(rtMzs >= rtRange[1]) & (rtMzs <= rtRange[2]);

  if(all(which.peak==FALSE))
    stop(paste("No MS2 found in within RT: ", rtRange[1],"-",rtRange[2]))

  x=unlist(lapply(spectra.a[which.peak], function(x){x[,1]}));
  y=unlist(lapply(spectra.a[which.peak], function(x){x[,2]}));

  ms2.avg=tapply(y, factor(round(x,0)), median); #round(x, 4) for high res MS
  #apply a filter?
  ms2.avg=ms2.avg[which(ms2.avg>mean(ms2.avg))]
  MS2.avg=topIons(as.numeric(names(ms2.avg)), ms2.avg, top=100);

  ##################################
  #MS3, should be the same ancestor
  ##################################
  which.ion=which(ms3$premz[,1]>=mzRange[1] & ms3$premz[,1]<=mzRange[2])
  if(is.null(ms3) || length(which.ion)==0){
    cat("No MS3\n");
    ionTree=new("iontree", mz=mzRange, rt=range(rtMzs), MS2=MS2.avg, MS3=list())
    return(ionTree)
  }

  ms3.premz.selected=ms3$premz[which.ion,,drop=FALSE];
  rtMz.3=ms3$rt[which.ion]
  which.peak3=which((rtMz.3 >= rtRange[1]) & (rtMz.3 <= rtRange[2]));

  if(length(which.peak3)==0){
    cat("Raw MS3 rtRange [", range(rtMz.3), "]; No MS3 in time: ", rtRange, "\n")
    ionTree=new("iontree", mz=mzRange, rt=range(rtMz.3), MS2=MS2.avg, MS3=list())
    return(ionTree)
  }else{
    ms3.premz.selected=ms3.premz.selected[which.peak3,,drop=FALSE]
    #/// sort out spectra
    spectra3=ms3$msn.sp[which.ion]
    spectra3=spectra3[which.peak3]

    if(max(sapply(spectra3, function(x) max(x[,2])))<100)
      warning("All very weak MS3 specta with intensity < 100!")
  }

  #another scenario: empty spectra first, although premz exist, too weak!
  ms3.whichIsEmpty=sapply(spectra3, function(x) all(x<0.001)) #not test x==0

  ms3s=list();
  if(all(ms3.whichIsEmpty)){
   ionTree=new("iontree", mz=mzRange, rt=range(rtMz.3), MS2=MS2.avg, MS3=list())
   return(ionTree)
  }else{
    spectra3=spectra3[!ms3.whichIsEmpty]
    ms3.premz.selected=ms3.premz.selected[!ms3.whichIsEmpty,,drop=FALSE]
    ms3.num=unique(round(ms3.premz.selected[,2]));

    #NEED a weighted mean??
    for(i in 1:length(ms3.num)){
      idx=which(round(ms3.premz.selected[,2])==ms3.num[i]);

      x=unlist(lapply(spectra3[idx], function(x){x[,1]}));
      y=unlist(lapply(spectra3[idx], function(x){x[,2]}));
      ms3.avg=tapply(y, factor(round(x, 0)), median);
      
      MS3.avg=topIons(as.numeric(names(ms3.avg)), ms3.avg, top=length(ms3.avg));
      ms3s[[i]]=list(premz2=ms3.num[i], sp3=MS3.avg);
      #????? may need attach rt3 here
    }
  }
  cat("the number of sibling ms3 spectra found: ", length(ms3s), "\n");

  ionTree=new("iontree", mz=mzRange, rt=range(rtMzs), MS2=MS2.avg, MS3=ms3s)
  return(ionTree);
}#>>> buildIonTree S4 @25/02/2011 11:50:18 a.m.


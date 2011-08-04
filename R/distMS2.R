distMS2 <-
function(a, b, topIon=20){
  if(is.null(ncol(a)) || ncol(a)!=2 ) stop("spectrum must be in 2-col matrix")
  topIon=min(c(topIon, nrow(a), nrow(b)))

  #order by intensity
  a.order=order(a[,2], decreasing=T)[1:topIon]
  b.order=order(b[,2], decreasing=T)[1:topIon]

  #need flexible way for high resolution mz
  a.mz=round(a[,1][a.order],0)
  a.inten=a[,2][a.order]
  b.mz=round(b[,1][b.order],0)
  b.inten=b[,2][b.order]

  a.inten=a.inten/sum(a.inten)
  b.inten=b.inten/sum(b.inten)

  common.ions=intersect(a.mz, b.mz)
  a.diff=setdiff(a.mz, common.ions)
  b.diff=setdiff(b.mz, common.ions)

  distance=0
  #sum up the different ions
  a.mismatch.inten=sum(a.inten[match(a.diff, a.mz)])
  b.mismatch.inten=sum(b.inten[match(b.diff, b.mz)])
  distance=distance + a.mismatch.inten + b.mismatch.inten

  common.ions.a=match(common.ions, a.mz)
  common.ions.b=match(common.ions, b.mz)

  #of the same length and order, it is sorted #need to check? all TRUE
  if(all(b.mz[common.ions.b]==a.mz[common.ions.a])){
    distance=distance + sum(abs(a.inten[common.ions.a] - b.inten[common.ions.b]))
  }else{
    warning("The name of the common ions not matched?");
  }

  as.numeric(distance);
}#>>>


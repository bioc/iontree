topIons <-
function(mz, intensity, top){
  if(length(mz)!=length(intensity)) stop("Wrong Spectrum! num of mz and intensity should be equal");
  if(length(mz) < top) top=length(mz);
  y=sort(intensity, decreasing=TRUE, index.return=TRUE);
  intensity=y$x[1:top];
  mz=mz[y$ix[1:top]];

  cbind(sort(mz), intensity[order(mz)]);
}#>>>


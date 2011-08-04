getMSnRaw <-
function(msdata, msLevel=2){
  msn=.jcall("XCMS", "Ljava/util/ArrayList;", "getMSn", msdata, as.integer(msLevel));
  rt=.jcall("XCMS", "[D", "callRT", msn);
  
  if(msLevel==1){
    print("retrieving MS1 data as a list")
    tic=.jcall("XCMS", "[D", "callTic", msn);
    ms1.scans=.jcall(msn, "I", "size");

    ms1.sp=sapply(1:ms1.scans, function(i){
                ms1.scan=.jcall(msn, "Ljava/lang/Object;", "get", as.integer(i-1));
                cbind(mz=.jcall(ms1.scan, "[D", "getMz"),
                      inten=.jcall(ms1.scan, "[D", "getIntensity"))
                })
    return(list(rt=rt, tic=tic, sp=ms1.sp))
  }

  premz=.jcall("XCMS", "[[D", "callPremz", msn);
  premz=lapply(premz, .jevalArray);
  m=length(premz)        #number of MSn scans
  n=length(premz[[1]])   #number of precusors, for example 2 for MS3
  premz.matrix=matrix(unlist(premz),  nrow=n, ncol=m); #dim(premz.matrix)

  msn.scans=.jcall(msn, "I", "size");
  msn.sp=sapply(1:msn.scans, function(i){
                msn.scan=.jcall(msn, "Ljava/lang/Object;", "get", as.integer(i-1));
                cbind(mz=.jcall(msn.scan, "[D", "getMz"),
                      inten=.jcall(msn.scan, "[D", "getIntensity"))
                })

  list(premz=t(premz.matrix), rt=rt, msn.sp=msn.sp);
}#>>>


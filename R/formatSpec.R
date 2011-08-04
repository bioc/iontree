formatSpec <-
function(x, fromTo=c("mat2str", "str2mat")){
  if(fromTo=="mat2str"){
    if (ncol(x) != 2)
        stop("spectrum must be of a 2-col matrix")
    
    if(max(x[,2]>100)){
      y=round(x[,2]/max(x[,2])*100,2)
      x[,2]=y
    }
    
    peak.db = NULL
    for (i in 1:nrow(x)) {
        peak.db = c(peak.db, paste(x[i, 1], " ", round(x[i, 2],2), ";", sep = ""))
    }
    return(paste(peak.db, collapse=" "))
  }else if(fromTo=="str2mat"){
    if(!is.character(x)){
      stop("provide spectrum in characters of mz-int, then parse it to 2-col matrix")
    }
    #conversion: #empty [ ], platform indep? better use [[:space:]]?
    tmp=unlist(strsplit(x, ";[ ]*")) 
    tmp=strsplit(tmp, "[ ]")
    c1=sapply(tmp, function(x) x[1])
    c2=sapply(tmp, function(x) x[2])
    return(cbind(mz=as.numeric(c1), inten=as.numeric(c2)))
  }else{
    stop("define type of conversion!")
  }
}#>>> 


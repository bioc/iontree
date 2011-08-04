rs2iontree=function(rs){
  if(class(rs)!="data.frame")
    stop("result set must be data.frame retrieved from database")
  if(all(c("mz", "ms2") %in% names(rs))==FALSE)
    stop("must have mz and ms2 data")

  numRecord=nrow(rs)
  if(numRecord==0) stop("no record found")

  iontrees=vector("list", numRecord)
  for(i in 1:numRecord){
    rs.ms2=formatSpec(rs[i,]$ms2, fromTo="str2mat")

    if(is.null(rs[i,]$ms3)){
      t1=new("iontree", mz=rs[i,]$mz, rt=as.numeric(NA), MS2=rs.ms2, MS3=list())
      iontrees[[i]]=t1
    }else{
      #ms3 to a list of matrix
      node3=xmlParse(rs[i,]$ms3)
      r=xmlRoot(node3)
      premz=unlist(xmlApply(r, xmlAttrs))
      ms3.sp=unlist(xmlApply(r, xmlValue))

      rs.ms3=list()
      for(j in 1:length(premz)){
       rs.ms3[[j]]=list(premz2=premz[j], sp3=formatSpec(ms3.sp[j], fromTo="str2mat"))
      }

      t2=new("iontree", mz=rs[i,]$mz, rt=as.numeric(NA), MS2=rs.ms2, MS3=rs.ms3)
      iontrees[[i]]=t2
    }
  }

  iontrees
}#>>>

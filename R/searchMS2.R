searchMS2=function(querySpec, premz, dbname="mzDB.db", scoreFun="distMS2",
                   output.record=5, plot.top=TRUE){
  if(ncol(querySpec)!=2){
     stop("please provide ms2 spectrum in 2-col matrix")
  }

  db=dbConnect(dbDriver("SQLite"), dbname)
  q1=dbSendQuery(db, "SELECT exp_id, mz, ms2 FROM mz")
  rs=fetch(q1, n=-1)  #data.frame
  dbClearResult(q1)
  dbDisconnect(db)

  allMS2=rs[, "ms2", drop=FALSE]
  MS2list=apply(allMS2, 1, function(x) formatSpec(x, fromTo="str2mat"))

  scoreMethod=c("distMS2", "cos", "tanimoto")
  SCORE=pmatch(scoreFun, scoreMethod)
  if (is.na(SCORE))
    stop("score function not found")

  if(SCORE==1){
    score1=sapply(MS2list, function(x) distMS2(querySpec, x))
    #which.min(score1)
    topIdx=order(score1)[1:output.record]    #top 5 similar spectrum
    top=topIdx[1]
    topScores=score1[topIdx]
  }
  if(SCORE==2){
    score2=sapply(MS2list, function(x){
                             top=min(nrow(x), nrow(querySpec))
                             a=topIons(x[,1], x[,2], top)
                             b=topIons(querySpec[,1], querySpec[,2], top)
                             a=a[,2]
                             b=b[,2]
                             cos=(a%*%b)/sqrt(crossprod(a)*crossprod(b))
                             cos
                             })
    topIdx=order(score2, decreasing = TRUE)[1:output.record]
    top=topIdx[1]
    topScores=score2[topIdx]
  }
  if(SCORE==3){
    score3=sapply(MS2list, function(x){
                             top=min(nrow(x), nrow(querySpec))
                             a=topIons(x[,1], x[,2], top)
                             b=topIons(querySpec[,1], querySpec[,2], top)
                             a=a[,1]
                             b=b[,1]
                             #assume integer ions
                             tanimoto=length(intersect(a, b))/length(union(a,b))
                             tanimoto
                             })
    topIdx=order(score3, decreasing=TRUE)[1:output.record]
    top=topIdx[1]
    topScores=score3[topIdx]
  }

  if(plot.top){
    par(mfrow=c(2,1))
    plotSpectrum(querySpec[,1], querySpec[,2], main=paste("query spectrum from m/z", premz))
    plotSpectrum(MS2list[[top]][,1], MS2list[[top]][,2], type="h", col="purple",
                 main=paste("top db spectrum from m/z", rs[top, "mz"]))
  }

  cat("most similar spectra by metric:", scoreMethod[SCORE], "\n") 
  print(cbind(rs[topIdx,c("exp_id", "mz")], score=round(topScores,2)))
  #rs[topIdx,c("exp_id", "mz")]
}#>>>

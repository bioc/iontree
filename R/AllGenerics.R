setMethod(show, signature(object="iontree"),
                    function(object){
                      cat("--- iontree object ---",  "\n\n")
                      cat("mz: ", object@mz, "\n")
                      cat("rt: ", round(object@rt), "sec\n")
                      cat("dim of ms2 matrix:", dim(object@MS2), "\n")
                      cat("num of ms3 spectra: ",length(object@MS3), "\n")
                    })
#>>>"show"

#isGeneric("plot"), replace "imgPlot" with plot !!!!
setMethod("plot", signature(x="iontree"),
          function(x, ...){
             MS2=x@MS2    #2-col matrix
             MS3s=x@MS3   #list
             RT=round(x@rt)      #range
             ms2.title=paste("MS2 of premz ", x@mz[1], "~", x@mz[2],
                 " @ time ", RT[1], "~", RT[2], "s", sep="")

            #if(length(MS3s)>4) stop("too many ms3 spectra")

            def.par <- par(no.readonly = TRUE)
            if(length(MS3s)==0){
              plotSpectrum(MS2[,1], MS2[,2], main=ms2.title, sub="no MS3", ...);
            }else if(length(MS3s)==1){
              par(mfrow=c(2,1))
              plotSpectrum(MS2[,1], MS2[,2], main=ms2.title, ...);
              plotSpectrum(MS3s[[1]]$sp3[,1], MS3s[[1]]$sp3[,2],
                   main=paste("MS3 of ",MS3s[[1]]$premz2), ...);
            }else if(length(MS3s)==2){
              layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
              plotSpectrum(MS2[,1], MS2[,2], main=ms2.title, ...);
              for(i in 1:2){
                plotSpectrum(MS3s[[i]]$sp3[,1], MS3s[[i]]$sp3[,2],
                   main=paste("MS3 of ",MS3s[[i]]$premz2), ...)
              }
            }else if(length(MS3s)==3){
              layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))
              plotSpectrum(MS2[,1], MS2[,2], main=ms2.title, ...);
              for(i in 1:3){
               plotSpectrum(MS3s[[i]]$sp3[,1], MS3s[[i]]$sp3[,2],
                   main=paste("MS3 of ", MS3s[[i]]$premz2), ...)
              }
            }else if(length(MS3s)==4){
             layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
             plotSpectrum(MS2[,1], MS2[,2], main=ms2.title, ...);
             for(i in 1:4){
                plotSpectrum(MS3s[[i]]$sp3[,1], MS3s[[i]]$sp3[,2],
                   main=paste("MS3 of ", MS3s[[i]]$premz2), ...)
             }
           }else{
             #only print 4 anyway!
             cat("More than 4 MS3 sibling spectra", "\n");
             layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
             plotSpectrum(MS2[,1], MS2[,2], main=ms2.title,
                          sub="there are > 4 MS3 sibling spectra", col.sub="red", ...);
             for(i in 1:4){
                plotSpectrum(MS3s[[i]]$sp3[,1], MS3s[[i]]$sp3[,2],
                   main=paste("MS3 of ", MS3s[[i]]$premz2), ...)
             }
           }
            par(def.par)

})#>>> "plot"

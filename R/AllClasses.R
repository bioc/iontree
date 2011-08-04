setClass("iontree",
         representation(mz="numeric", rt="numeric", MS2="matrix", MS3="list"),
         validity=function(object){
                   if(length(object@mz)>2 | length(object@rt)>2)
                    stop("provide range or single number of mz and rt")
                  }
         )#S4 iontree

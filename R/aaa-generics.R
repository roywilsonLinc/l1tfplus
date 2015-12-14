#'A summary method for the l1tf output
#'
#'\code{summary} returns a summary of the l1tf output
#'
#'@param object An object of class l1tf_obj
#'
#'@export
setMethod("summary",
          signature(object = "l1tf_obj"),
          function (object, ...)
          {
            return(object@summary.df)
          }
)


#'A plot method for the l1tf output
#'
#'\code{plot} returns a plot of the l1tf output
#'
#'@param x An object of class l1tf_obj
#'
#'@export
setMethod("plot", signature(x = "l1tf_obj",y = "missing"),
          function (x, y, ...) 
          {
            plot(slot(x,"series.df")$index,slot(x, "series.df")$trend,type="l",
                 xlab="Index",ylab="pre-processed Y",
                 ylim=c(min(slot(x,"series.df")$y.proc)-0.05*(range(slot(x,"series.df")$y.proc)[2]-range(slot(x,"series.df")$y.proc)[1]),
                        max(slot(x,"series.df")$y.proc)+0.05*(range(slot(x,"series.df")$y.proc)[2]-range(slot(x,"series.df")$y.proc)[1])))
            
            points(slot(x,"series.df")$index,slot(x, "series.df")$y.proc,type="p")
            
            abline(v=slot(x,"summary.df")$start[-1],lty=2,col=4)
            
          }
)




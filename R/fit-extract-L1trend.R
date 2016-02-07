#'wrapper function for fitting L1 trend estimator l1tf from package l1tf
#'
#'\code{l1trend} returns an object of class l1tf_obj
#'
#'This is a wrapper function for l1tf from the l1tf package.  The l1tf function fits an L1 regularised
#'piecewise linear function to a time series data set.  The wrapper function processes the vector returned
#'by l1tf and returns an S4 object with three slots, these being: summary.df, series.df and prop.na.
#'summary.df provides a summary of the linear functions and change-points.
#'series.df provides an output of the time-series with imputed NA values and piecewise linear trend.
#'prop.na provides the number of NAs as a proportion of the series length.
#'There is an associated summary and plot method fo rthe l1tf_obj class.
#'
#'
#'@param y.v A numeric vector
#'@param prop A scalar showing the proportion of NAs that are allowable in y.v
#'@param sens A sensitivity parameter that is involved in identifying break-points, defaults to 100
#'@param max.length the maximum allowable segment length, defaults to 5
#'@param max.prop.na the maximum allowable proportion of NAs within the input vector
#'
#'@importFrom zoo na.locf
#'@importFrom zoo na.approx
#'@importFrom l1tf l1tf
#'
#'
#'@return An l1tf_obj with slots summary.df, series.df and prop.na
#'
#'@examples
#'\dontrun{data(somestocks)
#'data.sub <- somestocks[,1]
#'trend.est <- l1trend(y.v = data.sub,prop=0.2,sens=100,max.length=5,max.prop.na=0.2)
#'summary(trend.est)
#'plot(trend.est)
#'}
#'
#'@export
l1trend <- function(y.v,prop,sens=100,max.length=5,max.prop.na = 0.2){

  #determine proportion of NAs
  prop.na <-
    length(y.v[is.na(y.v)==TRUE])/length(y.v)

  if(prop.na > max.prop.na)stop("Proportion of Detected NAs is > max.prop.na")

  #process series
  y.proc.v <-
    na.locf(na.locf(na.approx(y.v,na.rm=FALSE),fromLast=TRUE,na.rm=FALSE))

  #Fit L1 regularised model

  l1tf.out <- l1tf(y.proc.v,prop=prop)

  #Create vector of differences
  l1tf.out.diff.v <- c(diff(l1tf.out)[1],diff(l1tf.out))

  #Create matrix with index and differences
  dat.m <- matrix(c(1:length(l1tf.out.diff.v),l1tf.out.diff.v),
                  nrow=length(l1tf.out.diff.v),ncol=2,byrow=FALSE)

  #create difference vector and find change points
  diff2.v <- c(0,diff(dat.m[,2]))

  #Find change-points
  quartiles <-
    quantile(diff2.v,probs=c(0.25,0.5,0.75),na.rm=TRUE)

  iqr <- quartiles[3]-quartiles[1]
  med <- quartiles[2]
  out.limit <- sens*iqr

  #Get Vector of potential change-points
  cp.v <- which(abs(diff2.v-med)>out.limit)             #f'' that are extreme
  cp.diff2.v <- diff2.v[cp.v]                           #f'' for each element of cp.v
  cp.m <- cbind(1:length(cp.v),cp.v,cp.diff2.v)

  #Initial breaks based on outlier detection
  breaks <- c(0,which(diff(c(NA,cp.v))>max.length),length(cp.v)+1)
  breaks.shift <- c(breaks[-1],NA)
  breaks.grp <- c((1:(length(breaks)-1)),NA)
  breaks.comb <- cbind(breaks,breaks.shift,breaks.grp)

  if(length(cp.v)>0){
    #Find real change-points
    cp2.v <- rep(NA,length=(dim(breaks.comb)[1]-1))

  for(i in 1:(dim(breaks.comb)[1]-1)){
    cp.grp.tmp <- cp.m[cp.m[,1] >= (breaks.comb)[i,1]&(cp.m[,1] < breaks.comb[i,2]),,drop=FALSE]
    cp2.v[i] <- cp.grp.tmp[which(abs(cp.grp.tmp[,3])==max(abs(cp.grp.tmp[,3]))),2]-2
  }}else{cp2.v <- NULL}

  cuts.v <- c(0,cp2.v,length(diff2.v))
  start.v <- cuts.v[-length(cuts.v)]+1
  end.v <- cuts.v[-1]

  centers.v <- round((cuts.v[-1]-cuts.v[-length(cuts.v)])/2)+cuts.v[-length(cuts.v)]

  summary.df <- data.frame(start.v,end.v,dat.m[centers.v,2],(end.v-start.v+1))

  names(summary.df) <- c("start","end","grad","length")

  summary.df$segment <- rev(1:dim(summary.df)[1])

  #create segment id vector
  rep.ls <- list()
  for(i in 1:length(summary.df[,1])){
    rep.ls[[i]] <- rep(summary.df$segment[i],length=summary.df$length[i])
  }

  ts1.df <- data.frame(do.call("c",rep.ls))
  names(ts1.df) <- "segment"

  ts1.df$y <- y.v
  ts1.df$y.proc <- y.proc.v

  ts.df <- merge(ts1.df,summary.df,all.x=TRUE,sort=FALSE)
  ts.df$trend <- l1tf.out
  ts.df$index <- 1:(dim(ts.df)[1])

  L1.out <- new("l1tf_obj")
  L1.out@summary.df <- summary.df
  L1.out@series.df <- ts.df
  L1.out@prop.na <- prop.na

  return(L1.out)

}











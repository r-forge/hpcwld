#' @importFrom stats ecdf
#' @importFrom stats median
NULL

#' Distributional Measure of Correlation
#' 
#' This is a suggested by Dror Feitelson measure of correlation for dependent
#' variables, that may be successfully used to examine the datasets from a
#' High Performance Cluster logs    
#' 
#' @param X First variable (vector)
#' @param Y Second variable (vector)
#' @return One value between -1 and 1, characterizing the dependence between the variables
#' @references http://interstat.statjournals.net/YEAR/2004/abstracts/0412001.php?Name=412001
#' @examples
#' data(HPC_KRC)
#' DMC(HPC_KRC$service[1:1000], HPC_KRC$cores_requested[1:1000])
#' @export
# 
DMC <-
  function(X, Y) {
    if(is.null(X) || is.null(Y))
      stop("'X', 'Y' must be defined!")
    if(!is.numeric(X) || !is.numeric(Y))
      stop("Only numeric vectors allowed!")
    n=min(length(X),length(Y))
    Yl=Y[X<median(X)]
    Yu=Y[X>=median(X)]
    sum=0
    Y=sort(Y)
    for(i in 1:n)
      if(abs(ecdf(Yu)(Y[i])-ecdf(Yl)(Y[i]))>2*sqrt(i)/n)
        sum=sum+1
    res=sum/n
    res
  }

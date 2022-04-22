#' Workload of a High Performance Cluster model
#' 
#' This function computes the Kiefer-Wolfowitz modified vector for a HPC
#' model. This vector contains the work left on each of 'm' servers of a cluster
#' for the time of the arival of a task. Two methods are available, one for the 
#' case of concurrent server release (all the servers end a single task simultaneously),
#' other for independent release (service times on each server are independent).
#' 
#' @param T Interarrival times of tasks
#' @param S Service times of customers (a vector of length n, or a matrix nrows=n, ncols='m').
#' @param N Number of servers each customer needs
#' @param m Number of servers for a supercomputer
#' @param method Independent or concurrent
#' @return A dataset is returned, containing 'delay' as a vector of delays exhibited by
#' each task, 'total_cores' as the total busy CPUs in time of arrival of each task,
#' and 'workload' as total work left at each CPU.
#' @examples
#' Wld(T=rexp(1000,1), S=rexp(1000,1), round(runif(1000,1,10)), 10)
#' @export
# 
Wld <-
  function(T,S,N,m,method="concurrent") {
    if(is.null(T) || is.null(S) || is.null(N) || is.null(m) || is.null(method))
      stop("'T','S','N', and 'm' must be defined!")
    if (!is.numeric(T) || length(T) == 0) 
      stop("'T' must be a numeric vector!")
    if(method=="concurrent"){
      if (!is.numeric(S) || length(S) == 0) 
        stop("'S' must be a numeric vector!")
    }
    else if(method=="independent"){
      if (!is.numeric(S) || !is.matrix(S)) 
        stop("'S' must be a numeric matrix!")
    }
    else
      stop("'method' must be either 'concurrent' or 'independent'!")
    if (!is.numeric(N) || length(N) == 0) 
      stop("'T' must be a numeric vector!")
    if (!is.numeric(m))
      stop("'m' must be numeric!")
    if (length(T) != NROW(S) || NROW(S) != length(N))
      stop("All vectors and matrices must have equal row count!")
    
    n=length(T)
    
    workload=matrix(0,nrow=n,ncol=m)
    delay=vector("numeric",length=n)
    total_cores=vector("numeric",length=n)
    for(i in 1:(n-1)){
      if(method=="concurrent")
        for(j in 1:N[i])
          workload[i+1,j]=max(workload[i,N[i]]+S[i]-T[i],0)
      else
        for(j in 1:N[i])
          workload[i+1,j]=max(workload[i,N[i]]+S[i,j]-T[i],0)
        for(j in (N[i]+1):m)
          if(j<=m)
            workload[i+1,j]=max(workload[i,j]-T[i],0)
        workload[i+1,]=sort(workload[i+1,])
    }
    for(i in 1:n){
      delay[i]=workload[i,N[i]]
      total_cores[i]=sum(workload[i,]>0)
    }
    res=data.frame(workload,delay,total_cores)
    res
  }

Delay=function(T,S,N,s,n=length(T),init.workload=rep(0,s)) {
  workload=init.workload
  Delay=vector("numeric",n)
  for(i in 1:n){ 
    Delay[i]=workload[N[i]]
    workload=sort(pmax((Delay[i]+S[i])*(1:s<=N[i])+workload*(1:s>N[i])-T[i], 0))}
  Delay
}

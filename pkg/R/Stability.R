#' @importFrom stats convolve
NULL

#' Approximate, dynamic iterative computation of the stability constant for a workload of a High Performance Cluster model
#' 
#' This function calculates the constant C that is used in the stability criterion
#' of a supercomputer model, which is basically the following: lambda/mu<C, where lambda
#' is the task arrivals rate, and mu is the service intensity. The constant depends only on
#' the number of servers in the model and the distribution of classes of customers,
#' where class is the number of servers required. This method of calculation allows 
#' to stop on some depth of dynamics, thus allowing to calculate an approximate value in
#' faster time. The constant is valid only for the model with simultaneous service.
#' 
#' @param s number of servers in the model
#' @param p vector of class distribution
#' @param depth By default calculates up to groups of 3 tasks. When depth=s, calculates the exact value. However, depth=s might take a bit more time.
#' @return The value of a constant C in the relation lambda/mu < C is returned
#' @examples
#' ApproxC(s=2,p=c(.5,.5), depth=3)
#' @export
# 
ApproxC=function(s,p,depth=3){
	A=cumsum(p[s:1])
	B=p
	C=0
	for(i in 1:depth){
	C=C+sum(B[1:(s-i+1)]*A[i:s])/i
	B=convolve(B,rev(p),type="o")[1:s]
	}
	return(C)
}

#' This function gives the maximal throughput of a two-server supercomputer (Markov) model with various service speeds, various rates of classes and random speed scaling at arrival/depature
#' 
#' This function gives the maximal throughput of a two-server supercomputer (Markov) model with various service speeds, various rates of classes and random speed scaling at arrival/depature
#' 
#' @param p1 probability of class 1 arrival
#' @param pa probability of speed switch from f1 to f2 upon arrival
#' @param pd probability of speed switch from f2 to f1 upon departure
#' @param mu1 work amount parameter (for exponential distribution) for class 1
#' @param mu2 work amount parameter (for exponential distribution) for class 2
#' @param f1 low speed (workunits per unit time)
#' @param f2 high speed (workunits per unit time)
#' @return maximal input rate, that is the stability boundary
#' @export
# 
MaxThroughput2 <- function(p1,pa,pd,mu1,mu2,f1,f2){
  if(p1==0){
    if(pa==pd)
      return(mu2*sqrt(f1*f2))
    if(pa==0&pd==1)
      return(f1*mu2)
    if(pa==1&pd==0)
      return(f2*mu2)
    return(f2*mu2*(pa-pd)+sqrt(f2^2*mu2^2*(pa-pd)^2 + 4*f1*f2*mu2^2*pa*pd)/(2*pa))
  }
  if(p1==1){
    if(pa==pd)
      return(2*mu1*sqrt(f1*f2))
    if(pa==0&pd==1)
      return(2*f1*mu1)
    if(pa==1&pd==0)
      return(2*f2*mu1)
    return(f2*mu2*(pa-pd)+sqrt(f2^2*mu1^2 *(pa-pd)^2 + 4* f1* f2*mu1^2* pa* pd)/pa)
  }
  p2=1-p1
  a0=pa^3*(mu2*p1*(p1-2)-2*mu1*p2)
  a1=pa^2*(f1*(mu1*mu2*p1*(p1*(9-6*pd)+4*(pd-2)+2*p1^2*(pd-1))-2*mu1^2* p2*(3+2*p1*(pd-1))-mu2^2*(p1-2)*p1*(p1*(pd-1)-pd))+2*f2*mu1*mu2*(pa-pd))
  a2=f1*mu1*pa*(2*f2*mu2*(mu1*pa*(3+2*p1*(pd-1))+mu2*pa*pd-mu2*p1*(pa*(pd-1)+pd)+
                            mu1*pd*(-3-p1*(pd-2)+p1^2 *pd))+f1*(-4*mu1^2*p2*(1+p1*(pd-1))+
                                                                  2*mu1*mu2*p1*p2*(4+p1*(pd-2))*(pd-1)-mu2^2*p1*(p1^3*(pd-1)^2+4*pd+
                                                                                                                   p1^2*(-4+7*pd-3*pd^2)+p1*(4-9*pd+2*pd^2))))
  a3=2*f1^2*f2*mu1^2*mu2*(2*mu1*(pa+p1*pa*(pd-1)-p2*pd)+mu2*(-2*p1*pd+3*pa*pd+p1^2*(-pa*(pd-1)^2+pd)+p1*pa*(2-3*pd+pd^2)))
  a4=4 *f1^3*f2*mu1^3*mu2^2*pd
  return(max(Re(polyroot(c(a4,a3,a2,a1,a0)))))
}
#' @importFrom utils write.table
NULL

#' Convertor from a dataframe to Standart Workload Format
#' 
#' Note that this is only a wrapper for the ToSWF command with a dataframe
#' argument. It needs a correctly built dataframe and converts it to a Standart Workload
#' Format used to share the logfiles of High Performance Clusters  
#' 
#' The Standart Workload Format is a single format to store and exchange
#' high performance cluster logs, that is used in Parallel Workload Archive.
#' See references for current standard. The SWF format may contain additional
#' data, but in this package only the 1st to 5th fields are used. One may also
#' need to manually fill in the header of the file in order to completely
#' prepare the resulting SWF file.   
#' 
#' @references Feitelson, D.G. and Tsafrir, D. and Krakov D. 2012 Experience with the Parallel Workloads Archive. Technical Report 2012-6, School of Computer Science and Engineering, the Hebrew University April, 2012, Jerusalem, Israel
#' @references https://www.cs.huji.ac.il/labs/parallel/workload/swf.html
#' @param Frame A dataframe containing the variables needed by ToSWF function
#' @param filename The file to store the converted workload (output.swf by default)
#' @return Nothing is returned, but a file is created in the current working directory
#' (with default name output.swf) containing the converted data.
#' @examples 
#' \dontrun{
#' data(HPC_KRC)
#' tmp=data.frame(T=HPC_KRC$interarrival, S=HPC_KRC$service, N=HPC_KRC$cores_used, D=HPC_KRC$delay)
#' DataToSWF(tmp)
#' }
#' @export
# 
DataToSWF <-
  function(Frame,filename="output.swf") {
    if(is.null(Frame) || is.null(filename))
      stop("'Frame' must be defined!")
    if (is.null(Frame$T) || is.null(Frame$S) || is.null(Frame$N) || is.null(Frame$D)) 
      stop("'T','S','N','D' in data frame must be defined!")
    ToSWF(T=Frame$T,S=Frame$S,N=Frame$N,D=Frame$D,filename)
  }

#' Convertor to a dataset from a Standart Workload Format
#' 
#' This is a convertor from a Standart Workload Format (used to share the logfiles 
#' of High Performance Clusters) to an internally used in a package dataset format  
#' 
#' The Standart Workload Format is a single format to store and exchange
#' high performance cluster logs, that is used in Parallel Workload Archive.
#' See references for current standard. The SWF format may contain additional
#' data, but in this package only the 1st to 5th fields are used. One may also
#' need to manually fill in the header of the file in order to completely
#' prepare the resulting SWF file.   
#' 
#' @references Feitelson, D.G. and Tsafrir, D. and Krakov D. 2012 Experience with the Parallel Workloads Archive. Technical Report 2012-6, School of Computer Science and Engineering, the Hebrew University April, 2012, Jerusalem, Israel
#' @references https://www.cs.huji.ac.il/labs/parallel/workload/swf.html
#' @param filename A mandatory field containing the path to SWF file
#' @return A dataset is returned, containing 'delay' as a vector of delays exhibited by
#' each task, 'total_cores' as the total busy CPUs in time of arrival of each task,
#' and 'workload' as total work left at each CPU.
#' @export
# 
FromSWF <-
  function(filename) {
    if(is.null(filename))
      stop("The variable 'filename' must be defined!")
    if(!is.character(filename) || length(filename)==0)
      stop("Incorrect 'filename'!")
    
    inp=scan(filename,list(0, t=0, D=0, S=0, N=0, 0,0,0,0,0,0,0,0,0,0,0,0,0))
    n=length(inp$t)
    T=inp$t[2:n]-inp$t[1:(n-1)]
    S=inp$S[1:(n-1)]
    D=inp$D[1:(n-1)]
    N=inp$N[1:(n-1)]
    res=data.frame(T,S,N,D)
    res
  }

#' Convertor from a dataset to Standart Workload Format
#' 
#' This is a convertor from a correctly built dataset to a Standart Workload Format 
#' used to share the logfiles of High Performance Clusters
#' 
#' The Standart Workload Format is a single format to store and exchange
#' high performance cluster logs, that is used in Parallel Workload Archive.
#' See references for current standard. The SWF format may contain additional
#' data, but in this package only the 1st to 5th fields are used. One may also
#' need to manually fill in the header of the file in order to completely
#' prepare the resulting SWF file. 
#' 
#' @references Feitelson, D.G. and Tsafrir, D. and Krakov D. 2012 Experience with the Parallel Workloads Archive. Technical Report 2012-6, School of Computer Science and Engineering, the Hebrew University April, 2012, Jerusalem, Israel
#' @references https://www.cs.huji.ac.il/labs/parallel/workload/swf.html
#' @param T Interarrival times of tasks (a vector)
#' @param S Service times of tasks (a vector)
#' @param N Number of cores each task needs (a vector)
#' @param D The delays of tasks in a queue (a vector)
#' @param filename The file to store the converted workload (output.swf by default)
#' @return Nothing is returned, but a file is created in the current working directory (with default name output.swf) containing the converted data.
#' @examples
#' \dontrun{
#' data(HPC_KRC)
#' ToSWF(HPC_KRC$interarrival, HPC_KRC$service, HPC_KRC$cores_requested, HPC_KRC$delay)
#' }
#' @export
# 
ToSWF <-
  function(T, S, N, D, filename="output.swf") {
    if(is.null(T) || is.null(S) || is.null(N) || is.null(D) || is.null(filename))
      stop("'T','S','N', and 'D' must be defined!")
    Lst=list(T,S,N,D)
    for(i in 1:4)
      if (!is.numeric(Lst[[i]]) || length(Lst[[i]]) == 0) 
        stop("Only numeric vectors allowed!")
    n=length(T)
    if(length(S)<n || length(N)<n || length(D)<n)
      stop("Length of 'T' vector should be not more than others!")
    
    F1=1:n
    F2=vector("numeric",length=n)
    for(i in 2:n) F2[i]=F2[i-1]+T[i-1]
    F3=D[1:n]
    F4=F9=S[1:n]
    F5=F8=N[1:n]
    F6=F7=F10=F11=F14=F15=F16=F17=F18=rep(-1,times=n)
    F12=F13=rep(1,times=n)
    write.table(format(data.frame(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18)),file=filename,quote=FALSE,col.names=FALSE,row.names=FALSE)
  }
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Workload.R
\name{Wld}
\alias{Wld}
\title{Workload of a High Performance Cluster model}
\usage{
Wld(T, S, N, m, method = "concurrent")
}
\arguments{
\item{T}{Interarrival times of tasks}

\item{S}{Service times of customers (a vector of length n, or a matrix nrows=n, ncols='m').}

\item{N}{Number of servers each customer needs}

\item{m}{Number of servers for a supercomputer}

\item{method}{Independent or concurrent}
}
\value{
A dataset is returned, containing 'delay' as a vector of delays exhibited by
each task, 'total_cores' as the total busy CPUs in time of arrival of each task,
and 'workload' as total work left at each CPU.
}
\description{
This function computes the Kiefer-Wolfowitz modified vector for a HPC
model. This vector contains the work left on each of 'm' servers of a cluster
for the time of the arival of a task. Two methods are available, one for the 
case of concurrent server release (all the servers end a single task simultaneously),
other for independent release (service times on each server are independent).
}
\examples{
Wld(T=rexp(1000,1), S=rexp(1000,1), round(runif(1000,1,10)), 10)
}

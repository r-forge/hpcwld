% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stability.R
\name{MaxThroughput2}
\alias{MaxThroughput2}
\title{This function gives the maximal throughput of a two-server supercomputer (Markov) model with various service speeds, various rates of classes and random speed scaling at arrival/depature}
\usage{
MaxThroughput2(p1, pa, pd, mu1, mu2, f1, f2)
}
\arguments{
\item{p1}{probability of class 1 arrival}

\item{pa}{probability of speed switch from f1 to f2 upon arrival}

\item{pd}{probability of speed switch from f2 to f1 upon departure}

\item{mu1}{work amount parameter (for exponential distribution) for class 1}

\item{mu2}{work amount parameter (for exponential distribution) for class 2}

\item{f1}{low speed (workunits per unit time)}

\item{f2}{high speed (workunits per unit time)}
}
\value{
maximal input rate, that is the stability boundary
}
\description{
This function gives the maximal throughput of a two-server supercomputer (Markov) model with various service speeds, various rates of classes and random speed scaling at arrival/depature
}

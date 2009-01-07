\name{organismName-methods}
\docType{methods}
\alias{organismName}
\alias{organismName-methods}
\alias{organismName,psimi25Complex-method}
\alias{organismName,psimi25Interactor-method}
\title{ Returns the organism name(s) of the given item }
\description{
  The method serves as accessor to retrieve the organism name of the
  given item, in most cases these are common names of the species as character string.
}
\section{Methods}{
\describe{

\item{x = "psimi25Complex"}{ An object of
  \code{\link{psimi25Complex-class}}, indicating the complex belongs to
  which organism(s). Heterogeous complex is possible, which contains
  proteins from two or more species. In this case the returned vector
  has length larger than one}

\item{x = "psimi25Interactor"}{ An object of
  \code{\link{psimi25Interactor-class}}, returns the organism of the protein,
  this value is normally of length 1.}
}}
\keyword{methods}

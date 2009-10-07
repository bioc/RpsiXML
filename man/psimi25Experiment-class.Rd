\name{psimi25Experiment-class}
\docType{class}
\alias{psimi25Experiment-class}

\title{Class "psimi25Experiment"}
\description{ Representing an experiment recorded in PSI-MI 2.5 XML files}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{\link{parseExperiment}}
}
\section{Slots}{
	 \describe{
    \item{\code{sourceDb}:}{Object of class \code{"character"}, source
      database short label}
    \item{\code{sourceId}:}{Object of class \code{"character"},
      experiment ID of the source database}
    \item{\code{interactionType}:}{Object of class \code{"character"},
      interaction type, "Y2H","vv", etc}
    \item{\code{expPubMed}:}{Object of class \code{"character"}, PubMed ID
      of the experiment }
  }
}
\section{Methods}{
No methods defined with class "psimi25Experiment" in the signature.
}
\author{ Jitao David Zhang <j.zhang@dkfz.de>, Tony Chiang <tchiang@ebi.ac.uk> }
\seealso{
  \code{\link{parseExperiment}}
}
\examples{
showClass("psimi25Experiment")
}
\keyword{classes}

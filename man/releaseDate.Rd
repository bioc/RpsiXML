\name{releaseDate}
\alias{releaseDate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Release date of the psimi25InteractionEntry object }
\description{
  The function returns the release date of the
  \code{\link{psimi25InteractionEntry-class}} object, which can serve as
  a proxy for versioning
}
\usage{
releaseDate(x)
}
\arguments{
  \item{x}{ An object of \code{\link{psimi25InteractionEntry-class}}  }
}
\value{
  A character string indicating the release date
}
\author{ Jitao David Zhang <j.zhang@dkfz.de>, Tony Chiang <tchiang@ebi.ac.uk> }
\seealso{ \code{\link{psimi25InteractionEntry-class}}  }
\examples{
xmlDir <- system.file("/extdata/psi25files",package="RpsiXML")

gridxml <- file.path(xmlDir, "biogrid_200804_test.xml")
gridSet <- parsePsimi25Interaction(gridxml, BIOGRID.PSIMI25)

gridRD <- releaseDate(gridSet)
}
\keyword{ models }


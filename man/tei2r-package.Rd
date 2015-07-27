\name{tei2r-package}
\alias{tei2r-package}
\alias{tei2r}
\docType{package}
\title{
Import and parse TEI files in R
}
\description{
tei2r imports collections of plain-text or TEI-encoded files into
the R environment and makes them available for quantitative analysis.
}
\details{
\tabular{ll}{
Package: \tab tei2r\cr
Type: \tab Package\cr
Version: \tab 0.0.0.9000\cr
Date: \tab 2015-05-30\cr
License: \tab CC0 (http://bit.ly/1F96Zgp) \cr
}

}
\author{
Michael Gavin and Travis Mullen

Maintainer: Michael Gavin <mgavin@mailbox.sc.edu>
}
\references{}

\keyword{ tei, text analysis }
\examples{
dl = buildDocList(wizard = T) # Creates a structured list for your metadata
dt = texts(dl) # Imports and cleans up text from TEI or plain text sources
dc = concordance(dt, keyword = "rights", context = 10) # Builds a concordance
}

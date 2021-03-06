% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseTEI.R
\name{parseTEI}
\alias{parseTEI}
\title{Extract part of a TEI document}
\usage{
parseTEI(filepath, node = "")
}
\arguments{
\item{filepath}{A string that holds the location of the file
you want to extract values from.}

\item{node}{One or more node types that you want to extract
values from.}
}
\value{
wantedNodes  The values for all nodes matching \code{node} in
                     the file found at \code{filepath}.
}
\description{
This function will return the values of given \code{nodes} in a
given \code{filepath}.
}
\examples{
nodes = parseTEI("path/to/xml/file", node="hi") # Get values for all highlight nodes
}


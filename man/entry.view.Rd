\name{entry.view}
\alias{entry.view}
\title{Display a list (or rows of a data frame) in key-value-pairs.}
\usage{entry.view(x, sort=FALSE, fmt)
}
\description{Display a list (or rows of a data frame) in key-value-pairs.}
\details{Color coded according to class of contents.}
\value{Nothing.}
\author{Christofer \enc{Bäcklin}{Backlin}}
\arguments{\item{x}{List or data frame.}
\item{sort}{Display the elements in alphabetical order of the names.
Optional, default: FALSE.}
\item{fmt}{\code{\link{sprintf}} type formatting string which will be
applied to numbers, e.g. for specifying number of decimals or alignment.}
}
\examples{entry.view(Sys.getenv())
entry.view(rnorm(20), fmt="\%5.2f")}


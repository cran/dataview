\name{entry.view}
\alias{entry.view}
\title{Display a list (or rows of a data frame) in key-value-pairs.}
\usage{entry.view(x, sort=FALSE)
}
\description{Display a list (or rows of a data frame) in key-value-pairs.}
\details{Color coded according to class of contents.}
\value{Nothing.}
\author{Christofer Bäcklin}
\arguments{\item{x}{List or data frame.}
\item{sort}{Display the elements in alphabetical order of the names.
Optional, default: FALSE.}
}
\examples{entry.view(Sys.getenv())}


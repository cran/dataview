\name{entry.view}
\alias{entry.view}
\title{Display vectors, lists or rows of a data frames in key-value-pairs.}
\usage{
entry.view(x, i = 1, sort.fields = FALSE, fmt = NULL)
}
\arguments{
  \item{x}{List or data frame.}

  \item{i}{Row number to show.}

  \item{sort.fields}{Display the elements in alphabetical
  order of the names.  Optional, default: FALSE.}

  \item{fmt}{\code{\link{sprintf}} type formatting string
  which will be applied to numbers, e.g. for specifying
  number of decimals or alignment.}
}
\value{
Nothing.
}
\description{
Color coded according to class of contents.
}
\examples{
entry.view(Sys.getenv())
entry.view(rnorm(20), fmt="\\\%5.2f")
}
\author{
Christofer \enc{Bäcklin}{Backlin}
}


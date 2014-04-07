\name{whos.set.mask}
\alias{whos.set.mask}
\title{Set a default exclusion mask for \code{\link{whos}}.}
\usage{
whos.set.mask(lst, envir = globalenv())
}
\arguments{
  \item{lst}{List of object names. These will be hidden
  from view. Defaults to all objects in \code{envir}.}

  \item{envir}{Environment to work in.}
}
\value{
Nothing. The mask is stored as `whos.mask' in the global
option list.
}
\description{
Set a default exclusion mask for \code{\link{whos}}.
}
\author{
Christofer \enc{Bäcklin}{Backlin}
}


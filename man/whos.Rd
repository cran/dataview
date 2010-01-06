\name{whos}
\alias{whos}
\title{Display contents of an evironment, data...}
\usage{whos(pattern="", envir=globalenv(), exclude=getOption("whos.mask"))
whos.set.mask(lst, envir=globalenv())
}
\description{Display contents of an evironment, data.frame or list as a summary table}
\details{\code{whos}: Color coded according to class and dimensions of contents. See
\code{\link[xtermStyle]{style}} for details.

}
\value{\code{whos}: Nothing

\code{whos.set.mask}: Nothing. The mask is stored as `whos.mask' in the global option list.


}
\author{Christofer Bäcklin}
\arguments{\item{pattern}{Regexp filtering of objects. Only objects matching the pattern
are displayed. Optional, default: show all objects.}
\item{envir}{Environment, data frame or list to be displayed. Optional,
default: globalenv()}
\item{exclude}{A list of objects not to be displayed. To set a default exclusion
mask use the \code{whos.set.mask} function. If \code{whos.set.mask} is
called without a list of object names all objects currently in globalenv()
are hidden. This is useful for example if you have a lot of stuff in the
workspace that you aren't currently interested in but is needed to make
your code run.}
\item{lst}{List of object names. These will be hidden from view.}
}
\examples{whos()
data(USArrests)
whos(USArrests)

whos.set.mask()}
\alias{whos.set.mask}


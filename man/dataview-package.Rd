\name{dataview-package}
\alias{dataview-package}
\alias{dataview-package}
\title{Human readable data presentation.}
\description{Human readable data presentation.}
\details{\tabular{ll}{
Package: \tab dataview\cr
Type: \tab Package\cr
Version: \tab 1.2-3\cr
Date: \tab 2010-01-06\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
Depends: \tab xtermStyle\cr
}

This package provides clear, informative and colorfull ways of presenting
the contents of data.frame, list, environment etc. in the prompt.

Ideally this package should be used in terminals supporting xterm 256 colour
mode, e.g. the standard Ubuntu terminal. The Mac OS X terminal (as of Snow
Leopard) only support ANSI colour, which still works but doesn't look as
nice. On Windows systems no colouring is supported.

If the output of functions like \sQuote{whos} is a mess or somehow doesn't
look like it should check the help for the functions \sQuote{style} and
\sQuote{style.mode} of \sQuote{xtermStyle} package.}
\alias{dataview}
\docType{package}
\author{Christofer Bäcklin \url{http://christofer.backlin.se/R/}}
\keyword{data}
\keyword{presentation}
\keyword{xterm}
\keyword{terminal}
\keyword{prompt}
\seealso{\code{\link{whos}}, \code{\link{entry.view}}, \code{\link{tree.view}},
\code{\link{heat.view}}, \code{\link[xtermStyle]{style}}}


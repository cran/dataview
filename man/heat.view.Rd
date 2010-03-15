\name{heat.view}
\alias{heat.view}
\title{Display heatmaps and heatvectors.}
\usage{heat.view(x, pal, rng, width)
}
\description{Display heatmaps and heatvectors.}
\details{Quickly see the overall pattern of a variable in the terminal.}
\value{Nothing}
\author{Christofer \enc{Bäcklin}{Backlin}}
\arguments{\item{x}{Vector to be displayed.}
\item{pal}{Palette. Either the name of a palette defined in \code{\link[xtermStyle]{xterm.pal}}
or an integer vector with color indices (see \code{\link[xtermStyle]{display.xterm.colors}}).}
\item{rng}{The numerical range which the palette describes. See \code{\link[xtermStyle]{discrete.color}}
for more info.}
\item{width}{Length of each line. Optional.}
}
\examples{data(iris)
heat.view(iris$Species)
heat.view(matrix(iris$Petal.Width, 3, 50, byrow=TRUE, dimnames=list(levels(iris$Species), NULL)), pal="purples")

run.status <- factor(runif(100) < .95, labels=c("Fail", "Pass"))
heat.view(run.status, pal=1:2)

#Tip for displayig the element names of a named vector:
a <- runif(7)
names(a) <- c("ATM", "CHK1", "CDC25", "p53", "CDC2", "CDK2", "CDK4")
heat.view(a)            # No names displayed
heat.view(as.matrix(a)) # Names displayed}


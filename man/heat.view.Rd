\name{heat.view}
\alias{heat.view}
\title{Display heatvector, similar to 1-d heatmap.}
\usage{heat.view(x, col, width)
}
\description{Display heatvector, similar to 1-d heatmap.}
\details{Quickly see the overall pattern of a vector in the terminal.}
\value{Nothing}
\author{Christofer Bäcklin}
\arguments{\item{x}{Vector to be displayed.}
\item{col}{Colormap. Either the name of a palette defined in \code{\link[xtermStyle]{xterm.pal}}
or an integer vector with color indices (see \code{\link[xtermStyle]{display.xterm.colors}}).}
\item{width}{Length of each line. Optional.}
}
\examples{data(iris)
heat.view(iris$Species)
heat.view(iris$Petal.Width)

run.status <- factor(runif(100) < .95, labels=c("Fail", "Pass"))
heat.view(run.status, col=1:2)}


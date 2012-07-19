\name{wrap.view}
\alias{wrap.view}
\title{Display contents of a vector or list as line wrapped text}
\usage{
  wrap.view(x)
}
\arguments{
  \item{x}{Vector or list to be displayed.}
}
\value{
  Nothing
}
\description{
  Display contents of a vector or list as line wrapped text
}
\examples{
x <- rep(NA, 6)
for(i in 1:6) x[i] <- paste(c("m", "a", "r", "u", "l", "k", " ")[1+floor(7*runif(100+floor(500*runif(1))))], collapse="")
wrap.view(x)

x <- list(1:9, stuff=Sys.info(), today=date(), model=Outcome ~ Variables)
wrap.view(x)
}
\author{
  Christofer \enc{Bäcklin}{Backlin}
}


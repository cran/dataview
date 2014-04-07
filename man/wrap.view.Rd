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
x <- replicate(6, paste(sample(c("m", "a", "r", "u", "l", "k", " "),
    round(100+400*runif(1)), TRUE), collapse=""))
wrap.view(x)

x <- list(1:9, stuff=Sys.info(), today=date(), model=Outcome ~ Variables)
wrap.view(x)
}
\author{
Christofer \enc{Bäcklin}{Backlin}
}


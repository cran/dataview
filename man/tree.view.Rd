\name{tree.view}
\alias{tree.view}
\title{Display contents of a list of lists in a summarized tree structure.}
\usage{tree.view(x, compact="auto", show.data="auto", traverse.all=FALSE, lines=Inf,
    depth=Inf, indent=0)
}
\description{Display contents of a list of lists in a summarized tree structure.}
\value{Nothing}
\author{Christofer \enc{Bäcklin}{Backlin}}
\arguments{\item{x}{List to be displayed.}
\item{compact}{Wheter to display the list tree in compact mode. Optional,
default: 'auto' i.e. adapt to terminal height.}
\item{show.data}{Whether to show the contents of the list elements or just
the structure. Optional, default: 'auto' i.e. adapt to terminal width.}
\item{traverse.all}{Whether to treat objects of custom classes as lists or
not traverse them (except if they are the root of the tree).}
\item{lines}{Maximum number of lines to show.}
\item{depth}{Maximum number of levels to show.}
\item{indent}{Internal.}
}
\examples{# Create a tree structure of lists
make.list.tree <- function(boost=2) {
n.children <- round(boost + rexp(1))
if(n.children < 1){
return(rep("data", 1+floor(5*runif(1))))
} else {
ll <- vector("list", n.children)
names(ll) <- paste("node", 1:n.children)
return(lapply(ll, function(x) make.list.tree(boost-1)))
}
}

# Visualize it!
tree.view(make.list.tree())}


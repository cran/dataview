pkgname <- "dataview"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('dataview')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("entry.view")
### * entry.view

flush(stderr()); flush(stdout())

### Name: entry.view
### Title: Display a list (or rows of a data frame) in key-value-pairs.
### Aliases: entry.view

### ** Examples
entry.view(Sys.getenv())
entry.view(rnorm(20), fmt="%5.2f")


cleanEx()
nameEx("heat.view")
### * heat.view

flush(stderr()); flush(stdout())

### Name: heat.view
### Title: Display heatmaps and heatvectors.
### Aliases: heat.view

### ** Examples
data(iris)
heat.view(iris$Species)
heat.view(matrix(iris$Petal.Width, 3, 50, byrow=TRUE, dimnames=list(levels(iris$Species), NULL)), pal="purples")

run.status <- factor(runif(100) < .95, labels=c("Fail", "Pass"))
heat.view(run.status, pal=1:2)

#Tip for displayig the element names of a named vector:
a <- runif(7)
names(a) <- c("ATM", "CHK1", "CDC25", "p53", "CDC2", "CDK2", "CDK4")
heat.view(a)            # No names displayed
heat.view(as.matrix(a)) # Names displayed


cleanEx()
nameEx("tree.view")
### * tree.view

flush(stderr()); flush(stdout())

### Name: tree.view
### Title: Display contents of a list of lists in a summarized tree
###   structure.
### Aliases: tree.view

### ** Examples
# Create a tree structure of lists
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
tree.view(make.list.tree())


cleanEx()
nameEx("whos")
### * whos

flush(stderr()); flush(stdout())

### Name: whos
### Title: Display contents of an evironment, data...
### Aliases: whos whos.set.mask

### ** Examples
whos()
data(USArrests)
whos(USArrests)

whos.set.mask()


### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

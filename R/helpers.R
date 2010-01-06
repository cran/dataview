##' Wrapper for several methods to test if a variable is empty
##'
##' @param x A variable.
##' @param false.triggers Whether \code{FALSE} should be considered as empty.
##' @return Logical telling if variable is blank.
##' @examples
##' is.blank(NULL)
##' @author Christofer Bäcklin
##' @nord
is.blank <- function(x, false.triggers=FALSE){
    return(
        is.null(x) ||
        length(x) == 0 ||
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}

##' Human readable object dimensions
##'
##' @param x Object
##' @return A string describing the dimension of an object e.g. `scalar' or `50x2'
##' @examples
##' data(iris)
##' dimfun(iris)
##' @author Christofer Bäcklin
##' @nord
dimfun <- function(x, use.names=FALSE) {
    if(!is.null(dim(x))){
        if(use.names && !is.null(names(dimnames(x)))){
            return(paste(names(dimnames(x)), "(", dim(x), ")", sep="", collapse=" x "))
        } else {
            return(paste(dim(x), collapse="x"))
        }
    } else if(is.vector(x) || is.factor(x)) {
        if(length(x) == 1 && !is.list(x)) return("scalar") else return(paste("#", length(x), sep=""))
    } else if(isS4(x)){
        return(paste("#", length(slotNames(x)), sep=""))
    } else return("")
}

##' Human readable object size
##'
##' @param x Object
##' @return A two elemt string vector describing the memory occupied by the
##'   variable e.g. c('32.0', 'B') or c('14.5', 'MiB')
##' @examples
##' data(iris)
##' sizefun(iris)
##' @author Christofer Bäcklin
##' @nord
sizefun <- function(x) {
    if(object.size(x) == 0){
        return(c("0", ""))
    } else {            
        i <- trunc(log(object.size(x)) / log(1024))
        return(c(exp(log(object.size(x)) - i*log(1024)), c("B", "KiB", "MiB", "GiB", "TiB")[i+1]))
    }
}

##' Generic object fetching for both S3 and S4
##'
##' @param envir Environment, list or object to search in.
##' @name name of object to look for.
##' @examples
##' data(iris)
##' objfun(iris, "Species")
##' @author Christofer Bäcklin
##' @nord
objfun <- function(envir, name){
    return(if(is.environment(envir)){
        get(name, envir=envir)
    } else if(isS4(envir)){
        slot(envir, name)
    } else {
        envir[[name]]
    })
}



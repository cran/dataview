##' @import xtermStyle
##' @include helpers.R
{}

##' Display contents of an evironment, data.frame or list as a summary table
##'
##' Color coded according to class and dimensions of contents. See
##' \code{\link[xtermStyle]{style}} for details.
##'
##' @param pattern Regexp filtering of objects. Only objects matching the pattern
##'   are displayed. Optional, default: show all objects.
##' @param envir Environment, data frame or list to be displayed. Optional,
##'   default: globalenv()
##' @param exclude A list of objects not to be displayed. To set a default exclusion
##'   mask use the \code{whos.set.mask} function. If \code{whos.set.mask} is
##'   called without a list of object names all objects currently in globalenv()
##'   are hidden. This is useful for example if you have a lot of stuff in the
##'   workspace that you aren't currently interested in but is needed to make
##'   your code run.
##' @return Nothing
##' @examples
##' whos()
##' data(USArrests)
##' whos(USArrests)
##' 
##' whos.set.mask()
##' @author Christofer Bäcklin
##' @export
whos <- function(pattern="", envir=globalenv(), exclude=getOption("whos.mask")){
    # Check if the user specified a pattern, environment or both
    if(missing(pattern)){ pattern <- ""
    } else if(!is.character(pattern) && missing(envir)) {
        envir <- pattern
        pattern <- ""
    }
    # Get names of objects in environment matching pattern
    if(is.environment(envir)){
        obj.name <- ls(envir=envir, pattern=pattern)
    } else if(isS4(envir)){
        obj.name <- slotNames(envir)
    } else {
        obj.name <- if(!is.null(names(envir))) names(envir) else 1:length(envir)
    }
    obj.name <- obj.name[!obj.name %in% exclude]
    # Present objects
    n <- length(obj.name)
    if(n == 0){
        cat("No objects found\n")
    } else {
        # Make an object/property matrix (objects as rows, properties as columns)
        obj <- matrix("", n, 6)
        # Go through all objects and assess their properties
        obj[,1] <- obj.name
        total.size <- 0
        for(i in 1:n){
            o <- objfun(envir, obj.name[i])
            obj[i,2] <- if(length(class(o)) > 1){
                paste(class(o)[1], "...")
            } else if(class(o) == "factor"){
                sprintf("factor (%i)", length(levels(o)))
            } else class(o)
            if(obj[i,2] %in% c("matrix", "array")) obj[i,2] <- paste(mode(o), obj[i,2])
            obj[i,3] <- if(isS4(o)) "S4  " else ""
            obj[i,4] <- dimfun(o)
            obj[i,5:6] <- sizefun(o)
            total.size <- total.size + object.size(o)
        }
        if(any(obj[,3] != "")) obj[obj[,3] == "",3] <- "    "
        # Determine how many characters each column occupies i.e. length of longest entry in each column
        nc <- apply(obj, 2, function(x) max(nchar(x)))
        # Output table
        for(i in 1:n){
            cat(style.dim(sprintf(paste("%", ceiling(log10(n+1)), "i", sep=""), i)),
                style.auto(objfun(envir, obj.name[i]),
                    sprintf(paste("  %-",nc[1],"s  %-",nc[2],"s  %s%-",nc[4],"s %6.1f %-3s  ", sep=""),
                        obj[i,1], obj[i,2], obj[i,3], obj[i,4], as.numeric(obj[i,5]), obj[i,6])),
                "\n", sep="")
        }
        # Output total size of all objects in table (not nessecarily all objects in workspace)
        i <- trunc(log(total.size) / log(1024))
        total.size <- exp(log(total.size) - i*log(1024))
        total.size.suffix <- c("B", "KiB", "MiB", "GiB", "TiB")[i+1]
        cat(style.dim(paste(c(rep(" ", ceiling(log10(n+1)) + 2*4+sum(nc[1:4]) - 7),
                          sprintf("Total %6.1f %-3s  \n", total.size, total.size.suffix)),
                          collapse="")))
    }
}

##' Set a default exclusion mask for \code{\link{whos}}.
##'
##' @param lst List of object names. These will be hidden from view.
##' @param envir Environment to work in.
##' @return Nothing. The mask is stored as `whos.mask' in the global option list.
##' @author Christofer Bäcklin
##' @rdname whos
##' @export
whos.set.mask <- function(lst, envir=globalenv()){
    # This function sets the default exclusion mask for the `whos' function.
    # It is stored as a list in the global options. When "whosing" objects fully
    # matching an entry in the mask will be omitted from the output.
    if(missing(lst)) options(whos.mask = ls(envir=envir))
    else options(whos.mask = lst)
}


##' Display a list (or rows of a data frame) in key-value-pairs.
##'
##' Color coded according to class of contents.
##'
##' @param x List or data frame.
##' @param sort Display the elements in alphabetical order of the names.
##'   Optional, default: FALSE.
##' @return Nothing.
##' @examples
##' entry.view(Sys.getenv())
##' @author Christofer Bäcklin
##' @export
entry.view <- function(x, sort=FALSE){
    if(class(x) == "data.frame"){
        if(nrow(x) > 1){
            cat("Only showing first of", nrow(x), "objects of data frame\n")
        }
        x <- as.list(x[1,])
    }

    if(is.blank(names(x))) names(x) <- 1:length(x)
    name_maxlength <- max(as.numeric(lapply(names(x), nchar)))
    terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80L else as.integer(Sys.getenv("COLUMNS"))
    output.width <- terminal.width - 4 - name_maxlength
    for(i in (if(sort) order(names(x)) else 1:length(x))){
        # objfun should be used here
        obj <- objfun(x, i)
        content.str <- if(mode(obj) %in% c("numeric", "logical", "character", "factor")){
                str <- paste(obj, collapse=", ")
                if(nchar(str) > output.width){
                    sprintf("%s %s", mode(obj), dimfun(obj))
                } else {
                    str
                }
            } else {
                if(mode(obj) != class(obj)){
                    sprintf("%s %s", mode(obj), class(obj))
                } else
                    class(obj)
            }
        
        cat(rep(" ", 2 + name_maxlength - nchar(names(x)[i])),
            style.dim(sprintf("%s: ", names(x)[i])),
            style.auto(obj, content.str),
            "\n", sep="")
    }
}


##' Display heatvector, similar to 1-d heatmap.
##'
##' Quickly see the overall pattern of a vector in the terminal.
##'
##' @param x Vector to be displayed.
##' @param col Colormap. Either the name of a palette defined in \code{\link[xtermStyle]{xterm.pal}}
##'   or an integer vector with color indices (see \code{\link[xtermStyle]{display.xterm.colors}}).
##' @param width Length of each line. Optional.
##' @return Nothing
##' @examples
##' data(iris)
##' heat.view(iris$Species)
##' heat.view(iris$Petal.Width)
##'
##' run.status <- factor(runif(100) < .95, labels=c("Fail", "Pass"))
##' heat.view(run.status, col=1:2)
##' @author Christofer Bäcklin
##' @export
heat.view <- function(x, col, width){
    if(is.null(options()$color.scheme) || options()$color.scheme != "dark on light"){
        col.f <- rev
    } else {
        col.f <- function(x) x
    }
    if(!missing(col) && is.character(col)) col <- col.f(xterm.pal(col)[[1]])
    
    if(is.logical(x)){
        if(missing(col)) col <- c(12,11)
        x <- x + 1
        legend.str <- c(style("False", fg=col[1]), style("True", fg=col[2]))
    } else if(is.numeric(x)){
        if(missing(col))
            col <- col.f(xterm.pal("long")[[1]])
        x <- floor(1+length(col)*(x-min(x)) / (max(x)-min(x)))
        x[x > length(col)] <- length(col)
        legend.str <- rep("", length(col)+2)
        legend.str[1] <- "Lowest"
        legend.str[length(col)+2] <- "Highest"
        for(i in 1:length(col)) legend.str[i+1] <- style("#", fg=col[i])
    } else if(is.factor(x)) {
        if(missing(col)){
            if(is.ordered(x)){
                col <- col.f(xterm.pal("long")[[1]])
                col <- col[round(seq(1,25, length=length(levels(x))))]
            } else {
                col <- xterm.pal("Set3")[[1]]
                col <- col[(1:length(levels(x))-1) %% length(col) + 1]
            }
        }
        legend.str <- rep("",length(levels(x)))
        for(i in 1:length(levels(x))) legend.str[i] <- style(levels(x)[i], fg=col[i])
        x <- as.integer(x)
    } else
        stop("Datatype not yet supported.")

    if(is.vector(x)){
        n <- length(x)
        n.digits <- ceiling(log10(n+1))
        if(missing(width)){
            terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80L else as.integer(Sys.getenv("COLUMNS"))
            width <- terminal.width - n.digits - 2
            width <- floor(width / 10)*10
        }
        for(i in 1:ceiling(n/width)){
            cat(sprintf(sprintf("%%%ii  ", n.digits), (i-1)*width+1))
            for(j in ((i-1)*width+1):min(n, i*width)){
                cat(style(" ", bg=col[x[j]]))
            }
            cat("\n")
        }
    } else if(is.matrix(x)) {
        n <- nrow(x)
        n.digits <- ceiling(log10(n+1))
        terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80 else as.numeric(Sys.getenv("COLUMNS"))
        chr <- if(ncol(x) > terminal.width/2 - n.digits-2) " " else "  "
        for(i in 1:nrow(x)){
            cat(sprintf(sprintf("%%%ii  ", n.digits), i))
            for(j in 1:ncol(x)){
                cat(style(chr, bg=col[x[i,j]]))
            }
            cat("\n")
        }
    } else
        stop("Datatype not yet supported.")

    cat(sprintf("\n%s%s\n\n", paste(rep(" ", n.digits + 2), collapse=""), paste(legend.str, collapse=" ")))
}


##' Display contents of a list of lists in a summarized tree structure.
##'
##' @param x List to be displayed.
##' @param compact Wheter to display the list tree in compact mode. Optional,
##'   default: 'auto' i.e. adapt to terminal height.
##' @param show.data Whether to show the contents of the list elements or just
##'   the structure. Optional, default: 'auto' i.e. adapt to terminal width.
##' @param depth Maximum number of levels to show.
##' @param indent Internal.
##' @return Nothing
##' @examples
##' # Create a tree structure of lists
##' make.list.tree <- function(boost=2) {
##'     n.children <- round(boost + rexp(1))
##'     if(n.children < 1){
##'         return(rep("data", 1+floor(5*runif(1))))
##'     } else {
##'         ll <- vector("list", n.children)
##'         names(ll) <- paste("node", 1:n.children)
##'         return(lapply(ll, function(x) make.list.tree(boost-1)))
##'     }
##' }
##'
##' # Visualize it!
##' tree.view(make.list.tree())
##' @author Christofer Bäcklin
##' @export
tree.view <- function(x, compact='auto', show.data='auto', depth=Inf, indent=0){
    terminal.lines <- if(is.blank(Sys.getenv("LINES"))) 24L else as.integer(Sys.getenv("LINES"))

    if(compact == 'auto'){
        count.lines <- function(xx){
            if(is.list(xx)){
                if(is.blank(xx)){
                    return(1)
                } else {
                    return(1 + sum(sapply(xx, count.lines)))
                }
            } else return(1)
        }
        compact <- count.lines(x) > terminal.lines
    }

    my.names <- if(is.list(x)){
        if(is.blank(x)){
            character(0)
        } else if(!is.null(names(x))){
            names(x)
        } else {
            1:length(x)
        }
    } else if(isS4(x)){
        slotNames(x)
    } else {
        NULL
    }
    
    if(is.null(my.names)){
        # We're in a leaf, show data
        data.str <- NULL
        try(data.str <- paste(x, collapse=", "), silent=TRUE)
        if(!is.null(data.str) && (show.data == TRUE ||
                (show.data == 'auto' && 
                nchar(data.str) < terminal.lines - indent))){
            if(is.null(x)){ cat(style.auto(NULL, "NULL"), "\n", sep="")
            } else cat(style.auto(x, data.str), "\n", sep="")
        } else {
            cat(style.auto(x, paste(class(x), dimfun(x, use.names=TRUE))), "\n")
        }
    } else {
        if(length(my.names) == 0){
            cat(style.auto(NULL, "%s Empty list\n"))
        } else {
            for(i in 1:length(my.names)){
                if(i == 1 && !compact) cat("\n")
                if(i > 1 || !compact) cat(rep(" ", indent), sep="")
                cat(style.auto(objfun(x, my.names[i]), my.names[i]),
                    style.dim(": "),
                    sep="")
                if(depth > 1){
                    tree.view(objfun(x, my.names[i]), compact, show.data, depth-1,
                        indent + 2 + compact * nchar(my.names[i]))
                } else {
                    cat(style.auto(NULL, "Max depth reached\n"))
                }
            }
        }
    }
}


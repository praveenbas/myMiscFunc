#' multi grep
#'
#' @author praveen Baskran \email{spemannpraveen@gmail.com}
#' @description grep vector with mutliple pattern and returns number of times each pattern match
#' \link{grep} This function uses grep.
#' @param pattern Multiple pattern in c()
#' @param x a vector
#' @return number of matches for each pattern
#' @export
#' @examples
#' x<- c("a","b","a")
#' multi_greplength(pattern=c("a","b"),x=x)

multi_greplength<-function(pattern,x, ...){
    results<-data.frame(pattern=pattern,Nmatch=rep(0,length(pattern)))
    for(i in 1:length(pattern)){
        results$Nmatch[i]<-length(grep(pattern = pattern[i],x = x, ...))

    }
    return(results)
}


#' multiple gsub
#'
#' @author praveen Baskran \email{spemannpraveen@gmail.com}
#' @description multiple gsub function  :: multiple substitution
#' see \link{gsub} for more details
#' @param pattern vector (mutliple/single) of pattern
#' @param replacement vector for replacement [should be same length as pattern]
#' @param x vector to search for
#' @return mutlitple substituted x
#' @export
#' @examples
#'  x<- c("a","b","a","c")
#'  mgsub(pattern=c("a","b"),replacement=c("x","y"),x=x)

mgsub <- function(pattern, replacement, x, ...) {
    if (length(pattern)!=length(replacement)) {
        stop("pattern and replacement do not have the same length.")
    }
    result <- x
    for (i in 1:length(pattern)) {
        result <- gsub(pattern[i], replacement[i], result, ...)
    }
    return(result)
}


#' pkgTest
#'
#' @author praveen Baskran \email{spemannpraveen@gmail.com}
#' @description Test package is installed if not installs it.
#' see \link{install.packages} for more details
#' @param package   Package to be checked
#' @return Checks and install package is not found
#' @export

pkgTest <- function(package){
    if (!suppressWarnings(require(package,character.only = TRUE,quietly = TRUE)))
    {
        message(sprintf("Required package :::: %s :::: is not installed",package))
        message ("Type yes if you want to install [yes/no]:::: ")
        User_check= readLines(con=stdin(),1)

        if(User_check %in% c("y","Y","Yes","yes","YES")){
            #install.packages(package,dep=TRUE)

            if(!require("BiocInstaller",character.only = T)){
                biocLite("BiocInstaller")
                source("https://bioconductor.org/biocLite.R")
                biocLite("BiocInstaller")
            }else{
                biocLite(package)
            }
            # load and check the installed packages
            # incase of install from github, remove the where thing before and including "/"
            if(!require(gsub(pattern = ".*/",replacement = "",package),character.only = TRUE)) {
                stop("package not found")
            }
        }else{
            stop()
        }
    }
}


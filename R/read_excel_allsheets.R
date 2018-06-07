#' function to plot read coverage for  region of intereset
#' @author praveen Baskran \email{spemannpraveen@gmail.com}
#' @usage read_excel_allsheets(file=xlx_filename)
#' @description Reads all sheets from a excel file
#' @description based on \url{http://stackoverflow.com/questions/12945687/how-to-read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frame-e}
#' @param file Path to the excel filename
#' @return returns a data frame with sheets info as comparison column. #### i.e (rbind of all sheeets)
#' @title read_excel_allsheets
#' @import readxl
#' @export


read_excel_allsheets <- function(file) {
    require(readxl)
    require(plyr)
    sheets <- readxl::excel_sheets(file)
    x<- ldply(.data =sheets,function(X){
        print(X)
        xd<-as.data.frame(readxl::read_excel(file, sheet = X))
        xd$Comparison<- X
        return(xd)
    })
    #x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    #names(x) <- sheets
    return(x)
}

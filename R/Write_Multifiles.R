#' Write multiple output file
#'
#' @author praveen Baskran \email{spemannpraveen@gmail.com}
#' @description write multiple output file using split variable
#' see \link{ddply} for more details
#' @param dTable a dataframe
#' @param split_variable colname in dTable to split the table using using values in this column
#' @param outfolder path to output folder otherwise "." or current working folder
#' @param append_text text to add to the file name [default= "significant_differentially_expressed_genes.txt"]
#' @param html_output save file also as sortable html table
#' @param genExcel a logical value. Generate a excel table addtionally. [Default= TRUE]
#' @return write output files.
#' @import plyr
## @import probeAnnotation
#' @import xlsx
#' @export

Write_Multifiles<-function(dTable,split_variable,outfolder=".",append_text="significant_differentially_expressed_genes.txt",html_output=FALSE,weblink_col="gene_id",weblink="",genExcel=T){
    message(sprintf("### All output will be saved in %s folder",outfolder))
    d_ply(dTable,split_variable,function(x){
        outfile= paste(unique(as.vector(x[,split_variable])),append_text,sep="_")
        write.table(x = x,file = paste(outfolder,outfile,sep="/"),quote = F,sep = "\t",row.names = F,col.names = T,dec = ".")
        message(sprintf("###### output of %s comparison is saved in %s ###",unique(as.vector(x[,split_variable])),outfile))
        if(html_output %in% c(TRUE,"T","TRUE")){
            outfile.html=gsub(pattern = "\\.txt",replacement = "\\.html",x = outfile)
            writeProbeDataHTML(x = x, filename = paste(outfolder,outfile.html,sep="/"),title =unique(as.vector(x[,split_variable])),indices = F,weblink_col=weblink_col,weblink=weblink)
        }
        if(genExcel %in% c(TRUE,"T","TRUE")){
            output.excel=gsub(pattern = "\\.txt",replacement = "\\.xls",x = outfile)
            write.xlsx(x = x,file = paste(outfolder,output.excel,sep="/"),sheetName = unique(as.vector(x[,split_variable])),row.names = F,col.names = T)

        }

    })
}




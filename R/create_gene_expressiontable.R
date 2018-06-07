#' function to create exp table for different sample from same species
#' @param path to the common file folder
#' @param  pattern \link{list.files} pattern to get input file name
#' @param header_number number of element to use as column header after spliting the full input filename
#' @param mergeby  merge different files using
#' @param expression_column index of expression value column
#' @param header booliean flag to indicate the expression file has header/not
#' @param skip  number of lines to skip before begining to read data
#' @param header_check checks the header in a looks "yes" dont check
#' @return  A matrix with expression value merged
#' @export
#'
#' @author Praveen Baskaran, \email{spemannpraveen@gmail.com}
#'

create_gene_expressiontable<- function(path,pattern,header_number=0,mergerby="tracking_id",header=T,expression_column=10,skip=0,header_check="yes"){
    filelist<-list.files(path = path,pattern = pattern,all.files = T,full.names = T,recursive = T)
    for(i in 1:length(filelist)) {
        exp_table_temp<- (read.table(filelist[i],header = header,sep = "\t",skip = skip))
        exp_table_temp <-exp_table_temp[,c(grep(pattern= mergerby,colnames(exp_table_temp)),expression_column)]
        exp_table_temp<-exp_table_temp[which(!duplicated(exp_table_temp[,1])),] ## remove duplicate enries.
        exp_table_temp[,2]<-as.numeric(gsub(pattern = ",",replacement = ".",exp_table_temp[,2]))
        head1 <- colnames(exp_table_temp)
        head1[2]<- strsplit(filelist[i], split='/', fixed=T)[[1]][header_number]
        if(header_check !="yes"){
            if(i == 1){
                while(header_check!="yes"){
                    message(sprintf("use of header_number :: %s gives \" %s \" as column header for differnt table",header_number,head1[2]))
                    message("Type \"yes\" if it is ok :::  or retype header_number for the column")
                    header_check<-readLines(con = stdin(),1)
                    if(header_check != "yes") {
                        header_number<- as.numeric(header_check)
                        message(sprintf("since u dont want use this as header column,:::  retrying with new headercolunm %s",header_number))
                        head1[2]<- strsplit(filelist[i], split='/', fixed=T)[[1]][header_number]
                    }
                }
            }
        }
        colnames(exp_table_temp) <- head1
        print(head1[2])

        if(i==1){
            Exp_table_F_temp<-exp_table_temp
        }else{

            Exp_table_F_temp <- merge(Exp_table_F_temp, exp_table_temp, by=mergerby)
        }
    }
    return(Exp_table_F_temp)
}


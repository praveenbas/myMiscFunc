#' function to plot read coverage for  region of intereset
#' @author pbaskaran
#'@usage Chromosomecoverageplot(filelistTable=df,gff=path to file,ylimvalue=c(-100,100),title="")
#'
#' @param filelistTable table of bedtools coverage output files with sample names as seperate column
#' @param gff path to gff file (parsed) , this file has to be parsed to include only gene features within the region of interest
#' @param ylimvalue y axis limit for the plots default c(-100,100)
#' @return plot
#' @title Chromosomecoverageplot
#' @import tidyr
#' @export


Chromosomecoverageplot<-function(filelistTable,gff,ylimvalue=c(-100,100),title=""){
    require("tidyr")


    E<- read.table(gff)
    tmp <- subset(E, E[,3]=="mRNA")

    filelistTable$strand <- "plus"
    filelistTable$strand[grep(pattern = "minus",x = filelistTable$file)] <- "minus"
    filelistTable_temp<-spread(filelistTable,key = strand,value = file)
    sample<-unique(as.vector(filelistTable_temp[,1]))
    filelistTable_temp[,"plus"]<-as.vector(filelistTable_temp[,"plus"])
    filelistTable_temp[,"minus"]<-as.vector(filelistTable_temp[,"minus"])

    A1<- read.table(filelistTable_temp[1,"plus"])[,c(1,2,8)]
    par(mfrow=c(length(sample) +1,1), mar=c(2.5,2.5,0.1,0.1) , cex=0.5, las=1,cex.axis=1)
    plot( A1[,2], A1[,3], type="n", ylab="", ylim=c(0,100), xlab="", yaxt="n", xaxt="n",axes=F,frame.plot=F,cex.main=2)
    title(main=paste(title,"Read coverage plot",sep=" "),line=-2)

    for( i in 1:nrow(tmp) ){
        # lines( c(tmp[i,][4],tmp[i,][5]), c(20,20))
        if( as.character(unlist( tmp[i,][7])) == "+" ){
            arrows( as.numeric(tmp[i,][4]) , 5 , as.numeric(tmp[i,][5]) + 150, 5 , length = 0.1, col="gray")
            text( ((as.numeric(tmp[i,][4]) + as.numeric(tmp[i,][5])) / 2 ), 20 , gsub(pattern = "ID=",replacement = "", as.character(unlist( tmp[i,][9]))),cex=1.4)
            lines( c(as.numeric(tmp[i,][4]),as.numeric(tmp[i,][5])) , c(5,5)  )
        }
        else{
            arrows( as.numeric(tmp[i,][5]) , 20 , as.numeric(tmp[i,][4]) - 150, 20 , length = 0.1, col="gray")
            text( (as.numeric(tmp[i,][4]) + as.numeric(tmp[i,][5])) / 2, 40 , gsub(pattern = "ID=",replacement = "",as.character(unlist( tmp[i,][9]))),cex=1.4)
            lines( c(as.numeric(tmp[i,][4]),as.numeric(tmp[i,][5])) , c(20,20)  )
        }

    }



    tmp <- subset(E, E[,3]=="exon")
    tmp <- tmp[order(tmp$V4),]
    for( i in 1:nrow(tmp) ){

        ybot = 0
        ytop = 10
        if( as.character(unlist( tmp[i,][7])) == "-" ){
            ybot = 15
            ytop = 25
        }
        #print (tmp[i,4])
        #print (tmp[i,5])
        rect( tmp[i,][4],  ybot, tmp[i,][5], ytop, col="white")
    }

    utr<-E[E$V3 %in% c("3'utr","5'utr"),]
    for( i in 1:nrow(tmp) ){

        ybot = 3
        ytop = 8
        rect( utr[i,][4],  ybot, utr[i,][5], ytop, col="gray")
    }

    legend("topright",legend = c("+ strand","- strand"),col = c("blue","red"),border =c("blue","red"),fill = c("blue","red"),cex = 1.5)

    ### coverage plot begins
    ylimvalues<-unname(quantile(ylimvalue))


    for(ii in 1:length(sample)){
        A2<-read.table(filelistTable_temp[ii,"plus"])[,c(1,2,8)]
        A3<- read.table(filelistTable_temp[ii,"minus"])[,c(1,2,8)]
        A3[,3] <- A3[,3] * -1
        plot( A2[,2], A2[,3], type="h", ylab="wild-type coverage", ylim=ylimvalue, xlab="", xaxt="n",col="blue",yaxt="n")
        lines(A3[,2],A3[,3],type = "h",col = "red")
        lines(A3[,2],rep(0,dim(A3)[1]),type="h",col="black")
        axis(side = 2,at = ylimvalues,labels = ylimvalues,lwd = 1)
        text( head(A1[,2],1) +500, ylimvalues[4], paste(sample[ii],"coverage"),cex=1.4)

    }

    axis(side = 1,at = c(seq(head(A1[,2],1),tail(A1[,2],1), by= 1000),tail(A1[,2],1)),labels =  c(seq(head(A1[,2],1),tail(A1[,2],1), by= 1000),tail(A1[,2],1)),lwd = 1)



}






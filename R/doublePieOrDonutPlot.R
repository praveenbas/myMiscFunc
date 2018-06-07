#' Function to create donut plot which is used to create double pie plot
#' @author Copied from internet and modified
#' @description Function to create donut plot
#' @description Run twice to create double layer pie plot
#' @param panle counts data
#' @param pctr # percentage in count [Default = c(.5,.2,.9)]
#' @param legend.label legend label
#' @param cols colors
#' @param outradius outer radius [Default = 1]
#' @param innerradius innerradius, if innerradius==innerradius then no suggest line [Default= .5]
#' @param radius 1-width of the donus [Default =.7]
#' @param add Add dounut to the current plot [Default = F]
#' @param legend  display legend [Default = F]
#' @param pilabels display lables for each section of piece [Default = F]
#' @param legend_offset non-negative number, legend right position control [Default = .25]
#' @param borderlit vector of colors for pie border
#' @param labelcex size for lables
#' @param maintitle title for the plot
#' @import RColorBrewer
#' @examples
#' ### Then get the plot is fairly easy:
#' # INPUT data
#' browsers <- structure(list(browser = structure(c(3L, 3L, 3L, 3L, 2L, 2L,
#'                                                 2L, 1L, 5L, 5L, 4L),
#'                                               .Label = c("Chrome", "Firefox", "MSIE","Opera", "Safari"),class = "factor"),
#'                           version = structure(c(5L,6L, 7L, 8L, 2L, 3L, 4L, 1L, 10L, 11L, 9L),
#'                                               .Label = c("Chrome 10.0", "Firefox 3.5", "Firefox 3.6", "Firefox 4.0", "MSIE 6.0",
#'                                                          "MSIE 7.0","MSIE 8.0", "MSIE 9.0", "Opera 11.x", "Safari 4.0", "Safari 5.0"),
#'                                              class = "factor"),
#'                           share = c(10.85, 7.35, 33.06, 2.81, 1.58,13.12, 5.43, 9.91, 1.42, 4.55, 1.65),
#'                           ymax = c(10.85, 18.2, 51.26,54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08, 91.73),
#'                           ymin = c(0,10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53,90.08)),
#'                      .Names = c("browser", "version", "share", "ymax", "ymin"),
#'                      row.names = c(NA, -11L), class = "data.frame")
#'## data clean
#' browsers=browsers[order(browsers$browser,browsers$share),]
#' arr=aggregate(share~browser,browsers,sum)
#' ### choose your cols
#' mainCol=c("Blue","Green","Purple","Red","Grey")
#' donuts_plot(browsers$share,rep(1,11),browsers$version,
#'            cols=subcolors(browsers,"browser",mainCol),
#'           legend=F,pilabels = T,borderlit = rep(F,4) )
#' donuts_plot(arr$share,rep(1,5),arr$browser,
#'            cols=mainCol,pilabels=F,legend=T,legend_offset=-.02,
#'            outradius = .71,radius = .0,innerradius=.0,add=T,
#'            borderlit = rep(F,4) )
#' ###end of line
#'
#' @export

donuts_plot <- function(
    panel = runif(3), # counts
    pctr = c(.5,.2,.9), # percentage in count
    legend.label='',
    cols = c('chartreuse', 'chocolate','deepskyblue'), # colors
    outradius = 1, # outter radius
    radius = .7,
    add = F,
    innerradius = .5,
    legend = F,
    pilabels=F,
    legend_offset=.25,
    borderlit=c(T,F,T,T),
    labelcex=1,
    maintitle=""
){
    par(new=add)
    if(sum(legend.label=='')>=1) legend.label=paste("Series",1:length(pctr))
    if(pilabels){
        pie(panel, col=cols,border = borderlit[1],labels = legend.label,radius = outradius,cex=labelcex,main = maintitle)
    }else{
    panel = panel/sum(panel)

    pctr2= panel*(1-pctr)
    pctr3 = c(pctr,pctr)
    pctr_indx=2*(1:length(pctr))
    pctr3[pctr_indx]=pctr2
    pctr3[-pctr_indx]=panel*pctr
    cols_fill = c(cols,cols)
    cols_fill[pctr_indx]='white'
    cols_fill[-pctr_indx]=cols
    par(new=TRUE)
    pie(pctr3, col=cols_fill,border = borderlit[2],labels = '',radius = outradius)
    par(new=TRUE)
    pie(panel, col='white',border = borderlit[3],labels = '',radius = radius)
    par(new=TRUE)
    pie(1, col='white',border = borderlit[4],labels = '',radius = innerradius)
    if(legend){
        # par(mar=c(5.2, 4.1, 4.1, 8.2), xpd=TRUE)
        legend("topright",inset=c(-legend_offset,0),legend=legend.label, pch=rep(15,'.',length(pctr)),
               col=cols,bty='n',cex=labelcex)
    }
    par(new=FALSE)
    }
}


#' function to create subclours for each donut
#' create
#' @export

subcolors <- function(.dta,main,mainCol){
    tmp_dta = cbind(.dta,1,'col')
    tmp1 = unique(.dta[[main]])
    for (i in 1:length(tmp1)){
        tmp_dta$"col"[.dta[[main]] == tmp1[i]] = mainCol[i]
    }
    u <- unlist(by(tmp_dta$"1",tmp_dta[[main]],cumsum))
    colnames(tmp_dta)[grep(pattern = main,colnames(tmp_dta) )]<- "Category"
    us<-ddply(.data = tmp_dta,.(Category),summarise,n=nrow(piece) +1)
    n <- dim(.dta)[1]
    subcol=rep(rgb(0,0,0),n);
    uniq_cat<-unique(tmp_dta$Category)
    us<-us[order(us$Category),]
    uniq_cat<-uniq_cat[order(uniq_cat)]

    for(i in 1: length(uniq_cat)){
        pattern<-  unique(as.vector(tmp_dta[tmp_dta$Category %in% uniq_cat[i],"col"]))
        tmp_dta[tmp_dta$Category %in% uniq_cat[i],"col"] <-rev(colorRampPalette(brewer.pal(paste(mainCol[i],"s",sep = ""),n = 9))(us$n[i])[-1])
    }
    return(tmp_dta$col);
}




##### ggpie

#dat = data.frame(count=c(10,20, 30,20,10,10), category=c("A", "B", "C","D","E","F"),Onto=c(rep("CC",2),rep("MF",2),rep("BP",2)))

# Add addition columns, needed for drawing with geom_rect.
#dat$fraction = dat$count / sum(dat$count)
#dat = dat[order(dat$fraction), ]
#dat$ymax = cumsum(dat$fraction)
#dat$ymin = c(0, head(dat$ymax, n=-1))

#dat$Onto<- factor(dat$Onto,levels = unique(dat$Onto))


#dat_onto$fraction = dat_onto$count / sum(dat_onto$count)
#dat = dat[order(dat$fraction), ]
#dat$ymax = cumsum(dat$fraction)
#dat$ymin = c(0, head(dat$ymax, n=-1))

   # Make the plot
   #p1 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    #   geom_rect() +
     #  coord_polar(theta="y") +
      # xlim(c(0, 4)) +
       #theme(panel.grid=element_blank()) +
       #theme(axis.text=element_blank()) +
       #theme(axis.ticks=element_blank()) +
       #annotate("text", x = 0, y = 0, label = "My Ring plot !") +
       #labs(title="")
   #p1
 #dat
#count category fraction ymax ymin
#1    10        A      0.1  0.1  0.0
#3    30        C      0.3  0.4  0.1
#2    60        B      0.6  1.0  0.4

 #ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) + geom_rect()
 #ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) + geom_rect() + coord_polar(theta="y")
 #ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) + geom_rect() + coord_polar(theta="y")  +xlim(c(0, 4))
 #ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) + geom_rect() + coord_polar(theta="y")  +xlim(c(0, 4)) + theme_Publication()
 #ggplot(dat, ) + geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3))
 #ggplot(dat, ) + geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) + coord_polar(theta="y")
 #ggplot(dat, ) + geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) + xlim(c(0, 4))
 #ggplot(dat, ) + geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) + geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=1, xmin=2)) +xlim(c(0, 4))
#
 #dat$gat<- "GeneOntology"
 #dat$gatshare<-1
 ##dat
#

 # ggplot(dat, ) +
 #  geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
 #  ggrepel::geom_text_repel(aes(label=category,x=4.5,y=ymin+0.1))+
 #  geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=2.98, xmin=2)) +
 #  geom_rect(aes(fill=gat,ymax=1,ymin=0,xmax=1.98,xmin=0),show.legend = FALSE)+
 #  xlim(c(0, 4.5)) +coord_polar(theta="y") + theme(panel.grid=element_blank()) +
 #  theme(axis.text=element_blank()) +
 #  theme(axis.ticks=element_blank())
 #

 #dat$gat<- "GeneOntology"
#dat$gatshare<-1
#dat


#ggplot(dat) +
  #geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
 # geom_rect(data = dat_onto,aes(fill=Onto,ymax=ymax, ymin=ymin, xmax=2.99, xmin=1.5))+
 #   xlim(0,4) + coord_polar(theta = "y")






#dat_onto<- data.frame(count=c(10,20, 30,20,10,10),Onto=c(rep("CC",2),rep("MF",2),rep("BP",2)))
#dat_onto<-ddply(dat_onto,.(Onto),function(x){
#  temp<- data.frame(fraction=NA)
#  temp$fraction<-sum(x$count)/sum(dat_onto$count)
#  temp<-temp[order(temp$fraction)]
#  return(temp)
#})

#dat_onto$ymax<-cumsum(dat_onto$fraction)
#dat_onto$ymin<- c(0,head(dat_onto$ymax,n=-1))  #


#ggplot(dat) +
  #geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
 # geom_rect(data = dat_onto,aes(fill=Onto,ymax=ymax, ymin=ymin, xmax=2.99, xmin=1.5))+
 # xlim(0,4) + coord_polar(theta = "y")


#dat = data.frame(count=c(10,20, 30,20,10,10), category=c("A", "B", "C","D","E","F"),Onto=c(rep("CC",2),rep("MF",2),rep("BP",2)))

## sorting under each ontoloy class
## to display large fraction first
#dat<- ddply(dat,.(Onto),function(x)
#    x<-x[order(x$count,decreasing = T),])

#dat$fraction <- dat$count/sum(dat$count)
#dat$ymax<- cumsum(dat$fraction)
#dat$ymin<- c(0,head(dat$ymax,n=-1))

#ggplot(dat) +
 # geom_rect(data = dat_onto,aes(fill=Onto,ymax=ymax, ymin=ymin, xmax=2.99, xmin=1.5),show.legend = F)+
  #geom_text(data = dat_onto,aes(label=Onto,x=2.25,y=(ymin +ymax)/2))+
  #geom_rect(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3),show.legend = F) +
  #geom_point(data = dat,aes(x =4.1,y = (ymin +ymax)/2),color="white") +
  #ggrepel::geom_text_repel(data = dat,aes(label=category,x =4.5,y = (ymin +ymax)/2),
  #                         arrow = arrow(length = unit(0.01, 'npc')))+
  #scale_fill_manual(values = scale_color)+
  ###geom_rect(ymin=0,ymax=1,xmin=0,xmax=1.49,fill="white")+
  #xlim(0,5) +
  #coord_polar(theta = "y") +
  # theme_Publication()  +
  #annotate("text", x = 0, y = 0, label = "Gene Ontology") +
  #theme(axis.title = element_blank(),axis.text = element_blank(),panel.grid = element_blank(),axis.ticks = element_blank())



  #currDT <- read.table("~/Downloads/SO_data", sep = "|", header = TRUE, strip.white = TRUE)

  ##### color schemes
  #dat_onto$color<- c("Red","Blue","Purple")
  #dat_color<-merge(dat,dat_onto[,c(1,5)],by="Onto")#


  #dat_color1<-ddply(dat_color,.(Onto),function(x){
  #  temp<-x
  #  col_temp<-colorRampPalette(brewer.pal(n=8,name = paste(unique(x$color),"s",sep="")))(nrow(x)+1)
  #  temp$color<-col_temp[-1]

   # return(temp)
  #})


#scale_color<-dat_color1$color
#names(scale_color)<-dat_color1$category
 #scale_color

 #dat_onto$color_N<-NA
 #for(i in 1:nrow(dat_onto)){
 #temp_c<- colorRampPalette(brewer.pal(n = 8,name = paste(dat_onto$color[i],"s",sep="")))(8)
 #dat_onto$color_N[i]<-temp_c[6]#

   #}
 #scale_color2<- dat_onto$color_N
 #names(scale_color2)<-dat_onto$Onto
 #scale_color<-c(scale_color,scale_color2)

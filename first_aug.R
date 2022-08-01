


##geom_bin=It requires only 1 numeric variable as input.
#This function automatically cut the variable in bins and count the number of data point per bin.
# to try different bin size using the binwidth argument
#its uses as histogram
#basic with y axis as character and x axis numeric

#requires only 1 numeric variable as input. This function automatically cut the variable in bins and count the number of data point per bin
library(data.table)
library(ggplot2)
fread("/home/pushpa/Downloads/data - Sheet1.csv")->data
data
ggplot(data, aes(x=value))+
    geom_histogram()

###but edit can be)
    geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9)
#######from geom_bar -it needs one numeric and one categorical variable
ggplot(data, aes(x=item, y=value)) +
    geom_bar(stat="identity", fill="#f68060",  width=0.4) +
    theme_bw()

 #if you want reverse the scale then
ggplot(data, aes(x=item, y=value)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=0.4) + theme_bw()+
coord_flip()
#######



##for box plot
######geom_box plot######summarizes the distribution of a continuous variable
#displays its median, its first and third quartiles and its outliers
ggplot(data, aes(x= item, y= value)) +
    geom_boxplot(fill="slateblue", alpha=0.2) +
    xlab("name")+
    theme(legend.position="none")->p
p
#####
p+ stat_summary(fun.y = mean,
        geom = "point",
        size = 3,
        color = "steelblue") +
    theme_classic()

##############
ggplot(data, aes(x= item, y= value, color = item)) +
    geom_boxplot(fill="slateblue", alpha=0.2) +
    xlab("name")+ theme_classic()
data
#########################geom_boxplot(aes(fill = ))
ggplot(data, aes(x= item, y= value)) +
    geom_boxplot(aes(fill= as.character(factor))) +
    theme_classic()
############################


###########geom_line-input a data frame with 2 numeric variables, one displayed on each axis(An ordered numeric variable for the X axis
#Another numeric variable for the Y axis)
xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.table(xValue,yValue)
data.table(name= rep(letters[1:2],each=6),
           yValue=sample(1:30,12),
           x=sample(1:30,12),
           xValue= (1:12))->data
data
ggplot(data, aes(x=xValue, y=yValue)) +
    geom_line( color="#69b3a2", size=2, linetype=1)

#Change line style with arguments like shape, size, color and more(geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2))
                                        #alpha=0.1 for colour thickness
#multipal bar graph
ggplot(data, aes(x=xValue, y=yValue, group=name, colour= name)) +
    geom_line()+
 #geom_hline,Geom_vline

geom_hline(yintercept=11, color="orange", size=1)
geom_vline(xintercept=6, color="black", size=1)
annotate("geom_text", x = 6.5, y = 10, ymin = 7, ymax = 14,colour = "blue", size = 1.5, alpha=0.4)
###geom_text and geom_label
data.frame(x = c(4,7.5),
   y = c(10,15),
   label = c("label1", "label2"))->text

###in your data frame i create but data set had value of x axis or y axis we can use that
ggplot(data, aes(x=xValue, y=yValue)) +
    geom_line()+
geom_text(data=text, aes( x=x, y=y, label=label),                 ,
           color="blue",
           size=7 , angle=45, fontface="bold" )


   geom_label(data=text, aes( x=x, y=y, label=label),                 ,
           color="blue",
           size=7 , angle=45, fontface="bold" )


                                        #facet_wrap and facet_grid
facet_grid(
  rows = NULL,
  cols = NULL,
  scales = "fixed",#vary across rows ("free_x"), columns ("free_y"), or both rows and columns ("free")
  space = "fixed",#"free_y" their height will be proportional to the length of the y scale; if "free_x"
               #their width will be proportional to the length of the x scale; or if "free" both height and width will vary
  shrink = TRUE,#will shrink scales to fit output of statistics, not raw data. If FALSE, will be range of raw data before statistical summary
  labeller = "label_value",
  as.table = TRUE,
  switch = NULL,#labels are displayed on the top and right of the plot


facet_wrap(
  facets,
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  shrink = TRUE,
  labeller = "label_value",
  as.table = TRUE,
  switch = NULL,
  drop = TRUE,
  dir = "h", # direction either horizontal,orv for vertical
  strip.position = "top"  #strip.position = c("top", "bottom", "left", "right")
)


######################### for facet wrap and facet_grid
as.data.table(read_sheet("https://docs.google.com/spreadsheets/d/1c2icQi7nKsXTJ5J1rtKG-K8OCCw9M6FpLlRf60ngQ8I/edit#gid=0",
           sheet="GVA"))->gva
#as.Date((gva$year))->gva$year
gva->gva1
gva1[,c(1:8,10)]->gg
#################################################################################
gg
names(gg)
melt(gg,id=c("year","base_year","constant_current","item"))->gg
gg
ggplot(gg,aes(y=value,x=year,group=constant_current,colour=constant_current))->p
p+theme(axis.text.x = element_text(angle = 90) )->p
p
p+geom_line()+facet_wrap(~variable,scales="free_y")

####################two types
p+geom_line()+facet_grid(rows=vars(variable))+theme_bw()

p+geom_line()+facet_grid(cols=vars(variable), scale="free_y")+theme_bw()


####extra
library(data.table)
           library(ggplot2)
        fread("~/research/data/output_r - export.csv")->exim_value
  exim_value[year>=2000]->exim_value
       exim_value[export_import=="export"&value_quantity=="quantity",]->t
       #make table for export value growth rate
       melt(t, id=c("year","item","export_import","value_quantity"))->t
t[variable!="indias_total_import"&variable!="total_export",]->t
#########################by geom_bar ###################################
          ggplot(t , aes(x= year, y= value,fill=variable))->p
          p+geom_bar(stat="identity",position="stack")->p
         p+theme_linedraw()->p
          p+ylab(label = "Value (in rupees)")+
           theme(axis.text.x= element_text(angle  = 90))+
           scale_y_continuous(labels=scales::comma)->p
p+ylab(label = "Quantity (in thousand)")->p
         p+ggtitle("Quantity of Export of Livestock Product")+
           theme(plot.title = element_text(hjust= 0.5),
                 legend.position="bottom",
                  legend.title=element_blank())->p
           p+scale_fill_brewer(palette="Dark2")->p
  p


###################same graph with geom_line
ggplot(t, aes(x= year, y= value, group = variable, color= variable))->p
   #p+geom_bar(stat="identity",position="stack")
   p+geom_line()+facet_wrap(~variable,scale="free_y")->p
          p+theme_linedraw()->p
          p+ylab(label = "Value (in rupees)")+
            theme(axis.text.x= element_text(angle  = 90))+
            scale_y_continuous(labels=scales::comma)->p
          p+ggtitle("Export and Import of Livestock Product")+
            theme(plot.title = element_text(hjust= 0.5),
                  legend.position="bottom",
                  legend.title=element_blank())->p
            p



############################


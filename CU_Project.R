library(stringr)
library(ggplot2)
library(pacman)
library(dplyr)

ICP <- Inventory_Cost_Projection

#Munchies are same as snacks so replace to combine for analysis
ICP$Category<-replace(ICP$Category,ICP$Category=="Munchies","Snacks")

#Remove rows with NAs in relative columns
ICP<-ICP[complete.cases(ICP[,5:7]),]

View(ICP)
head(ICP)
tail(ICP)
hist(ICP$`Profit per unit`)
ICP[ICP$`Profit per unit`>1,]
ICP[ICP$`Profit per unit`<1,]
ICP[ICP$`Profit per unit`<1 & ICP$`Cost per Unit`>1,]
tail(ICP$`Profit per unit`)
summary(ICP$`Profit per unit`)

#Adding percent column
ICP$Prof_Marg_Percent = ICP$`Profit per unit`/ICP$`Cost per Unit` *100

head(ICP)

#Histogram of Profit margin Percent
hist(ICP$Prof_Marg_Percent,
     main="Profit Margin Percent Histogram",
     xlab="Margin Percent",
     border="black",
     col="light blue",
     breaks=24)
ICP[ICP$Prof_Marg_Percent<100,]
mean(ICP$Prof_Marg_Percent)
summary(ICP$Prof_Marg_Percent)

#Profit margin percent densities

ICP_density <- density(ICP$Prof_Marg_Percent)
plot(ICP_density)
Perc_d1<-density(ICP$Prof_Marg_Percent[ICP$Category=="College Life"])
Perc_d2<-density(ICP$Prof_Marg_Percent[ICP$Category=="Drinks"])
Perc_d3<-density(ICP$Prof_Marg_Percent[ICP$Category=="Electronics"])
Perc_d4<-density(ICP$Prof_Marg_Percent[ICP$Category=="Necessities"])
Perc_d5<-density(ICP$Prof_Marg_Percent[ICP$Category=="Snacks"])

summary(Perc_d1)
plot(Perc_d1,xlim=c(0,600),ylim=c(0,0.015),col="#E53935",xlab="Margin (%)", main = "Profit Margin Percent Density by Category")
par(new=T)
plot(Perc_d2,xlim=c(0,600),ylim=c(0,0.015),col="#0026FF",xlab="", axes=F, main = "")
par(new=T)
plot(Perc_d3,xlim=c(0,600),ylim=c(0,0.015),col="#C0CA33",xlab="", axes=F,main = "")
par(new=T)
plot(Perc_d4,xlim=c(0,600),ylim=c(0,0.015),col="#43A047",xlab="",axes=F, main = "")
par(new=T)
plot(Perc_d5,xlim=c(0,600),ylim=c(0,0.015),col="#FB8C00",xlab="", axes=F,main = "")

legend("topright", legend=c("College Life","Drinks", "Electronics", "Necessities", "Snacks"), fill = c("#E53935","#0026FF","#C0CA33","#43A047","#FB8C00"))

#Color for categories

Man_Fill<-scale_fill_manual("Categories",values = c("College Life" = "#E53935", "Drinks" = "#0026FF", "Electronics" = "#C0CA33", "Necessities" = "#43A047", "Snacks" = "#FB8C00"))


#Anootations for bar chart

PM_Perc_Med_Segment <- annotate("segment",x=0, y=155.55, xend = 6, yend = 155.55, colour="#555555", size=1,linetype="dashed")
PM_Perc_Mean_Segment <- annotate("segment",x=0, y=191.24, xend = 6, yend = 191.24, colour="#555555", size=1, linetype="dashed")


PbC<-aggregate(Prof_Marg_Percent~Category,ICP,mean)


PbC_Col<-ggplot(PbC,aes(Category, x=reorder(Category, -Prof_Marg_Percent),y=Prof_Marg_Percent,fill=Category,label=Category)) + geom_col()

PbC_Final<-PbC_Col + xlab("Category") +
ylab("Average Margin Percent") +
labs(title = "Sqyre",
     subtitle = "Average Profit Margin Percent Analysis by Category",
     caption = "Based on pre-launch calculations") + 
     theme(plot.title = element_text(face = "bold", colour = "#6200EE", size = 30),
           plot.subtitle = element_text(face="bold"))

PbC_Final + geom_text(vjust=0,nudge_y = 5) + 
  Man_Fill + 
  PM_Perc_Med_Segment + 
  PM_Perc_Mean_Segment + 
  annotate("text", x=5,y=165.55, label="Median = 155.55%", colour="#555555") + 
  annotate("text", x=5,y=201.24, label= "Mean = 191.24%", colour="#555555")

cor(ICP[c(6,11,12)],ICP[c(6,11,12)])
cov(ICP$




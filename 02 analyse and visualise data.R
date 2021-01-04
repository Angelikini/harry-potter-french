library(ggplot2)
library(tidyverse)
library(zoo)


dataav <-   data %>% 
  count(pagenr)
dataav$rolmean<- rollmean(dataav$n, 10, na.pad=TRUE)

g<-ggplot()
g+geom_bar(data=data,aes(x=pagenr,fill=..count..),stat="count",na.rm = TRUE)+
  scale_fill_gradient(low = "firebrick1", high = "firebrick4")+
  ggtitle(label="Harry Potter a l'Ecole des Sorciers", subtitle="New words per page")+
  theme_classic()+
  theme(plot.title = element_text(family="Harry P",hjust = 0.5,size=18,colour="goldenrod"),
        plot.subtitle = element_text(family="Harry P", hjust = 0.5,size=20,colour="firebrick4"),
        axis.title.x = element_text(family="Harry P",size=14, colour="firebrick4"),
        axis.text.x = element_text(family="Harry P",size=14, colour="goldenrod"),
        axis.title.y = element_text(family="Harry P",size=14, colour="firebrick4"),
        axis.text.y = element_text(family="Harry P",size=14, colour="goldenrod"),
        axis.line = element_blank(),
        legend.text=element_text(family="Harry P",size=14, colour="goldenrod"),
        legend.title =element_text(family="Harry P",size=14, colour="firebrick4"))+
  labs(x="Page", y="Word count")+
  geom_line(data=dataav, aes(pagenr,rolmean),na.rm = TRUE, colour="goldenrod",size=1.2)+
  ggsave(filename="New words per page in Harry Potter.png",width=10)
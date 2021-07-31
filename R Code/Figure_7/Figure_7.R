library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)


df <- read.table('Hypo_Stackedplot.txt',sep='\t', header=T)
df$id <- factor(df$id, levels=c('SS_Hypothetical','SS_Annotated',	'ACC_Hypothetical','ACC_Annotated','Core_Hypothetical','Core_Annotated'))


df %>% 
ggplot(aes(fill=id, y=Value*100, x=Species, label=Value*100))+
  geom_bar(position='stack', stat='identity')+
  geom_text(size=5,face='bold', position=position_stack(vjust=0.5), color='white')+
  theme_bw()+
  scale_fill_manual(values=c('#88fca6', '#1fd14c', '#ffca69', '#ffa600', '#70b1ff', '#0073ff'))+
  ggtitle("Hypothetical protein in\nPan-genome constitution")+
  ylab("Percent(%)")+
  xlab("")+
  theme(plot.title = element_text(size=14, face='bold', hjust=0.5),
        axis.text.y=element_text(size=9, face='bold'),
        axis.text.x=element_text( size=12, face='bold'),
        axis.title.y=element_text(face='bold', size=17),
        axis.title.x=element_text(face='bold', size=15),
        strip.text = element_text(size=10, face='bold'),
        legend.title = element_text(size=13, face='bold'),
        legend.position='right',
        legend.text=element_text(face='bold')
  )



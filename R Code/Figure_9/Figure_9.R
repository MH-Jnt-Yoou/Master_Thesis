library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(gridExtra)
library(grid)

# Figure 9A--------------------------------------------------------------------------------------------------------------------------------------------------------

vir_df <- read.table('9A_input.txt', sep='\t', header=T)

rate_hypo <- vir_df %>% group_by(Strain, virulence,id) %>% summarize(Number=n()) %>% group_by(Strain) %>% mutate(rate = prop.table(Number)) %>% as.data.frame()
rate_hypo$id <- gsub('S_aureus_AE','aur\nAD',rate_hypo$id)
rate_hypo$id <- gsub('S_epidermidis_AE','epi\nAD',rate_hypo$id)
rate_hypo$id <- gsub('S_aureus_HE','aur\nHE',rate_hypo$id)
rate_hypo$id <- gsub('S_epidermidis_HE','epi\nHE',rate_hypo$id)


rich_hypo <- vir_df %>% group_by(Strain, virulence,id) %>% summarize(Number=n()) 
colnames(rich_hypo) <- c('Strain','Category','id','Number')
rich_hypo$id <- gsub('S_aureus_AE','aur\nAD',rich_hypo$id)
rich_hypo$id <- gsub('S_epidermidis_AE','epi\nAD',rich_hypo$id)
rich_hypo$id <- gsub('S_aureus_HE','aur\nHE',rich_hypo$id)
rich_hypo$id <- gsub('S_epidermidis_HE','epi\nHE',rich_hypo$id)

p1<- rate_hypo %>% 
  ggplot(aes(x=Strain, y=rate))+
  geom_col(aes(fill=virulence))+
  scale_fill_manual(values=c('#34a1eb', '#f52c4a'))+
  facet_grid(~id, scales='free',space='free')+
  ggtitle('Rate of predicted virulent factors\nin hypothetical proteins')+
  ylab('Rate')+
  xlab('')+
  theme(legend.position='left',
        legend.title = element_text(size=10, face='bold'),
        legend.text = element_text(size=9, face='bold'),
        legend.key.size=unit(0.5, 'cm'),
        axis.text.y=element_text(face='bold', size=9),
        axis.text.x=element_text(face='bold', size=10,angle=90),
        plot.title=element_text(hjust=0.5, size=18, face='bold'),
        axis.title=element_text(size=15, face='bold'),
        strip.text=element_text(size=9, face='bold')
  )

p2 <- rich_hypo %>% 
  ggplot(aes(x=Strain, y=Number))+
  geom_col(aes(fill=Category))+
  scale_fill_manual(values=c('#34a1eb', '#f52c4a'))+
  facet_grid(~id, scales='free',space='free')+
  ggtitle('Richness of predicted virulent factors\nin hypothetical proteins')+
  ylab('Richness')+
  theme(legend.position='none',
        legend.title = element_text(size=10, face='bold'),
        legend.text = element_text(size=9, face='bold'),
        legend.key.size=unit(0.5, 'cm'),
        axis.text.y=element_text(face='bold', size=9),
        axis.text.x=element_text(face='bold', size=10,angle=90),
        plot.title=element_text(hjust=0.5, size=18, face='bold'),
        axis.title=element_text(size=15, face='bold'),
        strip.text=element_text(size=9, face='bold')
  )


grid.arrange(p1,p2, nrow=2)


# Figure 9B-------------------------------------------------------------------------------------------------------------------------------------------------------

library(ggpubr)
rich_df <- read.table('9B_input.txt', sep='\t', header=T)

temp_df <- rich_df %>% filter(Category=='Vir') %>% select(c(Strain, Number))
colnames(temp_df) <- c('Strain','Vir')
temp_df2 <- rich_df %>% filter(Category=='NonVir')
temp_df3 <- rich_df %>% filter(Category=='Hypo') %>% select(c(Strain, Number))
colnames(temp_df3) <- c('Strain','Total')

test_df <- merge(temp_df2, temp_df, by='Strain')
test_df <- merge(test_df, temp_df3, by='Strain')


all_trend_Vir <- ggscatter(filter(test_df, id!='epi_HE'), x = "Total", y = "Vir", 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = FALSE,  color='id', palette='jco', shape='id',
                           xlab = "# of Hypothetical protein", ylab = "Score") + stat_cor(aes(color=id), method='pearson', label.x=1000, label.y=c(0.931,0.9345,0.938))

all_trend_NVir <- ggscatter(filter(test_df, id!='epi_HE'), x = "Total", y = "Number", 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = FALSE, color='id', palette='jco', shape='id',
                            xlab = "# of Hypothetical protein", ylab = "Score")+stat_cor(aes(color=id), method='pearson', show.legend=FALSE, label.x=1000)

grid.arrange(all_trend_Vir, all_trend_NVir, top=textGrob("Hypothetical proteins & Virulence in\neach type of strain", gp=gpar(fontsize=16, fontface='bold')))

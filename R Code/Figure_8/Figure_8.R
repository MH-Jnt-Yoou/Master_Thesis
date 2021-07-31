library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(gridExtra)
library(grid)

vir_df <- read.table('Pred_Vir_Score.txt', sep='\t', header=T)

vir_df$id2 <- gsub('aur_AD','aur\nAD',vir_df$id2)
vir_df$id2 <- gsub('epi_AD','epi\nAD',vir_df$id2)
vir_df$id2 <- gsub('aur_HE','aur\nHE',vir_df$id2)
vir_df$id2 <- gsub('epi_HE','epi\nHE',vir_df$id2)


vir_df %>% 
  ggplot(aes(x=Strain, y=score, fill=id2))+
  geom_boxplot()+
  facet_grid(~id2, scale='free_x', space='free')+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  #scale_x_discrete(limits=c('Core','ACC','SS'))+
  theme(
    plot.title = element_text(size=17, face='bold', hjust=0.5),
    axis.text.y=element_text( size=10),
    axis.text.x=element_text(angle=90, size=11),
    axis.title.y=element_text(face='bold', size=13),
    legend.position="none"
  ) +
  ggtitle("Predicted Virulent Factors of Hypothetical proteins\nin strains") +
  xlab("")

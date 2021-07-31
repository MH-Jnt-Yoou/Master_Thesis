library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(viridis)
# Figure 5A-------------------------------------------------------------------------------------------------------------------------------------------------------

df <- read.table('aur_epi_COG_comparison.txt',sep='\t', header=T)
df$id <- factor(df$id,levels=c("epi_AD","epi_HE","aur_AD","aur_HE"))
df$Category <- factor(df$Category,levels=c("Core","ACC","SS"))
df$Group <- factor(df$Group, levels=c("Metabolism", "Cellular processing\nand signaling", "Information storage\nand processing","Mobileome", "Poorly\ncharacterized"))
df$COG <- factor(df$COG, levels=c('E', 	'P', 	'H','G','C', 	'I', 	'F', 	'Q', 	'M', 	'T', 	'V', 	'O', 	'D', 	'U', 	'N','W','Z', 	'J', 	'K', 	'L', 	'X', 	'S', 	'R'))

Group_anno <- df %>% select(c(COG, Group))
df_temp <- df %>%select(-perc) %>%  group_by(id, COG) %>% summarise(total=sum(Count)) %>% mutate(perc=prop.table(total)) %>% as.data.frame() 


p <- df_temp %>% 
  ggplot(aes(x=id, y=perc*100, fill=id))+
  geom_bar(stat='identity')+
  facet_grid(COG~.)+
  theme_bw()+
  scale_fill_manual(values=c('#F2CB05','#0C5CED','#F2E18D', '#0C9EF7'), guide=guide_legend(reverse=TRUE))+
  coord_flip()+
  ggtitle('Overall COG distribution')+
  xlab("")+
  ylab("Percentage (%)")+
  #scale_fill_discrete(guide = guide_legend(reverse=TRUE))+
  theme(plot.title = element_text(size=14, face='bold', hjust=0.5),
        axis.text.y=element_text(size=9, face='bold'),
        axis.text.x=element_text( size=12, face='bold', angle=90),
        axis.title.y=element_text(face='bold', size=17),
        axis.title.x=element_text(face='bold', size=15),
        strip.text = element_text(size=10, face='bold'),
        legend.title = element_text(size=13, face='bold'),
        legend.position='null',
        legend.text=element_text(face='bold')
  )



# convert to grob
gp <- ggplotGrob(p) # where p is the original ggplot object

# assign the first 4 right-side facet strips with blue fill
for(i in 1:8){
  grob.i <- grep("strip-r", gp$layout$name)[i]
  gp$grobs[[grob.i]]$grobs[[1]]$children[[1]]$gp$fill <- "#c3c8ef"
}
# assign the next 4 right-side facet strips with red fill
for(i in 9:17){
  grob.i <- grep("strip-r", gp$layout$name)[i]
  gp$grobs[[grob.i]]$grobs[[1]]$children[[1]]$gp$fill <- "#d7e3cf"
}
for(i in 18:20){
  grob.i <- grep("strip-r", gp$layout$name)[i]
  gp$grobs[[grob.i]]$grobs[[1]]$children[[1]]$gp$fill <- "#f6f4bb"
}
for(i in 21:21){
  grob.i <- grep("strip-r", gp$layout$name)[i]
  gp$grobs[[grob.i]]$grobs[[1]]$children[[1]]$gp$fill <- "#c6bff3"
}
for(i in 22:23){
  grob.i <- grep("strip-r", gp$layout$name)[i]
  gp$grobs[[grob.i]]$grobs[[1]]$children[[1]]$gp$fill <- "#d3f8b9"
}

grid::grid.draw(gp)

# Figure 5B-------------------------------------------------------------------------------------------------------------------------------------------------------

cog_heatmap_df <- read.table('COG_comparison_result.txt', sep='\t', header=T, check.names = FALSE, row.names=2)


cog_group <- cog_heatmap_df %>% select(Category)


group_color <- c('#3042B3', '#5F7D4A', '#E3DD2B','#8C7EE6','#52B010')
names(group_color) <- c('Metabolism', 	'Cellular processing and signaling','Information storage and processing', 	 	'Mobileome', 	'Poorly characterized')

ann_clr <- list(Category=group_color)

cog_heatmap_df <- cog_heatmap_df %>% select(-Category)

pheatmap(cog_heatmap_df, cluster_cols = FALSE, cluster_rows = FALSE, fontsize_col = 10, show_rownames = TRUE, show_colnames=FALSE , annotation_row = cog_group, annotation_colors = ann_clr, color=inferno(10), fontsize_row = 16)

# Figure 5C-------------------------------------------------------------------------------------------------------------------------------------------------------
df$COG <- factor(df$COG, levels=c('E', 	'P', 	'H','G','C', 	'I', 	'F', 	'Q', 	'M', 	'D', 	'V', 	'O', 	'T', 	'U', 	'N','W','Z', 	'L', 	'K', 	'J', 	'X', 	'S', 	'R'))
df_detail <- df %>% group_by(id, COG) %>% summarize(per=prop.table(Count), groups=Category) %>% as.data.frame() 
df_temp <- df %>%select(-perc) %>%  group_by(id, COG) %>% summarise(total=sum(Count)) %>% mutate(perc=prop.table(total)) %>% as.data.frame() 

PCDTLJ_Overall <- df_temp %>% 
  filter(c(COG=='P' | COG=='C' | COG=='D' | COG=='T' | COG=='L' | COG=='J')) %>% 
  ggplot(aes(x=id, y=perc*100, fill=id))+
  geom_bar(stat='identity')+
  facet_grid(COG~.)+
  theme_bw()+
  #ggtitle('COG: Metabolism')+
  xlab("")+
  ylab("Percentage")+
  theme(plot.title = element_text(size=14, face='bold', hjust=0.5),
        axis.text.x=element_text( size=12, face='bold', angle=90),
        axis.title.y=element_text(face='bold', size=14),
        strip.text = element_text(size=10, face='bold'),
        legend.title = element_text(size=13, face='bold'),
        legend.position='null'
  )


PCDTLJ_Pan <- df_detail %>% 
  filter((COG=='P' | COG=='C' | COG=='D' | COG=='T' | COG=='L' | COG=='J')) %>% 
  ggplot(aes(x=id, y=per*100, fill=id))+
  geom_bar(stat='identity')+
  facet_grid(COG~groups)+
  theme_bw()+
  ggtitle('')+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
        axis.text.x=element_text( size=12, face='bold', angle=90),
        axis.text.y=element_text(size=8, face='bold'),
        axis.title.y=element_text(face='bold', size=14),
        strip.text = element_text(size=10, face='bold'),
        legend.position = 'none'
  )


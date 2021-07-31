library(pheatmap)
library(dplyr)
library(base)
library(ggplot2)

dat <- read.table('OverallVF_Heatmap.txt',sep='\t',header=T, check.names = FALSE, row.names=1)
anno_dat <- read.table('Strain_Metadata.txt',sep='\t', header=T, row.names = 1) %>% select(-c(ID, ID2))

species_color <- c('#4472C4','#ED7D31')
names(species_color) <- c('S.aureus','S.epidermidis')

stated_color <- c('#FFC000', '#70AD47')
names(stated_color) <- c('AD','HE')

patient_color <- c('#19E61A', '#B4E4BE', '#E08379', '#57AD4C', '#7A3A11', '#F0E640', '#8D99E6','#749499','#347BE6', '#ebb238', '#67E698','#E6C25C','#9CABE6','#E64E45','#20D3E6','#E646D2', '#F9EBDE', '#815854', '#A4193D', '#1AAFBC', '#6A7BA2')
names(patient_color) <- c('9030', 	'122_OLD', 	'80', 	'119', 	'9008', 	'9014', 	'9024', 	'9055', 	'31', 	'150', 	'291', 	'E39/21', 	'162', 	'180', 	'83', 	'49', 'Reference', '9036', '122', '159', '161')

site_color <- c('#3042B3', '#5F7D4A', '#E3DD2B','#8C7EE6','#52B010')
names(site_color) <- c('Skin', 	'Nose', 	'Skin_ctrl_AC', 	'Skin_lesional_hand', 	'Unknown')

ann_clr <- list(Site=site_color, Health=stated_color, Patient_ID=patient_color, Species=species_color)


pheatmap(dat, cluster_cols = TRUE, cluster_rows = TRUE, fontsize_col = 13, show_rownames = FALSE, annotation_col=anno_dat, annotation_colors = ann_clr, annotation_legend=TRUE)



# Figure 6B--------------------------------------------------------------------------------------------------------------------------------------------------------
df <- read.table('Significant_VF.txt', sep='\t', header=T)

ggplot(df, aes(x=Comparison, y=VF_Gene))+
  geom_point(aes(colour=exp_value, size=-log10(p_value)))+
  scale_colour_gradientn(colours=c('green','black','red'), values=c(0,1,2), breaks=c(0,1,2), rescaler=function(x,...) x, limit=c(0,2), trans='identity', na.value='red')+
  theme(axis.text.x=element_text(angle=90, size=11, hjust=1, vjust=1),
        axis.title.x=element_blank())

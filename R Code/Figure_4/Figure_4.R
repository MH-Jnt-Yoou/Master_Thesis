library(pheatmap)
library(dplyr)
# EGs in Overall (Figure 4A)-----------------------------------------------------------------------------------------------------------------------------
df <- read.table("OverallDEG_input.txt", sep='\t', header=T, row.names=1, check.names = FALSE)

anno_deg <- select(df, c(Location, Presence))
anno_strain <- read.table('Strain_Metadata.txt',sep='\t', header=T, check.names=FALSE)

row.names(anno_strain) <- anno_strain$Strain
anno_strain <- select(anno_strain,-c(Strain, Site, Patient_ID))

stated_color <- c('#FFC000', '#70AD47')
names(stated_color) <- c('AD','HE')

Location_color <- c('#eb4034', '#57AD4C')
names(Location_color) <- c('Plasmid', 	'Genome')

Presence_color <- c('#FFD662','#00539C')
names(Presence_color) <- c('Either','Both')

ann_clr <- list(Location=Location_color, Health=stated_color, Presence=Presence_color)


pheatmap(select(df,-c(id2,Location, Presence)), cluster_cols = TRUE, cluster_rows = FALSE, fontsize_col = 11, show_rownames = FALSE, annotation_col = anno_strain, annotation_row=anno_deg, annotation_colors = ann_clr)



# EGs only in Plasmid (Figure 4B)-----------------------------------------------------------------------------------------------------------------------------
dat_plasmid <- read.table('Plasmid_DEG.txt', sep='\t', header=T, check.names = FALSE, row.names=1)

anno_dat <- read.table('Strain_Metadata.txt',sep='\t', header=T, check.names=FALSE)
row.names(anno_dat) <- anno_dat$Strain
anno_dat <- select(anno_dat,-Strain)

stated_color <- c('#FFC000', '#70AD47')
names(stated_color) <- c('AD','HE')

patient_color <- c('#19E61A', '#B4E4BE', '#E08379', '#57AD4C', '#7A3A11', '#F0E640', '#8D99E6','#749499','#347BE6', '#ebb238', '#67E698','#E6C25C','#9CABE6','#E64E45','#20D3E6','#E646D2')
names(patient_color) <- c('9030', 	'122_OLD', 	'80', 	'119', 	'9008', 	'9014', 	'9024', 	'9055', 	'31', 	'150', 	'291', 	'E39/21', 	'162', 	'180', 	'83', 	'49')

site_color <- c('#3042B3', '#5F7D4A', '#E3DD2B','#8C7EE6','#52B010')
names(site_color) <- c('Skin', 	'Nose', 	'Skin_ctrl_AC', 	'Skin_lesional_hand', 	'Unknown')

ann_clr <- list(Site=site_color, Health=stated_color, Patient_ID=patient_color)

pheatmap(dat_plasmid, cluster_cols = TRUE, cluster_rows = TRUE, show_rownames = FALSE, fontsize_row=12, annotation_col=anno_dat, annotation_colors = ann_clr)

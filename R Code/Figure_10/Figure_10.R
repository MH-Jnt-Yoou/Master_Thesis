library(RColorBrewer)
library(pheatmap)
library(dplyr)
library(ggplot2)

# Api 50 & Staph 32 select--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

dat <- read.delim('api50.txt', sep='\t', header=TRUE, check.names = FALSE) 
dat <- read.delim('staph32.txt', sep='\t', header=TRUE, check.names = FALSE) 


anno <- select(dat, c('Status', 'id','Site','Species', 'Strain', 'Patient_ID')) %>% unique()
row.names(anno) <- anno$id
anno <- select(anno, -c(id,Strain))

Species_color <- c('#4472C4','#ED7D31')
names(Species_color) <- c('S.aureus','S.epidermidis')

Site_color <- c('#BAF03D', '#F5BC47', '#DE5D4B', '#B747F5', '#4490EB', '#5FEBBD')
names(Site_color) <- c('Nose', 	'Unknown', 	'Skin lesion', 	'Skin control AC', 	'Skin lesional hand', 	'Skin')

Status_color <- c('#FFC000', '#70AD47', '#292826')
names(Status_color) <- c('AD','HE', 'Unknown')

patient_color <- rainbow(22)
names(patient_color) <- anno$Patient_ID %>% unique

ann_clr <- list(Status = Status_color, Patient_ID = patient_color, Species=Species_color, Site=Site_color)

# Api50 for 48hr and Staph32 for 48hr filtering--------------------------------------------------------------------------------------------------------------------

main_dat_24_48 <- dat %>% filter(Time=='48h')
strain_24_48 <- main_dat_24_48$Strain
id_24_48 <- main_dat_24_48$id
Time_24_48 <- main_dat_24_48$Time
main_dat_24_48 <- select(main_dat_24_48, -c(Strain, Species, Time, id,Patient_ID, Site, Status)) %>% apply(2,as.numeric) %>% as.data.frame()
row.names(main_dat_24_48) <- id_24_48
main_dat_24_48 <- main_dat_24_48[,which(apply(main_dat_24_48,2,sum)>0)]


pheatmap(main_dat_24_48,
         labels_row = strain_24_48,
         fontsize_row = 12,
         annotation_row = anno,
         annotation_colors = ann_clr, 
         annotation_legend=TRUE,
         color=colorRampPalette(c("#EBEBEB", "#05d5e8", "#0025b8"))(3),
         cluster_row = TRUE,
         cluster_cols = FALSE)


# Staph 32 for 48hr and Staph32 for 24hr filtering-----------------------------------------------------------------------------------------------------------

main_dat_24_48 <- dat %>% filter(Time=='24h')
strain_24_48 <- main_dat_24_48$Strain
id_24_48 <- main_dat_24_48$id
Time_24_48 <- main_dat_24_48$Time
main_dat_24_48 <- select(main_dat_24_48, -c(Strain, Species, Time, id,Patient_ID, Site, Status)) %>% apply(2,as.numeric) %>% as.data.frame()
row.names(main_dat_24_48) <- id_24_48
main_dat_24_48 <- main_dat_24_48[,which(apply(main_dat_24_48,2,sum)>0)]


pheatmap(main_dat_24_48,
         labels_row = strain_24_48,
         fontsize_row = 12,
         annotation_row = anno,
         annotation_colors = ann_clr, 
         annotation_legend=TRUE,
         color=colorRampPalette(c("#EBEBEB", "#05d5e8", "#0025b8"))(3),
         cluster_row = TRUE,
         cluster_cols = FALSE)

library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)
library(grid)

# Average gene count for barplot and statement-----------------------------------------------------------------------------------------------------------------
# Choose species----------------------------------------------------------------------------------------------------------------------------
table_input <- read.csv("aur_gene_presence_absence.csv", sep=",", na.strings=c("","NA")) #S.aureus
table_input <- read.csv("epi_gene_presence_absence.csv", sep=",", na.strings=c("","NA")) # S.epidermidis
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

table_input <- as.data.frame(table_input)

table_values <- within(table_input, rm("Gene","Non.unique.Gene.name","No..isolates","No..sequences","Avg.sequences.per.isolate","Genome.Fragment","Order.within.Fragment","Accessory.Fragment","Accessory.Order.with.Fragment","QC","Min.group.size.nuc","Max.group.size.nuc","Avg.group.size.nuc"))

abscence_presence <- as.matrix(table_values[,-1])
rownames(abscence_presence) <- table_values[,1]
abscence_presence[is.na(abscence_presence)] <- 0
abscence_presence[which(abscence_presence!=0)] <- 1

a_p_matrix <- mapply(abscence_presence, FUN=as.numeric)
a_p_matrix <- matrix(data=a_p_matrix, ncol=length(colnames(abscence_presence)), nrow=length(row.names(abscence_presence)))
row.names(a_p_matrix) <- row.names(abscence_presence)
colnames(a_p_matrix) <- colnames(abscence_presence)



genomes_count <- length(colnames(a_p_matrix))
abscence_presence <- cbind(a_p_matrix, rowSums(a_p_matrix))

summary_table <- matrix(data=NA, nrow=3, ncol=length(colnames(abscence_presence)))
colnames(summary_table) <- colnames(abscence_presence)
rownames(summary_table) <- c("Total_genes","Unique_genes","Core_genes")

summary_table[1,] <- colSums(abscence_presence)
summary_table[2,] <- colSums(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] == 1),])
summary_table[3,] <- colSums(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] >= (genomes_count*0.95)),])
summary_table <- summary_table[,-ncol(summary_table)]
average_table <- data.frame(x=1:6, y=1:6, z=1:6)


average_table[,1] <- c("Total genes analyzed","Orthologous groups","Average gene count","Average core genes","Average unique genes","Total unique genes")
average_table[1,2] <- sum(summary_table[1,])
average_table[2,2] <- length(rownames(abscence_presence))
average_table[3,2] <- median(summary_table[1,])
average_table[4,2] <- length(rownames(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] >= (genomes_count*0.95)),]))
average_table[5,2] <- round(length(rownames(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] == 1),]))/length(colnames(abscence_presence)))
average_table[6,2] <- length(rownames(abscence_presence[which(abscence_presence[,ncol(abscence_presence)] == 1),]))

melt_summary_table <- melt(summary_table)
melt_summary_table <- melt_summary_table[order(melt_summary_table$value),]
melt_summary_table$Var2 <- gsub("X","",melt_summary_table$Var2)

average_table$x <- c('Total genes analyzed','Orthologous groups', 'Average\ngene count', 'Average\ncore genes','Average\nunique genes','Total\nunique genes')

# Stacked barplot-------------------------------------------------------------------------------------------------------------------------------

all_richness <- read.table("all_richness.txt", sep="\t", header=T)
all_richness$Category <- factor(all_richness$Category, levels=c('SS','ACC','Core'))
all_richness$Health <- gsub('AE','AD',all_richness$Health)



#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Draw plot

rich_bar <- all_richness %>% 
#  filter(Species=='epi') %>% 
 filter(Species=='aur') %>%  # S.aureus
  filter(Category=='Core' | Category=='ACC' | Category=='SS') %>% 
  ggplot(aes(x=Strain, y=Number))+
  geom_col(aes(fill=Category))+
  coord_flip()+
  facet_grid(Health~., scales='free',space='free', switch='y')+
  scale_fill_discrete(guide = guide_legend(reverse=TRUE))+
  theme(legend.position='top',
        legend.title = element_text(size=9, face='bold'),
        legend.text = element_text(size=8, face='bold'),
        legend.key.size=unit(0.4, 'cm'),
        axis.text.y=element_text(face='bold', size=10),
        axis.text.x=element_text(face='bold', size=11),
        axis.title=element_text(face='bold', size=14),
        strip.text=element_text(size=9, face='bold')
  )


p1 <- ggplot(data=average_table[-c(1,2,6),], aes(x=x, y=y))+
  geom_bar(stat = 'identity') +
  theme (axis.text.x=element_text(hjust=0.5,vjust=0.3, size=9, face='bold'),
         axis.title.x = element_blank()) +
  geom_text(aes(y = 10, label = paste("n =" ,y),vjust = 1.2), colour = "black", show.legend=FALSE, size=4) +
  ylab("Count")


t1  <- textGrob(paste(c("Total number of genomes:\n",
                        length(colnames(summary_table)),
                        "\n\nNumber of analyzed genes:\n",
                        as.numeric(average_table[1,2]),
                        "\n\nTotal orthologous groups\n",
                        as.numeric(average_table[2,2]),
                        "\n\nTotal unique genes\n",
                        as.numeric(average_table[6,2])), collapse = " ")) 

lay <- rbind(c(1,1,2),
             c(1,1,2),
             c(1,1,3),
             c(1,1,3))


grid.arrange(rich_bar,p1,t1, layout_matrix = lay)

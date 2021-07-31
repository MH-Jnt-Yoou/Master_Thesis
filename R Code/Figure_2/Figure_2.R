library(ggplot2)



# Staphylococcus aureus-----------------------------------------------------------------------------------------------------------------------
conserved = colMeans(read.table("aur_number_of_conserved_genes.Rtab"))
total = colMeans(read.table("aur_number_of_genes_in_pan_genome.Rtab"))

# No. Genomes - No. conserved genes
genes = data.frame( genes_to_genomes = c(conserved,total),
                    genomes = c(c(1:length(conserved)),c(1:length(conserved))),
                    Key = c(rep("Conserved genes",length(conserved)), rep("Total genes",length(total))) )

ggplot(data = genes, aes(x = genomes, y = genes_to_genomes, group = Key, linetype=Key)) +geom_line()+
  theme_classic() +
  ylim(c(1,max(total)))+
  xlim(c(1,length(total)))+
  xlab("No. of genomes") +
  ylab("No. of genes")+ theme_bw(base_size = 16) +  theme(legend.position='right', legend.text = element_text(size=10))



# No. Genomes - No. New genes
unique_genes = colMeans(read.table("number_of_unique_genes.Rtab"))
new_genes = colMeans(read.table("number_of_new_genes.Rtab"))

genes = data.frame( genes_to_genomes = c(unique_genes,new_genes),
                    genomes = c(c(1:length(unique_genes)),c(1:length(unique_genes))),
                    Key = c(rep("Unique genes",length(unique_genes)), rep("New genes",length(new_genes))) )

ggplot(data = genes, aes(x = genomes, y = genes_to_genomes, group = Key, linetype=Key)) +geom_line()+
  theme_classic() +
  ylim(c(1,max(unique_genes)))+
  xlim(c(1,length(unique_genes)))+
  xlab("No. of genomes") +
  ylab("No. of genes")+ theme_bw(base_size = 16) +  theme(legend.position='right', legend.text = element_text(size=10))



# Staphylococcus epidermidis-----------------------------------------------------------------------------------------------------------------------
conserved = colMeans(read.table("epi_number_of_conserved_genes.Rtab"))
total = colMeans(read.table("epi_number_of_genes_in_pan_genome.Rtab"))

# No. Genomes - No. conserved genes
genes = data.frame( genes_to_genomes = c(conserved,total),
                    genomes = c(c(1:length(conserved)),c(1:length(conserved))),
                    Key = c(rep("Conserved genes",length(conserved)), rep("Total genes",length(total))) )

ggplot(data = genes, aes(x = genomes, y = genes_to_genomes, group = Key, linetype=Key)) +geom_line()+
  theme_classic() +
  ylim(c(1,max(total)))+
  xlim(c(1,length(total)))+
  xlab("No. of genomes") +
  ylab("No. of genes")+ theme_bw(base_size = 16) +  theme(legend.position='right', legend.text = element_text(size=10))



# No. Genomes - No. New genes
unique_genes = colMeans(read.table("number_of_unique_genes.Rtab"))
new_genes = colMeans(read.table("number_of_new_genes.Rtab"))

genes = data.frame( genes_to_genomes = c(unique_genes,new_genes),
                    genomes = c(c(1:length(unique_genes)),c(1:length(unique_genes))),
                    Key = c(rep("Unique genes",length(unique_genes)), rep("New genes",length(new_genes))) )

ggplot(data = genes, aes(x = genomes, y = genes_to_genomes, group = Key, linetype=Key)) +geom_line()+
  theme_classic() +
  ylim(c(1,max(unique_genes)))+
  xlim(c(1,length(unique_genes)))+
  xlab("No. of genomes") +
  ylab("No. of genes")+ theme_bw(base_size = 16) +  theme(legend.position='right', legend.text = element_text(size=10))


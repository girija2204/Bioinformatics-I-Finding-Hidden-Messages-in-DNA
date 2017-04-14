fc_res <- read.delim("~/Downloads/MTech Thesis/crispr/GE-CRISPR/My Workspace/R/updated_dataset.csv",sep = ",",
                     col.names = c("Sl_No","X30_mer","Gene","Percent_Peptide","Amin_Acid_Cut_Position",
                                   "Score_drug_gene_rank","score_drug_gene_threshold","drug","predictions",
                                   "class"),header = FALSE)
X30_mer <- fc_res[,2]
X30_mer = as.data.frame(X30_mer)
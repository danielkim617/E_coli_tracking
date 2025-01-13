source(file.path(here::here(),"0-config.R"))
# Load data
straingst.table = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_network_re.tsv"), sep = "\t", header = T)
#cleaning data
straingst.table[grepl("A|O|C",straingst.table$sample_type),"host"] = "human"
straingst.table$strain = gsub(".fa.gz", "",straingst.table$strain)
straingst.table$sample = gsub("_straingst","",straingst.table$sample)
#Add sample without any E.coli strain detected
straingst.table[801:804,1] = c("S1051", "W1068", "W1103", "W1108")
straingst.table[801:804,2] = c("S1", "W1", "W1", "W1")
straingst.table[801:804,3] = c("household soil", "stored water", "stored water", "stored water")
straingst.table[801:804,4] = substr(straingst.table[801:804,1],3,5)

#Add number of colonies picked for each of samples
actual.numbers = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/data/number_of_actual_strains.txt"),sep = "\t", header = T)
actual.numbers = actual.numbers[actual.numbers$sampleid != "S1011",] #Remove S1011 as no sequence file exist for this sample
#Add village info
#Add subcounty
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$hh_id_3dig = substr(hh_survey$hh_id_5dig,3,5)
Kib.hh = hh_survey[hh_survey$subcountyid == "Kibera", "hh_id_3dig"]
Kib.hh = Kib.hh[!is.na(Kib.hh)] # Kib hh list
Dag.hh = hh_survey[hh_survey$subcountyid == "Dagoretti South", "hh_id_3dig"]
Dag.hh = Dag.hh[!is.na(Dag.hh)] # Dag hh list

straingst.table[straingst.table$house.hold %in% Dag.hh, "vil"] = "Dagoretti South"
straingst.table[straingst.table$house.hold %in% Kib.hh, "vil"] = "Kibera"

straingst.table = merge(straingst.table, actual.numbers, by.x = "sample", by.y = "sampleid", all.y = T)

#Add phylogroups to the identified strains
phylogroups = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/straingst_ref_phylogroups_ext.tsv"), sep = "\t")
phylogroups$V1 = gsub(".fa", "", phylogroups$V1) # remove .fa in the names
#Update phylogroup of Shigella spp.
phylogroups[grepl("Shig_dysenteriae",phylogroups$V1), "V2"] = "S. dysenteriae"
phylogroups[grepl("Shig_flexneri",phylogroups$V1), "V2"] = "S. flexneri"
phylogroups[grepl("Shig_sp.",phylogroups$V1), "V2"] = "Shigella sp."	

straingst.table = merge(straingst.table, phylogroups, by.x = "strain", by.y = "V1", all = T)
straingst.table$phylotype = straingst.table$V2
##For tree labels on iTOL 
phylogroups.tree = straingst.table[!is.na(straingst.table$strain),c("strain", "V2")]

phylogroups.tree[phylogroups.tree$V2 == "albertii", "Color"] = "#ff0101"
phylogroups.tree[phylogroups.tree$V2 == "A", "Color"] = "#ff0606"
phylogroups.tree[phylogroups.tree$V2 == "B1", "Color"] = "#0039ff"
phylogroups.tree[phylogroups.tree$V2 == "B2", "Color"] = "#2fd24e"
phylogroups.tree[phylogroups.tree$V2 == "D", "Color"] = "#db00a2"
phylogroups.tree[phylogroups.tree$V2 == "E", "Color"] = "#08e5ed"
phylogroups.tree[phylogroups.tree$V2 == "G", "Color"] = "#ff7e00"
phylogroups.tree[phylogroups.tree$V2 == "F", "Color"] = "#ffe100"
phylogroups.tree[phylogroups.tree$V2 == "cladeI", "Color"] = "#0b6d10"
phylogroups.tree[phylogroups.tree$V2 == "C", "Color"] = "#9d4800"
phylogroups.tree[phylogroups.tree$V2 == "Unknown", "Color"] = "#ff7e38"
phylogroups.tree[phylogroups.tree$V2 == "fergusonii", "Color"] = "#ffe5ed"
phylogroups.tree[phylogroups.tree$V2 == "S. dysenteriae", "Color"] = "#00e5ff"
phylogroups.tree[phylogroups.tree$V2 == "S. flexneri", "Color"] = "#2f7e00"
phylogroups.tree[phylogroups.tree$V2 == "Shigella sp.", "Color"] = "#582a29"

write.table(phylogroups.tree, "/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/phylogroups.tree.tsv", sep = "\t",row.names = F, quote = F)

# Adding MLST information
mlst = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/MLST/ref_MLST_ecoli_achtman_4.tsv")
mlst = mlst[,1:3]
colnames(mlst) = c("strain", "scheme", "MLST")
mlst$strain = basename(mlst$strain) %>% gsub(".fa", "", .)

# Ref strain information with MLST
phylogroups.tree1 = straingst.table[!is.na(straingst.table$strain),c("strain", "sample","sample_type","host","V2")]
phylogroups.tree.mlst = merge(phylogroups.tree1, mlst, by = "strain")

ST_freq = phylogroups.tree.mlst %>% select(MLST,host) %>% table %>% as.data.frame.matrix()
ST_freq$MLST = rownames(ST_freq)
ST_freq = ST_freq[, c("MLST", "human", "poultry", "dog", "stored water", "household soil")]

ST_freq$MLST <- factor(ST_freq$MLST, levels = sort(unique(ST_freq$MLST)))

# Change into percentage values
ST_freq$human_pct = ST_freq$human/132*100
ST_freq$poultry_pct = ST_freq$poultry/111*100
ST_freq$dog_pct = ST_freq$dog/17*100
ST_freq$water_pct = ST_freq$`stored water`/49*100
ST_freq$soil_pct = ST_freq$`household soil`/39*100

library(pheatmap)
library(viridis)

ST_freq.m = ST_freq[,c(7:11)]

pheatmap(
  ST_freq.m,
  cluster_rows = TRUE,  # Enable clustering for rows
  cluster_cols = TRUE,  # Enable clustering for columns
  color = colorRampPalette(c("#FCF9CE", "#3C84C5","#3955A4"))(100),  # Color gradient
  main = "Heatmap with Hierarchical Clustering",
  fontsize_row = 10,
  fontsize_col = 10,
  na_col = "grey" 
)





source(file.path(here::here(),"0-config.R"))
# #VF analysis
# EPEC: eae,bfp
# EAEC: agg, aat, or aai
# DAEC: afa or dra
# EIEC: ipa
# ETEC: elt or est
# EHEC: stx

#Load data
#Load VF annotation (take out "#" in the first line of the original file)
all_sample_VF = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_VF_results.csv"), sep = "\t", header = T)
all_sample_VF = all_sample_VF[all_sample_VF$SEQUENCE != "SEQUENCE",] # remove headers between rows which came when concatenating outputs
all_sample_VF = all_sample_VF[,2:ncol(all_sample_VF)-1]
all_sample_VF$sample = substr(all_sample_VF$SEQUENCE, 1, 5)
all_sample_VF$ref = gsub("_NODE.*", "", all_sample_VF$SEQUENCE) %>% substr(.,7,length(.))

path.freq = data.frame(matrix(ncol=9))
colnames(path.freq) = c("pathotype", "human", "adult","older_child","child","poultry", "dog", "water", "soil")
n=1
#To take Shigella to consideration as well.
straingst.table = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_network_re.tsv") , sep = "\t", header = T, colClasses = "character")
#cleaning data
straingst.table[grepl("A|O|C",straingst.table$sample_type),"host"] = "human"
straingst.table$strain = gsub(".fa.gz", "",straingst.table$strain)
straingst.table$sample = gsub("_straingst","",straingst.table$sample)
#Add sample with no E.coli strain detected
straingst.table[nrow(straingst.table)+1:nrow(straingst.table)+4,1] = c("S1051", "W1068", "W1103", "W1108")
straingst.table[nrow(straingst.table)+1:nrow(straingst.table)+4,2] = c("S1", "W1", "W1", "W1")
straingst.table[nrow(straingst.table)+1:nrow(straingst.table)+4,3] = c("household soil", "stored water", "stored water", "stored water")
straingst.table[nrow(straingst.table)+1:nrow(straingst.table)+4,4] = substr(straingst.table[nrow(straingst.table)+1:nrow(straingst.table)+4,1],3,5)

for (i in c("EPEC", "EAEC", "DAEC", "EIEC", "ETEC", "EHEC", "Shigella")) {
  if(i == "EPEC"){
    sam.list = all_sample_VF[grepl("eae|bfp",all_sample_VF$GENE), "sample"] %>% unique()
    
  }
  if(i == "EAEC"){
    sam.list = all_sample_VF[grepl("agg|aat|aai",all_sample_VF$GENE), "sample"] %>% unique()
  }
  if(i == "DAEC"){
    sam.list = all_sample_VF[grepl("afa|drg",all_sample_VF$GENE), "sample"] %>% unique()
  }
  if(i == "EIEC"){
    sam.list = all_sample_VF[grepl("ipa",all_sample_VF$GENE), "sample"] %>% unique()
  }
  if(i == "ETEC"){
    sam.list = all_sample_VF[grepl("elt|est",all_sample_VF$GENE), "sample"] %>% unique()
  }
  if(i == "EHEC"){
    sam.list = all_sample_VF[grepl("stx",all_sample_VF$GENE), "sample"] %>% unique()
  }
  if(i == "Shigella"){
    sam.list = straingst.table[grepl("Shig", straingst.table$strain), "sample"] %>% unique()
  }
  path.freq[n,1] = i
  path.freq[n,2] = sam.list[grepl("A|O|C",sam.list)] %>% length()
  path.freq[n,3] = sam.list[grepl("A",sam.list)] %>% length()
  path.freq[n,4] = sam.list[grepl("O",sam.list)] %>% length()
  path.freq[n,5] = sam.list[grepl("C",sam.list)] %>% length()
  path.freq[n,6] = sam.list[grepl("P",sam.list)] %>% length()
  path.freq[n,7] = sam.list[grepl("D",sam.list)] %>% length()
  path.freq[n,8] = sam.list[grepl("W",sam.list)] %>% length()
  path.freq[n,9] = sam.list[grepl("S",sam.list)] %>% length()
  n=n+1
}

# #humans: 132
# #adults: 46
# #older child: 34
# #child: 52
# #poultry: 111
# #dog: 17
# #water: 22 (3) + 27 (samples with no colony) = 49
# #soil: 39 (1)

path.freq$human_percent = path.freq$human/132*100
path.freq$adult_percent = path.freq$adult/46*100
path.freq$older_child_percent = path.freq$older_child/34*100
path.freq$child_percent = path.freq$child/52*100
path.freq$poultry_percent = path.freq$poultry/111*100
path.freq$dog_percent = path.freq$dog/17*100
path.freq$water_percent = path.freq$water/49*100
path.freq$soil_percent = path.freq$soil/39*100

path.freq.m = melt(path.freq[,c(1,10:17)])
path.freq.m[path.freq.m$value == 0,"value"] = NA
path.freq.m$pathotype = factor(path.freq.m$pathotype, levels = c("EAEC", "EPEC", "ETEC", "EHEC", "DAEC", "EIEC", "Shigella"))
path.freq.m$variable = factor(path.freq.m$variable, levels = c("human_percent", "adult_percent","older_child_percent","child_percent","poultry_percent", "dog_percent", "water_percent", "soil_percent"), labels = c("Human", "Adult","Older child","child","Poultry", "Dog", "Stored water", "Household soil"))
write.table(path.freq.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/path.freq.m.tsv"), quote = F, row.names = F, sep = "\t")
saveRDS(path.freq.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/path.freq.m.tsv"))

#
all_sample_VF[grepl("eae|bfp",all_sample_VF$GENE), "path"] = "EPEC"
all_sample_VF[grepl("agg|aat|aai",all_sample_VF$GENE), "path"] = "EAEC"
all_sample_VF[grepl("afa|drg",all_sample_VF$GENE), "path"] = "DAEC"
all_sample_VF[grepl("ipa",all_sample_VF$GENE), "path"] = "EIEC"
all_sample_VF[grepl("elt|est",all_sample_VF$GENE), "path"] = "ETEC"
all_sample_VF[grepl("stx",all_sample_VF$GENE), "path"] = "EHEC"
all_sample_VF_path = all_sample_VF[!is.na(all_sample_VF$path),]
all_sample_VF_path$hh = substr(all_sample_VF_path$sample,3,5)

# Save into a file
saveRDS(all_sample_VF_path, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_VF_path.rds"))


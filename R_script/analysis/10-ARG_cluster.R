source(file.path(here::here(),"0-config.R"))
#Load data
all_sample_ARG_clean = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean_re.rds"))
#Comparison of ARGs at sequence level (100% identity)
ARG.clstr = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ARG_cluster_1.0.clstr"), sep = "\t", header = F, fill = T)
ARG.clstr = separate(ARG.clstr, col = "V2", c("1st", "2nd"), sep = "\\.\\.\\.")
ARG.clstr = separate(ARG.clstr, col = "1st", c("length", "name"), sep = ', >')
a = rownames(ARG.clstr[ARG.clstr$V1 == 0,]) %>% as.numeric %>% sort()

for (i in 1:length(a)) {
  if(i == length(a)){
    ARG.clstr[a[i]:nrow(ARG.clstr),"cluster"] = ARG.clstr[a[i]-1,"V1"]
  }
  else{
    ARG.clstr[a[i]:(a[i+1]-2),"cluster"] = ARG.clstr[a[i]-1,"V1"]
  }
}
# data cleaning
ARG.clstr = ARG.clstr[!is.na(ARG.clstr$cluster),]
ARG.clstr$cluster = gsub(">","",ARG.clstr$cluster)

#Add cluster to clean ARG table
all_sample_ARG_clean.clstr = merge(all_sample_ARG_clean, ARG.clstr[,c("name", "cluster")], by.x = "cds", by.y = "name")
all_sample_ARG_clean.clstr = all_sample_ARG_clean.clstr[!(all_sample_ARG_clean.clstr$sample %in% c("S1051", "W1068", "W1103", "W1108")),]#Exclude the samples that don't have any E.coli strains identified
#Save into a file
write.csv(all_sample_ARG_clean.clstr, file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean.clstr_re.csv"), row.names = F, quote = F)

#Assignment of mobility based on cluster information
#Cluster mobility table
cluster.dist = all_sample_ARG_clean.clstr[,c("cluster", "mobility")] %>% table %>% as.data.frame()
cluster.dist = dcast(cluster.dist, cluster ~ mobility)

for (i in cluster.dist$cluster %>% unique()) {
  cluster.dist[cluster.dist$cluster == i, "Gene"] = paste(all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$cluster == i,"Gene"] %>% unique(), collapse = ", ")
  cluster.dist[cluster.dist$cluster == i, "Human"] = all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$cluster == i & grepl("A|O|C", all_sample_ARG_clean.clstr$sample), "sample"] %>% unique() %>% length()
  cluster.dist[cluster.dist$cluster == i, "Poultry"] = all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$cluster == i & grepl("P", all_sample_ARG_clean.clstr$sample), "sample"] %>% unique() %>% length()
  cluster.dist[cluster.dist$cluster == i, "Dog"] = all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$cluster == i & grepl("D", all_sample_ARG_clean.clstr$sample), "sample"] %>% unique() %>% length()
  cluster.dist[cluster.dist$cluster == i, "Stored water"] = all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$cluster == i & grepl("W", all_sample_ARG_clean.clstr$sample), "sample"] %>% unique() %>% length()
  cluster.dist[cluster.dist$cluster == i, "Household soil"] = all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$cluster == i & grepl("S", all_sample_ARG_clean.clstr$sample), "sample"] %>% unique() %>% length()
}

# #humans: 132
# #adults: 46
# #older child: 34
# #child: 52
# #poultry: 111
# #dog: 17
# #water: 22 (3) + 27 (samples with no colony) = 49
# #soil: 39 (1)
cluster.dist$Human.pct = cluster.dist$Human/132*100
cluster.dist$Poultry.pct = cluster.dist$Poultry/111*100
cluster.dist$Dog.pct = cluster.dist$Dog/17*100
cluster.dist$`Stored water.pct` = cluster.dist$`Stored water`/19*100 # Among samples with colonies
cluster.dist$`Household soil.pct` = cluster.dist$`Household soil`/38*100

#Using ARG gene annotation of representative sequences of each cluster 
ARG.clstr.rep = ARG.clstr[ARG.clstr$`2nd` == " *",]
all_sample_ARG = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_blastp.csv"), header = F)
rep.clstr.blast =  all_sample_ARG[all_sample_ARG$V1 %in% ARG.clstr.rep$name, ]

#Add annotation
all_sample_ARG_clean.clstr.blast = merge(all_sample_ARG_clean.clstr, rep.clstr.blast[,1:2], by.x = "cds", by.y = "V1", all.x = T)

cluster.dist.ARG = merge(cluster.dist, all_sample_ARG_clean.clstr.blast[!is.na(all_sample_ARG_clean.clstr.blast$V2), c("cluster", "V2")], by = "cluster", all.x = T)
cluster.dist.ARG$V2 = gsub(".*\\|","",cluster.dist.ARG$V2)
cluster.dist.ARG$total = cluster.dist.ARG$mobile + cluster.dist.ARG$`non-mobile` + cluster.dist.ARG$`not known`

#All cluster information (Supplementary table 7)
write.csv(cluster.dist.ARG, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/cluster.dist.ARG_re.csv"), row.names = F, quote = F)

#Number of cluster comparison
set.seed(1992)

num = 10 # Number of samples to subsample
h = replicate(1000,sample(all_sample_ARG_clean.clstr[grepl("A|O|C", all_sample_ARG_clean.clstr$sample),"sample"] %>% unique(), num, replace = F))
p = replicate(1000,sample(all_sample_ARG_clean.clstr[grepl("P", all_sample_ARG_clean.clstr$sample),"sample"] %>% unique(), num, replace = F))
d = replicate(1000,sample(all_sample_ARG_clean.clstr[grepl("D", all_sample_ARG_clean.clstr$sample),"sample"] %>% unique(), num, replace = F))
w = replicate(1000,sample(all_sample_ARG_clean.clstr[grepl("W", all_sample_ARG_clean.clstr$sample),"sample"] %>% unique(), num, replace = F))
s = replicate(1000,sample(all_sample_ARG_clean.clstr[grepl("S", all_sample_ARG_clean.clstr$sample),"sample"] %>% unique(), num, replace = F))

tmp = data.frame(matrix(ncol=1002))

sample_types = c("human", "poultry", "dog", "water", "soil")

# Assuming 'num' is defined somewhere in your code
# Loop through each sample type
for (i in 1:length(sample_types)) {
  tmp[i, 1] = num
  tmp[i, 2] = sample_types[i]
  
  # Select the appropriate sample set based on the sample type
  sample_set = switch(sample_types[i],
                      "human" = h,
                      "poultry" = p,
                      "dog" = d,
                      "water" = w,
                      "soil" = s)
  
  for (j in 1:1000) {
    a = all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$sample %in% sample_set[, j], ]
    tmp[i, j + 2] = length(unique(a$cluster))
  }
}

num.cluster.host = tmp

num.cluster.host.m = num.cluster.host %>% melt(id=c("X1", "X2"))
colnames(num.cluster.host.m) = c("n", "host", "iter", "num.uniq.clusters")
num.cluster.host.m$host = factor(num.cluster.host.m$host, levels = c("human","poultry", "dog", "water", "soil"), labels = c("Human", "Poultry", "Dog", "Stored water", "Household soil"))

# Save into a file
saveRDS(num.cluster.host.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.cluster.host.m.rds"))

#Statistical test
uniq.cluster.host.test = combn(as.character(num.cluster.host.m$host) %>% unique(),2) %>% t() %>% data.frame()

for (i in 1:nrow(uniq.cluster.host.test)) {
  a = num.cluster.host.m[num.cluster.host.m$host == uniq.cluster.host.test[i,1], "num.uniq.clusters"]
  b = num.cluster.host.m[num.cluster.host.m$host == uniq.cluster.host.test[i,2], "num.uniq.clusters"]
  uniq.cluster.host.test$host1_mean[i] = mean(a)
  uniq.cluster.host.test$host2_mean[i] = mean(b)
  c = wilcox.test(a,b,exact = F)
  uniq.cluster.host.test$p.value[i] = c$p.value
}

uniq.cluster.host.test$adj.p = uniq.cluster.host.test$p.value %>% p.adjust(., method = "BH")

# Comparing the average number of ARGs by host type without subsampling
all_sample_ARG_host.tab = all_sample_ARG_clean.clstr %>% select(sample, mobility) %>% table() %>% as.data.frame.matrix()
all_sample_ARG_host.tab$sample = rownames(all_sample_ARG_host.tab)
all_sample_ARG_host.tab$total = all_sample_ARG_host.tab$mobile + all_sample_ARG_host.tab$`non-mobile` + all_sample_ARG_host.tab$`not known`
all_sample_ARG_host.tab[grepl("A|O|C", all_sample_ARG_host.tab$sample), "type"] = "Human"
all_sample_ARG_host.tab[grepl("P", all_sample_ARG_host.tab$sample), "type"] = "Poultry"
all_sample_ARG_host.tab[grepl("D", all_sample_ARG_host.tab$sample), "type"] = "Dog"
all_sample_ARG_host.tab[grepl("W", all_sample_ARG_host.tab$sample), "type"] = "Stored water"
all_sample_ARG_host.tab[grepl("S", all_sample_ARG_host.tab$sample), "type"] = "Household soil"
all_sample_ARG_host.tab$type = factor(all_sample_ARG_host.tab$type, levels = c("Human", "Poultry", "Dog", "Stored water", "Household soil"))

saveRDS(all_sample_ARG_host.tab, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_host.tab.rds"))

#Statistical test
uniq.cluster.host.test.all = combn(as.character(num.cluster.host.m$host) %>% unique(),2) %>% t() %>% data.frame()

for (i in 1:nrow(uniq.cluster.host.test.all)) {
  a = all_sample_ARG_host.tab[all_sample_ARG_host.tab$type == uniq.cluster.host.test.all[i,1], "total"]
  b = all_sample_ARG_host.tab[all_sample_ARG_host.tab$type == uniq.cluster.host.test.all[i,2], "total"]
  uniq.cluster.host.test.all$host1_median[i] = median(a)
  uniq.cluster.host.test.all$host2_median[i] = median(b)
  c = wilcox.test(a,b,exact = F)
  uniq.cluster.host.test.all$p.value[i] = c$p.value
}

uniq.cluster.host.test.all$adj.p = p.adjust(uniq.cluster.host.test.all$p.value, method = "BH")

# Save into a file
write.csv(all_sample_ARG_host.tab, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_host.tab.csv"))



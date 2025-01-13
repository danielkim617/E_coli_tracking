source(file.path(here::here(),"0-config.R"))
#ARG cluster sharing analysis 
all_sample_ARG_clean.clstr = read.csv(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean.clstr_re.csv"), header = T)
#Get sample information
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$hh_id_3dig = substr(hh_survey$hh_id_5dig,3,5)
Kib.hh = hh_survey[hh_survey$subcountyid == "Kibera", "hh_id_3dig"]
Kib.hh = Kib.hh[!is.na(Kib.hh)] # Kib hh list
Dag.hh = hh_survey[hh_survey$subcountyid == "Dagoretti South", "hh_id_3dig"]
Dag.hh = Dag.hh[!is.na(Dag.hh)] # Dag hh list
##Load sample list 
sample.list = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/data/sample_list.txt"), sep = '\t')
colnames(sample.list) = "sample"
sample.list = sample.list[!(sample.list$sample %in% c("S1051", "W1068", "W1103", "W1108")),] #Exclude the samples that don't have any E.coli strains identified

#Create data.frame
ARG.share.table.clstr = combn(sample.list %>% unique, 2) %>% t() %>% as.data.frame()
colnames(ARG.share.table.clstr) = c("sample1", "sample2")
ARG.share.table.clstr$HH1 = substr(ARG.share.table.clstr$sample1, 3, 5)
ARG.share.table.clstr$HH2 = substr(ARG.share.table.clstr$sample2, 3, 5)
ARG.share.table.clstr[ARG.share.table.clstr$HH1 == ARG.share.table.clstr$HH2, "HH_type"] = "Within"
ARG.share.table.clstr[ARG.share.table.clstr$HH1 != ARG.share.table.clstr$HH2, "HH_type"] = "Between"
ARG.share.table.clstr[ARG.share.table.clstr$HH1 %in% Kib.hh, "subcounty1"] = "Kibera"
ARG.share.table.clstr[ARG.share.table.clstr$HH1 %in% Dag.hh, "subcounty1"] = "Dagoretti South"
ARG.share.table.clstr[ARG.share.table.clstr$HH2 %in% Kib.hh, "subcounty2"] = "Kibera"
ARG.share.table.clstr[ARG.share.table.clstr$HH2 %in% Dag.hh, "subcounty2"] = "Dagoretti South"
ARG.share.table.clstr[ARG.share.table.clstr$subcounty1 == ARG.share.table.clstr$subcounty2 & ARG.share.table.clstr$subcounty1 == "Kibera","subcounty_type"] = "Kibera"
ARG.share.table.clstr[ARG.share.table.clstr$subcounty1 == ARG.share.table.clstr$subcounty2 & ARG.share.table.clstr$subcounty1 == "Dagoretti South","subcounty_type"] = "Dagoretti South"
ARG.share.table.clstr[ARG.share.table.clstr$subcounty1 != ARG.share.table.clstr$subcounty2,"subcounty_type"] = "Between subcounties"
ARG.share.table.clstr[grepl("A|O|C",ARG.share.table.clstr$sample1), "host1"] = "Human"
ARG.share.table.clstr[grepl("A|O|C",ARG.share.table.clstr$sample2), "host2"] = "Human"
ARG.share.table.clstr[grepl("P",ARG.share.table.clstr$sample1), "host1"] = "Poultry"
ARG.share.table.clstr[grepl("P",ARG.share.table.clstr$sample2), "host2"] = "Poultry"
ARG.share.table.clstr[grepl("D",ARG.share.table.clstr$sample1), "host1"] = "Dog"
ARG.share.table.clstr[grepl("D",ARG.share.table.clstr$sample2), "host2"] = "Dog"
ARG.share.table.clstr[grepl("W",ARG.share.table.clstr$sample1), "host1"] = "Stored water"
ARG.share.table.clstr[grepl("W",ARG.share.table.clstr$sample2), "host2"] = "Stored water"
ARG.share.table.clstr[grepl("S",ARG.share.table.clstr$sample1), "host1"] = "Household soil"
ARG.share.table.clstr[grepl("S",ARG.share.table.clstr$sample2), "host2"] = "Household soil"
ARG.share.table.clstr[ARG.share.table.clstr$host1 == ARG.share.table.clstr$host2, "host_type"] = "Same"
ARG.share.table.clstr[ARG.share.table.clstr$host1 != ARG.share.table.clstr$host2, "host_type"] = "Different"
ARG.share.table.clstr$cat = paste(ARG.share.table.clstr$host1,"-",ARG.share.table.clstr$host2)
ARG.share.table.clstr[ARG.share.table.clstr$cat == "Dog - Human", "cat"] = "Human - Dog"
ARG.share.table.clstr[ARG.share.table.clstr$cat == "Dog - Poultry", "cat"] = "Poultry - Dog"
ARG.share.table.clstr[ARG.share.table.clstr$cat == "Household soil - Stored water" , "cat"] = "Stored water - Household soil" 
ARG.share.table.clstr$cat = factor(ARG.share.table.clstr$cat, levels = c("Human - Human", "Poultry - Poultry", "Dog - Dog", "Household soil - Household soil", "Stored water - Stored water","Human - Poultry", "Human - Dog", "Human - Stored water", "Human - Household soil", "Poultry - Dog", "Poultry - Stored water", "Poultry - Household soil", "Dog - Stored water", "Dog - Household soil", "Stored water - Household soil"))

for (i in 1:nrow(ARG.share.table.clstr)) {
  #ARG cluster of sample1
  clstr1=all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$sample == ARG.share.table.clstr[i,"sample1"],]
  #ARG cluster of sample2
  clstr2=all_sample_ARG_clean.clstr[all_sample_ARG_clean.clstr$sample == ARG.share.table.clstr[i,"sample2"],]
  #
  clstr1.all = clstr1$cluster %>% unique()
  clstr2.all = clstr2$cluster %>% unique()
  clstr1.non = clstr1[clstr1$mobility == "non-mobile", "cluster"] %>% unique()
  clstr2.non = clstr2[clstr2$mobility == "non-mobile", "cluster"] %>% unique()
  clstr1.mob = clstr1[clstr1$mobility == "mobile", "cluster"] %>% unique()
  clstr2.mob = clstr2[clstr2$mobility == "mobile", "cluster"] %>% unique()
  #
  ARG.share.table.clstr[i,"overall.num1"] = clstr1.all %>% length()
  ARG.share.table.clstr[i,"non.num1"] = clstr1.non %>% length()
  ARG.share.table.clstr[i,"mobile.num1"] = clstr1.mob %>% length()
  ARG.share.table.clstr[i,"overall.num2"] = clstr2.all %>% length()
  ARG.share.table.clstr[i,"non.num2"] = clstr2.non %>% length()
  ARG.share.table.clstr[i,"mobile.num2"] = clstr2.mob %>% length()
  #
  ARG.share.table.clstr[i,"overall.jac.dist"] = 1 - length(intersect(clstr1.all, clstr2.all))/length(unique(c(clstr1.all, clstr2.all)))
  ARG.share.table.clstr[i,"non.jac.dist"] = 1 - length(intersect(clstr1.non, clstr2.non))/length(unique(c(clstr1.non, clstr2.non)))
  ARG.share.table.clstr[i,"mob.jac.dist"] = 1 - length(intersect(clstr1.mob, clstr2.mob))/length(unique(c(clstr1.mob, clstr2.mob)))
  #
  ARG.share.table.clstr[i,"overall.share"] = length(intersect(clstr1.all, clstr2.all))
  ARG.share.table.clstr[i,"non.share"] = length(intersect(clstr1.non, clstr2.non))
  ARG.share.table.clstr[i,"mob.share"] = length(intersect(clstr1.mob, clstr2.mob))
}

ARG.share.table.clstr$overall.jac.sim = 1- ARG.share.table.clstr$overall.jac.dist
ARG.share.table.clstr$non.jac.sim = 1 - ARG.share.table.clstr$non.jac.dist
ARG.share.table.clstr$mob.jac.sim = 1 - ARG.share.table.clstr$mob.jac.dist

ARG.share.table.clstr.m = melt(ARG.share.table.clstr[,c("sample1", "sample2", "HH1", "HH2", "HH_type", "subcounty1","subcounty2", "subcounty_type", "host1", "host2", "host_type","cat", "overall.jac.sim", "non.jac.sim", "mob.jac.sim")])
ARG.share.table.clstr.m$variable = factor(ARG.share.table.clstr.m$variable, levels = c("overall.jac.sim", "non.jac.sim", "mob.jac.sim"))
ARG.share.table.clstr.m$HH_type = factor(ARG.share.table.clstr.m$HH_type, levels = c("Within", "Between"))

# Save into a file
saveRDS(ARG.share.table.clstr, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ARG.share.table.clstr.rds"))
saveRDS(ARG.share.table.clstr.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ARG.share.table.clstr.m.rds"))

#Perform Statistical test on ARG jaccard distant between household types
ARG.clstr.wit.bt.test = data.frame(matrix(ncol = 5))
colnames(ARG.clstr.wit.bt.test) = c("cat", "mobility", "mean_within", "mean_between", "p.value")

n=1
for (i in ARG.share.table.clstr.m[ARG.share.table.clstr.m$HH_type == "Within", "cat"] %>% unique()) {
  for (j in ARG.share.table.clstr.m$variable %>% unique()) {
    a = ARG.share.table.clstr.m[ARG.share.table.clstr.m$HH_type == "Within" & ARG.share.table.clstr.m$cat == i & ARG.share.table.clstr.m$variable == j,"value"]
    b = ARG.share.table.clstr.m[ARG.share.table.clstr.m$HH_type == "Between" & ARG.share.table.clstr.m$cat == i & ARG.share.table.clstr.m$variable == j,"value"]
    c = wilcox.test(a,b, exact = F)
    
    ARG.clstr.wit.bt.test[n,1] = i
    ARG.clstr.wit.bt.test[n,2] = j
    ARG.clstr.wit.bt.test[n,3] = mean(a)
    ARG.clstr.wit.bt.test[n,4] = mean(b)
    ARG.clstr.wit.bt.test[n,5] = c$p.value
    
    n=n+1
  }
}

# Save into a file
write.csv(ARG.clstr.wit.bt.test, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ARG.clstr.wit.bt.test.csv"))
# 

# kmer.stat.clstr = wilcox.bt.cat(ARG.share.table.clstr, "kmer.dist")
ARG.overall.stat.clstr = wilcox.bt.cat(ARG.share.table.clstr, "overall.jac.dist")
ARG.non.stat.clstr = wilcox.bt.cat(ARG.share.table.clstr, dist="non.jac.dist")
ARG.mob.stat.clstr = wilcox.bt.cat(ARG.share.table.clstr, dist="mob.jac.dist")
# #
ARG.overall.stat.clstr$adj.p[ARG.overall.stat.clstr$HH_type == "Within"] = ARG.overall.stat.clstr$p.value[ARG.overall.stat.clstr$HH_type == "Within"] %>% p.adjust(., method = "BH")
ARG.overall.stat.clstr$adj.p[ARG.overall.stat.clstr$HH_type == "Between"] = ARG.overall.stat.clstr$p.value[ARG.overall.stat.clstr$HH_type == "Between"] %>% p.adjust(., method = "BH")

ARG.overall.stat.sig.clstr = ARG.overall.stat.clstr[ARG.overall.stat.clstr$adj.p < 0.05,]
a = ARG.overall.stat.clstr[,c("HH_type", "cat1", "cat1_mean")]
colnames(a) = c("HH_type","cat", "overall_mean")
b = ARG.overall.stat.clstr[,c("HH_type","cat2", "cat2_mean")]
colnames(b) = c("HH_type","cat", "overall_mean")
ARG.overall.stat.rank.clstr = data.frame(rbind(a,b)) %>% unique()
# # 
# #
ARG.non.stat.clstr$adj.p[ARG.non.stat.clstr$HH_type == "Within"] = ARG.non.stat.clstr$p.value[ARG.non.stat.clstr$HH_type == "Within"] %>% p.adjust(., method = "BH")
ARG.non.stat.clstr$adj.p[ARG.non.stat.clstr$HH_type == "Between"] = ARG.non.stat.clstr$p.value[ARG.non.stat.clstr$HH_type == "Between"] %>% p.adjust(., method = "BH")
ARG.non.stat.sig.clstr = ARG.non.stat.clstr[ARG.non.stat.clstr$adj.p < 0.05,]
a = ARG.non.stat.clstr[,c("HH_type", "cat1", "cat1_mean")]
colnames(a) = c("HH_type","cat", "non_mean")
b = ARG.non.stat.clstr[,c("HH_type","cat2", "cat2_mean")]
colnames(b) = c("HH_type","cat", "non_mean")
ARG.non.stat.rank.clstr = data.frame(rbind(a,b)) %>% unique()
# 
# #
ARG.mob.stat.clstr$adj.p[ARG.mob.stat.clstr$HH_type == "Within"] = ARG.mob.stat.clstr$p.value[ARG.mob.stat.clstr$HH_type == "Within"] %>% p.adjust(., method = "BH")
ARG.mob.stat.clstr$adj.p[ARG.mob.stat.clstr$HH_type == "Between"] = ARG.mob.stat.clstr$p.value[ARG.mob.stat.clstr$HH_type == "Between"] %>% p.adjust(., method = "BH")
ARG.mob.stat.sig.clstr = ARG.mob.stat.clstr[ARG.mob.stat.clstr$adj.p < 0.05,]
a = ARG.mob.stat.clstr[,c("HH_type", "cat1", "cat1_mean")]
colnames(a) = c("HH_type","cat", "mob_mean")
b = ARG.mob.stat.clstr[,c("HH_type","cat2", "cat2_mean")]
colnames(b) = c("HH_type","cat", "mob_mean")
ARG.mob.stat.rank.clstr = data.frame(rbind(a,b)) %>% unique()

##Rank based correlation analysis among strain-sharing rates, kmer similarity, overall ARG, non-mobile ARG, mobile ARG composition similarity
#
prevlance.stat5 = readRDS(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/prevlance.stat5_update.rds"))
prevlance.stat6 = prevlance.stat5[prevlance.stat5$subcounty == "Overall",]

#########For the comparison within households
prevlance.stat.wit = prevlance.stat6[prevlance.stat6$HH_type == "Same",]
HH.perm.wit.bt.wit = prevlance.stat.wit[order(prevlance.stat.wit$rate, decreasing = T), c("category", "rate")]
HH.perm.wit.bt.wit$Rank.rates = rank(-HH.perm.wit.bt.wit$rate, ties.method = "min")
HH.perm.wit.bt.wit = HH.perm.wit.bt.wit[!is.na(HH.perm.wit.bt.wit$rate),]
#
#
ARG.overall.stat.rank.wit.clstr = ARG.overall.stat.rank.clstr[ARG.overall.stat.rank.clstr$HH_type == "Within",]
ARG.overall.stat.rank.wit.clstr$Rank.overall.arg = rank(ARG.overall.stat.rank.wit.clstr$overall_mean, ties.method = "min")
#
ARG.non.stat.rank.wit.clstr = ARG.non.stat.rank.clstr[ARG.non.stat.rank.clstr$HH_type == "Within",]
ARG.non.stat.rank.wit.clstr$Rank.non.arg = rank(ARG.non.stat.rank.wit.clstr$non_mean, ties.method = "min")
#
ARG.mob.stat.rank.wit.clstr = ARG.mob.stat.rank.clstr[ARG.mob.stat.rank.clstr$HH_type == "Within",]
ARG.mob.stat.rank.wit.clstr$Rank.mob.arg = rank(ARG.mob.stat.rank.wit.clstr$mob_mean, ties.method = "min")

#For the comparison between households
prevlance.stat.bet = prevlance.stat6[prevlance.stat6$HH_type == "Different",]
HH.perm.wit.bt.bet = prevlance.stat.bet[order(prevlance.stat.bet$rate, decreasing = T), c("category", "rate")]
HH.perm.wit.bt.bet$Rank.rates = rank(-HH.perm.wit.bt.bet$rate, ties.method = "min")
HH.perm.wit.bt.bet = HH.perm.wit.bt.bet[!is.na(HH.perm.wit.bt.bet$rate),]
#
ARG.overall.stat.rank.bet.clstr = ARG.overall.stat.rank.clstr[ARG.overall.stat.rank.clstr$HH_type == "Between",]
ARG.overall.stat.rank.bet.clstr$Rank.overall.arg = rank(ARG.overall.stat.rank.bet.clstr$overall_mean, ties.method = "min")
#
ARG.non.stat.rank.bet.clstr = ARG.non.stat.rank.clstr[ARG.non.stat.rank.clstr$HH_type == "Between",]
ARG.non.stat.rank.bet.clstr$Rank.non.arg = rank(ARG.non.stat.rank.bet.clstr$non_mean, ties.method = "min")
#
ARG.mob.stat.rank.bet.clstr = ARG.mob.stat.rank.clstr[ARG.mob.stat.rank.clstr$HH_type == "Between",]
ARG.mob.stat.rank.bet.clstr$Rank.mob.arg = rank(ARG.mob.stat.rank.bet.clstr$mob_mean, ties.method = "min")
##Figures with the actual values ########################################
wit.rank.clstr.actual = merge(HH.perm.wit.bt.wit[,c("category", "rate")], ARG.overall.stat.rank.wit.clstr[,c("cat","overall_mean")], by.x = "category", by.y="cat")
wit.rank.clstr.actual = merge(wit.rank.clstr.actual, ARG.non.stat.rank.wit.clstr[,c("cat","non_mean")], by.x = "category", by.y="cat")
wit.rank.clstr.actual = merge(wit.rank.clstr.actual, ARG.mob.stat.rank.wit.clstr[,c("cat","mob_mean")], by.x = "category", by.y="cat")
wit.rank.clstr.actual$HH_type = "Within"

bet.rank.clstr.actual = merge(HH.perm.wit.bt.bet[,c("category", "rate")], ARG.overall.stat.rank.bet.clstr[,c("cat","overall_mean")], by.x = "category", by.y="cat")
bet.rank.clstr.actual = merge(bet.rank.clstr.actual, ARG.non.stat.rank.bet.clstr[,c("cat","non_mean")], by.x = "category", by.y="cat")
bet.rank.clstr.actual = merge(bet.rank.clstr.actual, ARG.mob.stat.rank.bet.clstr[,c("cat","mob_mean")], by.x = "category", by.y="cat")
bet.rank.clstr.actual$HH_type = "Between"

ARG.share.table.clstr.pairs = data.frame(matrix(ncol=3))
colnames(ARG.share.table.clstr.pairs) = c("HH_type", "cat", "num.pairs")
n=1
for (i in c("Within", "Between")) {
  for (j in ARG.share.table.clstr$cat %>% unique()) {
    ARG.share.table.clstr.pairs[n,1] = i
    ARG.share.table.clstr.pairs[n,2] = j
    ARG.share.table.clstr.pairs[n,3] = ARG.share.table.clstr[ARG.share.table.clstr$HH_type == i & ARG.share.table.clstr$cat == j,] %>% nrow()
    n = n+1
  }
}

clstr.actual = rbind(wit.rank.clstr.actual, bet.rank.clstr.actual)

clstr.actual = merge(clstr.actual, ARG.share.table.clstr.pairs, by.x = c("category", "HH_type"), by.y = c("cat", "HH_type"))

clstr.actual.wit = clstr.actual[clstr.actual$HH_type == "Within",]
#Append columns with Jaccard similarity
clstr.actual.wit$overall_sim = 1 - clstr.actual.wit$overall_mean
clstr.actual.wit$non_sim = 1 - clstr.actual.wit$non_mean
clstr.actual.wit$mob_sim = 1 - clstr.actual.wit$mob_mean

clstr.actual.bet = clstr.actual[clstr.actual$HH_type == "Between", ]
#Append columns with Jaccard similarity
clstr.actual.bet$overall_sim = 1 - clstr.actual.bet$overall_mean
clstr.actual.bet$non_sim = 1 - clstr.actual.bet$non_mean
clstr.actual.bet$mob_sim = 1 - clstr.actual.bet$mob_mean

#For source data
write.csv(clstr.actual.wit, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/clstr.actual.wit_re.csv"), row.names = F, quote = F)
saveRDS(clstr.actual.wit, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/clstr.actual.wit_re.rds"))

### Between households
write.csv(clstr.actual.bet, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/clstr.actual.bet_re.csv"), row.names = F, quote = F)
saveRDS(clstr.actual.bet, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/clstr.actual.bet_re.rds"))

################################################################################################################################################################################################################################################
#####For bootstrapped analysis
strain.share = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strain.share_updated.tsv", sep = '\t', header = T, colClasses = "character")

strain.share = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strain.share_updated.rds"))
#Change number as numeric values
strain.share[,5] = as.numeric(strain.share[,5])
strain.share[,6] = as.numeric(strain.share[,6])
strain.share[,7] = as.numeric(strain.share[,7])

strain.share1 = strain.share
strain.share1$subcategory = factor(strain.share1$subcategory, levels = c("human_human", "poultry_poultry", "dog_dog", "soil_soil", "water_source_water_source", "human_poultry", "dog_human", "human_water_source", "human_soil", "dog_poultry", "poultry_water_source", "poultry_soil", "dog_water_source", "dog_soil", "soil_water_source"), labels = c("Human - Human", "Poultry - Poultry", "Dog - Dog", "Household soil - Household soil", "Stored water - Stored water","Human - Poultry", "Human - Dog", "Human - Stored water", "Human - Household soil", "Poultry - Dog", "Poultry - Stored water", "Poultry - Household soil", "Dog - Stored water", "Dog - Household soil", "Stored water - Household soil"))

ARG.share.table.clstr$overall.jac.sim = 1- ARG.share.table.clstr$overall.jac.dist
ARG.share.table.clstr$non.jac.sim = 1 - ARG.share.table.clstr$non.jac.dist
ARG.share.table.clstr$mob.jac.sim = 1 - ARG.share.table.clstr$mob.jac.dist

hh.list = strain.share$HH1 %>% unique()
set.seed(1992)
hh_1000 = replicate(1000, sample(hh.list, replace = T)) %>% as.data.frame()

iter.rate = data.frame(matrix(ncol = 8))
colnames(iter.rate) = c("iter", "HH_type", "cat", "rate", "kmer", "overall.ARG", "non.ARG", "mob.ARG")

n=1
for (i in 1:1000) {
  a = hh_1000[,i] %>% as.data.frame()
  sub.sampled.strain = combn(a[,1], 2) %>% t() %>% as.data.frame()
  sub.sampled.strain = cbind(c(sub.sampled.strain[,1], a[,1]), c(sub.sampled.strain[,2], a[,1])) %>% data.frame()
  colnames(sub.sampled.strain) = c("HH1", "HH2")
  #stain-sharing
  for (k in strain.share1$subcategory %>% unique()) {
    tmp1 = merge(sub.sampled.strain, strain.share1[strain.share1$subcategory == k,], by.x = c("HH1", "HH2"), by.y = c("HH1", "HH2"), all.x = T)
    tmp2 = tmp1[is.na(tmp1$rate),1:2] %>% dplyr::rename(HH1 = HH2, HH2 = HH1) #Need to use rename from dply package
    tmp2 = merge(tmp2, strain.share1[strain.share1$subcategory == k,], by.x = c("HH1", "HH2"), by.y = c("HH1", "HH2"), all.x = T)
    tmp3 = rbind(tmp1[!is.na(tmp1$rate),], tmp2)
    #For calculations of kmer similarity, overall, non, mobile resistomes
    tmp4 = merge(sub.sampled.strain, ARG.share.table.clstr[ARG.share.table.clstr$cat == k,], by.x = c("HH1", "HH2"), by.y = c("HH1", "HH2"))
    tmp5 = merge(sub.sampled.strain, ARG.share.table.clstr[ARG.share.table.clstr$cat == k,], by.x = c("HH1", "HH2"), by.y = c("HH2", "HH1"))
    tmp6 = rbind(tmp4, tmp5)
    
    for (h in c("Within", "Between")) {
      if(h == "Within"){
        iter.rate[n,1] = i
        iter.rate[n,2] = h
        iter.rate[n,3] = k
        iter.rate[n,4] = tmp3[tmp3$HH_type == "Same", "rate"] %>% na.omit() %>% mean()
        iter.rate[n,5] = tmp6[tmp6$HH_type == "Within", "kmer.sim"] %>% na.omit() %>% mean()
        iter.rate[n,6] = tmp6[tmp6$HH_type == "Within", "overall.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,7] = tmp6[tmp6$HH_type == "Within", "non.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,8] = tmp6[tmp6$HH_type == "Within", "mob.jac.sim"] %>% na.omit() %>% mean()
        n=n+1
      }
      if(h == "Between"){
        iter.rate[n,1] = i
        iter.rate[n,2] = h
        iter.rate[n,3] = k
        iter.rate[n,4] = tmp3[tmp3$HH_type == "Different", "rate"] %>% na.omit() %>% mean()
        iter.rate[n,5] = tmp6[tmp6$HH_type == "Between", "kmer.sim"] %>% na.omit() %>% mean()
        iter.rate[n,6] = tmp6[tmp6$HH_type == "Between", "overall.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,7] = tmp6[tmp6$HH_type == "Between", "non.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,8] = tmp6[tmp6$HH_type == "Between", "mob.jac.sim"] %>% na.omit() %>% mean()
        n=n+1
      }
    }
    #print(i)
  }
}

iter.cor = data.frame(matrix(ncol=9))
colnames(iter.cor) = c("iter", "HH_type", "rate.vs.kmer", "rate.vs.overall", "rate.vs.non", "rate.vs.mob", "kmer.vs.overall", "kmer.vs.non", "kmer.vs.mob")
n=1
for (i in 1:1000) {
  for (j in c("Within", "Between")) {
    
    iter.cor[n, "iter"] = i
    iter.cor[n, "HH_type"] = j
    
    a = iter.rate[iter.rate$iter == i & iter.rate$HH_type == j,]
    # correlation rate vs. kmer
    b = cor.test(a$rate, a$kmer, method = "spear", exact = F)
    iter.cor[n, "rate.vs.kmer"] = b$estimate
    # correlation rate vs. ARGs
    c = cor.test(a$rate, a$overall.ARG, method = "spear", exact = F)
    iter.cor[n, "rate.vs.overall"] = c$estimate
    
    d = cor.test(a$rate, a$non.ARG, method = "spear", exact = F)
    iter.cor[n, "rate.vs.non"] = d$estimate
    
    e = cor.test(a$rate, a$mob.ARG, method = "spear", exact = F)
    iter.cor[n, "rate.vs.mob"] = e$estimate
    # correlation kmer vs. ARGs
    f = cor.test(a$kmer, a$overall.ARG, method = "spear", exact = F)
    iter.cor[n, "kmer.vs.overall"] = f$estimate
    
    g = cor.test(a$kmer, a$non.ARG, method = "spear", exact = F)
    iter.cor[n, "kmer.vs.non"] = g$estimate
    
    h = cor.test(a$kmer, a$mob.ARG, method = "spear", exact = F)
    iter.cor[n, "kmer.vs.mob"] = h$estimate
    
    n=n+1
  } 
}

iter.cor.m = melt(iter.cor, id.vars = c("iter", "HH_type"))
iter.cor.m$HH_type = factor(iter.cor.m$HH_type, levels = c("Within", "Between"))

#Plot figures of bootstrapped correlation results
ggplot(iter.cor.m, aes(x=value, fill=HH_type)) +
  geom_density(alpha = 0.7) +
  xlab("Spearman's coefficient (r)") +
  ylab("Density") +
  facet_wrap(.~variable, nrow = 2) +
  theme_base()

#For source data
write.csv(iter.cor, "/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/iter.cor.csv", quote = F, row.names = F)

iter.cor.test = data.frame(matrix(ncol=4))
colnames(iter.cor.test) = c("variable", "wit_mean", "bet_mean", "p.value")

n=1
for (i in iter.cor.m$variable %>% unique()) {
  a = iter.cor.m[iter.cor.m$HH_type == "Within" & iter.cor.m$variable == i, "value"]
  b = iter.cor.m[iter.cor.m$HH_type == "Between" & iter.cor.m$variable == i, "value"]
  c = wilcox.test(a,b)
  
  iter.cor.test[n,1] = i
  iter.cor.test[n,2] = mean(a)
  iter.cor.test[n,3] = mean(b)
  iter.cor.test[n,4] = c$p.value
  n=n+1
}

iter.cor.m[iter.cor.m$HH_type == "Within" & iter.cor.m$variable == "rate.vs.kmer","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)
iter.cor.m[iter.cor.m$HH_type == "Between" & iter.cor.m$variable == "rate.vs.kmer","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)


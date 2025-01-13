source(file.path(here::here(),"0-config.R"))

# #Rank I ARGs
# Focusing on ARGs of Rank I as a threat. (Zhang et al., 2021, Nat. comm.)
# 
# Rank I
# Aminoglycoside: aac(3)-II, aac(3)-VI, aac(6')-I, aadE, ant(2")-I, aph(3')-I, aph(3')-III, aph(6)-I, rmtF, rmtG
# 
# Beta-lactam: bacA, blaZ, CMY-4, CMY-6, CMY-111, CTX-M-2, CTX-M-15, CTX-M-24, CTX-M-55, CTX-M-129, GES-11, IMP-4, KPC-2, KPC-4, KPC-6, mecA, mecR1, NDM-5, NDM-6, OXA-1, OXA-4, OXA-10, SHV-1, SHV-5, TEM-1, TEM-156, TEM-169, VEB-3, VIM-1, VIM-2
# 
# Chloramphenicol: catA, catB, cmlA, floR
# 
# Collistin: mcr-1
# 
# MLS: ermB, ermC, ermT, lnuA, lnuB, mphA, mphB, msrA
# 
# Multidrug: emrB-qacA, mdtE, mdtL, mepA, norA, TolC
# 
# Quinolone: qnrA, qnrB, qnrS
# 
# Tetracycline: tetL, tetM
# 
# Trimethoprim: dfrA1, dfrA5, dfrA12, dfrA14, dfrA15, dfrA17, dfrA25, dfrB1
# 
# Vancomycin: vanY

cluster.dist.ARG = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/cluster.dist.ARG.csv"), header =  T)
all_sample_ARG_clean.clstr = read.csv(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean.clstr_re.csv"), header = T)
#Aminoglycoside
cluster.dist.ARG[grepl("aac\\(3\\)",cluster.dist.ARG$V2, ignore.case = T), "drug_class"] = "Aminoglycoside"
cluster.dist.ARG[grepl("aac\\(6'\\)",cluster.dist.ARG$V2, ignore.case = T), "drug_class"] = "Aminoglycoside"
cluster.dist.ARG[cluster.dist.ARG$V2 %in% c("APH(3')-Ia", "APH(3')-IIIa"), "drug_class"] = "Aminoglycoside"
cluster.dist.ARG[grepl("aph\\(6\\)-I",cluster.dist.ARG$V2, ignore.case = T), "drug_class"] = "Aminoglycoside"

#Peptide
cluster.dist.ARG[cluster.dist.ARG$V2 %in% c("bacA"), "drug_class"] = "Peptide"
#Beta-lactam
cluster.dist.ARG[cluster.dist.ARG$V2 %in% c("CTX-M-15", "CTX-M-55", "SHV-1", "TEM-1","OXA-1", "OXA-10"), "drug_class"] = "Beta-lactam"
#Chloramphenicol
cluster.dist.ARG[grepl("catb|cmlA|floR",cluster.dist.ARG$V2,ignore.case = T), "drug_class"] = "Chloramphenicol"
#MLS
cluster.dist.ARG[cluster.dist.ARG$V2 %in% c("Ermb", "ErmT", "mphA"), "drug_class"] = "MLS"
#Quinolone
cluster.dist.ARG[grepl("QnrB|QnrS",cluster.dist.ARG$V2, ignore.case = T), "drug_class"] = "Quinolone"
#Tetracycline
cluster.dist.ARG[cluster.dist.ARG$V2 %in% c("tet(L)", "tet(M)"), "drug_class"] = "Tetracycline"
#Trimethoprim
cluster.dist.ARG[cluster.dist.ARG$V2 %in% c("dfrA1", "dfrA5", "dfrA12", "dfrA14", "dfrA15", "dfrA17"), "drug_class"] = "Trimethoprim"
#Multidrug
cluster.dist.ARG[cluster.dist.ARG$V2 %in% c("mdtE", "TolC"), "drug_class"] = "Multidrug"

cluster.dist.ARG.rankI = cluster.dist.ARG[!is.na(cluster.dist.ARG$drug_class),]
cluster.dist.ARG.rankI = cluster.dist.ARG.rankI[order(cluster.dist.ARG.rankI$total, decreasing = T),]
cluster.dist.ARG.rankI$cluster = factor(cluster.dist.ARG.rankI$cluster, levels = cluster.dist.ARG.rankI[order(cluster.dist.ARG.rankI$total, decreasing = F),"cluster"])
cluster.dist.ARG.rankI$drug_class = factor(cluster.dist.ARG.rankI$drug_class, levels = c("Aminoglycoside", "Beta-lactam", "Chloramphenicol", "Peptide", "Trimethoprim", "MLS", "Quinolone", "Tetracycline", "Multidrug"))

cluster.dist.ARG.no.sing.rankI = cluster.dist.ARG.rankI[cluster.dist.ARG.rankI$total>1,] #Remove singleton cluster for figure

rankI_clstr_fig = data.frame()
for (i in cluster.dist.ARG.no.sing.rankI$V2 %>% unique()) {
  a = cluster.dist.ARG.no.sing.rankI[cluster.dist.ARG.no.sing.rankI$V2 == i,]
  a = a[1:5,]
  a = a[!is.na(a$cluster),]
  rankI_clstr_fig = rbind(rankI_clstr_fig, a)
}

rankI_clstr_fig.m1 = melt(rankI_clstr_fig[,c(1:4,16,18)])
rankI_clstr_fig.m2 = melt(rankI_clstr_fig[,c(1,11:16,18)])
rankI_clstr_fig.m2[rankI_clstr_fig.m2$value == 0, "value"] = NA

# Save into files
saveRDS(rankI_clstr_fig.m1, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/rankI_clstr_fig.m1.rds"))
saveRDS(rankI_clstr_fig.m2, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/rankI_clstr_fig.m2.rds"))

#Number of rank I ARG cluster comparison
#sample.list = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/sample.list.rds")) # Including water samples without colonies
sample.list = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/data/sample_list.txt"), sep = '\t') 
colnames(sample.list) = "sample"

all_sample_ARG_clean.clstr.rankI = all_sample_ARG_clean.clstr %>% filter(cluster %in% unique(cluster.dist.ARG.rankI$cluster))
all_sample_ARG_clean.clstr.rankI = merge(all_sample_ARG_clean.clstr.rankI, cluster.dist.ARG.rankI[,c("cluster","drug_class")], by = "cluster")

num.cluster.host.Multidrug.m = rankI_ARG_cluster_drugclass("Multidrug")
num.cluster.host.Beta_lactam.m = rankI_ARG_cluster_drugclass("Beta-lactam")
num.cluster.host.Tetracycline.m = rankI_ARG_cluster_drugclass("Tetracycline")
num.cluster.host.Chloramphenicol.m = rankI_ARG_cluster_drugclass("Chloramphenicol")
num.cluster.host.MLS.m = rankI_ARG_cluster_drugclass("MLS")
num.cluster.host.Aminoglycoside.m = rankI_ARG_cluster_drugclass("Aminoglycoside")
num.cluster.host.Peptide.m = rankI_ARG_cluster_drugclass("Peptide")
num.cluster.host.Quinolone.m = rankI_ARG_cluster_drugclass("Quinolone")
num.cluster.host.Trimethoprim.m = rankI_ARG_cluster_drugclass("Trimethoprim")

num.cluster.host.rankI.m = data.frame(rbind(num.cluster.host.Multidrug.m, num.cluster.host.Beta_lactam.m, num.cluster.host.Tetracycline.m, num.cluster.host.Chloramphenicol.m, num.cluster.host.MLS.m, num.cluster.host.Aminoglycoside.m, num.cluster.host.Peptide.m, num.cluster.host.Quinolone.m, num.cluster.host.Trimethoprim.m))
saveRDS(num.cluster.host.rankI.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.cluster.host.rankI.m.rds"))

#Statistical test
Multidrug.stat = pairwise.wilcox.test(num.cluster.host.Multidrug.m$num.uniq.clusters, num.cluster.host.Multidrug.m$host, p.adjust.method = "bonferroni")
Beta_lactam.stat = pairwise.wilcox.test(num.cluster.host.Beta_lactam.m$num.uniq.clusters, num.cluster.host.Beta_lactam.m$host, p.adjust.method = "bonferroni")
Tetracycline.stat = pairwise.wilcox.test(num.cluster.host.Tetracycline.m$num.uniq.clusters, num.cluster.host.Tetracycline.m$host, p.adjust.method = "bonferroni")
Chloramphenicol.stat = pairwise.wilcox.test(num.cluster.host.Chloramphenicol.m$num.uniq.clusters, num.cluster.host.Chloramphenicol.m$host, p.adjust.method = "bonferroni")
MLS.stat = pairwise.wilcox.test(num.cluster.host.MLS.m$num.uniq.clusters, num.cluster.host.MLS.m$host, p.adjust.method = "bonferroni")
Aminoglycoside.stat = pairwise.wilcox.test(num.cluster.host.Aminoglycoside.m$num.uniq.clusters, num.cluster.host.Aminoglycoside.m$host, p.adjust.method = "bonferroni")
Peptide.stat = pairwise.wilcox.test(num.cluster.host.Peptide.m$num.uniq.clusters, num.cluster.host.Peptide.m$host, p.adjust.method = "bonferroni")
Quinolone.stat = pairwise.wilcox.test(num.cluster.host.Quinolone.m$num.uniq.clusters, num.cluster.host.Quinolone.m$host, p.adjust.method = "bonferroni")
Trimethoprim.stat = pairwise.wilcox.test(num.cluster.host.Trimethoprim.m$num.uniq.clusters, num.cluster.host.Trimethoprim.m$host, p.adjust.method = "bonferroni")

### Comparison without subsampling
all_sample_ARG_clean.clstr.rankI.host = all_sample_ARG_clean.clstr.rankI %>% select(sample, drug_class) %>% table %>% as.data.frame()
all_sample_ARG_clean.clstr.rankI.host[grepl("A|O|C", all_sample_ARG_clean.clstr.rankI.host$sample), "host"] = "Human"
all_sample_ARG_clean.clstr.rankI.host[grepl("P", all_sample_ARG_clean.clstr.rankI.host$sample), "host"] = "Poultry"
all_sample_ARG_clean.clstr.rankI.host[grepl("D", all_sample_ARG_clean.clstr.rankI.host$sample), "host"] = "Dog"
all_sample_ARG_clean.clstr.rankI.host[grepl("W", all_sample_ARG_clean.clstr.rankI.host$sample), "host"] = "Stored water"
all_sample_ARG_clean.clstr.rankI.host[grepl("S", all_sample_ARG_clean.clstr.rankI.host$sample), "host"] = "Household soil"
all_sample_ARG_clean.clstr.rankI.host$host = factor(all_sample_ARG_clean.clstr.rankI.host$host, levels = c("Human", "Poultry", "Dog", "Stored water", "Household soil"))

saveRDS(all_sample_ARG_clean.clstr.rankI.host, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean.clstr.rankI.host.rds"))

#Statistical test
rankI_stat = list()
for (i in c("Multidrug", "Beta-lactam", "Tetracycline", "Chloramphenicol", "MLS", "Aminoglycoside", "Peptide", "Quinolone", "Trimethoprim")) {
  a =  all_sample_ARG_clean.clstr.rankI.host %>% filter(drug_class == i)
  durg.class.stat.n = pairwise.wilcox.test(a$Freq, a$host, p.adjust.method = "BH", exact = F)
  rankI_stat <- c(rankI_stat, setNames(list(durg.class.stat.n), i))
}
# Save into a file
capture.output(print(rankI_stat), file = file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/rankI_stat.txt"))


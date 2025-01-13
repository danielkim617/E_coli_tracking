source(file.path(here::here(),"0-config.R"))
# #Load library
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# library(viridis)
# library(ggthemes)

# Load data
straingst.table = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_network_re.rds"))
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

straingst.table[straingst.table$`house hold` %in% Dag.hh, "vil"] = "Dagoretti South"
straingst.table[straingst.table$`house hold` %in% Kib.hh, "vil"] = "Kibera"

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

# Phylogroup frequency
phy.freq = straingst.table %>% filter(!is.na(strain)) %>% select(sample, host, V2) %>% unique %>%  select(host, V2) %>% table %>% melt()
colnames(phy.freq) = c("host", "phylogroup", "number")

phy.freq[phy.freq$host == "human", "RF"] = phy.freq[phy.freq$host == "human", "number"]/132*100
phy.freq[phy.freq$host == "poultry", "RF"] = phy.freq[phy.freq$host == "poultry", "number"]/111*100
phy.freq[phy.freq$host == "dog", "RF"] = phy.freq[phy.freq$host == "dog", "number"]/17*100
phy.freq[phy.freq$host == "stored water", "RF"] = phy.freq[phy.freq$host == "stored water", "number"]/49*100 
phy.freq[phy.freq$host == "household soil", "RF"] = phy.freq[phy.freq$host == "household soil", "number"]/39*100

phy.freq$phylogroup = factor(phy.freq$phylogroup, levels = c("A", "B1", "B2", "C", "D", "E", "F", "G","cladeI", "albertii", "fergusonii", "S. dysenteriae", "S. flexneri", "Shigella sp.", "Unknown"), labels = c("A", "B1", "B2", "C", "D", "E", "F", "G","cladeI", "albertii", "fergusonii", "S. dysenteriae", "S. flexneri", "Shigella sp.", "Unknown"))
phy.freq$host = factor(phy.freq$host, levels = c("human", "poultry", "dog", "household soil", "stored water"), labels = c("Human", "Poultry", "Dog", "Household soil", "Stored water"))
#Save into files
saveRDS(phy.freq, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/phy.freq.rds"))

#Distribution of strains across sample types 
straingst.table1 = straingst.table %>% filter(!is.na(strain))
straingst.ref.list = straingst.table1$strain %>% unique %>% data.frame()
write.table(straingst.ref.list, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/straingst.ref.list"), sep = "\t", quote = F, row.names = F, col.names = F)

ref.table = data.frame(matrix(ncol = 16))
colnames(ref.table) = c("ref", "human", "poultry", "dog","water", "soil","Dag_human", "Dag_poultry", "Dag_dog","Dag_water", "Dag_soil", "Kib_human", "Kib_poultry", "Kib_dog","Kib_water", "Kib_soil")

n=1
for (i in straingst.table$strain %>% unique()) {
  #straingst.table
  ref.table[n,1] = i
  ref.table[n,2] = straingst.table[straingst.table$strain == i & grepl("A|O|C", straingst.table$sample), "sample"] %>% unique() %>% length() #human count
  ref.table[n,3] = straingst.table[straingst.table$strain == i & grepl("P", straingst.table$sample), "sample"] %>% unique() %>% length() #poultry count
  ref.table[n,4] = straingst.table[straingst.table$strain == i & grepl("D", straingst.table$sample), "sample"] %>% unique() %>% length() #dog count
  ref.table[n,5] = straingst.table[straingst.table$strain == i & grepl("W", straingst.table$sample), "sample"] %>% unique() %>% length() #water count
  ref.table[n,6] = straingst.table[straingst.table$strain == i & grepl("S", straingst.table$sample), "sample"] %>% unique() %>% length() #soil count
  
  #Dagoretti South
  ref.table[n,7] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Dagoretti South" & grepl("A|O|C", straingst.table$sample), "sample"] %>% unique() %>% length() #human count
  ref.table[n,8] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Dagoretti South" & grepl("P", straingst.table$sample), "sample"] %>% unique() %>% length() #poultry count
  ref.table[n,9] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Dagoretti South" & grepl("D", straingst.table$sample), "sample"] %>% unique() %>% length() #dog count
  ref.table[n,10] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Dagoretti South" & grepl("W", straingst.table$sample), "sample"] %>% unique() %>% length() #water count
  ref.table[n,11] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Dagoretti South" & grepl("S", straingst.table$sample), "sample"] %>% unique() %>% length() #soil count
  
  #Kibera
  ref.table[n,12] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Kibera" & grepl("A|O|C", straingst.table$sample), "sample"] %>% unique() %>% length() #human count
  ref.table[n,13] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Kibera" & grepl("P", straingst.table$sample), "sample"] %>% unique() %>% length() #poultry count
  ref.table[n,14] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Kibera" & grepl("D", straingst.table$sample), "sample"] %>% unique() %>% length() #dog count
  ref.table[n,15] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Kibera" & grepl("W", straingst.table$sample), "sample"] %>% unique() %>% length() #water count
  ref.table[n,16] = straingst.table[straingst.table$strain == i & straingst.table$vil == "Kibera" & grepl("S", straingst.table$sample), "sample"] %>% unique() %>% length() #soil count
  
  n=n+1
}

# #humans: 132 (Dag: 65, Kib: 67)
# #adults: 46
# #older child: 34
# #child: 52
# #poultry: 111 (Dag: 58, Kib: 53)
# #dog: 17 (Dag:7, Kib:10)
# #water: 22 (3) + 27 (samples with no colony) = 49 (Dag: 11 (25), Kib: 8 (24))
# #soil: 39 (1) (Dag:20, Kib: 18 (19))

# Change into percentage values
ref.table$human = ref.table$human/132*100
ref.table$poultry = ref.table$poultry/111*100
ref.table$dog = ref.table$dog/17*100
ref.table$water = ref.table$water/49*100
ref.table$soil = ref.table$soil/39*100
#Dag
ref.table$Dag_human = ref.table$Dag_human/65*100
ref.table$Dag_poultry = ref.table$Dag_poultry/58*100
ref.table$Dag_dog = ref.table$Dag_dog/7*100
ref.table$Dag_water = ref.table$Dag_water/25*100
ref.table$Dag_soil = ref.table$Dag_soil/20*100
#Kib
ref.table$Kib_human = ref.table$Kib_human/67*100
ref.table$Kib_poultry = ref.table$Kib_poultry/53*100
ref.table$Kib_dog = ref.table$Kib_dog/10*100
ref.table$Kib_water = ref.table$Kib_water/24*100
ref.table$Kib_soil = ref.table$Kib_soil/19*100
#Replace 0 with NAs
ref.table[ref.table == 0] = NA
# Save into files
write.table(ref.table, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ref_freq_table.tsv"), quote = F, row.names = F, sep = "\t")

# Figures for the comparison of number of colonies picked (including water samples that did not have any colony)
actual.numbers[grepl("A|O|C", actual.numbers$sampleid), "Host"] = "Human"
actual.numbers[grepl("P", actual.numbers$sampleid), "Host"] = "Poultry"
actual.numbers[grepl("D", actual.numbers$sampleid), "Host"] = "Dog"
actual.numbers[grepl("W", actual.numbers$sampleid), "Host"] = "Stored water"
actual.numbers[grepl("S", actual.numbers$sampleid), "Host"] = "Household soil"
actual.numbers$Host = factor(actual.numbers$Host, levels = c("Human", "Poultry", "Dog", "Stored water", "Household soil"))
#Save into files for a figure
saveRDS(actual.numbers, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/actual.numbers.rds"))

#Pairwise statistical test
num.pick.test = combn(as.character(actual.numbers$Host) %>% unique(),2) %>% t() %>% data.frame()

for (i in 1:nrow(num.pick.test)) {
  a = actual.numbers[actual.numbers$Host == num.pick.test[i,1],"picked"]
  b = actual.numbers[actual.numbers$Host == num.pick.test[i,2],"picked"]
  num.pick.test$host1_mean[i] = mean(a)
  num.pick.test$host2_mean[i] = mean(b)
  c = wilcox.test(a,b, exact = F)
  num.pick.test$p.value[i] = c$p.value
}

num.pick.test$adj.p = num.pick.test$p.value %>% p.adjust(., method = "BH")
#Save into files
write.table(num.pick.test, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.pick.test.tsv"), quote = F, row.names = F, sep = "\t")

# Number of average strains identified by host types and subcounties
num.strain.iden = data.frame(matrix(ncol=4))
colnames(num.strain.iden) = c("sample", "host.type", "num.strains", "subcounty")

n=1
for (i in straingst.table$sample %>% unique()) {
  num.strain.iden[n,1] = i
  num.strain.iden[n,2] = straingst.table[straingst.table$sample == i,"host"] %>% unique()
  num.strain.iden[n,3] = straingst.table[straingst.table$sample == i & !is.na(straingst.table$strain),"strain"] %>% length()
  num.strain.iden[n,4] = straingst.table[straingst.table$sample == i,"vil"] %>% unique()
  
  n=n+1
}
num.strain.iden = num.strain.iden[!is.na(num.strain.iden$subcounty),]
num.strain.iden$host.type = factor(num.strain.iden$host.type, levels = c("human", "poultry", "dog","stored water","household soil"), labels = c("Human", "Poultry", "Dog", "Stored water","Household soil"))

#Save into files
saveRDS(num.strain.iden, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.strain.iden.rds"))

#Statistical testing
num.strain.iden.test = num.strain.iden %>% pull(host.type) %>% as.character(.) %>% unique() %>% combn(.,m=2) %>% t() %>% as.data.frame()
colnames(num.strain.iden.test) = c("host1", "host2")

for (i in 1:nrow(num.strain.iden.test)) {
  a = num.strain.iden %>% filter(host.type == num.strain.iden.test[i,"host1"]) %>% pull(num.strains) 
  b = num.strain.iden %>% filter(host.type == num.strain.iden.test[i,"host2"]) %>% pull(num.strains) 
  num.strain.iden.test$host1_mean[i] = median(a)
  num.strain.iden.test$host2_mean[i] = median(b)
  c = wilcox.test(a,b, exact = F)
  num.strain.iden.test$p.value[i] = c$p.value
}
# Add adjusted p values
num.strain.iden.test$adj.p = p.adjust(num.strain.iden.test$p.value, method = "BH")
# Save into files
write.csv(num.strain.iden.test, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.strain.iden.test.csv"), quote = F, row.names = F)

#Number of unique strains identified when subsampled (n=10) with equal number of samples for each host type
set.seed(1992)

num = 10 # Number of samples to subsample
h = replicate(1000,sample(straingst.table[straingst.table$host == "human", "sample"] %>% unique(), num, replace = F))
a1 = replicate(1000,sample(straingst.table[straingst.table$host == "human" & grepl("A", straingst.table$sample), "sample"] %>% unique(), num, replace = F))
c1 = replicate(1000,sample(straingst.table[straingst.table$host == "human" & grepl("C", straingst.table$sample), "sample"] %>% unique(), num, replace = F))
o1 = replicate(1000,sample(straingst.table[straingst.table$host == "human" & grepl("O", straingst.table$sample), "sample"] %>% unique(), num, replace = F))
p = replicate(1000,sample(straingst.table[straingst.table$host == "poultry", "sample"] %>% unique(), num, replace = F))
d = replicate(1000, sample(straingst.table[straingst.table$host == "dog", "sample"] %>% unique(), num, replace = F))
w = replicate(1000,sample(straingst.table[straingst.table$host == "stored water", "sample"] %>% unique(), num, replace = F))
s = replicate(1000,sample(straingst.table[straingst.table$host == "household soil", "sample"] %>% unique(), num, replace = F))

tmp = data.frame(matrix(ncol=1002))

tmp[1,1] = num
tmp[1,2] = "human"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% h[,j],]
  tmp[1,j+2] = a$strain %>% unique() %>% length()
}
tmp[2,1] = num
tmp[2,2] = "adult"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% a1[,j],]
  tmp[2,j+2] = a$strain %>% unique() %>% length()
}
tmp[3,1] = num
tmp[3,2] = "child"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% c1[,j],]
  tmp[3,j+2] = a$strain %>% unique() %>% length()
}
tmp[4,1] = num
tmp[4,2] = "older_child"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% o1[,j],]
  tmp[4,j+2] = a$strain %>% unique() %>% length()
}
tmp[5,1] = num
tmp[5,2] = "poultry"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% p[,j],]
  tmp[5,j+2] = a$strain %>% unique() %>% length()
}
tmp[6,1] = num
tmp[6,2] = "dog"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% d[,j],]
  tmp[6,j+2] = a$strain %>% unique() %>% length()
}
tmp[7,1] = num
tmp[7,2] = "water"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% w[,j],]
  tmp[7,j+2] = a$strain %>% unique() %>% length()
}
tmp[8,1] = num
tmp[8,2] = "soil"
for (j in 1:1000) {
  a = straingst.table[straingst.table$sample %in% s[,j],]
  tmp[8,j+2] = a$strain %>% unique() %>% length()
}

uniq.strains.host = tmp

uniq.strains.host.m = uniq.strains.host %>% melt(id=c("X1", "X2"))
colnames(uniq.strains.host.m) = c("n", "host", "iter", "num.uniq.strains")
uniq.strains.host.m$host = factor(uniq.strains.host.m$host, levels = c("human", "adult", "older_child", "child","poultry", "dog", "water", "soil"), labels = c("Human", "Adult (> 15 yr)", "Child (5 - 15 yr)", "Child (0 - 5 yr)", "Poultry", "Dog", "Stored water", "Household soil"))

#Save into files
saveRDS(uniq.strains.host.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/uniq.strains.host.m.rds"))

#Statistical test
uniq.strains.host.test = combn(as.character(uniq.strains.host.m$host) %>% unique(),2) %>% t() %>% data.frame()

for (i in 1:nrow(uniq.strains.host.test)) {
  a = uniq.strains.host.m[uniq.strains.host.m$host == uniq.strains.host.test[i,1], "num.uniq.strains"]
  b = uniq.strains.host.m[uniq.strains.host.m$host == uniq.strains.host.test[i,2], "num.uniq.strains"]
  uniq.strains.host.test$host1_mean[i] = mean(a)
  uniq.strains.host.test$host2_mean[i] = mean(b)
  c = wilcox.test(a,b,exact = F)
  uniq.strains.host.test$p.value[i] = c$p.value
}
# Add adjusted p values
uniq.strains.host.test$adj.p = p.adjust(uniq.strains.host.test$p.value, method = "BH")
#Save into files
write.csv(uniq.strains.host.test, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/uniq.strains.host.test.csv"), quote = F, row.names = F)

source(file.path(here::here(),"0-config.R"))
#Load data
strain.share = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strain.share_updated.rds")) # after update
#Add additional information
#Load data
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
stool_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-stool_survey-CLEANED-20200130.csv"))
animal_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-animal_survey-CLEANED-20200130.csv"))
#Add 3 digit household ID
hh_survey$HH_ID = substr(hh_survey$hh_id_5dig, 3,5)
#Add geographical GPS coordinate to strain sharing table
#gps_hhlatitude, gps_hhlongitude, countyid, subcountyid, location, sublocation, village 
strain.share$HH1_gps_latitude = NA
strain.share$HH2_gps_latitude = NA

strain.share$HH1_gps_longitude = NA
strain.share$HH2_gps_longitude = NA

strain.share$HH1_countyid = NA
strain.share$HH2_countyid = NA

strain.share$HH1_subcountyid = NA
strain.share$HH2_subcountyid = NA

strain.share$HH1_location = NA
strain.share$HH2_location = NA

strain.share$HH1_sublocation = NA
strain.share$HH2_sublocation = NA

strain.share$HH1_village = NA
strain.share$HH2_village = NA

for (i in 1:nrow(strain.share)) {
  #Add location information for HH1
  strain.share$HH1_gps_latitude[i] = hh_survey[hh_survey$HH_ID == strain.share$HH1[i],"gps_hhlatitude"] %>% na.omit()
  strain.share$HH1_gps_longitude[i] = hh_survey[hh_survey$HH_ID == strain.share$HH1[i],"gps_hhlongitude"] %>% na.omit()
  strain.share$HH1_countyid[i] = hh_survey[hh_survey$HH_ID == strain.share$HH1[i],"countyid"] %>% na.omit()
  strain.share$HH1_subcountyid[i] = hh_survey[hh_survey$HH_ID == strain.share$HH1[i],"subcountyid"] %>% na.omit()
  strain.share$HH1_location[i] = hh_survey[hh_survey$HH_ID == strain.share$HH1[i],"location"] %>% na.omit()
  strain.share$HH1_sublocation[i] = hh_survey[hh_survey$HH_ID == strain.share$HH1[i],"sublocation"] %>% na.omit()
  strain.share$HH1_village[i] = hh_survey[hh_survey$HH_ID == strain.share$HH1[i],"village"] %>% na.omit()
  
  #Add location information for HH2
  strain.share$HH2_gps_latitude[i] = hh_survey[hh_survey$HH_ID == strain.share$HH2[i],"gps_hhlatitude"] %>% na.omit()
  strain.share$HH2_gps_longitude[i] = hh_survey[hh_survey$HH_ID == strain.share$HH2[i],"gps_hhlongitude"] %>% na.omit()
  strain.share$HH2_countyid[i] = hh_survey[hh_survey$HH_ID == strain.share$HH2[i],"countyid"] %>% na.omit()
  strain.share$HH2_subcountyid[i] = hh_survey[hh_survey$HH_ID == strain.share$HH2[i],"subcountyid"] %>% na.omit()
  strain.share$HH2_location[i] = hh_survey[hh_survey$HH_ID == strain.share$HH2[i],"location"] %>% na.omit()
  strain.share$HH2_sublocation[i] = hh_survey[hh_survey$HH_ID == strain.share$HH2[i],"sublocation"] %>% na.omit()
  strain.share$HH2_village[i] = hh_survey[hh_survey$HH_ID == strain.share$HH2[i],"village"] %>% na.omit()
}

#Plot the overall average strain-sharing rate (including both within and between households) (Dep)
overall.rate = aggregate(strain.share[,7], by = list(strain.share$subcategory), FUN = mean)
overall.rate = melt(overall.rate) #melt data for plotting
overall.rate$Group.1 = factor(overall.rate$Group.1, levels = c("human_human", "poultry_poultry", "dog_dog", "water_source_water_source", "soil_soil", "human_poultry", "dog_human", "human_water_source", "human_soil", "dog_poultry", "poultry_water_source", "poultry_soil", "dog_water_source", "dog_soil", "soil_water_source"))
overall.rate$variable = "diff.host"
overall.rate$variable[c(1,6,10,13,15)] = "same.host"

#For within and between households
HH.rate = aggregate(strain.share[,7], by = list(strain.share$subcategory, strain.share$HH_type), FUN = mean)
HH.rate$Group.1 = factor(HH.rate$Group.1, levels = c("human_human", "poultry_poultry", "dog_dog", "water_source_water_source", "soil_soil", "human_poultry", "dog_human", "human_water_source", "human_soil", "dog_poultry", "poultry_water_source", "poultry_soil", "dog_water_source", "dog_soil", "soil_water_source"))
HH.rate$Group.2 = factor(HH.rate$Group.2, levels = c("Same", "Different"))

# Within vs. between comparison by village (Excluded)
#Exclude household pair from different subcounty
strain.share.subcount = strain.share[strain.share$HH1_subcountyid == strain.share$HH2_subcountyid,] 
#Add subcounty to each pair (Dagoretti South or Kibera)
strain.share.subcount$subcount_type = strain.share.subcount$HH1_subcountyid
#Get average value for the rates
subcount.rate = aggregate(strain.share.subcount[,7], by = list(strain.share.subcount$subcategory, strain.share.subcount$HH_type,strain.share.subcount$subcount_type), FUN = mean)
#Change labels with factors
subcount.rate$Group.1 = factor(subcount.rate$Group.1, levels = c("human_human", "poultry_poultry", "dog_dog", "water_source_water_source", "soil_soil","human_poultry", "dog_human", "human_water_source", "human_soil", "dog_poultry", "poultry_water_source", "poultry_soil", "dog_water_source", "dog_soil", "soil_water_source"), labels = c("Human - Human", "Poultry - Poultry", "Dog - Dog", "Stored water - Stored water", "Household soil - Household soil",  "Human - Poultry", "Human - Dog", "Human - Stored water", "Human - Household soil", "Poultry - Dog", "Poultry - Stored water", "Poultry - Household soil", "Dog - Stored water", "Dog - Household soil", "Stored water - Household soil"))
subcount.rate$Group.2 = factor(subcount.rate$Group.2, levels = c("Same", "Different"))

#########################################################################################################################
#To make a figure for prevalence (number of hhs or hh pairs with strain-sharing observed out of total number of possibilities)
prevlance.stat = data.frame(matrix(ncol = 6))
colnames(prevlance.stat) = c("category", "subcounty", "HH_type", "num.obs", "all.poss", "percent")
n=1
for (i in c("Overall", "Dagoretti South", "Kibera")) {
  for (j in c("Same", "Different")) {
    for (k in c("human_human", "poultry_poultry", "dog_dog", "water_source_water_source", "soil_soil",
                "human_poultry", "dog_human", "human_water_source", "human_soil","dog_poultry" ,  "poultry_water_source",
                "poultry_soil",  "dog_water_source", "dog_soil", "soil_water_source" )) {
      #print(i)
      prevlance.stat[n,1] = k #Add category
      prevlance.stat[n,2] = i #Add subcounty
      prevlance.stat[n,3] = j #Add HH_type
      
      if(i == "Overall"){
        prevlance.stat[n,4] = strain.share[strain.share$subcategory == k & strain.share$HH_type == j & strain.share$rate > 0, ] %>% nrow() #add numb of obs
        prevlance.stat[n,5] = strain.share[strain.share$subcategory == k & strain.share$HH_type == j , ] %>% nrow() #add all poss of obs
        prevlance.stat[n,6] = prevlance.stat[n,4]/prevlance.stat[n,5]*100
        n=n+1
      }
      if(i == "Dagoretti South"){
        prevlance.stat[n,4] = strain.share.subcount[strain.share.subcount$subcategory == k & strain.share.subcount$HH_type == j & strain.share.subcount$subcount_type == i & strain.share.subcount$rate > 0, ] %>% nrow() #add numb of obs
        prevlance.stat[n,5] = strain.share.subcount[strain.share.subcount$subcategory == k & strain.share.subcount$HH_type == j & strain.share.subcount$subcount_type == i, ] %>% nrow() #add all poss of obs
        prevlance.stat[n,6] = prevlance.stat[n,4]/prevlance.stat[n,5]*100
        n=n+1
      }
      if(i == "Kibera"){
        prevlance.stat[n,4] = strain.share.subcount[strain.share.subcount$subcategory == k & strain.share.subcount$HH_type == j & strain.share.subcount$subcount_type == i & strain.share.subcount$rate > 0, ] %>% nrow() #add numb of obs
        prevlance.stat[n,5] = strain.share.subcount[strain.share.subcount$subcategory == k & strain.share.subcount$HH_type == j & strain.share.subcount$subcount_type == i, ] %>% nrow() #add all poss of obs
        prevlance.stat[n,6] = prevlance.stat[n,4]/prevlance.stat[n,5]*100
        n=n+1
      }
      
    }
  }
}

prevlance.stat$subcounty = factor(prevlance.stat$subcounty, levels = c("Overall", "Dagoretti South", "Kibera"))
prevlance.stat$HH_type = factor(prevlance.stat$HH_type, levels = c("Same", "Different"))
prevlance.stat$category = factor(prevlance.stat$category, levels = c("human_human", "poultry_poultry", "dog_dog", "water_source_water_source", "soil_soil",  "human_poultry", "dog_human", "human_water_source", "human_soil","dog_poultry" ,  "poultry_water_source", "poultry_soil",  "dog_water_source", "dog_soil", "soil_water_source" ), labels = c("Human - Human", "Poultry - Poultry", "Dog - Dog", "Stored water - Stored water", "Household soil - Household soil", "Human - Poultry", "Human - Dog", "Human - Stored water", "Human - Household soil", "Poultry - Dog", "Poultry - Stored water", "Poultry - Household soil", "Dog - Stored water", "Dog - Household soil", "Stored water - Household soil"))

prevlance.stat$text = paste0("(",prevlance.stat$num.obs,"/",prevlance.stat$all.poss, ")")

#New figure for the comparison of strain-sharing and prevalence
HH.rate$Group.1 = factor(HH.rate$Group.1, levels = c("human_human", "poultry_poultry", "dog_dog", "water_source_water_source", "soil_soil", "human_poultry", "dog_human", "human_water_source", "human_soil","dog_poultry" , "poultry_water_source","poultry_soil",  "dog_water_source", "dog_soil", "soil_water_source" ), labels = c("Human - Human", "Poultry - Poultry", "Dog - Dog", "Stored water - Stored water", "Household soil - Household soil", "Human - Poultry", "Human - Dog", "Human - Stored water", "Human - Household soil", "Poultry - Dog",  "Poultry - Stored water", "Poultry - Household soil", "Dog - Stored water", "Dog - Household soil", "Stored water - Household soil"))
colnames(HH.rate) = c("category", "HH_type", "rate")
HH.rate$subcounty = "Overall"

# subcount.rate (rates by village)
colnames(subcount.rate) = c("category", "HH_type", "subcounty", "rate")
#Add prevalance values to strain-sharing rates
prevlance.stat1 = merge(prevlance.stat, HH.rate, by=c("category", "subcounty", "HH_type")) #overall
prevlance.stat2 = merge(prevlance.stat, subcount.rate,  by=c("category", "subcounty", "HH_type")) # Dagoretti South and Kibera
prevlance.stat3 = prevlance.stat[is.nan(prevlance.stat$percent) ,] #categoreis that did not merged with any of the previous table because of no pairs
prevlance.stat3$rate = NA
prevlance.stat4 = data.frame(rbind(prevlance.stat1, prevlance.stat2, prevlance.stat3))
prevlance.stat.county = prevlance.stat4[prevlance.stat4$subcounty != "Overall",] #For facets in figure to compare within vs. between in the figures
colnames(prevlance.stat.county) = c("category", "HH_type","subcounty", "num.obs", "all.poss",  "percent", "text", "rate")#For facets in figure to compare within vs. between in the figures switched "HH_type" and "subcounty" column
prevlance.stat5 = data.frame(rbind(prevlance.stat4, prevlance.stat.county))
#Save into a file
write.table(prevlance.stat5, file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/prevlance.stat5_update.tsv"), sep = "\t", row.names = F, quote = F)
saveRDS(prevlance.stat5, file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/prevlance.stat5_update.rds"))

#Statistical analysis (Permutation/bootstrapping) #######################################################################################################################
##Comparison of overall rates between categories (Excluded - but permutation test function included)
#Comparison of overall rates between categories
set.seed(1992)
overall.perm = combn(strain.share$subcategory %>% unique(),2) %>% t() %>% data.frame() # Get all combination of categories for the comparison
overall.perm$host1_mean = NA
overall.perm$host2_mean = NA
overall.perm$mean_diff = NA
overall.perm$host1_median = NA
overall.perm$host2_median = NA
overall.perm$p.value = NA

for (i in 1:nrow(overall.perm)) {
  print(i)
  a = strain.share[strain.share$subcategory == overall.perm[i,1],] #Extract first category
  b = strain.share[strain.share$subcategory == overall.perm[i,2],] #Extract second category
  
  overall.perm$host1_mean[i] = mean(a$rate)
  overall.perm$host2_mean[i] = mean(b$rate)
  overall.perm$mean_diff[i] = abs(mean(a$rate) - mean(b$rate))
  overall.perm$host1_median[i] = median(a$rate)
  overall.perm$host2_median[i] = median(b$rate)
  overall.perm$p.value[i] = permutation.test(a$rate, b$rate)
  
}
overall.perm$adj.p = p.adjust(overall.perm$p.value, method = "BH")
overall.perm = overall.perm[overall.perm$adj.p < 0.05,] #take only significant results
# Save into a file
write.table(overall.perm, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/overall.perm_updated.tsv"), sep = "\t", row.names = F, quote = F)

##Comparison of  rates between categories (Human, Animal, and Environments)
cat.list = strain.share %>% pull(category) %>% unique
cat.comp = combn(cat.list,2) %>% t() %>% data.frame() #Get all
cat.comp.tab = data.frame(matrix(ncol=6))
colnames(cat.comp.tab) = c("HH_type" , "cat1", "cat2", "cat1_mean", "cat2_mean", "p.value")
n=1
for (HH in c("Overall", "Same", "Different")) {
  for (i in 1:nrow(cat.comp)) {
    cat.comp.tab[n,1] = HH
    cat.comp.tab[n,2] = cat.comp[i,1]
    cat.comp.tab[n,3] = cat.comp[i,2]
    if(HH == "Overall"){
      a = strain.share %>% filter(category == cat.comp[i,1]) %>% pull(rate)
      b = strain.share %>% filter(category == cat.comp[i,2]) %>% pull(rate)
      cat.comp.tab[n,4] = mean(a)
      cat.comp.tab[n,5] = mean(b)
      cat.comp.tab[n,6] = permutation.test(a,b)
      n=n+1
    }
    else{
      a = strain.share %>% filter(category == cat.comp[i,1] & HH_type == HH) %>% pull(rate)
      b = strain.share %>% filter(category == cat.comp[i,2] & HH_type == HH) %>% pull(rate)
      cat.comp.tab[n,4] = mean(a)
      cat.comp.tab[n,5] = mean(b)
      cat.comp.tab[n,6] = permutation.test(a,b)
      n=n+1
    }
  }
}

#p adjustment
cat.comp.tab$adj.p[cat.comp.tab$HH_type == "Overall"] = cat.comp.tab$p.value[cat.comp.tab$HH_type == "Overall"] %>% p.adjust(.,method = "BH")
cat.comp.tab$adj.p[cat.comp.tab$HH_type == "Same"] = cat.comp.tab$p.value[cat.comp.tab$HH_type == "Same"] %>% p.adjust(.,method = "BH")
cat.comp.tab$adj.p[cat.comp.tab$HH_type == "Different"] = cat.comp.tab$p.value[cat.comp.tab$HH_type == "Different"] %>% p.adjust(.,method = "BH")

cat.comp.tab$HH_type = factor(cat.comp.tab$HH_type, levels = c("Overall", "Same", "Different"), labels = c("Overall", "Within", "Between"))
#Save into a file
write.table(cat.comp.tab, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/cat.comp.tab.tsv"), sep = "\t", quote = F, row.names = F)

#Treating water and soil separately.
strain.share.env = strain.share
strain.share.env <- strain.share.env %>%
  mutate(category = if_else(subcategory %in% c("poultry_water_source", "dog_water_source"), "animal-water_source", category))
strain.share.env <- strain.share.env %>%
  mutate(category = if_else(subcategory %in% c("poultry_soil", "dog_soil"), "animal-soil", category))
strain.share.env <- strain.share.env %>%
  mutate(category = if_else(category =="human-environment", subcategory, category))
strain.share.env <- strain.share.env %>%
  mutate(category = if_else(category =="environment-environment", subcategory, category))

#cat.env.list = strain.share.env$category %>% unique
#Exclude comparisons between same host types 
cat.env.list = c("human-animal", "animal-soil", "animal-water_source", "human_soil", "human_water_source")

cat.env.comp = combn(cat.env.list,2) %>% t() %>% data.frame() #Get all
cat.env.comp.tab = data.frame(matrix(ncol=6))
colnames(cat.env.comp.tab) = c("HH_type" , "cat1", "cat2", "cat1_mean", "cat2_mean", "p.value")

n=1
for (HH in c("Overall", "Same", "Different")) {
  for (i in 1:nrow(cat.env.comp)) {
    cat.env.comp.tab[n,1] = HH
    cat.env.comp.tab[n,2] = cat.env.comp[i,1]
    cat.env.comp.tab[n,3] = cat.env.comp[i,2]
    if(HH == "Overall"){
      a = strain.share.env %>% filter(category == cat.env.comp[i,1]) %>% pull(rate)
      b = strain.share.env %>% filter(category == cat.env.comp[i,2]) %>% pull(rate)
      cat.env.comp.tab[n,4] = mean(a)
      cat.env.comp.tab[n,5] = mean(b)
      cat.env.comp.tab[n,6] = permutation.test(a,b)
      n=n+1
    }
    else{
      a = strain.share.env %>% filter(category == cat.env.comp[i,1] & HH_type == HH) %>% pull(rate)
      b = strain.share.env %>% filter(category == cat.env.comp[i,2] & HH_type == HH) %>% pull(rate)
      if(length(a) > 0 & length(b) > 0){
        cat.env.comp.tab[n,4] = mean(a)
        cat.env.comp.tab[n,5] = mean(b)
        cat.env.comp.tab[n,6] = permutation.test(a,b)
        n=n+1
      }
      else{
        next
      }
    }
  }
}

#Adjusting p-values
cat.env.comp.tab$adj.p[cat.env.comp.tab$HH_type == "Overall"] = cat.env.comp.tab$p.value[cat.env.comp.tab$HH_type == "Overall"] %>% p.adjust(.,method = "BH")
cat.env.comp.tab$adj.p[cat.env.comp.tab$HH_type == "Same"] = cat.env.comp.tab$p.value[cat.env.comp.tab$HH_type == "Same"] %>% p.adjust(.,method = "BH")
cat.env.comp.tab$adj.p[cat.env.comp.tab$HH_type == "Different"] = cat.env.comp.tab$p.value[cat.env.comp.tab$HH_type == "Different"] %>% p.adjust(.,method = "BH")

#Save into a file
write.table(cat.env.comp.tab, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/cat.env.comp.tab.tsv"), sep = "\t", quote = F, row.names = F)

##Comparison of overall rates between categories (by HH types)
#by HH_type
a = combn(strain.share[strain.share$HH_type == "Same","subcategory"] %>% unique(),2) %>% t() %>% data.frame() #Get all combination of categories from same household
a$HH_type = "Same"
b = combn(strain.share[strain.share$HH_type == "Different","subcategory"] %>% unique(),2) %>% t() %>% data.frame() #Get all combination of categories from different household
b$HH_type = "Different"
HH.perm = data.frame(rbind(a,b))
HH.perm$host1_mean = NA
HH.perm$host2_mean = NA
HH.perm$mean_diff = NA
HH.perm$host1_median = NA
HH.perm$host2_median = NA
HH.perm$p.value = NA

for (i in 1:nrow(HH.perm)) {
  a = strain.share[strain.share$subcategory == HH.perm[i,1] & strain.share$HH_type == HH.perm[i,3],] #Extract first category
  b = strain.share[strain.share$subcategory == HH.perm[i,2] & strain.share$HH_type == HH.perm[i,3],] #Extract second category
  
  HH.perm$host1_mean[i] = mean(a$rate)
  HH.perm$host2_mean[i] = mean(b$rate)
  HH.perm$mean_diff[i] = abs(mean(a$rate) - mean(b$rate))
  HH.perm$host1_median[i] = median(a$rate)
  HH.perm$host2_median[i] = median(b$rate)
  HH.perm$p.value[i] = permutation.test(a$rate, b$rate)
}

HH.perm = HH.perm[!is.na(HH.perm$p.value),] # where both categories have 0 rates

HH.perm$adj.p[HH.perm$HH_type == "Same"]  = p.adjust(HH.perm$p.value[HH.perm$HH_type == "Same"], method = "BH")
HH.perm$adj.p[HH.perm$HH_type == "Different"]  = p.adjust(HH.perm$p.value[HH.perm$HH_type == "Different"], method = "BH")

HH.perm.sig = HH.perm[HH.perm$adj.p < 0.05,] #take only significant results
colnames(HH.perm) = c("cat1", "cat2", "HH_type", "cat1_mean", "cat2_mean", "mean_diff", "cat1_median", "cat1_median", "p.value", "adj.p")
# Save into a file
write.table(HH.perm, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/HH.perm_update.tsv"), sep = "\t", row.names = F, quote = F)

##Within vs. between HHs
HH.perm.wit.bt = data.frame(matrix(ncol = 4))
colnames(HH.perm.wit.bt) = c("subcategory", "Same_mean", "Different_mean", "p.value")
n=1
for (i in strain.share[strain.share$HH_type == "Same", "subcategory"] %>% unique()) {
  a = strain.share[strain.share$HH_type == "Same" & strain.share$subcategory == i,]
  b = strain.share[strain.share$HH_type == "Different" & strain.share$subcategory == i,]
  set.seed(1992)
  
  rep1 = replicate(1000, sample(c(a$rate,b$rate), replace = F))
  a1 = rep1[1:length(a$rate),]
  b1 = rep1[(length(a$rate)+1):(length(a$rate) + length(b$rate)),]
  
  diffs = apply(a1, 2, mean) - apply(b1, 2, mean)
  
  if(mean(a$rate) == 0 & mean(b$rate) == 0){
    p = NA
  }
  else{
    p = sum(abs(diffs) >= (abs(mean(a$rate) - mean(b$rate))))/1000
  }
  HH.perm.wit.bt[n,1] = i
  HH.perm.wit.bt[n,2] = mean(a$rate)
  HH.perm.wit.bt[n,3] = mean(b$rate)
  HH.perm.wit.bt[n,4] = p
  n=n+1
}

HH.perm.wit.bt$adj.p = p.adjust(HH.perm.wit.bt$p.value, method = "BH")
# Save into a file
write.table(HH.perm.wit.bt,"/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/HH.perm.wit.bt_update.tsv", sep = "\t", row.names = F, quote = F)

## Comparison among categories by village
##by village between categories
vil.perm = data.frame(matrix(ncol = 4))
colnames(vil.perm) = c("host1", "host2", "HH_type", "village")

for (vil in strain.share.subcount$subcount_type %>% unique()) {
  #print(vil)
  a = combn(strain.share.subcount[strain.share.subcount$HH_type == "Same" & strain.share.subcount$subcount_type == vil,"subcategory"] %>% unique(),2) %>% t() %>% data.frame()
  a$HH_type = "Same"
  a$village = vil
  
  b = combn(strain.share.subcount[strain.share.subcount$HH_type == "Different" & strain.share.subcount$subcount_type == vil,"subcategory"] %>% unique(),2) %>% t() %>% data.frame()
  b$HH_type = "Different"
  b$village = vil
  c = data.frame(rbind(a,b))
  colnames(c) = c("host1", "host2", "HH_type", "village")
  vil.perm = data.frame(rbind(vil.perm,c))
}
vil.perm = vil.perm[!is.na(vil.perm$village),] #Strip first row with empty cells

for (i in 1:nrow(vil.perm)) {
  #print(i)
  a = strain.share.subcount[strain.share.subcount$subcategory == vil.perm[i,1] & strain.share.subcount$HH_type == vil.perm[i,3] & strain.share.subcount$subcount_type == vil.perm[i,4],] #Extract first category
  b = strain.share.subcount[strain.share.subcount$subcategory == vil.perm[i,2] & strain.share.subcount$HH_type == vil.perm[i,3] & strain.share.subcount$subcount_type == vil.perm[i,4],] #Extract second category
  
  vil.perm$host1_mean[i] = mean(a$rate)
  vil.perm$host2_mean[i] = mean(b$rate)
  vil.perm$mean_diff[i] = abs(mean(a$rate) - mean(b$rate))
  vil.perm$host1_median[i] = median(a$rate)
  vil.perm$host2_median[i] = median(b$rate)
  vil.perm$p.value[i] = permutation.test(a$rate, b$rate)
}
#

vil.perm$adj.p[vil.perm$HH_type == "Same" & vil.perm$village == "Dagoretti South"] = vil.perm[vil.perm$HH_type == "Same" & vil.perm$village == "Dagoretti South","p.value"] %>% p.adjust(., method = "bonferroni")
vil.perm$adj.p[vil.perm$HH_type == "Same" & vil.perm$village == "Kibera"] = vil.perm[vil.perm$HH_type == "Same" & vil.perm$village == "Kibera","p.value"] %>% p.adjust(., method = "bonferroni")
vil.perm$adj.p[vil.perm$HH_type == "Different" & vil.perm$village == "Dagoretti South"] = vil.perm[vil.perm$HH_type == "Different" & vil.perm$village == "Dagoretti South","p.value"] %>% p.adjust(., method = "bonferroni")
vil.perm$adj.p[vil.perm$HH_type == "Different" & vil.perm$village == "Kibera"] = vil.perm[vil.perm$HH_type == "Different" & vil.perm$village == "Kibera","p.value"] %>% p.adjust(., method = "bonferroni")
vil.perm.sig = vil.perm %>% filter(vil.perm$adj.p < 0.05) #take only significant results
# Save in to a file
write.table(vil.perm, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/vil.perm_update_re.tsv"), sep = "\t", row.names = F, quote = F)

##Within vs. between by village
vil.perm.wit.bt = data.frame(matrix(ncol = 5))
colnames(vil.perm.wit.bt) = c("village","subcategory", "Same_mean", "Different_mean", "p.value")
n=1
for (j in strain.share.subcount$subcount_type %>% unique()) {
  #print(j)
  for (i in strain.share.subcount[strain.share.subcount$HH_type == "Same", "subcategory"] %>% unique()) {
    #print(i)
    a = strain.share.subcount[strain.share.subcount$HH_type == "Same" & strain.share.subcount$subcategory == i & strain.share.subcount$subcount_type == j,]
    b = strain.share.subcount[strain.share.subcount$HH_type == "Different" & strain.share.subcount$subcategory == i & strain.share.subcount$subcount_type == j,]
    
    vil.perm.wit.bt[n,1] = j
    vil.perm.wit.bt[n,2] = i
    vil.perm.wit.bt[n,3] = mean(a$rate)
    vil.perm.wit.bt[n,4] = mean(b$rate)
    vil.perm.wit.bt[n,5] = permutation.test(a$rate, b$rate)
    n=n+1
  }
}

vil.perm.wit.bt[vil.perm.wit.bt$village == "Kibera", "adj.p"] = vil.perm.wit.bt[vil.perm.wit.bt$village == "Kibera", "p.value"] %>% p.adjust(., method = "BH")
vil.perm.wit.bt[vil.perm.wit.bt$village == "Dagoretti South", "adj.p"] = vil.perm.wit.bt[vil.perm.wit.bt$village == "Dagoretti South", "p.value"] %>% p.adjust(., method = "BH")

# Save into a file
write.table(vil.perm.wit.bt,file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/vil.perm.wit.bt_update_re.tsv"), sep = "\t", row.names = F, quote = F)

##Kibera vs. Dagoretti South
vil.perm.kib.dag = data.frame(matrix(ncol = 5))
colnames(vil.perm.kib.dag) = c("subcategory", "HH_type","Kib_mean", "Dag_mean", "p.value")

n=1
for (i in c("Same", "Different")) {
  for (j in strain.share.subcount[strain.share.subcount$HH_type == i, "subcategory"] %>% unique()) {
    a = strain.share.subcount[strain.share.subcount$HH_type == i & strain.share.subcount$subcategory == j & strain.share.subcount$subcount_type == "Kibera",]
    b = strain.share.subcount[strain.share.subcount$HH_type == i & strain.share.subcount$subcategory == j & strain.share.subcount$subcount_type == "Dagoretti South",]
    
    vil.perm.kib.dag[n,1] = j
    vil.perm.kib.dag[n,2] = i
    vil.perm.kib.dag[n,3] = mean(a$rate)
    vil.perm.kib.dag[n,4] = mean(b$rate)
    vil.perm.kib.dag[n,5] = permutation.test(a$rate, b$rate)
    n=n+1
  }
}

vil.perm.kib.dag[vil.perm.kib.dag$HH_type == "Same", "adj.p"] = vil.perm.kib.dag[vil.perm.kib.dag$HH_type == "Same", "p.value"] %>% p.adjust(., method = "BH")
vil.perm.kib.dag[vil.perm.kib.dag$HH_type == "Different", "adj.p"] = vil.perm.kib.dag[vil.perm.kib.dag$HH_type == "Different", "p.value"] %>% p.adjust(., method = "BH")
# Save into a file
write.table(vil.perm.kib.dag, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/vil.perm.kib.dag_update_re.tsv"), sep = "\t", row.names = F, quote = F)

#Comparison of within strain-sharing rates between households with water contamination
##Load sample list 
sample.list = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/data/sample_list.txt"), sep = '\t')
colnames(sample.list) = "sample"
sample.list$household = substr(sample.list$sample, 3, 5)
#Environmental samples that E. coli was not grown on an agar plate
env.e.coli = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/additional_resources/KenyaES1DataEntryForm-ecoli_soilwater_samples-CLEANED-20210326.csv"))
#Get information of households included in this study for stored water and hhsoil
env.e.coli$hhid = substr(env.e.coli$hh_id_5dig_ecsoilwat,3,5)
env.e.coli = env.e.coli[(env.e.coli$hhid %in% (sample.list$household %>% unique())) & (env.e.coli$sample_type_ecsoilwat %in% c("stored", "hhsoil")),c("hhid", "sample_type_ecsoilwat", "ecoli_soilwater_pos", "ecoli_soilwater_Comments_text")]
env.e.coli.w = env.e.coli[env.e.coli$sample_type_ecsoilwat == "stored",]
#Add subcounty info
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$HH_ID = substr(hh_survey$hh_id_5dig, 3,5)
env.e.coli.w = merge(env.e.coli.w, hh_survey[,c("HH_ID", "subcountyid")], by.x = "hhid", by.y = "HH_ID")
#Get HHID with or without water contamination
hh.w.pos = env.e.coli.w[env.e.coli.w$ecoli_soilwater_pos == 1,"hhid"]
hh.w.neg = env.e.coli.w[env.e.coli.w$ecoli_soilwater_pos == 0,"hhid"]

w.contam.comp = data.frame(matrix(ncol = 6))
colnames(w.contam.comp) = c("subcategory","w.con.pos.mean", "w.con.neg.mean", "p.value","w.con.pos.prev", "w.con.neg.prev")

n=1
for (i in strain.share[strain.share$HH_type == "Same","subcategory"] %>% unique()) {
  wp = strain.share[strain.share$HH_type == "Same" & strain.share$subcategory == i & (strain.share$HH1 %in% hh.w.pos),"rate"] #HH with water contamination
  wn = strain.share[strain.share$HH_type == "Same" & strain.share$subcategory == i & (strain.share$HH1 %in% hh.w.neg),"rate"] #HH without water contamination
  set.seed(1992)
  rep1 = replicate(1000, sample(c(wp,wn), replace = F))
  a1 = rep1[1:length(wp),]
  b1 = rep1[(length(wp)+1):(length(wp) + length(wn)),]
  if(length(wp) > 1 & length(wn) > 1){
    diffs = apply(a1, 2, mean) - apply(b1, 2, mean)
  }
  else{
    
  }
  if(length(wp) <= 1 | length(wn) <= 1){
    p = NA
  }
  if(mean(wp) == 0 & mean(wn) == 0){
    p = NA
  }
  else{
    p = sum(abs(diffs) >= (abs(mean(wp) - mean(wn))))/1000
  }
  w.contam.comp[n,1] = i
  w.contam.comp[n,2] = mean(wp)
  w.contam.comp[n,3] = mean(wn)
  w.contam.comp[n,4] = p
  w.contam.comp[n,5] = paste0(wp[wp>0] %>% length(),"/",length(wp))
  w.contam.comp[n,6] = paste0(wp[wn>0] %>% length(),"/",length(wn))
  
  n=n+1
}
w.contam.comp$subcounty = "Overall"
# at Subcounty level
w.contam.sub.comp = data.frame(matrix(ncol = 7))
colnames(w.contam.sub.comp) = c("subcounty","subcategory","w.con.pos.mean", "w.con.neg.mean", "p.value","w.con.pos.prev", "w.con.neg.prev")

n=1
for (sub in c("Kibera", "Dagoretti South")) {
  for (i in strain.share[strain.share$HH_type == "Same","subcategory"] %>% unique()) {
    wp = strain.share[strain.share$HH1_subcountyid == sub & strain.share$HH_type == "Same" & strain.share$subcategory == i & (strain.share$HH1 %in% hh.w.pos),"rate"] #HH with water contamination
    wn = strain.share[strain.share$HH1_subcountyid == sub & strain.share$HH_type == "Same" & strain.share$subcategory == i & (strain.share$HH1 %in% hh.w.neg),"rate"] #HH without water contamination
    set.seed(1992)
    rep1 = replicate(1000, sample(c(wp,wn), replace = F))
    a1 = rep1[1:length(wp),]
    b1 = rep1[(length(wp)+1):(length(wp) + length(wn)),]
    if(length(wp) > 1 & length(wn) > 1){
      diffs = apply(a1, 2, mean) - apply(b1, 2, mean)
    }
    else{
      
    }
    if(length(wp) <= 1 | length(wn) <= 1){
      p = NA
    }
    else if(mean(wp) == 0 & mean(wn) == 0){
      p = NA
    }
    else{
      p = sum(abs(diffs) >= (abs(mean(wp) - mean(wn))))/1000
    }
    w.contam.sub.comp[n,1] = sub
    w.contam.sub.comp[n,2] = i
    w.contam.sub.comp[n,3] = mean(wp)
    w.contam.sub.comp[n,4] = mean(wn)
    w.contam.sub.comp[n,5] = p
    w.contam.sub.comp[n,6] = paste0(wp[wp>0] %>% length(),"/",length(wp))
    w.contam.sub.comp[n,7] = paste0(wp[wn>0] %>% length(),"/",length(wn))
    
    n=n+1
  }
}

w.contam.all.comp = data.frame(rbind(w.contam.comp, w.contam.sub.comp))
w.contam.all.comp.m = melt(w.contam.all.comp)

w.contam.all.comp1.m = melt(w.contam.all.comp[,c(1:3,7)], id.vars = c("subcategory", "subcounty"))
colnames(w.contam.all.comp1.m) = c("subcategory", "subcounty", "contam", "mean")
w.contam.all.comp1.m$contam = as.character(w.contam.all.comp1.m$contam)
w.contam.all.comp1.m[w.contam.all.comp1.m$contam == "w.con.pos.mean","contam"] = "E. coli positive"
w.contam.all.comp1.m[w.contam.all.comp1.m$contam == "w.con.neg.mean","contam"] = "E. coli negative"

w.contam.all.comp2.m = melt(w.contam.all.comp[,c(1,5:7)], id.vars = c("subcategory", "subcounty"))
colnames(w.contam.all.comp2.m) = c("subcategory", "subcounty", "contam", "prevlaence")
w.contam.all.comp2.m$contam = as.character(w.contam.all.comp2.m$contam)
w.contam.all.comp2.m[w.contam.all.comp2.m$contam == "w.con.pos.prev","contam"] = "E. coli positive"
w.contam.all.comp2.m[w.contam.all.comp2.m$contam == "w.con.neg.prev","contam"] = "E. coli negative"

w.contam.all.comp.m = merge(w.contam.all.comp1.m, w.contam.all.comp2.m, by = c("subcategory", "subcounty", "contam"))
w.contam.all.comp.m$num.hh.pos = sapply(strsplit(w.contam.all.comp.m$prevlaence, "/"), `[`, 1)
w.contam.all.comp.m$num.hh.all = sapply(strsplit(w.contam.all.comp.m$prevlaence, "/"), `[`, 2)
w.contam.all.comp.m$prev.rate = as.numeric(w.contam.all.comp.m$num.hh.pos)/as.numeric(w.contam.all.comp.m$num.hh.all)*100
w.contam.all.comp.m$prevlaence = paste0("(",w.contam.all.comp.m$prevlaence,")")

w.contam.all.comp.m = w.contam.all.comp.m[w.contam.all.comp.m$subcategory != "human_water_source",]
w.contam.all.comp.m$subcounty = factor(w.contam.all.comp.m$subcounty, levels = c("Overall", "Dagoretti South", "Kibera"))
w.contam.all.comp.m$contam = factor(w.contam.all.comp.m$contam, levels = c("E. coli positive", "E. coli negative"))
w.contam.all.comp.m$subcategory = factor(w.contam.all.comp.m$subcategory , levels = c("human_human", "poultry_poultry", "dog_dog", "water_source_water_source", "soil_soil", "human_poultry", "dog_human", "human_water_source", "human_soil", "dog_poultry", "poultry_water_source", "poultry_soil", "dog_water_source", "dog_soil", "soil_water_source"), labels = c("Human - Human", "Poultry - Poultry", "Dog - Dog", "Stored water - Stored water", "Household soil - Household soil", "Human - Poultry", "Human - Dog", "Human - Stored water", "Human - Household soil", "Poultry - Dog", "Poultry - Stored water", "Poultry - Household soil", "Dog - Stored water", "Dog - Household soil", "Stored water - Household soil"))
#Save into a file
write.csv(w.contam.all.comp.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/w.contam.all.comp.m_re.csv"), quote = F, row.names = F)
saveRDS(w.contam.all.comp.m, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/w.contam.all.comp.m_re.rds"))


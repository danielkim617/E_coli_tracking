source(file.path(here::here(),"0-config.R"))
##Load sample list 
sample.list = read.table(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/data/sample_list.txt"), sep = '\t')
colnames(sample.list) = "sample"
# Add household id
sample.list$household = substr(sample.list$sample, 3, 5)

#Add stored water samples without contamination
#Environmental samples that E. coli was not grown on an agar plate
env.e.coli = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/additional_resources/KenyaES1DataEntryForm-ecoli_soilwater_samples-CLEANED-20210326.csv"))
#Get information of households included in this study for stored water and hhsoil
env.e.coli$hhid = substr(env.e.coli$hh_id_5dig_ecsoilwat,3,5)
env.e.coli = env.e.coli[(env.e.coli$hhid %in% (sample.list$household %>% unique())) & (env.e.coli$sample_type_ecsoilwat %in% c("stored", "hhsoil")),c("hhid", "sample_type_ecsoilwat", "ecoli_soilwater_pos", "ecoli_soilwater_Comments_text")]
ext.water.sample = env.e.coli[env.e.coli$ecoli_soilwater_pos == 0 & env.e.coli$sample_type_ecsoilwat == "stored","hhid"] %>% paste0("W1",.)
sample.list.tmp = as.data.frame(c(sample.list$sample, ext.water.sample))

### Repeat steps
sample.list = sample.list.tmp
colnames(sample.list) = "sample"
#Add sample description
sample.list$household = NA
sample.list$host_type = NA
sample.list$spec_host = NA
sample.list$household = substr(sample.list$sample, 3, 5)

###### Add sample information
for (i in 1:nrow(sample.list)) {
  print(sample.list[i,1])
  if(startsWith(sample.list[i,1], "A")){
    sample.list[i,3] =  "human"
    sample.list[i,4] =  "human" #When adult, child, older child are combined as human
    #sample.list[i,4] =  "adult" 
  }
  if(startsWith(sample.list[i,1], "C")){
    sample.list[i,3] =  "human"
    sample.list[i,4] =  "human" #When adult, child, older child are combined as human
    #sample.list[i,4] =  "child"
  }
  if(startsWith(sample.list[i,1], "O")){
    sample.list[i,3] =  "human"
    sample.list[i,4] =  "human" #When adult, child, older child are combined as human
    #sample.list[i,4] =  "older_child"
  }
  if(startsWith(sample.list[i,1], "P")){
    sample.list[i,3] =  "animal"
    sample.list[i,4] =  "poultry"
  }
  if(startsWith(sample.list[i,1], "D")){
    sample.list[i,3] =  "animal"
    sample.list[i,4] =  "dog"
  }
  if(startsWith(sample.list[i,1], "W")){
    sample.list[i,3] =  "environment"
    sample.list[i,4] =  "water_source"
  }
  if(startsWith(sample.list[i,1], "S")){
    sample.list[i,3] =  "environment"
    sample.list[i,4] =  "soil"
  }
}

#Save into a file
saveRDS(sample.list,  file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/sample.list.rds"))

#Get proportion of water samples detected with E.coli
#Environmental samples that E. coli was not grown on an agar plate
env.e.coli = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/additional_resources/KenyaES1DataEntryForm-ecoli_soilwater_samples-CLEANED-20210326.csv"))
#Get information of households included in this study for stored water and hhsoil
env.e.coli$hhid = substr(env.e.coli$hh_id_5dig_ecsoilwat,3,5)
env.e.coli = env.e.coli[(env.e.coli$hhid %in% (sample.list$household %>% unique())),c("hhid", "sample_type_ecsoilwat", "ecoli_soilwater_pos", "ecoli_soilwater_Comments_text")]
env.e.coli = env.e.coli[(env.e.coli$hhid %in% (sample.list$household %>% unique())) & (env.e.coli$sample_type_ecsoilwat %in% c("stored", "hhsoil")),c("hhid", "sample_type_ecsoilwat", "ecoli_soilwater_pos", "ecoli_soilwater_Comments_text")]
env.e.coli.w = env.e.coli[env.e.coli$sample_type_ecsoilwat == "stored",]

#Add subcounty info
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$HH_ID = substr(hh_survey$hh_id_5dig, 3,5)

env.e.coli.w = merge(env.e.coli.w, hh_survey[,c("HH_ID", "subcountyid")], by.x = "hhid", by.y = "HH_ID")

stored.pos = data.frame(matrix(ncol=4))
colnames(stored.pos) = c("cat", "num.pos", "total", "percent")
#overall
stored.pos[1,1] = "overall"
stored.pos[1,2] = env.e.coli.w[env.e.coli.w$ecoli_soilwater_pos == 1,"hhid"] %>% length # 1 means colony detected
stored.pos[1,3] = env.e.coli.w[env.e.coli.w$ecoli_soilwater_pos == 1 | env.e.coli.w$ecoli_soilwater_pos == 0 ,"hhid"] %>% length
stored.pos[1,4] = stored.pos[1,2]/stored.pos[1,3] * 100
#Dagoretti South
stored.pos[2,1] = "Dagoretti South"
stored.pos[2,2] = env.e.coli.w[env.e.coli.w$ecoli_soilwater_pos == 1 & env.e.coli.w$subcountyid == "Dagoretti South","hhid"] %>% length
stored.pos[2,3] = env.e.coli.w[(env.e.coli.w$ecoli_soilwater_pos == 1 |env.e.coli.w$ecoli_soilwater_pos == 0) & env.e.coli.w$subcountyid == "Dagoretti South","hhid"] %>% length
stored.pos[2,4] = stored.pos[2,2]/stored.pos[2,3] * 100
#Kibera
stored.pos[3,1] = "Kibera"
stored.pos[3,2] = env.e.coli.w[env.e.coli.w$ecoli_soilwater_pos == 1 & env.e.coli.w$subcountyid == "Kibera","hhid"] %>% length
stored.pos[3,3] = env.e.coli.w[(env.e.coli.w$ecoli_soilwater_pos == 1 |env.e.coli.w$ecoli_soilwater_pos == 0) & env.e.coli.w$subcountyid == "Kibera","hhid"] %>% length
stored.pos[3,4] = stored.pos[3,2]/stored.pos[3,3] * 100

stored.pos$cat = factor(stored.pos$cat, levels = c("overall", "Dagoretti South", "Kibera"))
# Save into a file
saveRDS(stored.pos, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/stored.pos.rds"))

#Statistical testing (Fisher's test)
wat.tab = stored.pos[2:3, 2:3]
rownames(wat.tab) = stored.pos[2:3,1]
colnames(wat.tab) = c("pos", "neg")
wat.tab$neg = wat.tab$neg - wat.tab$pos
fisher.test(wat.tab) %>% print()
# Perform the Fisher's test and capture the output
fisher_output <- capture.output(fisher.test(wat.tab))
# Save the output to a text file
writeLines(fisher_output, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/stored.pos_fisher.tsv"))

# When all households are included (HHs without sequence data)
wat.tab1 = matrix(ncol = 2, nrow = 2)
wat.tab1[1:2,1] = c(30,14)
wat.tab1[1:2,2] = c(14,48)

# Perform the Fisher's test and capture the output
fisher_output_all <- capture.output(fisher.test(wat.tab1))
# Save the output to a text file
writeLines(fisher_output_all, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/stored.pos_fisher_all.tsv"))


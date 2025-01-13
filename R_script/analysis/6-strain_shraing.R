source(file.path(here::here(),"0-config.R"))
#Load data
sample.list = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/sample.list.rds"))
strainge.compare.99.95 = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strainge.compare.99.95_update.rds"))
#Calculation of strain-sharing events for within and between households
#sharing comparison
strain.share = data.frame(matrix(ncol = 10))
colnames(strain.share) = c("HH1", "HH2", "host1", "host2", "pairs", "all_pairs", "rate", "category", "subcategory", "HH_type")

n=1
for (hh1 in unique(sort(sample.list$household))) {
  print(hh1)
  for (hh2 in unique(sort(sample.list$household))) {
    #To remove duplicate
    if(hh2 %in% strain.share$HH1){
      next
    }
    #
    for (host1 in unique(sort(sample.list$spec_host))) {
      for (host2 in unique(sort(sample.list$spec_host))) {
        #To remove duplicate
        if(host2 %in% subset(strain.share, strain.share$HH1==hh1 & strain.share$HH2==hh2)$host1){
          next
        }
        #
        sample.list.hh1 = subset(sample.list, sample.list$household == hh1)
        sample.list.hh2 = subset(sample.list, sample.list$household == hh2)
        #host1
        if(host1 =="human"){
          pat1 = c("A|O|C")
        }
        if(host1 =="adult"){
          pat1 = "A"
        }
        if(host1 =="child"){
          pat1 = "C"
        }
        if(host1 =="older_child"){
          pat1 = "O"
        }
        if(host1 =="poultry"){
          pat1 = "P"
        }
        if(host1 =="dog"){
          pat1 = "D"
        }
        if(host1 =="soil"){
          pat1 = "S"
        }
        if(host1 =="water_source"){
          pat1 = "W"
        }
        #host2
        if(host2 =="human"){
          pat2 = c("A|O|C")
        }
        if(host2 =="adult"){
          pat2 = "A"
        }
        if(host2 =="child"){
          pat2 = "C"
        }
        if(host2 =="older_child"){
          pat2 = "O"
        }
        if(host2 =="poultry"){
          pat2 = "P"
        }
        if(host2 =="dog"){
          pat2 = "D"
        }
        if(host2 =="soil"){
          pat2 = "S"
        }
        if(host2 =="water_source"){
          pat2 = "W"
        }
        strainge.compare.99.95.hh = subset(strainge.compare.99.95, (strainge.compare.99.95$sample1_hh == hh1 & strainge.compare.99.95$sample2_hh == hh2) | (strainge.compare.99.95$sample1_hh == hh2 & strainge.compare.99.95$sample2_hh == hh1))
        strainge.compare.99.95.host = subset(strainge.compare.99.95.hh, (grepl(pat1, strainge.compare.99.95.hh$sample1) & grepl(pat2, strainge.compare.99.95.hh$sample2)) | (grepl(pat2, strainge.compare.99.95.hh$sample1) & grepl(pat1, strainge.compare.99.95.hh$sample2)))
        
        strain.share[n,1] = hh1
        strain.share[n,2] = hh2
        strain.share[n,3] = host1
        strain.share[n,4] = host2
        #within household
        if(hh1 == hh2){
          #if host type are the same
          if(host1 == host2){
            
            n_host1 = sum(str_count(sample.list.hh1$spec_host, paste0("^",host1)))
            
            strain.share[n,5] = nrow(strainge.compare.99.95.host[,1:2] %>% unique())
            strain.share[n,6] = (n_host1*(n_host1-1))/2
            strain.share[n,7] = strain.share[n,5]/strain.share[n,6]
            n = n+1
          }
          if(host1 != host2){
            n_host1 = sum(str_count(sample.list.hh1$spec_host, paste0("^",host1)))
            n_host2 = sum(str_count(sample.list.hh2$spec_host, paste0("^",host2)))
            
            strain.share[n,5] = nrow(strainge.compare.99.95.host[,1:2] %>% unique())
            strain.share[n,6] = n_host1*n_host2
            strain.share[n,7] = strain.share[n,5]/strain.share[n,6]
            n = n+1
          }
        }
        #between household
        if(hh1 != hh2){
          if(host1 == host2){
            n_host1 = sum(str_count(sample.list.hh1$spec_host, paste0("^",host1)))
            n_host2 = sum(str_count(sample.list.hh2$spec_host, paste0("^",host2)))
            
            strain.share[n,5] = nrow(strainge.compare.99.95.host[,1:2] %>% unique())
            strain.share[n,6] = n_host1*n_host2
            strain.share[n,7] = strain.share[n,5]/strain.share[n,6]
            n = n+1
          }
          if(host1 != host2){
            n_host1 = sum(str_count(sample.list.hh1$spec_host, paste0("^",host1)))
            n_host2 = sum(str_count(sample.list.hh2$spec_host, paste0("^",host2)))
            n_host3 = sum(str_count(sample.list.hh1$spec_host, paste0("^",host2)))
            n_host4 = sum(str_count(sample.list.hh2$spec_host, paste0("^",host1)))
            
            strain.share[n,5] = nrow(strainge.compare.99.95.host[,1:2] %>% unique())
            strain.share[n,6] = n_host1*n_host2 + n_host3*n_host4
            strain.share[n,7] = strain.share[n,5]/strain.share[n,6]
            n = n+1
            
          }
          
        }
      }
    }
  }
}

#Remove impossible matches
strain.share = subset(strain.share, strain.share$all_pairs > 0)
#Add categories
for (i in 1:nrow(strain.share)) {
  
  strain.share[i,9] = paste0(strain.share[i,3],"_",strain.share[i,4] )
  if(strain.share[i,1] == strain.share[i,2]){
    
    strain.share[i,10] = "Same"
    
  }
  else{
    strain.share[i,10] = "Different"
  }
  if(strain.share[i,9] %in% c("adult_adult", "adult_child" , "adult_older_child", "child_child", "child_older_child" , "older_child_older_child", "human_human")){
    strain.share[i,8] = "human-human"
  }
  if(strain.share[i,9] %in% c("dog_dog", "dog_poultry", "poultry_poultry" )){
    strain.share[i,8] = "animal-animal"
  }
  if(strain.share[i,9] %in% c("soil_soil","soil_water_source",   "water_source_water_source")){
    strain.share[i,8] = "environment-environment"
  }
  if(strain.share[i,9] %in% c("adult_dog", "adult_poultry", "child_dog", "child_poultry", "dog_older_child", "older_child_poultry" , "dog_human", "human_poultry")){
    strain.share[i,8] = "human-animal"
  }
  if(strain.share[i,9] %in% c("adult_soil", "adult_water_source","child_soil" , "child_water_source" ,"older_child_soil","older_child_water_source", "human_soil", "human_water_source")){
    strain.share[i,8] = "human-environment"
  }
  if(strain.share[i,9] %in% c("dog_soil","dog_water_source","poultry_soil" ,"poultry_water_source")){
    strain.share[i,8] = "animal-environment"
  }
}

#Save data
write.table(strain.share, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strain.share_updated.tsv"), sep = "\t", row.names = F, quote = F) # after update
saveRDS(strain.share, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strain.share_updated.rds")) # after update



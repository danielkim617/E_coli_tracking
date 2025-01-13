source(file.path(here::here(),"0-config.R"))

#Parsing straingst results from illumina data of the actual samples
straingst = read.table(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_results.tsv"), sep = "\t")
straingst.table = data.frame(matrix(ncol = 8))
colnames(straingst.table) = c("sample", "sample_type", "host","house hold", "strain", "phylotype", "score", "number of strains")

n = 1
for(l in 1:nrow(straingst)){
  
  if(straingst[l,1] == "i"){
    for (m in 1:5) {
      #sample
      straingst.table[n, 1] = straingst[l-1,1]
      #sample_type
      straingst.table[n, 5] = straingst[l+m,2]
      #strain
      straingst.table[n, 7] = straingst[l+m,15]
      #house hold
      straingst.table[n, 4] = gsub("_straingst", "", straingst[l-1,1]) %>% substr(3,5)
      #sample type
      straingst.table[n, 2] = substr(straingst[l-1,1], 1,2)
      
      if(startsWith(straingst.table[n, 2], "A")){
        #host
        straingst.table[n, 3] = "adult"
        
      }
      if(startsWith(straingst.table[n, 2], "C")){
        #host
        straingst.table[n, 3] = "child"
        
      }
      if(startsWith(straingst.table[n, 2], "O")){
        #host
        straingst.table[n, 3] = "older_child"
        
      }
      if(startsWith(straingst.table[n, 2], "P")){
        #host
        straingst.table[n, 3] = "poultry"
        
      }
      if(startsWith(straingst.table[n, 2], "D")){
        #host
        straingst.table[n, 3] = "dog"
        
      }
      if(startsWith(straingst.table[n, 2], "W1")){
        #host
        straingst.table[n, 3] = "stored water"
        
      }
      if(startsWith(straingst.table[n, 2], "W2")){
        #host
        straingst.table[n, 3] = "source water"
        
      }
      if(startsWith(straingst.table[n, 2], "S1")){
        #host
        straingst.table[n, 3] = "household soil"
        
      }
      if(startsWith(straingst.table[n, 2], "S2")){
        #host
        straingst.table[n, 3] = "water source soil"
        
      }
      if(straingst[l+m,1] == "sample" | is.na(straingst[l+m,1])){
        
        #print(m-1)
        break
      }
      n=n+1
    }
  }
}

#Remove NA
straingst.table = straingst.table[complete.cases(straingst.table[,5]), ]

#save to a file
write.table(straingst.table,file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_network_re.tsv"), sep = "\t", quote = F, row.names = F)
saveRDS(straingst.table,file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_network_re.rds"))

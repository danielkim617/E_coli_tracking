source(file.path(here::here(),"0-config.R"))

#####For bootstrapped analysis
strain.share = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strain.share_updated.rds"))
ARG.share.table.clstr = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ARG.share.table.clstr.rds"))

strain.share1 = strain.share
strain.share1$subcategory = factor(strain.share1$subcategory, levels = c("human_human", "poultry_poultry", "dog_dog", "soil_soil", "water_source_water_source", "human_poultry", "dog_human", "human_water_source", "human_soil", "dog_poultry", "poultry_water_source", "poultry_soil", "dog_water_source", "dog_soil", "soil_water_source"), labels = c("Human - Human", "Poultry - Poultry", "Dog - Dog", "Household soil - Household soil", "Stored water - Stored water","Human - Poultry", "Human - Dog", "Human - Stored water", "Human - Household soil", "Poultry - Dog", "Poultry - Stored water", "Poultry - Household soil", "Dog - Stored water", "Dog - Household soil", "Stored water - Household soil"))

hh.list = strain.share$HH1 %>% unique()
set.seed(1992)
hh_1000 = replicate(1000, sample(hh.list, replace = T)) %>% as.data.frame()

iter.rate = data.frame(matrix(ncol = 7))
colnames(iter.rate) = c("iter", "HH_type", "cat", "rate", "overall.ARG", "non.ARG", "mob.ARG")

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
        #iter.rate[n,5] = tmp6[tmp6$HH_type == "Within", "kmer.sim"] %>% na.omit() %>% mean()
        iter.rate[n,5] = tmp6[tmp6$HH_type == "Within", "overall.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,6] = tmp6[tmp6$HH_type == "Within", "non.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,7] = tmp6[tmp6$HH_type == "Within", "mob.jac.sim"] %>% na.omit() %>% mean()
        n=n+1
      }
      if(h == "Between"){
        iter.rate[n,1] = i
        iter.rate[n,2] = h
        iter.rate[n,3] = k
        iter.rate[n,4] = tmp3[tmp3$HH_type == "Different", "rate"] %>% na.omit() %>% mean()
        #iter.rate[n,5] = tmp6[tmp6$HH_type == "Between", "kmer.sim"] %>% na.omit() %>% mean()
        iter.rate[n,5] = tmp6[tmp6$HH_type == "Between", "overall.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,6] = tmp6[tmp6$HH_type == "Between", "non.jac.sim"] %>% na.omit() %>% mean()
        iter.rate[n,7] = tmp6[tmp6$HH_type == "Between", "mob.jac.sim"] %>% na.omit() %>% mean()
        n=n+1
      }
    }
    #print(i)
  }
}

# Run correlation and get distributions of coefficients
iter.cor = data.frame(matrix(ncol=5))
colnames(iter.cor) = c("iter", "HH_type", "rate.vs.overall", "rate.vs.non", "rate.vs.mob")
n=1
for (i in 1:1000) {
  for (j in c("Within", "Between")) {
    iter.cor[n, "iter"] = i
    iter.cor[n, "HH_type"] = j
    
    a = iter.rate[iter.rate$iter == i & iter.rate$HH_type == j,]
    # correlation rate vs. ARGs
    c = cor.test(a$rate, a$overall.ARG, method = "spear", exact = F)
    iter.cor[n, "rate.vs.overall"] = c$estimate
    
    d = cor.test(a$rate, a$non.ARG, method = "spear", exact = F)
    iter.cor[n, "rate.vs.non"] = d$estimate
    
    e = cor.test(a$rate, a$mob.ARG, method = "spear", exact = F)
    iter.cor[n, "rate.vs.mob"] = e$estimate
    
    n=n+1
  } 
}

iter.cor.m = melt(iter.cor, id.vars = c("iter", "HH_type"))
iter.cor.m$HH_type = factor(iter.cor.m$HH_type, levels = c("Within", "Between"))
#For source data
write.csv(iter.cor, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/iter.cor_re.csv") , quote = F, row.names = F)

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

iter.cor.m[iter.cor.m$HH_type == "Within" & iter.cor.m$variable == "rate.vs.overall","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)
iter.cor.m[iter.cor.m$HH_type == "Within" & iter.cor.m$variable == "rate.vs.non","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)
iter.cor.m[iter.cor.m$HH_type == "Within" & iter.cor.m$variable == "rate.vs.mob","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)

iter.cor.m[iter.cor.m$HH_type == "Between" & iter.cor.m$variable == "rate.vs.overall","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)
iter.cor.m[iter.cor.m$HH_type == "Between" & iter.cor.m$variable == "rate.vs.non","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)
iter.cor.m[iter.cor.m$HH_type == "Between" & iter.cor.m$variable == "rate.vs.mob","value"] %>% wilcox.test(.,conf.int=TRUE,conf.level=0.95)

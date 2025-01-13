#function for permutation test
permutation.test = function(a,b){
  set.seed(1992)
  rep1 = replicate(1000, sample(c(a,b), replace = F))
  a1 = rep1[1:length(a),]
  b1 = rep1[(length(a)+1):(length(a) + length(b)),]
  
  diffs = apply(a1, 2, mean) - apply(b1, 2, mean)
  if(mean(a) == 0 & mean(b) == 0){
    p = NA
  }
  else{
    p = sum(abs(diffs) >= (abs(mean(a) - mean(b))))/1000
  }
  return(p)
}

# Function for ARG data cleaning
ARG.clean = function(ratio_ARG, ratio_mob, all.ratio.flanking){
  a = ratio_ARG
  b = ratio_mob
  
  a$sample = substr(a[,1],1,5)
  a$ref = gsub("_NODE.*", "", a[,1]) %>% substr(.,7,length(.))
  
  ARG.table = data.frame(matrix(ncol = 5))
  colnames(ARG.table) = c("sample", "ref", "cds","ARG","mobility")
  
  n=1
  for (s in a$sample %>% unique()) {
    for (r in a[a$sample == s,"ref"] %>% unique) {
      for (arg in a[a$sample == s & a$ref == r, "V2"] %>% unique()) {
        c = a[a$sample == s & a$ref == r & a$V2 == arg, ]
        for ( copy in 1:(c$V1 %>% length())) {
          
          ARG.table[n,"sample"] = s
          ARG.table[n,"ref"] = r
          ARG.table[n,"cds"] = c[copy,1]
          ARG.table[n,"ARG"] = arg
          
          if(grepl(c[copy,1],b$Specific.Contig) %>% sum() > 0){
            ARG.table[n,"mobility"] = "mobile"
          }
          else if(c$ref[copy] == "plasmid"){
            ARG.table[n,"mobility"] = "mobile"
          }
          else{
            ARG.table[n,"mobility"] = "non-mobile"
          }
          n=n+1
        }
      }
    }
  }
  #all.ratio.flanking$new_id = gsub("^*.*_cov_.*_[0-9]*-[0-9]*_","",all.ratio.flanking$V1)
  all.ratio.flanking$new_id = gsub("^.*[0-9]*-[0-9]*_A","A",all.ratio.flanking$V1)
  all.ratio.flanking$new_id = gsub("^.*[0-9]*-[0-9]*_C","C",all.ratio.flanking$new_id)
  all.ratio.flanking$new_id = gsub("^.*[0-9]*-[0-9]*_O","O",all.ratio.flanking$new_id)
  all.ratio.flanking$new_id = gsub("^.*[0-9]*-[0-9]*_P","P",all.ratio.flanking$new_id)
  all.ratio.flanking$new_id = gsub("^.*[0-9]*-[0-9]*_D","D",all.ratio.flanking$new_id)
  all.ratio.flanking$new_id = gsub("^.*[0-9]*-[0-9]*_W","W",all.ratio.flanking$new_id)
  all.ratio.flanking$new_id = gsub("^.*[0-9]*-[0-9]*_S","S",all.ratio.flanking$new_id)
  ARG.table = merge(ARG.table, all.ratio.flanking[,c("new_id", "size")], by.x="cds", by.y = "new_id")
  ARG.table[ARG.table$ref == "plasmid", "size"] = gsub("^.*length_", "", ARG.table[ARG.table$ref == "plasmid", "cds"]) %>% gsub("_cov.*$", "", .)
  ARG.table[ARG.table$mobility == "non-mobile" & as.numeric(ARG.table$size) < 5000,"mobility"] = "not known"
  return(ARG.table)
}

rankI_ARG_cluster_drugclass = function(x){
  #Set seeds
  set.seed(1992)
  num = 10 # Number of samples to subsample
  h = replicate(1000,sample(sample.list %>% filter(grepl("A|O|C", sample)) %>% unique() %>% unlist, num, replace = F))
  p = replicate(1000,sample(sample.list %>% filter(grepl("P", sample)) %>% unique() %>% unlist, num, replace = F))
  d = replicate(1000,sample(sample.list %>% filter(grepl("D", sample)) %>% unique() %>% unlist, num, replace = F))
  w = replicate(1000,sample(sample.list %>% filter(grepl("W", sample)) %>% unique() %>% unlist, num, replace = F))
  s = replicate(1000,sample(sample.list %>% filter(grepl("S", sample)) %>% unique() %>% unlist, num, replace = F))
  
  tmp = data.frame(matrix(ncol=1002))
  sample_types = c("human", "poultry", "dog", "water", "soil")
  # Assuming 'num' is defined somewhere in your code
  # Loop through each sample type
  for (i in 1:length(sample_types)) {
    tmp[i, 1] = num
    tmp[i, 2] = sample_types[i]
    # Select the appropriate sample set based on the sample type
    sample_set = switch(sample_types[i],"human" = h, "poultry" = p, "dog" = d,"water" = w,"soil" = s)
    
    for (j in 1:1000) {
      a = all_sample_ARG_clean.clstr.rankI %>% filter(sample %in% sample_set[, j] & drug_class == x)
      tmp[i, j + 2] = length(unique(a$cluster))
    }
  }
  num.cluster.host = tmp
  num.cluster.host.m = num.cluster.host %>% melt(id=c("X1", "X2"))
  colnames(num.cluster.host.m) = c("n", "host", "iter", "num.uniq.clusters")
  num.cluster.host.m$host = factor(num.cluster.host.m$host, levels = c("human","poultry", "dog", "water", "soil"), labels = c("Human", "Poultry", "Dog", "Stored water", "Household soil"))
  num.cluster.host.m$drug_class = x
  return(num.cluster.host.m)
}

# Wilcox test between categories
wilcox.bt.cat  = function(input.table, dist) {
  for (i in c("Within", "Between")) {
    if(i== "Within"){
      a = combn(input.table[input.table$HH_type == "Within", "cat"] %>% unique() %>% as.character(),2) %>% t() %>% data.frame()
      colnames(a) = c("cat1", "cat2")
      a$HH_type = "Within"
      a$cat1_mean = NA
      a$cat2_mean = NA
      a$p.value = NA
      for (j in 1:nrow(a)) {
        a$cat1_mean[j] = input.table[input.table$HH_type == "Within" & input.table$cat == a$cat1[j], dist] %>% mean()
        a$cat2_mean[j] = input.table[input.table$HH_type == "Within" & input.table$cat == a$cat2[j], dist] %>% mean()
        c = wilcox.test(input.table[input.table$HH_type == "Within" & input.table$cat == a$cat1[j], dist], input.table[input.table$HH_type == "Within" & input.table$cat == a$cat2[j], dist], exact = F)
        a$p.value[j] = c$p.value
      }
    }
    if(i== "Between"){
      b = combn(input.table[input.table$HH_type == "Between", "cat"] %>% unique() %>% as.character(),2) %>% t() %>% data.frame()
      colnames(b) = c("cat1", "cat2")
      b$HH_type = "Between"
      b$cat1_mean = NA
      b$cat2_mean = NA
      b$p.value = NA
      for (j in 1:nrow(b)) {
        b[j,"cat1_mean"] = input.table[input.table$HH_type == "Between" & input.table$cat == b$cat1[j], dist] %>% mean()
        b[j,"cat2_mean"] = input.table[input.table$HH_type == "Between" & input.table$cat == b$cat2[j], dist] %>% mean()
        c = wilcox.test(input.table[input.table$HH_type == "Between" & input.table$cat == b$cat1[j], dist], input.table[input.table$HH_type == "Between" & input.table$cat == b$cat2[j], dist], exact = F)
        b[j,"p.value"] = c$p.value
      }
    }
  }
  kmer.stat = data.frame(rbind(a,b))
  return(kmer.stat)
}

#Plotting - ratio
stat.figure = function(x){
  if(colnames(x)[1] == "Ratio1"){
    ratio = "1 (73.6x): 1 (80.6x): 1 (81.3x): 1 (77.5x): 1 (84.4x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio2"){
    ratio = "1 (88.5x): 0.1 (9.8x): 1 (99.2x): 1 (95.4x): 1 (103.0x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio3"){
    ratio = "1 (90.2x): 0.025 (2.5x): 1 (101.0x): 1 (96.3x): 1 (104.9x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio4"){
    ratio = "0.1 (2.8x): 1 (30.8x): 1 (31.0x): 1 (29.6x): 10 (322.3x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio5"){
    ratio = "0.1 (3.0x): 0.1 (3.3x): 1 (33.3x): 1 (31.8x): 10 (346.1x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio6"){
    ratio = "1 (3.0x): 1 (0.8x): 1 (33.5x): 1 (32.0x): 1 (348.2x) \n Ratio (Coverage)"
  }
  colnames(x)[1] = "Ratio" 
  
  coverage_p <- ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=Coverage, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
    ylab("Genome fraction (%)") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  
  contiguity_p =  ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=Contiguity, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
    ylab("Largest alignment length / Genome size") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  
  avg_p = ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=Average.percent.identity, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    coord_cartesian(ylim = c(95, 100)) +
    ylab("Average identity (%)") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  
  N50_p = ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=N50/1000, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    ylab("N50 (kbp)") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  return(ggarrange(coverage_p, contiguity_p, avg_p, N50_p, ncol=2, nrow = 2))
}

# Function for ARG data cleaning
ARG.clean = function(ratio_ARG, ratio_mob,all.ratio.flanking){
  a = ratio_ARG
  b = ratio_mob
  
  a$sample = substr(a$V1,1,6)
  a$ref = gsub("_NODE.*", "", a$V1) %>% substr(.,8,length(.))
  
  ARG.table = data.frame(matrix(ncol = 5))
  colnames(ARG.table) = c("sample", "ref", "cds","ARG","mobility")
  
  n=1
  for (s in a$sample %>% unique()) {
    for (r in a[a$sample == s,"ref"] %>% unique) {
      for (arg in a[a$sample == s & a$ref == r, "V2"] %>% unique()) {
        c = a[a$sample == s & a$ref == r & a$V2 == arg, ]
        for ( copy in 1:(c$V1 %>% length())) {
          
          ARG.table[n,"sample"] = s
          ARG.table[n,"ref"] = r
          ARG.table[n,"cds"] = c[copy,1]
          ARG.table[n,"ARG"] = arg
          
          if(grepl(c[copy,1],b$Specific.Contig) %>% sum() > 0){
            ARG.table[n,"mobility"] = "mobile"
          }
          else if(c$ref[copy] == "plasmid"){
            ARG.table[n,"mobility"] = "mobile"
          }
          else{
            ARG.table[n,"mobility"] = "non-mobile"
          }
          n=n+1
        }
      }
    }
  }
  for(i in 1: nrow(ARG.table)){
    a=paste0(ARG.table$cds[i], "$")
    ARG.table[i,"size"] = all.ratio.flanking[grepl(a,all.ratio.flanking$V1), "size"]
    if(ARG.table[i,"ref"] == "plasmid"){
      ARG.table[i,"size"] = gsub("^.*length_", "", ARG.table[i,"cds"]) %>% gsub("_cov.*$", "", .)
    }
  }
  ARG.table[ARG.table$mobility == "non-mobile" & as.numeric(ARG.table$size) < 5000,"mobility"] = "not known"
  return(ARG.table)
}

#Cleaning ARG cluster
clstr.clean = function(x){
  
  x = separate(x, col = "V2", c("1st", "2nd"), sep = "\\.\\.\\.")
  x = separate(x, col = "1st", c("length", "name"), sep = ', >')
  
  a = rownames(x[x$V1 == 0,]) %>% as.numeric %>% sort()
  
  for (i in 1:length(a)) {
    if(i == length(a)){
      x[a[i]:nrow(x),"cluster"] = x[a[i]-1,"V1"]
    }
    else{
      x[a[i]:(a[i+1]-2),"cluster"] = x[a[i]-1,"V1"]
    }
  }
  
  x = x[!is.na(x$cluster),]
  x$cluster = gsub(">","",x$cluster)
  return(x)
}

# ARG comparison in benchmarking
ARG.clstr.comp.clean = function(x){
  Bech_ARG_table = data.frame(matrix(ncol = 9))
  colnames(Bech_ARG_table) = c("ratio", "mobility", "ref", "binning_TP", "binning_FP", "binning_Jaccard_sim","de_novo_TP", "de_novo_FP", "de_novo_Jaccard_sim")
  
  n=1
  for (i in c("ratio1", "ratio2", "ratio3", "ratio4", "ratio5", "ratio6")) {
    for (j in c("overall", "non-mobile", "mobile")) {
      Bech_ARG_table[n,1] = i
      Bech_ARG_table[n,2] = j
      if(j == "overall"){
        a = x[x$type == "reference", "cluster"] %>% unique() #reference clusters
        b = x[grepl(i, x$V1) & x$type == "binning", "cluster"] %>% unique() #clusters from binning approach
        c = x[grepl(i, x$V1) & x$type == "de_novo", "cluster"] %>% unique() #clusters from de novo approach
        Bech_ARG_table[n,3] = length(a) # Number of all reference clusters
        Bech_ARG_table[n,4] = intersect(a,b) %>% length() # Number of all true positive clusters (binning)
        Bech_ARG_table[n,5] = setdiff(b,a) %>% length() # Number of all false positive clusters (binning)
        Bech_ARG_table[n,6] = length(intersect(a,b))/length(unique(c(a,b))) # Jaccard similarity compared with reference (binning)
        Bech_ARG_table[n,7] = intersect(a,c) %>% length() # Number of all true positive clusters (de novo)
        Bech_ARG_table[n,8] = setdiff(c,a) %>% length() # Number of all false positive clusters (de novo)
        Bech_ARG_table[n,9] = length(intersect(a,c))/length(unique(c(a,c))) # Jaccard similarity compared with reference (de novo)
        n=n+1
      }
      if(j == "non-mobile"){
        a = x[x$type == "reference" & x$mobility == "non-mobile", "cluster"] %>% unique() #reference clusters
        b = x[grepl(i, x$V1) & x$type == "binning" & x$mobility == "non-mobile", "cluster"] %>% unique() #clusters from binning approach
        c = x[grepl(i, x$V1) & x$type == "de_novo" & x$mobility == "non-mobile", "cluster"] %>% unique() #clusters from de novo approach
        Bech_ARG_table[n,3] = length(a) # Number of all reference clusters
        Bech_ARG_table[n,4] = intersect(a,b) %>% length() # Number of all true positive clusters (binning)
        Bech_ARG_table[n,5] = setdiff(b,a) %>% length() # Number of all false positive clusters (binning)
        Bech_ARG_table[n,6] = length(intersect(a,b))/length(unique(c(a,b))) # Jaccard similarity compared with reference (binning)
        Bech_ARG_table[n,7] = intersect(a,c) %>% length() # Number of all true positive clusters (de novo)
        Bech_ARG_table[n,8] = setdiff(c,a) %>% length() # Number of all false positive clusters (de novo)
        Bech_ARG_table[n,9] = length(intersect(a,c))/length(unique(c(a,c))) # Jaccard similarity compared with reference (de novo)
        n=n+1
      }
      if(j == "mobile"){
        a = x[x$type == "reference" & x$mobility == "mobile", "cluster"] %>% unique() #reference clusters
        b = x[grepl(i, x$V1) & x$type == "binning" & x$mobility == "mobile", "cluster"] %>% unique() #clusters from binning approach
        c = x[grepl(i, x$V1) & x$type == "de_novo" & x$mobility == "mobile", "cluster"] %>% unique() #clusters from de novo approach
        Bech_ARG_table[n,3] = length(a) # Number of all reference clusters
        Bech_ARG_table[n,4] = intersect(a,b) %>% length() # Number of all true positive clusters (binning)
        Bech_ARG_table[n,5] = setdiff(b,a) %>% length() # Number of all false positive clusters (binning)
        Bech_ARG_table[n,6] = length(intersect(a,b))/length(unique(c(a,b))) # Jaccard similarity compared with reference (binning)
        Bech_ARG_table[n,7] = intersect(a,c) %>% length() # Number of all true positive clusters (de novo)
        Bech_ARG_table[n,8] = setdiff(c,a) %>% length() # Number of all false positive clusters (de novo)
        Bech_ARG_table[n,9] = length(intersect(a,c))/length(unique(c(a,c))) # Jaccard similarity compared with reference (de novo)
        n=n+1
      }
      
    }
    
  }
  return(Bech_ARG_table)
}

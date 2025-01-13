source(file.path(here::here(),"0-config.R"))
# Load data
straingst.table = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_network_re.rds"))

# To get sample pairs with the same strain identified
sample.pair = combn(straingst.table$sample %>% unique() %>% gsub("_straingst","",.),2) %>% t() %>% data.frame()

for (i in 1:nrow(sample.pair)) {
  a = straingst.table[grepl(sample.pair[i,1], straingst.table$sample),"strain"] %>% unique() # strain from sample 1
  b = straingst.table[grepl(sample.pair[i,2], straingst.table$sample),"strain"] %>% unique() # strain from sample 2
  sample.pair$num.strain.same[i] = intersect(a,b) %>% length()
}
# Exclude sample pairs without any overlapping strains
sample.pair = sample.pair[sample.pair$num.strain.same > 0,] 

#save to a file
write.table(sample.pair[,1:2], file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/pair_list_re.txt"), sep = "\t", quote = F, row.names = F, col.names = F)

#list of samples in pairs
samples.in.pair = c(sample.pair[,1],sample.pair[,2]) %>% unique()
write.table(samples.in.pair, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/samples.in.pair_re.txt")  , sep = "\t", quote = F, row.names = F, col.names = F)




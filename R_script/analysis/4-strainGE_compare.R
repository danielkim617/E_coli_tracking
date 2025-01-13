source(file.path(here::here(),"0-config.R"))

#Load output file from StrainGR
strainge.compare = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strainGR_update_compare.tsv") , sep =  "\t", header = F) # updated method
colnames(strainge.compare) = c("sample1","sample2","ref","scaffold","length","common","commonPct","single","singlePct","singleAgree","singleAgreePct","multi","multiPct","sharedAlleles", "sharedAllelesPct", "variants" ,"variantPct" ,  "commonVariant","commonVariantPct", "variantExact" ,    "variantExactPct" , "AnotB"   , "AnotBpct"   ,"BnotA" , "BnotApct" ,"Agaps" ,"AgapPct", "Bgaps","BgapPct" ,"gapJaccardSim" )
#Add household ID
strainge.compare$sample1_hh = substr(strainge.compare$sample1,3,5)
strainge.compare$sample2_hh = substr(strainge.compare$sample2,3,5)
#Add sharing household types
strainge.compare[strainge.compare$sample1_hh == strainge.compare$sample2_hh, "HH_type"] = "Within"
strainge.compare[strainge.compare$sample1_hh != strainge.compare$sample2_hh, "HH_type"] = "Between"
#Add host types
strainge.compare[grepl("A|O|C", strainge.compare$sample1), "sample1_host"] = "Human"
strainge.compare[grepl("P|D", strainge.compare$sample1), "sample1_host"] = "Animal"
strainge.compare[grepl("W|S", strainge.compare$sample1), "sample1_host"] = "Environment"

strainge.compare[grepl("A|O|C", strainge.compare$sample2), "sample2_host"] = "Human"
strainge.compare[grepl("P|D", strainge.compare$sample2), "sample2_host"] = "Animal"
strainge.compare[grepl("W|S", strainge.compare$sample2), "sample2_host"] = "Environment"
# Write into a file
write.table(strainge.compare, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strainge.compare_update.tsv"), sep = '\t', col.names = T, row.names = F, quote = F)
#Subset strain sharing with ACNI > 99.95
strainge.compare.99.95 = strainge.compare%>%subset(singleAgreePct>=99.95)
#Save it to a file for network visualization
write.table(strainge.compare.99.95, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strainge.compare.99.95_update.tsv") , sep = '\t', col.names = T, row.names = F, quote = F)
saveRDS(strainge.compare.99.95, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strainge.compare.99.95_update.rds"))
#Count number of strain-sharing events depending on cutoffs
ACNI_count = data.frame(matrix(ncol = 6))
colnames(ACNI_count) = c("ACNI", "total", "same_hh", "diff_hh", "same_hh_pct", "diff_hh_pct")

cutoff = 99.8
for (i in 1:101) {
  #print(i)
  ACNI_count[i,1] = cutoff
  ACNI_count[i,2] = strainge.compare[strainge.compare$singleAgreePct >= cutoff, 1:2] %>% unique() %>% nrow()
  ACNI_count[i,3] = strainge.compare[strainge.compare$singleAgreePct >= cutoff & strainge.compare$HH_type == "Within", 1:2] %>% unique %>% nrow()
  ACNI_count[i,4] = strainge.compare[strainge.compare$singleAgreePct >= cutoff & strainge.compare$HH_type == "Between", 1:2] %>% unique %>% nrow()
  ACNI_count[i,5] = ACNI_count[i,3]/ACNI_count[i,2] *100
  ACNI_count[i,6] = ACNI_count[i,4]/ACNI_count[i,2] *100
  
  cutoff = round(cutoff + 0.01,2) # round of values due to floating-point precision errors.
}
# Save into a file
saveRDS(ACNI_count, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ACNI_count.rds"))

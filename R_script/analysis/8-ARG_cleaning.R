source(file.path(here::here(),"0-config.R"))
#Load data
#Load ARG annotation
all_sample_ARG = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_blastp.csv"), header = F)
#Load MGE annotation
all_sample_MOB = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_w_flank.fna.mobileOG.Alignment.Out.csv"))
all_sample_MOB = all_sample_MOB[all_sample_MOB$Sequence.Title != "Sequence Title",] # remove headers between rows which came when concatenating outputs
# load flanking size information
all.sample.flanking = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all.sample.flanking.contig.id.tsv"))
all.sample.flanking$range = regmatches(all.sample.flanking[,1], gregexpr("cov_[0-9]*\\.[0-9]*_[0-9]*-[0-9]*", all.sample.flanking[,1], perl=TRUE)) %>% gsub("cov_[0-9]*\\.[0-9]*_", "",.)
all.sample.flanking$position1 = regmatches(all.sample.flanking$range, gregexpr("[0-9]+(?=-)", all.sample.flanking$range, perl=TRUE))
all.sample.flanking$position2 = regmatches(all.sample.flanking$range, gregexpr("(?<=-)[0-9]+", all.sample.flanking$range, perl=TRUE))
all.sample.flanking$size = as.numeric(all.sample.flanking$position2) - as.numeric(all.sample.flanking$position1)

#Run ARG cleaning
all_sample_ARG_clean=ARG.clean(all_sample_ARG, all_sample_MOB, all.sample.flanking)
# Save into a file
write.csv(all_sample_ARG_clean, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean_re.csv"), row.names = F, quote = F)
saveRDS(all_sample_ARG_clean, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean_re.rds"))


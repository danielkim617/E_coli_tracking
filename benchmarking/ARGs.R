source(file.path(here::here(),"0-config.R"))
#ARG annotation data
Bench_ARG_blastp = read.table(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/new_data/all_blastp1.tsv"),sep = "\t", header = F)
#Load MGE data
Bench_MGE = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/new_data/all_mobileOG.Alignment.Out.csv"), header = T, fill = T)
Bench_MGE = Bench_MGE[Bench_MGE$Sequence.Title != "Sequence Title",] # remove headers between rows which came when concatenating outputs
#Load flanking region information of ARGs from binned and de novo approach
Bench_flank = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/new_data/all_flank_ID.tsv"))
Bench_flank$range = regmatches(Bench_flank[,1], gregexpr("cov_[0-9]*\\.[0-9]*_[0-9]*-[0-9]*", Bench_flank[,1], perl=TRUE)) %>% gsub("cov_[0-9]*\\.[0-9]*_", "",.)
Bench_flank$position1 = regmatches(Bench_flank$range, gregexpr("[0-9]+(?=-)", Bench_flank$range, perl=TRUE))
Bench_flank$position2 = regmatches(Bench_flank$range, gregexpr("(?<=-)[0-9]+", Bench_flank$range, perl=TRUE))
Bench_flank$size = as.numeric(Bench_flank$position2) - as.numeric(Bench_flank$position1)
Bench_flank$cds = gsub("ratio.*ratio","ratio",Bench_flank$V1)
###
Bench_ARG_blastp.clean = Bench_ARG_blastp[,1:4]
Bench_ARG_blastp.clean = merge(Bench_ARG_blastp.clean, Bench_flank, all.x = T, by.x = "V1", by.y = "cds")
#Add data type information
Bench_ARG_blastp.clean[grepl("Esch_coli_GTEN",Bench_ARG_blastp.clean$V1), "type"] = "reference"
Bench_ARG_blastp.clean[grepl("ratio[0-9]*_NODE",Bench_ARG_blastp.clean$V1), "type"] = "de_novo"
Bench_ARG_blastp.clean[is.na(Bench_ARG_blastp.clean$type), "type"] = "binning"

#Add mobility
Bench_ARG_blastp.clean[Bench_ARG_blastp.clean$V1.y %in% Bench_MGE$Specific.Contig, "mobility"] = "mobile"
#Add mobility for ARG from reference genomes
Bench_ARG_blastp.clean[Bench_ARG_blastp.clean$V1 %in% (Bench_MGE$Specific.Contig %>% gsub("scaffold.*_scaffold","scaffold",.)), "mobility"] = "mobile"
Bench_ARG_blastp.clean[!(is.na(Bench_ARG_blastp.clean$size)) & Bench_ARG_blastp.clean$size >= 5000 & is.na(Bench_ARG_blastp.clean$mobility),"mobility"] = "non-mobile"
Bench_ARG_blastp.clean[!(is.na(Bench_ARG_blastp.clean$size)) & Bench_ARG_blastp.clean$size < 5000 & is.na(Bench_ARG_blastp.clean$mobility),"mobility"] = "ambiguous"
Bench_ARG_blastp.clean[is.na(Bench_ARG_blastp.clean$size) & is.na(Bench_ARG_blastp.clean$mobility), "mobility"] = "non-mobile"

#Cluster data
Bench_ARG_1.0 = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/new_data/all_ARG_ref_denovo_cds_cluster_1.0.fna.clstr"),sep = "\t", header = F, fill = T)

Bench_ARG_1.0.clean  = clstr.clean(Bench_ARG_1.0)
Bench_ARG_1.0.clean$name = gsub("scaffold", "_scaffold", Bench_ARG_1.0.clean$name)
Bench_ARG_1.0.clean$name = gsub("GTEN", "Esch_coli_GTEN", Bench_ARG_1.0.clean$name)	

#Add cluster to clean ARG table
Bench_ARG_1.0.clean.clstr = merge(Bench_ARG_blastp.clean, Bench_ARG_1.0.clean[,c(3,5)], by.x = "V1", by.y = "name", all.x = T)

clstr_1.0 = ARG.clstr.comp.clean(Bench_ARG_1.0.clean.clstr)
clstr_1.0$threshold = "100%"
#
clstr_all = rbind(clstr_1.0)
clstr_all$TP_rate_binning = clstr_all$binning_TP/(clstr_all$binning_TP + clstr_all$binning_FP)*100
clstr_all$TP_rate_de_novo = clstr_all$de_novo_TP/(clstr_all$de_novo_TP + clstr_all$de_novo_FP)*100

clstr_all.m = melt(clstr_all[,c(1:5, 7,8,10)])
clstr_all.m$mobility = factor(clstr_all.m$mobility, levels = c("overall", "non-mobile", "mobile"))

#Plot figures
ARG.bench.num = ggplot(clstr_all.m[clstr_all.m$threshold == "100%",], aes(x=mobility, y=value, fill=variable)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  facet_wrap(.~ratio) +
  ylab("Number of unique ARG clusters") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

#Calculation of Jaccard similarity index
clstr_Jac_all.m = melt(clstr_all[,c(1,2,6,9,10)])
clstr_Jac_all.m$mobility = factor(clstr_Jac_all.m$mobility, levels = c("overall", "non-mobile", "mobile"))

#Plot figures
ARG.bench.Jac = ggplot(clstr_Jac_all.m[clstr_Jac_all.m$threshold == "100%",], aes(x=mobility, y=value, fill=variable)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  facet_wrap(.~ratio) +
  ylab("Jaccard similarity") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))


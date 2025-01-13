source(file.path(here::here(),"0-config.R"))
# Load data
straingst.table = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_straingst_network_re.tsv"), sep = "\t", header = T)
#For SRA database submission
sample.des = data.frame(sample = straingst.table$sample %>% unique)
sample.des$hh_id_3dig = substr(sample.des$sample, 3,5)

hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$hh_id_3dig = substr(hh_survey$hh_id_5dig,3,5)

sample.des = merge(sample.des, hh_survey[,c("hh_id_3dig","date", "gps_hhlatitude", "gps_hhlongitude")], by = "hh_id_3dig", all.x = T)
sample.des$lat_new = ifelse(sample.des$gps_hhlatitude >= 0, paste0("N ",abs(sample.des$gps_hhlatitude)), paste0("S ",abs(sample.des$gps_hhlatitude)))
sample.des$lon_new = ifelse(sample.des$gps_hhlongitude >= 0, paste0("E ",abs(sample.des$gps_hhlongitude)), paste0("W ",abs(sample.des$gps_hhlongitude)))
sample.des$lat_lon = paste0(sample.des$lat_new, " ", sample.des$lon_new)

sample.des$organism = "metagenome"
sample.des$host = ifelse(grepl("A|O|C",sample.des$sample), "Homo sapiens", "")
sample.des$host = ifelse(grepl("P",sample.des$sample), "Poultry", sample.des$host)
sample.des$host = ifelse(grepl("D",sample.des$sample), "Canine", sample.des$host)
sample.des$isolation_source = ifelse(grepl("W",sample.des$sample), "stored drinking water", "")
sample.des$isolation_source = ifelse(grepl("S",sample.des$sample), "household soil", sample.des$isolation_source)
sample.des$filename = paste0(sample.des$sample, ".bam")
sample.des$title = paste0("DNA of pooled Escherchia colonies from ", paste0(sample.des$host, sample.des$isolation_source))
sample.des$filename_f = paste0(sample.des$sample, "_R1.fastq")
sample.des$filename_r = paste0(sample.des$sample, "_R2.fastq")

sample.des$geo_loc_name = "Kenya: Nairobi"
write.table(sample.des, file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/manuscript/submission/NCBI/sample.des.tsv"), sep = "\t", quote = F, row.names = F)


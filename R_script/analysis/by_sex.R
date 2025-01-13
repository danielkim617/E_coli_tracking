source(file.path(here::here(),"0-config.R"))
# Load data
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$hh_id_3dig = substr(hh_survey$hh_id_5dig,3,5)
sample.list = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/sample.list.rds"))
strainge.compare.99.95 = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strainge.compare.99.95_update.rds"))

# Get human samples
sample.list.sex = sample.list 

# Adult samples are from children's mother
for (i in 1:nrow(sample.list.sex)) {
  if (startsWith(sample.list.sex$sample[i], "A")) {
    sample.list.sex$sex[i] = "Female"
  } else if (startsWith(sample.list.sex$sample[i], "C1")) {
    sample.list.sex$sex[i] = hh_survey %>% filter(hh_id_3dig == sample.list.sex$household[i]) %>% pull(sexc5)
  } else if (startsWith(sample.list.sex$sample[i], "C2")) {
    sample.list.sex$sex[i] = hh_survey %>% filter(hh_id_3dig == sample.list.sex$household[i]) %>% pull(sexc5_2)
  } else if (startsWith(sample.list.sex$sample[i], "O1")) {
    sample.list.sex$sex[i] = hh_survey %>% filter(hh_id_3dig == sample.list.sex$household[i]) %>% pull(sexc15)
  } else if (startsWith(sample.list.sex$sample[i], "O2")) {
    sample.list.sex$sex[i] = hh_survey %>% filter(hh_id_3dig == sample.list.sex$household[i]) %>% pull(sexc15_2)
  } else {
    sample.list.sex$sex[i] = sample.list.sex$host_type[i]  # Or any default value to indicate no match
  }
}

# Add sex information to sharing pair data
strainge.compare.99.95$sample1_sex = sample.list.sex$sex[match(strainge.compare.99.95$sample1, sample.list.sex$sample)]
strainge.compare.99.95$sample2_sex = sample.list.sex$sex[match(strainge.compare.99.95$sample2, sample.list.sex$sample)]

# Categorize combination
for (i in 1:nrow(strainge.compare.99.95)) {
  if(strainge.compare.99.95$sample1_host[i] == strainge.compare.99.95$sample2_host[i] & strainge.compare.99.95$sample1_host[i] == "Human"){
    strainge.compare.99.95$sex_comb[i] = sort(c(strainge.compare.99.95$sample1_sex[i], strainge.compare.99.95$sample2_sex[i])) %>% paste0(., collapse = "_")
  }
  else if(strainge.compare.99.95$sample1_host[i] != strainge.compare.99.95$sample2_host[i] & strainge.compare.99.95$sample1_host[i] == "Human"){
    strainge.compare.99.95$sex_comb[i] = sort(c(strainge.compare.99.95$sample1_sex[i], strainge.compare.99.95$sample2_host[i])) %>% paste0(., collapse = "_")
  }
  else if(strainge.compare.99.95$sample1_host[i] != strainge.compare.99.95$sample2_host[i] & strainge.compare.99.95$sample2_host[i] == "Human"){
    strainge.compare.99.95$sex_comb[i] = sort(c(strainge.compare.99.95$sample1_host[i], strainge.compare.99.95$sample2_sex[i])) %>% paste0(., collapse = "_")
  }
  else{
    strainge.compare.99.95$sex_comb[i] = sort(c(strainge.compare.99.95$sample1_host[i], strainge.compare.99.95$sample2_host[i])) %>% paste0(., collapse = "_")
  }
}

# all pairs
sample.pairs = sample.list.sex$sample %>% combn(., 2) %>% t() %>% as.data.frame()
colnames(sample.pairs) = c("sample1", "sample2")
sample.pairs$sample1_hh = substr(sample.pairs$sample1, 3,5)
sample.pairs$sample2_hh = substr(sample.pairs$sample2, 3,5)
sample.pairs = sample.pairs %>% mutate(HH_type = ifelse(sample1_hh == sample2_hh, "Within", "Between"))
sample.pairs$sample1_sex = sample.list.sex$sex[match(sample.pairs$sample1, sample.list.sex$sample)]
sample.pairs$sample2_sex = sample.list.sex$sex[match(sample.pairs$sample2, sample.list.sex$sample)]
for (i in 1:nrow(sample.pairs)) {
  sample.pairs$sex_comb[i] = sort(c(sample.pairs$sample1_sex[i], sample.pairs$sample2_sex[i])) %>% paste0(., collapse = "_")
}



  






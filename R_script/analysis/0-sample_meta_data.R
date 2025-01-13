source(file.path(here::here(),"0-config.R"))
# Load data
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$hh_id_3dig = substr(hh_survey$hh_id_5dig,3,5)
sample.list = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/sample.list.rds"))
hh.list = sample.list$household %>% unique
# Extract household information included in this study
hh_survey.hh = hh_survey %>% filter(hh_id_3dig %in% hh.list)

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

# Get only human samples
sample.list.sex.human  = sample.list.sex %>% filter(host_type == "human") %>% rowwise() %>% mutate(spec_host = ifelse(startsWith(sample,"A"), "mother", spec_host)) %>% 
  mutate(spec_host = ifelse(startsWith(sample,"C"), "child", spec_host)) %>% mutate(spec_host = ifelse(startsWith(sample,"O"), "older_child", spec_host))

hh_survey.hh$num_residents %>% table
sample.list.sex.human %>% select(spec_host, sex) %>% table

variables <- c(
  "swavail", "swloc", "sw_extract", "swcontainer", "swcover", "swsource",
  "swtime", "swtime_hr", "swtime_day", "swtime_wk", "swtreat", 
  "swtrmethod_disp", "swtrmethod_bottle", "swtrmethod_boil", "swtrmethod_cloth", 
  "swtrmethod_filter", "swtrmethod_sodis", "swtrmethod_settle", 
  "swtrmethod_sand", "swtrmethod_vffilter", "swtrmethod_alum", 
  "swtrmethod_pur", "swtrmethod_aquatab", "swtrmethod_cl", "source_sample_collect", "no_source_sample_collect",
  "no_source_sample_collect_oth","no_source_oth_hhid", "source_notavail"
)

# To also include information of water source sharing info
hh_survey.hh.water = hh_survey.hh[, c("hh_id_3dig","subcountyid",variables)]
hh_survey.hh.water = hh_survey.hh.water %>% mutate(no_source_oth_hhid_3dig = hh_survey.hh.water$no_source_oth_hhid %>% substr(.,3,5))
hh_survey.hh.water = hh_survey.hh.water %>% rowwise %>% mutate(no_source_oth_hhid_3dig = ifelse(is.na(no_source_oth_hhid_3dig),hh_id_3dig ,no_source_oth_hhid_3dig))
# Correct mislabeled ID
hh_survey.hh.water[hh_survey.hh.water$hh_id_3dig == "083", "no_source_oth_hhid_3dig"] = "059" # As the original value 062 collects water from 059


# Number of household sharing each water source
water_source_hh_num = hh_survey.hh.water %>% select(no_source_oth_hhid_3dig, subcountyid) %>% table %>% data.frame() %>% filter(Freq != 0)
water_source_hh_num$water_source_id = rownames(water_source_hh_num)
water_source_hh_num = water_source_hh_num[c("subcountyid","water_source_id","no_source_oth_hhid_3dig","Freq")]

# Load E. coli contamination data
env.e.coli = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/additional_resources/KenyaES1DataEntryForm-ecoli_soilwater_samples-CLEANED-20210326.csv"))
#Get information of households included in this study for stored water and hhsoil
env.e.coli$hhid = substr(env.e.coli$hh_id_5dig_ecsoilwat,3,5)
#env.e.coli = env.e.coli[(env.e.coli$hhid %in% (sample.list$household %>% unique())),c("hhid", "sample_type_ecsoilwat", "ecoli_soilwater_pos", "ecoli_soilwater_Comments_text")]
#env.e.coli = env.e.coli[,c("hhid", "sample_type_ecsoilwat", "ecoli_soilwater_pos", "ecoli_soilwater_Comments_text")]
env.e.coli.w = env.e.coli[(env.e.coli$sample_type_ecsoilwat %in% c("stored", "source")),c("hhid", "sample_type_ecsoilwat", "ecoli_soilwater_pos", "ecoli_soilwater_Comments_text")]
#env.e.coli.w = env.e.coli %>% filter(sample_type_ecsoilwat %in% c("source", "stored"))
  #env.e.coli[env.e.coli$sample_type_ecsoilwat == "stored",]


# See if there was any non-E. coli contaminated stored water from any of water sources
for (i in 1:nrow(water_source_hh_num)) {
  hhid1 = water_source_hh_num$no_source_oth_hhid_3dig[i]
  a = hh_survey.hh.water %>% filter(no_source_oth_hhid_3dig == hhid1) %>% pull(hh_id_3dig)
  b = env.e.coli.w %>% filter(sample_type_ecsoilwat == "stored" & hhid %in% a)
  c = env.e.coli.w %>% filter(sample_type_ecsoilwat == "source" & hhid == hhid1)
  # For stored water
  if(nrow(b) == 0){
    next
  } else if(0 %in% b$ecoli_soilwater_pos){
    water_source_hh_num[i,"any_stored_not_contaminated"] = "Yes"
  }
  else if(!(0 %in% b$ecoli_soilwater_pos)){
    water_source_hh_num[i,"any_stored_not_contaminated"] = "No"
  }
  # For source water
  if(nrow(c) == 0){
    next
  } else if(c$ecoli_soilwater_pos == 1){
    water_source_hh_num[i,"source_contaminated"] = "Yes"
  }
    else if(c$ecoli_soilwater_pos == 0){
    water_source_hh_num[i,"source_contaminated"] = "No"
  }
}

write.csv(water_source_hh_num, "/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/water_source_hh_num.csv", quote = F, row.names = F)
# watersource ID (HH ID) shared with another households are: 058 (O), 059 (O), 004 (O), 011 (O), 018 (O), 030 (O), 064 (X), 071 (O), 081 (O), 092 (X)

# HH ID in each sub county
Dag.hh = hh_survey.hh.water %>% filter(subcountyid == "Dagoretti South") %>% pull(hh_id_3dig)
Kib.hh = hh_survey.hh.water %>% filter(subcountyid == "Kibera") %>% pull(hh_id_3dig)

# Add sub county information to sex information
sample.list.sex.human = sample.list.sex.human %>% rowwise() %>% mutate(subcountyid = ifelse(household %in% Dag.hh, "Dagoretti South", "Kibera"))

sample.list.sex.human %>% select(sex, spec_host, subcountyid) %>% table





source(file.path(here::here(),"0-config.R"))
# Load data
stored.pos = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/stored.pos.rds"))
#plot bar plot
ggplot(stored.pos[2:3,], aes(x=cat, y=percent, fill=cat)) +
  geom_bar(stat = "identity", position = "dodge", col="black", width = 0.7)+
  ylab("Percentage of E. coli positive (%)")+
  theme_classic2()

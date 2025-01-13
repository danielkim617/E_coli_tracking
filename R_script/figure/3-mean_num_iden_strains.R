source(file.path(here::here(),"0-config.R"))
#Load files
num.strain.iden = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.strain.iden.rds"))

#Plot
ggplot(num.strain.iden, aes(x=host.type, y=num.strains ,fill=host.type)) +
  geom_boxplot() +
  labs(x="", y="Number of identified strains", fill="") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

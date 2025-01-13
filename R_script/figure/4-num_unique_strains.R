source(file.path(here::here(),"0-config.R"))
#Load files
uniq.strains.host.m = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/uniq.strains.host.m.rds"))

#If equal number of samples were collected for each host type (10 in this case)
ggplot(uniq.strains.host.m, aes(x=host, y=num.uniq.strains, col = host)) +
  geom_boxplot() +
  ylab("Number of unique strains (n=10)") +
  #scale_y_continuous(limits = c(0,30)) +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

source(file.path(here::here(),"0-config.R"))
#Load data
num.cluster.host.m = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.cluster.host.m.rds"))
all_sample_ARG_host.tab = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_host.tab.rds"))
# Plot
#If equal number of samples were collected for each host type (10 in this case)
ggplot(num.cluster.host.m, aes(x=host, y=num.uniq.clusters, col = host)) +
  geom_boxplot() +
  ylab("Number of unique clusters (n = 10)") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

#Plot figure
ggplot(all_sample_ARG_host.tab, aes(x=type, y=total, col=type)) +
  geom_boxplot() +
  labs(y="Number of unique ARG clusters", col="Sample type", x="") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

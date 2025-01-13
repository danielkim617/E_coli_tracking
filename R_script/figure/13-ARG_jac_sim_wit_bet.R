source(file.path(here::here(),"0-config.R"))
#Load data
ARG.share.table.clstr.m = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ARG.share.table.clstr.m.rds"))

# Plot
ggplot(ARG.share.table.clstr.m, aes(x=cat, y=value, fill=HH_type)) +
  geom_boxplot() +
  facet_grid(.~variable) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.25)) +
  #scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5, 0.75, 1)) +
  ylab("Resistome similarity (Jaccard similarity)") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

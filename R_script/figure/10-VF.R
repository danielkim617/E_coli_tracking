source(file.path(here::here(),"0-config.R"))
#Load data
path.freq.m = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/path.freq.m.tsv"))

#Plot
ggplot(path.freq.m[!(path.freq.m$variable %in% c("Adult", "Older child", "child")),], aes(x=variable, y=pathotype, fill=value)) +
  geom_tile(col = "black") +
  scale_fill_gradient2(low = "#ffffcc", mid= "#41b6c4", high = "#253494", midpoint = 10, limits = c(0,22),breaks = c(0,5,10,15,20), labels = c(0,5,10,15,20)) +
  scale_y_discrete(expand=c(0, 0), limits=rev) +
  scale_x_discrete(expand=c(0, 0)) +
  labs(x = "", y = "Pathotype",fill = "Relative frequency (%)") +
  coord_fixed() +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

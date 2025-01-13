source(file.path(here::here(),"0-config.R"))
#Load data
w.contam.all.comp.m = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/w.contam.all.comp.m_re.rds"))
#Plot only human-humans
ggplot(w.contam.all.comp.m[w.contam.all.comp.m$subcategory == "Human - Human",], aes(x=contam, y=subcounty, fill = prev.rate)) +
  geom_tile(col = "black", fill = "white") +
  geom_point(aes(size=mean), pch=21, col = "black") +
  scale_size_continuous(limits = c(0,0.35), breaks = c(0,0.1, 0.2, 0.3), range = c(1,12)) +
  scale_y_discrete(expand=c(0, 0), limits=rev) +
  scale_x_discrete(expand=c(0, 0)) +
  scale_fill_gradient2(low = "#ffffcc", mid= "#41b6c4", high = "#253494", midpoint = 30, limits = c(0,65),breaks = c(0,20,40,60), labels = c(0,20,40,60)) +
  geom_text(aes(label = prevlaence), col = "black", size = 3, vjust = 2) +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))



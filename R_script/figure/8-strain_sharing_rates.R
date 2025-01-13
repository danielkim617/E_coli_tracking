source(file.path(here::here(),"0-config.R"))
#Load data
prevlance.stat5 = readRDS(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/prevlance.stat5_update.rds"))
#plot figure
ggplot(prevlance.stat5, aes(HH_type, category, fill = percent)) +
  geom_tile(col = "black", fill = "white") +
  #geom_point(aes(size=rate)) +
  geom_point(aes(size=rate), pch=21, col = "black") +
  scale_size_continuous(limits = c(0,0.25), breaks = c(0,0.05,0.10, 0.15, 0.2, 0.25), range = c(1,12)) +
  scale_y_discrete(expand=c(0, 0), limits=rev) +
  scale_x_discrete(expand=c(0, 0)) +
  labs(x="", y="") +
  #scale_color_gradient(low = "#6699CC",high = "#003366", limits = c(0,45), breaks = c(0,15,30,45), labels = c(0,15,30,45)) +
  scale_fill_gradient2(low = "#ffffcc", mid= "#41b6c4", high = "#253494", midpoint = 25, limits = c(0,50),breaks = c(0,10,20,30,40,50), labels = c(0,10,20,30,40,50)) +
  geom_text(aes(label = text), col = "black", size = 3, vjust = 2) +
  facet_grid(.~ subcounty, scales = "free_x") +
  #coord_equal() +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

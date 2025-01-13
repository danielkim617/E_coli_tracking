source(file.path(here::here(),"0-config.R"))
#Load data
rankI_clstr_fig.m1 = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/rankI_clstr_fig.m1.rds"))
rankI_clstr_fig.m2 = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/rankI_clstr_fig.m2.rds"))
num.cluster.host.rankI.m = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/num.cluster.host.rankI.m.rds"))
all_sample_ARG_clean.clstr.rankI.host = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/all_sample_ARG_clean.clstr.rankI.host.rds"))

# Plot
a= ggplot(rankI_clstr_fig.m1, aes(x=cluster, y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  coord_flip() +
  facet_grid(drug_class + V2 ~., scales = "free", space = "free") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

b = ggplot(rankI_clstr_fig.m2, aes(x=variable, y=cluster, fill = value)) +
  geom_tile(col = "black") +
  scale_y_discrete(expand=c(0, 0)) +
  scale_x_discrete(expand=c(0, 0)) +
  scale_fill_gradient2(low = "#ffffcc", mid= "#41b6c4", high = "#253494", midpoint = 50, limits = c(0,100), breaks = c(0,20,40,60,80,100), labels = c(0,20,40,60,80, 100)) +
  facet_grid(drug_class + V2~., scale = "free", space = "free") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8), axis.text.y = element_text(size = 10), strip.text.y = element_text(angle = 0, size = 10, hjust = 0, margin=margin(l=10)))

# Plotting
c = a + b + plot_layout(widths = c(0.8, 1))
print(c)

#If equal number of samples were collected for each host type (10 in this case)
ggplot(num.cluster.host.rankI.m, aes(x = host, y = num.uniq.clusters, color = host)) +
  geom_boxplot() +
  ylab("Number of unique rank I ARG clusters (n = 10)") +
  facet_wrap(. ~ drug_class, nrow = 2, scales = "free") +
  theme_bw(base_size = 14) + # Using a larger base font size for clarity
  theme(
    # Improve legibility of axis texts
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),
    # Adjust strip background and panel grid to match Nature style
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    # Remove legend if not necessary
    legend.position = "none",
    # Adjust margin to ensure labels are not cut off
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(color = "Host") # Make sure color legend title matches axis title if legend is used


#Plot figures
ggplot(all_sample_ARG_clean.clstr.rankI.host, aes(x=host, y=Freq, col=host)) +
  geom_boxplot() +
  ylab("Number of unique rank I ARG clusters") +
  scale_y_continuous(breaks = pretty_breaks(n = 5), expand = expansion(mult = c(0.1, 0.1))) +
  facet_wrap(. ~ drug_class, nrow = 2, scales = "free") +
  theme_bw(base_size = 14) + # Using a larger base font size for clarity
  theme(
    # Improve legibility of axis texts
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),
    # Adjust strip background and panel grid to match Nature style
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    # Remove legend if not necessary
    legend.position = "none",
    # Adjust margin to ensure labels are not cut off
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  #scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(color = "Host") # Make sure color legend title matches axis title if legend is used



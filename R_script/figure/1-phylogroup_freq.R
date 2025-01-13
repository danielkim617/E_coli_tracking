source(file.path(here::here(),"0-config.R"))
#Load data
phy.freq = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/phy.freq.rds"))

# Composition of phyloroups found in each host type
ggplot(phy.freq, aes(x=host, y=RF, fill=phylogroup)) + 
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Relative frequency (%)", x="", fill="Phylogroup") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1, vjust=1),
    axis.title = element_text(size=16),
    plot.title = element_text(size=20, face="bold"),
    legend.title = element_text(size=16),
    legend.text = element_text(size=14),
    strip.text.x = element_text(size=16, face="bold"),
    strip.text.y = element_text(size=16, face="bold", angle=0),  # Adjust y facet labels as well
    strip.background = element_rect(colour="white", fill="white")  # Optional: style facet label background
  )

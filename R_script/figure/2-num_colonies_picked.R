source(file.path(here::here(),"0-config.R"))
#Load data
actual.numbers = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/actual.numbers.rds"))

#Plot data
ggplot(actual.numbers, aes(x=Host, y=picked, fill = Host)) +
  #geom_violin(trim=T) + # Adds violin plots, trim=FALSE shows the full range of data
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) + # Sets y-axis limits and breaks
  scale_fill_brewer(palette="Pastel1") + # Optional: Use a color palette for fill
  labs(y="Number of pooled colonies", x="") + # Set labels for axes
  theme_classic() + # Use the classic theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), # Rotate x-axis labels
        axis.title.x = element_blank(), 
        legend.title = element_blank()) 

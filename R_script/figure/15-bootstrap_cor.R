source(file.path(here::here(),"0-config.R"))
#Load data
iter.cor = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/iter.cor_re.csv"))
iter.cor.m = melt(iter.cor, id.vars = c("iter", "HH_type"))
iter.cor.m$HH_type = factor(iter.cor.m$HH_type, levels = c("Within", "Between"))

#Plot figures of bootstrapped correlation results
ggplot(iter.cor.m, aes(x=value, fill=HH_type)) +
  geom_density(alpha = 0.7) +
  xlab("Spearman's coefficient (r)") +
  ylab("Density") +
  facet_wrap(.~variable, nrow = 1) +
  theme_base()

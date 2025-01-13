source(file.path(here::here(),"0-config.R"))
# Load data
strainge.compare = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/strainge.compare_update.tsv"), sep = '\t', header = T)

#Plot ACNI vs. Gap similarities
ggplot(strainge.compare, aes(x=gapJaccardSim, y=singleAgreePct, col=HH_type,size=commonPct)) +
  geom_point(alpha=0.5) +
  ylab("Pairwise ACNI (%)") +
  xlab("Gap similarity") +
  scale_y_continuous(limits = c(99.9, 100), breaks = c(seq(99.9, 100,0.02 ))) +
  scale_x_continuous(limits = c(0.925, 1), breaks = c(seq(0.920, 1.0, 0.01 ))) +
  geom_hline(yintercept = 99.95, linetype=2) +
  scale_size_continuous(name = "commonPct" ,breaks = c(20, 40, 60, 80), limits = c(0,100)) +
  theme_bw() +
  theme(aspect.ratio = 1)

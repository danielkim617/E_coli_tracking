source(file.path(here::here(),"0-config.R"))
#Load data
clstr.actual.wit = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/clstr.actual.wit_re.rds"))
clstr.actual.bet = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/clstr.actual.bet_re.rds"))

# Within HH
rates.vs.overall.clstr = ggscatter(clstr.actual.wit, x = "rate", y = "overall_sim", fill = "category",
                                   shape = 21, size = 3, # Points color, shape and size
                                   add = "reg.line",  # Add regressin line
                                   add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                                   conf.int = TRUE, # Add confidence interval
                                   cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                                   cor.coeff.args = list(method = "spear",  label.sep = "\n")
) +
  xlab("Strain-sharing rates") +
  ylab("Overall resistome Jaccard similarity)") +
  ggtitle("Strain-sharing rates vs. Overall resistome similarity") +
  theme_base()

rates.vs.non.clstr = ggscatter(clstr.actual.wit, x = "rate", y = "non_sim", fill = "category",
                               shape = 21, size = 3, # Points color, shape and size
                               add = "reg.line",  # Add regression line
                               add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                               conf.int = TRUE, # Add confidence interval
                               cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                               cor.coeff.args = list(method = "spear",  label.sep = "\n")
) +
  xlab("Strain-sharing rates") +
  ylab("Non-mobile resistome Jaccard similarity") +
  ggtitle("Strain-sharing rates vs. Non-mobile resistome similarity") +
  theme_base()

rates.vs.mob.clstr = ggscatter(clstr.actual.wit, x = "rate", y = "mob_sim", fill = "category",
                               shape = 21, size = 3, # Points color, shape and size
                               add = "reg.line",  # Add regressin line
                               add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                               conf.int = TRUE, # Add confidence interval
                               cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                               cor.coeff.args = list(method = "spear",  label.sep = "\n")
) +
  xlab("Strain-sharing rates") +
  ylab("Mobile resistome Jaccard similarity") +
  ggtitle("Strain-sharing rates vs. Mobile resistome similarity") +
  theme_base()

ggarrange(rates.vs.non.clstr, rates.vs.mob.clstr, ncol=2)

# Between HH
rates.vs.overall.bet.clstr = ggscatter(clstr.actual.bet, x = "rate", y = "overall_sim", fill = "category",
                                       shape = 21, size = 3, # Points color, shape and size
                                       add = "reg.line",  # Add regressin line
                                       add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                                       conf.int = TRUE, # Add confidence interval
                                       cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                                       cor.coeff.args = list(method = "spear", label.sep = "\n")
) +
  xlab("Strain-sharing rates") +
  ylab("Overall resistome Jaccard similarity") +
  ggtitle("Strain-sharing rates vs. Overall resistome similarity") +
  theme_base()

rates.vs.non.bet.clstr = ggscatter(clstr.actual.bet, x = "rate", y = "non_sim", fill = "category",
                                   shape = 21, size = 3, # Points color, shape and size
                                   add = "reg.line",  # Add regressin line
                                   add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                                   conf.int = TRUE, # Add confidence interval
                                   cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                                   cor.coeff.args = list(method = "spear", label.sep = "\n")
) +
  xlab("Strain-sharing rates") +
  ylab("Non-mobile resistome Jaccard similarity") +
  ggtitle("Strain-sharing rates vs. Non-mobile resistome similarity") +
  theme_base()

rates.vs.mob.bet.clstr = ggscatter(clstr.actual.bet, x = "rate", y = "mob_sim", fill = "category",
                                   shape = 21, size = 3, # Points color, shape and size
                                   add = "reg.line",  # Add regressin line
                                   add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                                   conf.int = TRUE, # Add confidence interval
                                   cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                                   cor.coeff.args = list(method = "spear", label.sep = "\n")
) +
  xlab("Strain-sharing rates") +
  ylab("Mobile resistome Jaccard similarity)") +
  ggtitle("Strain-sharing rates vs. Mobile ARG resistome similarity") +
  theme_base()

ggarrange(rates.vs.non.bet.clstr, rates.vs.mob.bet.clstr, nrow=1, ncol=2)


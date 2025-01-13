##Comparison of metrics between assembly strategies
library(reshape2)
library(ggplot2)
library(ggpubr)

#Assembly metrics
#Load data from all ratios
#ratio1
Stats_r1_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r1_s.txt", sep = "\t", header = F, row.names = 1)
Stats_r1_s_l = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r1_s_l.txt", sep = "\t", header = F, row.names = 1)
Stats_r1_l_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r1_l_s.txt", sep = "\t", header = F, row.names = 1)
#ratio2
Stats_r2_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r2_s.txt", sep = "\t", header = F, row.names = 1)
Stats_r2_s_l = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r2_s_l.txt", sep = "\t", header = F, row.names = 1)
Stats_r2_l_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r2_l_s.txt", sep = "\t", header = F, row.names = 1)
#ratio3
Stats_r3_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r3_s.txt", sep = "\t", header = F, row.names = 1)
Stats_r3_s_l = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r3_s_l.txt", sep = "\t", header = F, row.names = 1)
Stats_r3_l_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r3_l_s.txt", sep = "\t", header = F, row.names = 1)
#ratio4
Stats_r4_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r4_s.txt", sep = "\t", header = F, row.names = 1)
Stats_r4_s_l = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r4_s_l.txt", sep = "\t", header = F, row.names = 1)
Stats_r4_l_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r4_l_s.txt", sep = "\t", header = F, row.names = 1)
#ratio5
Stats_r5_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r5_s.txt", sep = "\t", header = F, row.names = 1)
Stats_r5_s_l = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r5_s_l.txt", sep = "\t", header = F, row.names = 1)
Stats_r5_l_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r5_l_s.txt", sep = "\t", header = F, row.names = 1)
#ratio6
Stats_r6_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r6_s.txt", sep = "\t", header = F, row.names = 1)
Stats_r6_s_l = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r6_s_l.txt", sep = "\t", header = F, row.names = 1)
Stats_r6_l_s = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r6_l_s.txt", sep = "\t", header = F, row.names = 1)

combine_data = function(x,y,z) {
  Stats_r_s.t = data.frame(t(x))
  Stats_r_s_l.t = data.frame(t(y))
  Stats_r_l_s.t = data.frame(t(z))
  
  ratio.stats = data.frame(rbind(Stats_r_s.t,Stats_r_s_l.t,Stats_r_l_s.t))
  
  for( i in 3:6 ){
    ratio.stats[,i] = as.numeric(ratio.stats[,i]) #changing character to numeric
  }
  
  ratio.stats$Status_f = factor(ratio.stats$Status, levels = c("Unbinned", "Binned"))
  ratio.stats$Strategy_f = factor(ratio.stats$Strategy, levels = c("short_only", "short_long", "long_short"), labels = c("Short read\n(metaSPAdes)", "Hybrid assembly\n(HybridSPAdes)", "Long read\n(Flye + Medaka + Pilon)"))
  ratio.stats[,1] = factor(ratio.stats[,1], levels = c("ISO1", "ISO2", "ISO3", "ISO4", "ISO5"), labels = c("GTEN 247", "GTEN 291", "GTEN293", "GTEN306", "GTEN378"))
  return(ratio.stats)
}

ratio1.stats = combine_data(Stats_r1_s, Stats_r1_s_l, Stats_r1_l_s)
ratio2.stats = combine_data(Stats_r2_s, Stats_r2_s_l, Stats_r2_l_s)
ratio3.stats = combine_data(Stats_r3_s, Stats_r3_s_l, Stats_r3_l_s)
ratio4.stats = combine_data(Stats_r4_s, Stats_r4_s_l, Stats_r4_l_s)
ratio5.stats = combine_data(Stats_r5_s, Stats_r5_s_l, Stats_r5_l_s)
ratio6.stats = combine_data(Stats_r6_s, Stats_r6_s_l, Stats_r6_l_s)

#Plotting - ratio1
stat.figure = function(x){
  if(colnames(x)[1] == "Ratio1"){
    ratio = "1 (73.6x): 1 (80.6x): 1 (81.3x): 1 (77.5x): 1 (84.4x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio2"){
    ratio = "1 (88.5x): 0.1 (9.8x): 1 (99.2x): 1 (95.4x): 1 (103.0x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio3"){
    ratio = "1 (90.2x): 0.025 (2.5x): 1 (101.0x): 1 (96.3x): 1 (104.9x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio4"){
    ratio = "0.1 (2.8x): 1 (30.8x): 1 (31.0x): 1 (29.6x): 10 (322.3x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio5"){
    ratio = "0.1 (3.0x): 0.1 (3.3x): 1 (33.3x): 1 (31.8x): 10 (346.1x) \n Ratio (Coverage)"
  }
  if(colnames(x)[1] == "Ratio6"){
    ratio = "1 (3.0x): 1 (0.8x): 1 (33.5x): 1 (32.0x): 1 (348.2x) \n Ratio (Coverage)"
  }
  colnames(x)[1] = "Ratio" 
  
  coverage_p <- ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=Coverage, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
    ylab("Genome fraction (%)") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  
  contiguity_p =  ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=Contiguity, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
    ylab("Largest alignment length / Genome size") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  
  avg_p = ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=Average.percent.identity, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    coord_cartesian(ylim = c(95, 100)) +
    ylab("Average identity (%)") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  
  N50_p = ggplot(x %>% filter(Status == "Binned"), aes(x=Ratio, y=N50/1000, fill=Strategy_f)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Set2") + # Change color palette for visual appeal
    ylab("N50 (kbp)") +
    xlab(ratio) +
    theme_classic(base_size = 12) + # Adjust base font size for better readability
    theme(strip.placement = "outside",
          legend.position = "right", # Adjust legend position to make it visible or remove it if not needed
          legend.title = element_blank(), # Remove legend title for cleaner look
          legend.text = element_text(size = 10), # Adjust legend text size
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # Ensure x-axis labels are readable
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14), # Center and bold plot title
          plot.subtitle = element_text(size = 12), # Subtitle size, if you add one
          plot.caption = element_text(size = 10), # Caption size
          legend.spacing.y = unit(0.5, "cm")) # Increase vertical space between legend texts
  return(ggarrange(coverage_p, contiguity_p, avg_p, N50_p, ncol=2, nrow = 2))
}


stat.figure(ratio1.stats)
stat.figure(ratio2.stats)
stat.figure(ratio3.stats)
stat.figure(ratio4.stats)
stat.figure(ratio5.stats)
stat.figure(ratio6.stats)

coverage_p2 = ggplot(ratio2.stats, aes(x=Ratio2, y=Coverage, fill=Status_f)) +
            geom_bar(stat = "identity", position = position_dodge(0.9)) +
            #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
            scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
            ggtitle("Ratio2") +
            facet_wrap(~Strategy_f, strip.position = "bottom") +
            theme_classic() +
            theme(strip.placement = "outside") +
            theme(legend.position = "none") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coverage_p3 = ggplot(ratio3.stats, aes(x=Ratio3, y=Coverage, fill=Status_f)) +
            geom_bar(stat = "identity", position = position_dodge(0.9)) +
            #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
            scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
            ggtitle("Ratio3") +
            facet_wrap(~Strategy_f, strip.position = "bottom") +
            theme_classic() +
            theme(strip.placement = "outside") +
            theme(legend.position = "none") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coverage_p4 = ggplot(ratio4.stats, aes(x=Ratio4, y=Coverage, fill=Status_f)) +
            geom_bar(stat = "identity", position = position_dodge(0.9)) +
            #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
            scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
            ggtitle("Ratio4") +
            facet_wrap(~Strategy_f, strip.position = "bottom") +
            theme_classic() +
            theme(strip.placement = "outside") +
            theme(legend.position = "none") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coverage_p5 = ggplot(ratio5.stats, aes(x=Ratio5, y=Coverage, fill=Status_f)) +
            geom_bar(stat = "identity", position = position_dodge(0.9)) +
            #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
            scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
            ggtitle("Ratio5") +
            facet_wrap(~Strategy_f, strip.position = "bottom") +
            theme_classic() +
            theme(strip.placement = "outside") +
            theme(legend.position = "none") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coverage_p6 = ggplot(ratio6.stats, aes(x=Ratio6, y=Coverage, fill=Status_f)) +
            geom_bar(stat = "identity", position = position_dodge(0.9)) +
            #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
            scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
            ggtitle("Ratio6") +
            facet_wrap(~Strategy_f, strip.position = "bottom") +
            theme_classic() +
            theme(strip.placement = "outside") +
            theme(legend.position = "none") +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

coverage_all = ggarrange(coverage_p1,coverage_p2,coverage_p3,coverage_p4,coverage_p5,coverage_p6)

#Plotting - Contiguity


contiguity_p2 = ggplot(ratio2.stats, aes(x=Ratio2, y=Contiguity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) +
  ggtitle("Ratio2") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

contiguity_p3 = ggplot(ratio3.stats, aes(x=Ratio3, y=Contiguity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) +
  ggtitle("Ratio3") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

contiguity_p4 = ggplot(ratio4.stats, aes(x=Ratio4, y=Contiguity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ggtitle("Ratio4") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

contiguity_p5 = ggplot(ratio5.stats, aes(x=Ratio5, y=Contiguity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,60), expand = c(0,0)) +
  ggtitle("Ratio5") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

contiguity_p6 = ggplot(ratio6.stats, aes(x=Ratio6, y=Contiguity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,60), expand = c(0,0)) +
  ggtitle("Ratio6") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

contiguity_all = ggarrange(contiguity_p1,contiguity_p2,contiguity_p3,contiguity_p4,contiguity_p5,contiguity_p6)

#Plotting - average percent identity

avg_p2 = ggplot(ratio2.stats, aes(x=Ratio2, y=Average.percent.identity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio2") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

avg_p3 = ggplot(ratio3.stats, aes(x=Ratio3, y=Average.percent.identity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio3") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

avg_p4 = ggplot(ratio4.stats, aes(x=Ratio4, y=Average.percent.identity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio4") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

avg_p5 = ggplot(ratio5.stats, aes(x=Ratio5, y=Average.percent.identity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio5") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

avg_p6 = ggplot(ratio6.stats, aes(x=Ratio6, y=Average.percent.identity, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio6") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

avg_all = ggarrange(avg_p1,avg_p2,avg_p3,avg_p4,avg_p5,avg_p6)

#Plotting - N50

N50_p2 = ggplot(ratio2.stats, aes(x=Ratio2, y=N50/1000, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  #scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  #coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio2") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N50_p3 = ggplot(ratio3.stats, aes(x=Ratio3, y=N50/1000, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  #scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  #coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio3") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N50_p4 = ggplot(ratio4.stats, aes(x=Ratio4, y=N50/1000, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  #scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  #coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio4") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N50_p5 = ggplot(ratio5.stats, aes(x=Ratio5, y=N50/1000, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  #scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  #coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio5") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N50_p6 = ggplot(ratio6.stats, aes(x=Ratio6, y=N50/1000, fill=Status_f)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  #scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  #scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  #coord_cartesian(ylim = c(95, 100)) +
  ggtitle("Ratio6") +
  facet_wrap(~Strategy_f, strip.position = "bottom") +
  theme_classic() +
  theme(strip.placement = "outside") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N50_all = ggarrange(N50_p1,N50_p2,N50_p3,N50_p4,N50_p5,N50_p6)


#ARG detection

#Load data from all ratios
ARG_N.r1 = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/danielkim617/R_scripts/Benchmarking/data/ARG_raw_short_r1.txt", sep = "\t", header = F, row.names = 1)
ARG_N.r2 = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/danielkim617/R_scripts/Benchmarking/data/ARG_raw_short_r2.txt", sep = "\t", header = F, row.names = 1)
ARG_N.r3 = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/danielkim617/R_scripts/Benchmarking/data/ARG_raw_short_r3.txt", sep = "\t", header = F, row.names = 1)
ARG_N.r4 = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/danielkim617/R_scripts/Benchmarking/data/ARG_raw_short_r4.txt", sep = "\t", header = F, row.names = 1)
ARG_N.r5 = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/danielkim617/R_scripts/Benchmarking/data/ARG_raw_short_r5.txt", sep = "\t", header = F, row.names = 1)
ARG_N.r6 = read.table("/Users/danielkim617/Library/CloudStorage/Box-Box/danielkim617/R_scripts/Benchmarking/data/ARG_raw_short_r6.txt", sep = "\t", header = F, row.names = 1)


ARG_N.r1.t = data.frame(t(ARG_N.r1))
ARG_N.r2.t = data.frame(t(ARG_N.r2))
ARG_N.r3.t = data.frame(t(ARG_N.r3))
ARG_N.r4.t = data.frame(t(ARG_N.r4))
ARG_N.r5.t = data.frame(t(ARG_N.r5))
ARG_N.r6.t = data.frame(t(ARG_N.r6))

ARG_N.r1.m = melt(ARG_N.r1.t, id = "Ratio1")
ARG_N.r2.m = melt(ARG_N.r2.t, id = "Ratio2")
ARG_N.r3.m = melt(ARG_N.r3.t, id = "Ratio3")
ARG_N.r4.m = melt(ARG_N.r4.t, id = "Ratio4")
ARG_N.r5.m = melt(ARG_N.r5.t, id = "Ratio5")
ARG_N.r6.m = melt(ARG_N.r6.t, id = "Ratio6")

colnames(ARG_N.r1.m) = c("Methods", "Metric", "Values")
colnames(ARG_N.r2.m) = c("Methods", "Metric", "Values")
colnames(ARG_N.r3.m) = c("Methods", "Metric", "Values")
colnames(ARG_N.r4.m) = c("Methods", "Metric", "Values")
colnames(ARG_N.r5.m) = c("Methods", "Metric", "Values")
colnames(ARG_N.r6.m) = c("Methods", "Metric", "Values")

ARG_N.r1.m$Values = as.numeric(ARG_N.r1.m$Values)
ARG_N.r2.m$Values = as.numeric(ARG_N.r2.m$Values)
ARG_N.r3.m$Values = as.numeric(ARG_N.r3.m$Values)
ARG_N.r4.m$Values = as.numeric(ARG_N.r4.m$Values)
ARG_N.r5.m$Values = as.numeric(ARG_N.r5.m$Values)
ARG_N.r6.m$Values = as.numeric(ARG_N.r6.m$Values)

#Subset number of detected ARG and ARG with 100% identity
ARG_N.r1 = subset(ARG_N.r1.m, ARG_N.r1.m$Metric %in% c("N.of.ARGs", "N.of.ARGs..100.."))
ARG_N.r2 = subset(ARG_N.r2.m, ARG_N.r2.m$Metric %in% c("N.of.ARGs", "N.of.ARGs..100.."))
ARG_N.r3 = subset(ARG_N.r3.m, ARG_N.r3.m$Metric %in% c("N.of.ARGs", "N.of.ARGs..100.."))
ARG_N.r4 = subset(ARG_N.r4.m, ARG_N.r4.m$Metric %in% c("N.of.ARGs", "N.of.ARGs..100.."))
ARG_N.r5 = subset(ARG_N.r5.m, ARG_N.r5.m$Metric %in% c("N.of.ARGs", "N.of.ARGs..100.."))
ARG_N.r6 = subset(ARG_N.r6.m, ARG_N.r6.m$Metric %in% c("N.of.ARGs", "N.of.ARGs..100.."))

#Ploting graphs
N_ARG_p1  =  ggplot(ARG_N.r1, aes(x=Methods, y=Values, fill=Metric)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
                ggtitle("Ratio1") +              
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N_ARG_p2  =  ggplot(ARG_N.r2, aes(x=Methods, y=Values, fill=Metric)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
                ggtitle("Ratio2") +              
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N_ARG_p3  =  ggplot(ARG_N.r3, aes(x=Methods, y=Values, fill=Metric)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
                ggtitle("Ratio3") +              
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N_ARG_p4  =  ggplot(ARG_N.r4, aes(x=Methods, y=Values, fill=Metric)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
                ggtitle("Ratio4") +              
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N_ARG_p5  =  ggplot(ARG_N.r5, aes(x=Methods, y=Values, fill=Metric)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
                ggtitle("Ratio5") +              
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

N_ARG_p6  =  ggplot(ARG_N.r6, aes(x=Methods, y=Values, fill=Metric)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
                ggtitle("Ratio6") +
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggarrange(N_ARG_p1, N_ARG_p2, N_ARG_p3, N_ARG_p4, N_ARG_p5, N_ARG_p6)

###
#Subset coverage of ARG, which had 100% identity
ARG_C.r1 = subset(ARG_N.r1.m, ARG_N.r1.m$Metric == c("avg..coverage.of.reference.ARG...."))
ARG_C.r2 = subset(ARG_N.r2.m, ARG_N.r2.m$Metric == c("avg..coverage.of.reference.ARG...."))
ARG_C.r3 = subset(ARG_N.r3.m, ARG_N.r3.m$Metric == c("avg..coverage.of.reference.ARG...."))
ARG_C.r4 = subset(ARG_N.r4.m, ARG_N.r4.m$Metric == c("avg..coverage.of.reference.ARG...."))
ARG_C.r5 = subset(ARG_N.r5.m, ARG_N.r5.m$Metric == c("avg..coverage.of.reference.ARG...."))
ARG_C.r6 = subset(ARG_N.r6.m, ARG_N.r6.m$Metric == c("avg..coverage.of.reference.ARG...."))

#Plot average coverage ARG
C_ARG_p1  =  ggplot(ARG_C.r1, aes(x=Methods, y=Values)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  ggtitle("Ratio1") +              
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

C_ARG_p2  =  ggplot(ARG_C.r2, aes(x=Methods, y=Values)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  ggtitle("Ratio2") +              
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

C_ARG_p3  =  ggplot(ARG_C.r3, aes(x=Methods, y=Values)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  ggtitle("Ratio3") +              
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

C_ARG_p4  =  ggplot(ARG_C.r4, aes(x=Methods, y=Values)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  ggtitle("Ratio4") +              
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

C_ARG_p5  =  ggplot(ARG_C.r5, aes(x=Methods, y=Values)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  ggtitle("Ratio5") +              
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

C_ARG_p6  =  ggplot(ARG_C.r6, aes(x=Methods, y=Values)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = c("raw_short",	"raw_long",	"SPAdes",	"Hybrid_SPAdes",	"Flye_Pilon")) +
  ggtitle("Ratio6") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggarrange(C_ARG_p1, C_ARG_p2, C_ARG_p3, C_ARG_p4, C_ARG_p5, C_ARG_p6)

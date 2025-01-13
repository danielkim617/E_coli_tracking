source(file.path(here::here(),"0-config.R"))
#Benchmarking on contigs (short reads vs. long reads vs. hybrid)
#ratio1
Stats_r1_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r1_s.txt"), sep = "\t", header = F, row.names = 1)
Stats_r1_s_l = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r1_s_l.txt"), sep = "\t", header = F, row.names = 1)
Stats_r1_l_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r1_l_s.txt"), sep = "\t", header = F, row.names = 1)
#ratio2
Stats_r2_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r2_s.txt"), sep = "\t", header = F, row.names = 1)
Stats_r2_s_l = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r2_s_l.txt"), sep = "\t", header = F, row.names = 1)
Stats_r2_l_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r2_l_s.txt"), sep = "\t", header = F, row.names = 1)
#ratio3
Stats_r3_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r3_s.txt"), sep = "\t", header = F, row.names = 1)
Stats_r3_s_l = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r3_s_l.txt"), sep = "\t", header = F, row.names = 1)
Stats_r3_l_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r3_l_s.txt"), sep = "\t", header = F, row.names = 1)
#ratio4
Stats_r4_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r4_s.txt"), sep = "\t", header = F, row.names = 1)
Stats_r4_s_l = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r4_s_l.txt"), sep = "\t", header = F, row.names = 1)
Stats_r4_l_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r4_l_s.txt"), sep = "\t", header = F, row.names = 1)
#ratio5
Stats_r5_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r5_s.txt"), sep = "\t", header = F, row.names = 1)
Stats_r5_s_l = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r5_s_l.txt"), sep = "\t", header = F, row.names = 1)
Stats_r5_l_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r5_l_s.txt"), sep = "\t", header = F, row.names = 1)
#ratio6
Stats_r6_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r6_s.txt"), sep = "\t", header = F, row.names = 1)
Stats_r6_s_l = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r6_s_l.txt"), sep = "\t", header = F, row.names = 1)
Stats_r6_l_s = read.table(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Benchmarking/R_scripts/data/Assembly_r6_l_s.txt"), sep = "\t", header = F, row.names = 1)

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

# Plot figures
stat.figure(ratio1.stats)
stat.figure(ratio2.stats)
stat.figure(ratio3.stats)
stat.figure(ratio4.stats)
stat.figure(ratio5.stats)
stat.figure(ratio6.stats)



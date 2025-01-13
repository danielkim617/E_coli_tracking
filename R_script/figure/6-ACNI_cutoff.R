source(file.path(here::here(),"0-config.R"))
# Load data
ACNI_count = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/ACNI_count.rds"))

#plot number of sharing based on differing cut-offs
ACNI_count.pct.m = ACNI_count[,c(1,5,6)] %>% melt("ACNI")

ggplot(ACNI_count.pct.m, aes(x=ACNI, y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept=99.95, col="red") +
  scale_x_continuous( limits = c(99.79, 100.01), breaks = c(seq(99.8, 100, 0.05))) +
  #coord_cartesian(ylim=c(75,100)) + 
  ylab("Relative abundance (%)") +
  theme_base() +
  theme_classic2()

ACNI_count.m = ACNI_count[,c(1,2,3,4)]%>%melt("ACNI")

ggplot(ACNI_count.m, aes(x=ACNI, y=value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept=99.95, col="red") +
  scale_x_continuous(limits = c(99.79, 100.01)) +
  ylab("Number of strain-sharing events") +
  xlab("ACNI (%)") +
  theme_classic2()

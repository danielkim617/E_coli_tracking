source(file.path(here::here(),"0-config.R"))
#Load data
library(ggsn)
library(ggmap)
register_stadiamaps("a57f4e4a-a56c-41ca-8855-2b58e5ffbb72")
hh_survey = read.csv(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/survey_data/data_cleaning/KEMRI_env_surv-hh_survey-CLEANED-20200130.csv"))
hh_survey$hh_id_3dig = substr(hh_survey$hh_id_5dig,3,5)
sample.list = readRDS(file.path(box.path, "Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/new_data1/sample.list.rds"))
hh.list = sample.list$household %>% unique
# Extract household information included in this study
hh_survey.hh = hh_survey %>% filter(hh_id_3dig %in% hh.list)

my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
# map for urban area
kenya_map.urban <- get_stadiamap(bbox = c(left = 36.73734-0.01, bottom = -1.316038-0.01, right = 36.79044+0.01, top = -1.298977+0.01), zoom = 15, maptype = "stamen_toner_lite")

ggmap(kenya_map.urban) + geom_point(data = hh_survey.hh, aes(x=gps_hhlongitude, y=gps_hhlatitude, col=subcountyid)) + labs(x="Longitude", y="Latitude") + theme_bw()

scale_bar_lon <- 36.74 # slightly adjusted longitude
scale_bar_lat <- -1.32 # slightly adjusted latitude

map.urban = ggmap(kenya_map.urban) +
  geom_point(data = hh_survey.hh, aes(x = gps_hhlongitude, y = gps_hhlatitude, color = subcountyid), size = 2, alpha = 0.5) +
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  labs(x = "Longitude", y = "Latitude", title = "Nairobi study area", subtitle = "",shape = "Treatment", color = "Location") +
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  # Add scale bar manually
  geom_segment(aes(x = scale_bar_lon, y = scale_bar_lat, 
                   xend = scale_bar_lon + 0.005, yend = scale_bar_lat), 
               color = "black", linewidth = 1) +
  annotate("text", x = scale_bar_lon + 0.0025, y = scale_bar_lat - 0.0005, 
           label = "~500m", size = 3)

# Save into a file
ggsave(file.path(box.path,"Pickering_Kenya_AMR/ecoli/danielkim617/Kenyan_dataset/Figures/Finals/Map.pdf"), dpi = 300, scale = 0.3, width = 850, height = 450, units = "mm", plot = map.urban)


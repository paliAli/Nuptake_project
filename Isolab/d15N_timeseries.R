library(ggplot2)
library(ggthemes)
library(dplyr)



mean_d15N <- merged_data_CN %>%
  group_by(date, `Material`) %>%
  summarize(`Delta_N` = mean(`Delta_N`)) %>%
  arrange(date)%>%
  na.omit()

index_labels <- c(grass = "grassland", legume = "legume", mix = "grass", `aboveground biomass` = "winter wheat")

d15N_timeseries_mean <- mean_d15N %>%
  ggplot(aes(x = date, y = Delta_N, color = `Material`, group = factor(`Material`)))+
  geom_point(size = 1.5)+
  geom_line(linewidth = 1)+
  labs(x = "Date", y = "d15N", title = "Delta 15 N changes in time")+
  theme_minimal()+
  scale_color_discrete(name = "Vegetation type",
                       labels = c("winter wheat", "grassland", "legume", "grass")) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  theme(axis.text.x = element_text(size = 14, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30),
        legend.text = element_text(size = 11))

d15N_timeseries_mean
  
d15N_timeseries_mix <- merged_data_CN %>%
  filter(grepl("mix", `Material`, ignore.case = TRUE))%>%
  ggplot(aes(x = date, y = `%N corr.`, color = factor(`sample`), group = factor(`sample`)))+
  geom_point(size = 1.5)+
  geom_line()+
  labs(x = "Date", y = "% N content in leaves", title = "Delta 15 N in mix vegetation", color = "sample number")+
  theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))

d15N_timeseries_mix

d15N_timeseries_grass <- merged_data_CN %>%
  filter(grepl("grass", `Material`, ignore.case = TRUE))%>%
  ggplot(aes(x = date, y = `%N corr.`, color = factor(`sample`), group = factor(`sample`)))+
  geom_point(size = 1.5)+
  geom_line()+
  labs(x = "Date", y = "% N content in leaves", title = "Delta 15 N in grassland", color = "sample number")+
  theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))

d15N_timeseries_grass

d15N_timeseries_legume <- merged_data_CN %>%
  filter(grepl("legume", `Material`, ignore.case = TRUE))%>%
  ggplot(aes(x = date, y = `%N corr.`, color = factor(`sample`), group = factor(`sample`)))+
  geom_point(size = 1.5)+
  geom_line()+
  labs(x = "Date", y = "% N content in leaves", title = "Delta 15 N in legumes", color = "sample number")+
  theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))

d15N_timeseries_legume

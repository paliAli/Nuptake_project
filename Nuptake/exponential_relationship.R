
# NDVI --------------------------------------------------------------------

exponential_model <- lm(log(mean_Nuptake) ~ meanNDVI, data = Nuptake_NDVI)
summary(exponential_model)

# Extract coefficients
exponential_coef <- coef(exponential_model)
intercept_exp <- exponential_coef[1]
slope_exp <- exponential_coef[2]

r_squared_exp <- 1 - sum(residuals(exponential_model)^2) / sum((log(Nuptake_NDVI$mean_Nuptake) - mean(log(Nuptake_NDVI$mean_Nuptake)))^2)


exponential_Nuptake_NDVI_plot <- Nuptake_NDVI %>%
  ggplot(aes(x = meanNDVI, y = mean_Nuptake, group = 1))+
  geom_point(aes(shape = factor(material)), size = 3, )+
  geom_smooth(method = lm, se = FALSE, formula = y ~ exp(x), color = "blue", aes(group = 1))+
  labs(x = "NDVI", y = "N uptake (g/m2)", title = "NDVI vs N uptake (exponential)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(Nuptake_NDVI$meanNDVI) + 0.02,
    y = max(Nuptake_NDVI$mean_Nuptake) - 1,
    label = paste(
      "y = ",
      "exp(",
      format(slope_exp, digits = 2),
      "*x +", format(exp(intercept_exp), digits = 2),
      ")\nR2 =", round(r_squared_exp, 2)
    ),
    hjust = 0, vjust = 1, color = "black", size = 6
  )+
  scale_shape_manual(values = c(16, 17))+
  guides(shape = guide_legend(title = "Biomass type"))

exponential_Nuptake_NDVI_plot

ggsave("NDVIvsNuptake_exp.png", exponential_Nuptake_NDVI_plot, width = 7, height = 10, dpi = 350)


# NDRE --------------------------------------------------------------------


# Fit an exponential model
exponential_model <- lm(log(mean_Nuptake) ~ meanNDRE, data = Nuptake_NDRE)
summary(exponential_model)

# Extract coefficients
exponential_coef <- coef(exponential_model)
intercept_exp <- exponential_coef[1]
slope_exp <- exponential_coef[2]

# Calculate R-squared for the exponential model
r_squared_exp <- 1 - sum(residuals(exponential_model)^2) / sum((log(Nuptake_NDRE$mean_Nuptake) - mean(log(Nuptake_NDRE$mean_Nuptake)))^2)

# Plot the exponential relationship
exponential_Nuptake_NDRE_plot <- Nuptake_NDRE %>%
  ggplot(aes(x = meanNDRE, y = mean_Nuptake)) +
  geom_point(aes(shape = factor(material)), size = 3) +
  geom_smooth(method = lm, se = FALSE, formula = y ~ exp(x), color = "blue") +
  labs(x = "NDRE", y = "N uptake (g/m2)", title = "NDRE vs N uptake (exponential)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 15),
    axis.title.y = element_text(margin = margin(r = 20), size = 15),
    plot.title = element_text(margin = margin(b = 20), size = 22),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(Nuptake_NDRE$meanNDRE) + 0.02,
    y = max(Nuptake_NDRE$mean_Nuptake) - 1,
    label = paste(
      "y = ",
      "exp(",
      format(slope_exp, digits = 2),
      "*x +", format(exp(intercept_exp), digits = 2),
      ")\nR2 =", round(r_squared_exp, 2)
    ),
    hjust = 0,
    vjust = 1,
    color = "black",
    size = 6
  )+
  scale_shape_manual(values = c(16, 17))+
  guides(shape = guide_legend(title = "Biomass type"))

exponential_Nuptake_NDRE_plot

ggsave("NDREvsNuptake_exp.png", exponential_Nuptake_NDRE_plot, width = 7, height = 10, dpi = 350)


# EVI ---------------------------------------------------------------------


# Fit an exponential model
exponential_model <- lm(log(mean_Nuptake) ~ meanEVI, data = Nuptake_EVI)
summary(exponential_model)

# Extract coefficients
exponential_coef <- coef(exponential_model)
intercept_exp <- exponential_coef[1]
slope_exp <- exponential_coef[2]

# Calculate R-squared for the exponential model
r_squared_exp <- 1 - sum(residuals(exponential_model)^2) / sum((log(Nuptake_EVI$mean_Nuptake) - mean(log(Nuptake_EVI$mean_Nuptake)))^2)

# Plot the exponential relationship
exponential_Nuptake_EVI_plot <- Nuptake_EVI %>%
  ggplot(aes(x = meanEVI, y = mean_Nuptake)) +
  geom_point(aes(shape = factor(material)), size = 3) +
  geom_smooth(method = lm, se = FALSE, formula = y ~ exp(x), color = "blue") +
  labs(x = "EVI", y = "N uptake (g/m2)", title = "EVI vs N uptake (exponential)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 15),
    axis.title.y = element_text(margin = margin(r = 20), size = 15),
    plot.title = element_text(margin = margin(b = 20), size = 22),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(Nuptake_EVI$meanEVI) + 0.02,
    y = max(Nuptake_EVI$mean_Nuptake) - 1,
    label = paste(
      "y = ",
      "exp(",
      format(slope_exp, digits = 2),
      "*x +", format(exp(intercept_exp), digits = 2),
      ")\nR2 =", round(r_squared_exp, 2)),
    hjust = 0,
    vjust = 1,
    color = "black",
    size = 6
  )+
  scale_shape_manual(values = c(16, 17))+
  guides(shape = guide_legend(title = "Biomass type"))

exponential_Nuptake_EVI_plot

ggsave("EVIvsNuptake_exp.png", exponential_Nuptake_EVI_plot, width = 7, height = 10, dpi = 350)


# MCARI -------------------------------------------------------------------


# Fit an exponential model
exponential_model <- lm(log(mean_Nuptake) ~ meanMCARI, data = Nuptake_MCARI)
summary(exponential_model)


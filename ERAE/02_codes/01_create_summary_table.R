## Construction of Table 1##

soybean_gravity <- readRDS("../data/soybean_gravity.rds")
soyoil_gravity <- readRDS("../data/soyoil_gravity.rds")
soycake_gravity <- readRDS("../data/soycake_gravity.rds")


# Calculate summary statistics for each dataset
soybean_stats <- soybean_gravity %>%
  summarise(
    Year_mean = mean(year, na.rm = TRUE),
    Year_sd = sd(year, na.rm = TRUE),
    Year_min = min(year, na.rm = TRUE),
    Year_max = max(year, na.rm = TRUE),
    Soybean_mean = mean(soybean_v, na.rm = TRUE),
    Soybean_sd = sd(soybean_v, na.rm = TRUE),
    Soybean_min = min(soybean_v, na.rm = TRUE),
    Soybean_max = max(soybean_v, na.rm = TRUE),
    Distance_mean = mean(distw_harmonic, na.rm = TRUE),
    Distance_sd = sd(distw_harmonic, na.rm = TRUE),
    Distance_min = min(distw_harmonic, na.rm = TRUE),
    Distance_max = max(distw_harmonic, na.rm = TRUE),
    Contiguity_mean = mean(contig, na.rm = TRUE),
    Contiguity_sd = sd(contig, na.rm = TRUE),
    Contiguity_min = min(contig, na.rm = TRUE),
    Contiguity_max = max(contig, na.rm = TRUE),
    Comlang_mean = mean(comlang_off, na.rm = TRUE),
    Comlang_sd = sd(comlang_off, na.rm = TRUE),
    Comlang_min = min(comlang_off, na.rm = TRUE),
    Comlang_max = max(comlang_off, na.rm = TRUE),
    Comcol_mean = mean(comcol, na.rm = TRUE),
    Comcol_sd = sd(comcol, na.rm = TRUE),
    Comcol_min = min(comcol, na.rm = TRUE),
    Comcol_max = max(comcol, na.rm = TRUE),
    Tariff1_mean = mean(tariff1, na.rm = TRUE),
    Tariff1_sd = sd(tariff1, na.rm = TRUE),
    Tariff1_min = min(tariff1, na.rm = TRUE),
    Tariff1_max = max(tariff1, na.rm = TRUE)
  )

soyoil_stats <- soyoil_gravity %>%
  summarise(
    Soyoil_mean = mean(soyoil_v, na.rm = TRUE),
    Soyoil_sd = sd(soyoil_v, na.rm = TRUE),
    Soyoil_min = min(soyoil_v, na.rm = TRUE),
    Soyoil_max = max(soyoil_v, na.rm = TRUE),
    Tariff2_mean = mean(tariff2, na.rm = TRUE),
    Tariff2_sd = sd(tariff2, na.rm = TRUE),
    Tariff2_min = min(tariff2, na.rm = TRUE),
    Tariff2_max = max(tariff2, na.rm = TRUE)
  )

soycake_stats <- soycake_gravity %>%
  summarise(
    Soycake_mean = mean(soycake_v, na.rm = TRUE),
    Soycake_sd = sd(soycake_v, na.rm = TRUE),
    Soycake_min = min(soycake_v, na.rm = TRUE),
    Soycake_max = max(soycake_v, na.rm = TRUE),
    Tariff3_mean = mean(tariff3, na.rm = TRUE),
    Tariff3_sd = sd(tariff3, na.rm = TRUE),
    Tariff3_min = min(tariff3, na.rm = TRUE),
    Tariff3_max = max(tariff3, na.rm = TRUE)
  )

# Create the summary table
summary_table <- data.frame(
  Variable = c(
    "Year", 
    "Soybean (USD '000)", 
    "Soy oil (USD '000)", 
    "Soy cake (USD '000)",
    "Distance (KM)",
    "Contiguity*",
    "Common language*",
    "Common colony*",
    "Soybean tariff",
    "Soy oil tariff",
    "Soy cake tariff"
  ),
  Mean = c(
    "-",
    format(soybean_stats$Soybean_mean, big.mark = ",", nsmall = 3),
    format(soyoil_stats$Soyoil_mean, big.mark = ",", nsmall = 3),
    format(soycake_stats$Soycake_mean, big.mark = ",", nsmall = 3),
    format(soybean_stats$Distance_mean, big.mark = ",", nsmall = 3),
    sprintf("%.3f%%", soybean_stats$Contiguity_mean * 100),
    sprintf("%.3f%%", soybean_stats$Comlang_mean * 100),
    sprintf("%.3f%%", soybean_stats$Comcol_mean * 100),
    sprintf("%.3f", soybean_stats$Tariff1_mean),
    sprintf("%.3f", soyoil_stats$Tariff2_mean),
    sprintf("%.3f", soycake_stats$Tariff3_mean)
  ),
  SD = c(
    "-",
    format(soybean_stats$Soybean_sd, big.mark = ",", nsmall = 3),
    format(soyoil_stats$Soyoil_sd, big.mark = ",", nsmall = 3),
    format(soycake_stats$Soycake_sd, big.mark = ",", nsmall = 3),
    format(soybean_stats$Distance_sd, big.mark = ",", nsmall = 3),
   "-",
   "-",
   "-",
    sprintf("%.3f", soybean_stats$Tariff1_sd),
    sprintf("%.3f", soyoil_stats$Tariff2_sd),
    sprintf("%.3f", soycake_stats$Tariff3_sd)
  ),
  Minimum = c(
    sprintf("%.0f", soybean_stats$Year_min),
    format(soybean_stats$Soybean_min, big.mark = ",", nsmall = 3),
    format(soyoil_stats$Soyoil_min, big.mark = ",", nsmall = 3),
    format(soycake_stats$Soycake_min, big.mark = ",", nsmall = 3),
    format(soybean_stats$Distance_min, big.mark = ",", nsmall = 3),
    sprintf("%.3f", soybean_stats$Contiguity_min),
    sprintf("%.3f", soybean_stats$Comlang_min),
    sprintf("%.3f", soybean_stats$Comcol_min),
    sprintf("%.3f", soybean_stats$Tariff1_min),
    sprintf("%.3f", soyoil_stats$Tariff2_min),
    sprintf("%.3f", soycake_stats$Tariff3_min)
  ),
  Maximum = c(
    sprintf("%.0f", soybean_stats$Year_max),
    format(soybean_stats$Soybean_max, big.mark = ",", nsmall = 3),
    format(soyoil_stats$Soyoil_max, big.mark = ",", nsmall = 3),
    format(soycake_stats$Soycake_max, big.mark = ",", nsmall = 3),
    format(soybean_stats$Distance_max, big.mark = ",", nsmall = 3),
    sprintf("%.3f", soybean_stats$Contiguity_max),
    sprintf("%.3f", soybean_stats$Comlang_max),
    sprintf("%.3f", soybean_stats$Comcol_max),
    sprintf("%.3f", soybean_stats$Tariff1_max),
    sprintf("%.3f", soyoil_stats$Tariff2_max),
    sprintf("%.3f", soycake_stats$Tariff3_max)
  )
)

# Create the flextable
ft <- flextable(summary_table) %>%
  set_header_labels(
    Variable = "Variable",
    Mean = "Mean or proportion*",
    SD = "SD",
    Minimum = "Minimum",
    Maximum = "Maximum"
  ) %>%
  align(j = "Variable", align = "left") %>%
  align(j = c("Mean", "SD", "Minimum", "Maximum"), align = "right") %>%
  bold(part = "header") %>%
  add_footer_lines("* Binary variables shown as proportions") %>%
  border_outer() %>%
  border_inner_h() %>%
  fontsize(size = 10) %>%
  padding(padding = 4) %>%
  autofit()

# Print the flextable
ft

# Save the summary table data for the QMD file
saveRDS(summary_table, "../data/summary_table.rds")

496,497
496,497
564,153
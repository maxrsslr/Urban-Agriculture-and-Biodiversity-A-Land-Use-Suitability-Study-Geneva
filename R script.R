##############################
# Master Thesis: Urban Agriculture and Biodiversity: 
# A Land-Use Suitability Study of Municipalities in the Geneva Agglomeration.
# Maxence Rössler
##############################

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(viridis)
library(sf)
library(units)
library(xtable)
library('Cairo') # for anti aliasing plots 

# set the working directory
setwd("~/ST GALLEN/Thesis/QGIS/GENEVE")

##### Study area statistics #####

# Define the file path
file_path <- "Population résidante totale par commune, depuis 1950 GE.xls"

# Read the first sheet of the Excel file
df <- read_excel(file_path, sheet = 1)

# delete second column and from lines 47 to 51th lines
df <- df[-c(47:51), -2]

# change column 1's name to Municipality
colnames(df)[1] <- "Municipality"

# Ensure column names (years) are properly formatted as characters
colnames(df)[-1] <- as.character(colnames(df)[-1]) 

# Check the structure of the data
str(df)

# convert 2024 column as numeric 
df$`2024` <- as.numeric(df$`2024`)

# Convert data to long format
df_long <- df %>%
  pivot_longer(cols = -1, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.numeric(Year),  # Convert Year to numeric
         Population = as.numeric(Population)) %>%  # Convert Population to numeric
  filter(Year >= 2004 & Year <= 2024)  # Filter required years

# Check structure
str(df_long)

# Compute Average Annual Growth Rate (AAGR)
aagr_df_all <- df_long %>%
  filter(Year %in% c(2004, 2024)) %>%  # Keep only 2004 and 2024
  pivot_wider(names_from = Year, values_from = Population, names_prefix = "Year_") %>%  # Reshape wide
  mutate(AAGR = ((Year_2024 - Year_2004) / (Year_2004 * (2024 - 2004))) * 100) %>%  # Compute AAGR
  arrange(desc(AAGR))  # Sort by highest AAGR

# Print results
print(aagr_df_all)

# Plot AAGR for municipalities
ggplot(aagr_df_all, aes(x = reorder(Municipality, -AAGR), y = AAGR, fill = AAGR)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Annual Growth Rate (2004-2024)",
       x = "Municipality", y = "AAGR (%)") +
  scale_fill_gradient(low = "blue", high = "red")

# Compute Percentage Change from 2004 to 2024
percent_change_df <- df_long %>%
  filter(Year %in% c(2004, 2024)) %>%  # Keep only 2004 and 2024
  pivot_wider(names_from = Year, values_from = Population, names_prefix = "Year_") %>%  # Reshape wide
  mutate(Percent_Change = ((Year_2024 - Year_2004) / Year_2004) * 100) %>%  # Compute percentage change
  arrange(desc(Percent_Change))  # Sort by highest growth

# Print results
print(percent_change_df)

# Plot the Percentage Change for each municipality
ggplot(percent_change_df, aes(x = reorder(Municipality, -Percent_Change), y = Percent_Change, fill = Percent_Change)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Percentage Population Change (2004-2024)",
       x = "Municipality", y = "Percentage Change (%)") +
  scale_fill_gradient(low = "blue", high = "red")

# extract percentage change for Thonex, Veyrier, Plan les Ouates, Meyrin, Confignon
pct_df <- percent_change_df %>% filter(Municipality %in% c("Thônex", "Veyrier", "Plan-les-Ouates", "Meyrin", "Confignon"))

# add average annual growth rate for the same municipalities
aagr_df <- aagr_df_all %>% filter(Municipality %in% c("Thônex", "Veyrier", "Plan-les-Ouates", "Meyrin", "Confignon"))

# join + order by % change (descending)
plot_df <- pct_df %>% 
  left_join(aagr_df %>% select(Municipality, AAGR), by = "Municipality") %>% 
  arrange(desc(Percent_Change)) %>% 
  mutate(Municipality = factor(Municipality, levels = Municipality))

ggplot(plot_df) +
  # Bars, percent change 
  geom_col(aes(Municipality, Percent_Change, fill = Percent_Change), width = 0.7) +
  # Points,AAGR 
  geom_point(aes(Municipality, AAGR), shape = 21, size = 3.5,fill = "#FF6961", colour = "black" ,stroke = 0.6) +
  # Text labels
  geom_text(aes(Municipality, Percent_Change, 
                label = sprintf("%.1f%%", Percent_Change)),
            hjust = -0.1, size = 3.5, colour = "grey20") +
  geom_text(aes(Municipality, AAGR, 
                label = sprintf("%.2f%%", AAGR)),
            vjust = 1.5, size = 3, colour = "white", fontface = "bold") +
  scale_fill_viridis(
    name = "Percent change\n(2004→2024)",
    option = "cividis", direction = -1
  ) +
  scale_y_continuous(
    name = "Percent change (2004 → 2024)",
    sec.axis = sec_axis(~ ., name = "Average annual growth rate (AAGR)")
  ) +
  coord_flip(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.margin = margin(10, 30, 10, 10)
  )

# save as png
ggsave("Population_change.png", width = 8, height = 6, dpi = 300)

##### Summary Statistics of the Land-use map #####

# Load the land-use map
LU_map <- read.csv("Land use map final.csv")

# compute the summary statistics of the LU map
lu_stats_tbl <- LU_map %>% 
  group_by(CAT_LU) %>% 
  summarise(
    `Number of patches`       = n(),
    `% num of patches`        = n() / nrow(LU_map),
    `Total area (ha)`         = sum(area, na.rm = TRUE),
    `% area`                  = `Total area (ha)` / sum(LU_map$area, na.rm = TRUE),
    `Average patch area (ha)` = mean(area, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    `% num of patches` = percent(`% num of patches`, accuracy = 0.01),
    `% area`           = percent(`% area`,           accuracy = 0.01)
  ) %>% 
  arrange(CAT_LU)                     # optional, pick any ordering you like

# print as Latex table
print(xtable(lu_stats_tbl, type = "latex"), file = "LU_stats.tex")


##### Land-cover analysis #####

# function to round percentages to 2 decimal places
round_row_percent_2dec <- function(row) {
  total <- sum(row, na.rm = TRUE)
  percentages <- row / total * 100
  
  # Scale to whole numbers (2 decimals = *100), to handle rounding accurately
  scaled <- percentages * 100
  floored <- floor(scaled)
  remainders <- scaled - floored
  
  missing <- round(10000 - sum(floored))  # 100.00 * 100 = 10000
  
  if (missing > 0) {
    indices <- order(remainders, decreasing = TRUE)[1:missing]
    floored[indices] <- floored[indices] + 1
  }
  
  # Scale back to real percentages with 2 decimals
  return(floored / 100)
}

# Land Cover analysis per patch

# load the data 
INT <- read.csv("Intersection Milieu NUAs_5.csv")

# pivot table 
LC_analysis <- pivot_wider(INT, names_from = CAT_LC  , values_from = area)

# sort fid column
LC_analysis <- LC_analysis[order(LC_analysis$fid),]

# merge all rows that have identical fid and keep CAT_LU
LC_analysis <- LC_analysis %>% 
  group_by(fid) %>% 
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
            across(where(is.character), first))

# drop OBJECT ID 
LC_analysis <- LC_analysis[,-2]

# change the position of the CAT_LU column to the 2nd position
LC_analysis <- LC_analysis[c(1, 37, 2:36)]

# merge all tree-dominated ecosystems
LC_analysis$Trees <- rowSums(LC_analysis[,c("Forêts inondables", "Chênaies", "Autres forêts", 
                                            "Arbres isolés et alignements", "Plantations d'arbres",
                                            "Chênaies artificialisées", "Hêtraies", "Bosquets urbains")], na.rm = TRUE)

# merge all shrubs formation
LC_analysis$Shrubs <- rowSums(LC_analysis[,c("Haies, buissons et ronciers", "Saulaies buissonnantes",
                                             "Lisières et formations préforestières")], na.rm = TRUE)

# merge all agricultural systems
LC_analysis$Cultivated <- rowSums(LC_analysis[,c("Cultures maraîchères et potagers", "Vergers",
                                                 "Grandes cultures et flore des champs", "Vignes",
                                                 "Pépinières")], na.rm = TRUE)

# merge Water systems
LC_analysis$Water <- rowSums(LC_analysis[,c("Eaux calmes végétalisées", "Eaux")], na.rm = TRUE)

# merge all grasslands
LC_analysis$Grassland <- rowSums(LC_analysis[,c("Milieux herbacés extensifs", "Prairies sèches",
                                                "Gazons et massifs entretenus", "Milieux herbacés intensifs",
                                                "Prairies humides","Rudérales et jachères")], na.rm = TRUE)
# merge wetlands
LC_analysis$Wetland <- rowSums(LC_analysis[,c("Roselières",
                                              "Végétations des rivages",
                                              "Bas marais", "Bas-marais")], na.rm = TRUE)
# merge bare surfaces
LC_analysis$Bare_soil <- rowSums(LC_analysis[,c("Sols et substrats nus", "Glariers", "Gravières")], na.rm = TRUE)

# merge built environment
LC_analysis$Buildings <- rowSums(LC_analysis[,c("Bâtiments")], na.rm = TRUE)

# merge impervious surfaces
LC_analysis$Impervious <- rowSums(LC_analysis[,c("Autres surfaces dures", "Routes", "Voies ferrées")], na.rm = TRUE)

# delete the columns that have been merged using their names
cols.dont.want <- c("Forêts inondables", "Chênaies", "Autres forêts", 
                    "Arbres isolés et alignements", "Plantations d'arbres",
                    "Chênaies artificialisées", "Hêtraies", "Bosquets urbains","Haies, buissons et ronciers", "Saulaies buissonnantes",
                    "Lisières et formations préforestières","Cultures maraîchères et potagers", "Vergers",
                    "Grandes cultures et flore des champs", "Vignes",
                    "Pépinières","Eaux calmes végétalisées", "Eaux","Milieux herbacés extensifs", "Prairies sèches",
                    "Gazons et massifs entretenus", "Milieux herbacés intensifs",
                    "Prairies humides","Rudérales et jachères", "Roselières",
                    "Végétations des rivages", "Bas-marais", "Bas marais",
                    "Sols et substrats nus", "Glariers", "Gravières","Bâtiments", "Autres surfaces dures", "Routes", "Voies ferrées")

LC_analysis <- LC_analysis[, ! names(LC_analysis) %in% cols.dont.want, drop = F]

# create a new dataset for the summary
LC_summary <- LC_analysis

# create a new dataset for Evapotranspirating analysis
LC_EVAPO <- LC_analysis %>% 
  select(fid, CAT_LU, Trees, Shrubs, Cultivated, Grassland, Wetland, Water, Bare_soil, Buildings, Impervious)

# compute the percentage and round the values with rounding function
LC_analysis_percent <- LC_analysis[3:ncol(LC_analysis)]

LC_analysis[3:ncol(LC_analysis)] <- t(apply(LC_analysis_percent, 1, round_row_percent_2dec))

# compute ET_score by doing the weighted sum of land-cover percentages, following the values of Ambrose & Sterling 2014. 
LC_analysis$ET_score <- (LC_analysis$Trees * 0.70) + (LC_analysis$Shrubs * 0.37) + (LC_analysis$Cultivated * 0.58) + (LC_analysis$Grassland * 0.64) + (LC_analysis$Wetland * 1) 

# save as csv
write.csv(LC_analysis, "LC_analysis.csv", row.names = FALSE)

# Calculate evapotranspirating surfaces
LC_EVAPO$Evapotranspirating <- rowSums(LC_EVAPO[, c("Trees", "Shrubs", "Cultivated", "Grassland", "Wetland")], na.rm = TRUE)

# Calculate non-evapotranspirating surfaces (Impervious + Buildings)
LC_EVAPO$Impervious_surfaces <- rowSums(LC_EVAPO[, c("Impervious", "Buildings")], na.rm = TRUE)

# drop merged columns with their names
cols.dont.want <- c("Trees", "Shrubs", "Cultivated", "Grassland", "Wetland", "Water", "Buildings", "Impervious")
LC_EVAPO <- LC_EVAPO[, ! names(LC_EVAPO) %in% cols.dont.want, drop = F]

# new dataset for summary evapo
LC_summary_EVAPO <- LC_EVAPO

# compute percentage and round the values with rounding function
LC_EVAPO_percent <- LC_EVAPO[3:ncol(LC_EVAPO)]

LC_EVAPO[3:ncol(LC_EVAPO)] <- t(apply(LC_EVAPO[3:ncol(LC_EVAPO)], 1, round_row_percent_2dec))

# write as csv
write.csv(LC_EVAPO, "LC_EVAPO.csv", row.names = FALSE)

# continue with Summary analysis 
# group by CAT_LU category 
LC_summary <- LC_summary %>% 
  group_by(CAT_LU) %>% 
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
            across(where(is.character), first))

# drop fid column
LC_summary <- LC_summary[,-2]

# compute percentage and round the values with rounding function
LC_summary_percent <- LC_summary[2:ncol(LC_summary)]

LC_summary[2:ncol(LC_summary)] <- t(apply(LC_summary_percent, 1, round_row_percent_2dec))

# compute total area per row with a new column
LC_summary$Total <- rowSums(LC_summary[2:ncol(LC_summary)], na.rm = TRUE)

# save as csv
write.csv(LC_summary, "LC_summary.csv", row.names = FALSE)

# print as Latex
print(xtable(LC_summary, type = "latex"), file = "Percentage_lU_LC.tex")

# continue with Summary evapo
# group by CAT_LU category
LC_summary_EVAPO <- LC_summary_EVAPO %>% 
  group_by(CAT_LU) %>% 
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
            across(where(is.character), first))

# drop fid column
LC_summary_EVAPO <- LC_summary_EVAPO[,-2]

# compute percentage and round the values with rounding function
LC_summary_EVAPO_percent <- LC_summary_EVAPO[2:ncol(LC_summary_EVAPO)]

LC_summary_EVAPO[2:ncol(LC_summary_EVAPO)] <- t(apply(LC_summary_EVAPO_percent, 1, round_row_percent_2dec))

# put Bare soil at the end
LC_summary_EVAPO <- LC_summary_EVAPO[c(1, 3, 4, 2)]

# save as csv
write.csv(LC_summary_EVAPO, "LC_summary_EVAPO.csv", row.names = FALSE)

# print as Latex table
print(xtable(LC_summary_EVAPO, type = "latex"), file = "Percentage_evapo_lU.tex")

# plot histogram of the distribution of ET_score 
ET_score_plot <- ggplot(LC_analysis, aes(x = ET_score)) +
  geom_histogram(binwidth = 3, fill = "#00441b", color = "black", alpha = 0.7) +
  labs(x = "ET_score", y = "Frequency") +
  theme_minimal()

# save as png
ggsave("ET_score_plot.png",plot = ET_score_plot, width = 8, height = 6, dpi = 300)

# plot histogram of the distribution of Evapotranspirating surfaces
evapotranspirating <- ggplot(LC_EVAPO, aes(x = Evapotranspirating)) +
  geom_histogram(binwidth = 3, fill = "#00441b", color = "black", alpha = 0.7) +
  labs(x = "Percentage of evapotranspirating surfaces", y = "Frequency") +
  theme_minimal()

# save as png
ggsave("Evapotranspirating_plot.png", plot = evapotranspirating, width = 8, height = 6, dpi = 300)

##### Results and plots ####

# Load the data
NUAs_final <- read.csv("NUAs_final.csv")

## Table of the different non-compatible cases and the Final PLUs assigned

# Count the number of incompatible cases for each combination of CAT_LU, Suitability.matrix_2_PLU, and Final.PLU
incompat_stats <- NUAs_final %>%
  filter(unique_compat_Compatibility == "false") %>%
  count(CAT_LU, Suitability.matrix_2_PLU, Final.PLU, name = "n") %>%
  arrange(desc(n))

# print as latex table
print(xtable(incompat_stats, type = "latex"), file = "incompatibility_stats.tex")

## Make an pie chart of the different PLU Final with their percentage

# Set the default graphic device to Cairo
CairoWin()

pie_df <- NUAs_final %>% 
  count(Final.PLU) %>% 
  mutate(label = paste0(Final.PLU, " (", percent(n / sum(n), 0.1), ")"))

pie_chart <- ggplot(NUAs_final, aes(x = "", fill = Final.PLU)) +
  geom_bar(width = 1, colour = NA) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(
    values = c(
      "Agricultural Parks"                = "#e8770e",
      "Allotment gardens"                = "#ffff20",
      "Community Supported Agriculture"  = "#f9ae19",
      "Informal Recreational areas"      = "#47df47",
      "Micro-forest"                    = "#a6d96a",
      "Natural Park"                     = "#1b5100",
      "Urban Gardens"                    = "#cda4dd",
      "Wild spaces"                      = "#33a02c"
    ),
    breaks  = pie_df$Final.PLU,     # keep the original order
    labels  = pie_df$label,         # --> shows “category (12.3%)”
    guide   = guide_legend(ncol = 1) # stack them in one column
  )


# save plot
ggsave(pie_chart, filename = 'PLU_distribution.png', dpi = 300, type = 'cairo',
       width = 8, height = 4, units = 'in')

# close the Cairo graphic device
dev.off()
# Set the default graphic device back to the original
options(device = "windows")

## Compute the summary statistics of the Final PLUs 

stats_tbl <- NUAs_final %>% 
  group_by(Final.PLU) %>% 
  summarise(
    `Number of patches`        = n(),
    `% num of patches`         = n() / nrow(NUAs),      
    `Total area (ha)`          = sum(PA, na.rm = TRUE),
    `% area`                   = `Total area (ha)` / sum(NUAs_final$PA, na.rm = TRUE),
    `Average patch area (ha)`  = mean(PA, na.rm = TRUE),
    ET                         = mean(LC_analysis_ET_score, na.rm = TRUE),
    FR                         = mean(FR.analysis.5_FR, na.rm = TRUE),
    PD                         = mean(proximity.analysis.5_PI, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ## format the two percentage columns as readable %
  mutate(
    `% num of patches` = percent(`% num of patches`, accuracy = 0.01),
    `% area`           = percent(`% area`,           accuracy = 0.01)
  ) %>% 
  arrange(`Final.PLU`)          # optional ordering

# print as Latex table
print(xtable(stats_tbl, type = "latex"), file = "PLU_stats.tex")
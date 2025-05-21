# Replication script for:
# "Mixed Effects by Design? EU Biofuels and the Foreign Spillovers of Hybrid Climate Governance"

rm(list = ls())
set.seed(123)

# Load the necessary libraries
library(did)
library(hrbrthemes)
library(ggplot2)
library(panelView)
library(tidyverse)
library(readxl)
library(hrbrthemes)
library(broom)
library(psych)
library(xtable)

data <- readxl::read_excel("bonsucro_data.xlsx")

##################################################################################
#Effects of Bonsucro-RED on Family Farms and Gender Gap
##################################################################################
data$X_WEIGHTS <- data[["_weight"]]

prop_af_red <- lm(PROP_AF ~ RED_TREATED * DUMMY_YEAR +
                 PARTIDO_PT + SHARE_PT + CANA_HEC + UNI_LOC_CANA +
                 UNI_LOC_ALC, data = data)


prop_af_red_w <- lm(PROP_AF ~ RED_TREATED * DUMMY_YEAR +
                      PARTIDO_PT + SHARE_PT + CANA_HEC + UNI_LOC_CANA +
                      UNI_LOC_ALC, , weights = X_WEIGHTS, data = data)

prop_gg_red <- lm(GENDER_GAP ~ RED_TREATED * DUMMY_YEAR +
                    PARTIDO_PT + SHARE_PT + CANA_HEC + UNI_LOC_CANA +
                    UNI_LOC_ALC, data = data)

prop_gg_red_w <- lm(GENDER_GAP ~ RED_TREATED * DUMMY_YEAR +
                      PARTIDO_PT + SHARE_PT + CANA_HEC + UNI_LOC_CANA +
                      UNI_LOC_ALC, , weights = X_WEIGHTS, data = data)


##################################################################################
# Effects of Bonsucro-RED on exports to the EU
##################################################################################

# Prepare the data for analysis
data_final_eeu <- data %>%
  group_by(CO_MUN) %>%
  mutate(SUM_EXPORT = sum(EXPORT_EU),
         SUM_EXPORT_2010 = sum(EXPORT_EU[YEAR <= 2010]),
         SUM_UN = sum(PROD_UNIT)) %>%
  filter(!(SUM_EXPORT <= 10 & RED_TREATED == 0))

# Assign decile labels based on exports up to 2010
data_final_eeu_slice <- data_final_eeu %>%
  group_by(CO_MUN) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(SUM_EXPORT_2010) %>%
  mutate(DECILE_NTILE = ntile(SUM_EXPORT_2010, 10)) %>%
  select(CO_MUN, DECILE_NTILE)

# Join the decile information back to the main data
data_final_eeu <- left_join(data_final_eeu, data_final_eeu_slice)

# Define the treatment variable and filter for the relevant years
data_final_eeu <- data_final_eeu %>%
  mutate(TREATED = ifelse(YEAR >= FIRST_TREAT_YEAR_RED, 1, 0)) %>%
  filter(YEAR > 2009)

# Visualize the treatment status over time
panelview(EXPORT_EU ~ TREATED, data = data_final_eeu, index = c("CO_MUN", "YEAR"),
          axis.lab = "time", xlab = "Year", ylab = "Municipality", gridOff = FALSE,
          background = "lightgrey", main = "Treatment Status", display.all = TRUE)

# Split the data into upper and lower deciles for heterogeneous effects
upper <- data_final_eeu %>%
  filter(DECILE_NTILE > 5) %>%
  mutate(EXPORT_EU = ifelse(EXPORT_EU == 0, 1, EXPORT_EU)) %>%
  mutate(LN_EXP = log(EXPORT_EU))

lower <- data_final_eeu %>%
  filter(DECILE_NTILE < 6) %>%
  mutate(EXPORT_EU = ifelse(EXPORT_EU == 0, 1, EXPORT_EU)) %>%
  mutate(LN_EXP = log(EXPORT_EU))


# Estimate the average treatment effects using the difference-in-differences method
data_final_eeu$FIRST_TREAT_YEAR_RED <- as.numeric(as.character(data_final_eeu$FIRST_TREAT_YEAR_RED))

dr.overall <- att_gt(yname = "EXPORT_EU",
                     gname = "FIRST_TREAT_YEAR_RED",
                     idname = "CO_MUN",
                     tname = "YEAR",
                     xformla = ~ 1,
                     biters = 1000,
                     control_group = "notyettreated",
                     data = data_final_eeu)

lower$FIRST_TREAT_YEAR_RED <- as.numeric(as.character(lower$FIRST_TREAT_YEAR_RED))
dr.lower <- att_gt(yname = "EXPORT_EU",
                   gname = "FIRST_TREAT_YEAR_RED",
                   idname = "CO_MUN",
                   tname = "YEAR",
                   xformla = ~ 1,
                   biters = 1000,
                   control_group = "notyettreated",
                   data = lower)

upper$FIRST_TREAT_YEAR_RED <- as.numeric(as.character(upper$FIRST_TREAT_YEAR_RED))
dr.upper <- att_gt(yname = "EXPORT_EU",
                   gname = "FIRST_TREAT_YEAR_RED",
                   idname = "CO_MUN",
                   tname = "YEAR",
                   xformla = ~ 1,
                   biters = 1000,
                   control_group = "notyettreated",
                   data = upper)

# Aggregate the treatment effects over time
dr.overall.eff <- aggte(dr.overall, type = "dynamic", na.rm = TRUE)
dr.lower.eff <- aggte(dr.lower, type = "dynamic", na.rm = TRUE)
dr.upper.eff <- aggte(dr.upper, type = "dynamic", na.rm = TRUE)

#################################################################################
#Effects of Bonsucro-RED on deforestation
#################################################################################

# Load deforestation data
deforestation <- read.csv(gzfile("br_inpe_prodes_municipio_bioma.gz")) %>%
  rename(
    CO_MUN = id_municipio,
    YEAR = ano
  ) %>%
  mutate(YEAR = as.integer(YEAR)) %>%
  group_by(CO_MUN, YEAR) %>%
  arrange(CO_MUN, YEAR) %>%
  summarise(
    INCREMENTO = mean(desmatado - dplyr::lag(desmatado), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()

deforestation <- deforestation %>% 
  mutate(YEAR = as.integer(YEAR))

# Merging final dataset with deforestation data
data <- left_join(
  data,
  deforestation,
  by = c("CO_MUN", "YEAR")
)

data <- data %>% 
  mutate(INCREMENTO = ifelse(is.na(INCREMENTO),0,INCREMENTO))

data <- data %>%
  mutate(FIRST_TREAT_YEAR_RED = as.numeric(FIRST_TREAT_YEAR_RED))

dr.overall.deforestation <- att_gt(yname = "INCREMENTO",
                                   gname = "FIRST_TREAT_YEAR_RED",
                                   idname = "CO_MUN",
                                   tname = "YEAR",
                                   xformla = ~ 1,
                                   biters = 1000,
                                   control_group = "notyettreated",
                                   data = data)

dr.overall.deforestation.eff <- aggte(dr.overall.deforestation, type = "dynamic", na.rm = TRUE)

#################################################################################
#Effects of Bonsucro-RED on labor allocation
#################################################################################

# Units with 0 to 4 workers
data$FIRST_TREAT_YEAR_RED <- as.numeric(as.character(data$FIRST_TREAT_YEAR_RED))
dr.labor.0.4 <- att_gt(yname = "WORKERS_0_4_PER",
                       gname = "FIRST_TREAT_YEAR_RED",
                       idname = "CO_MUN",
                       tname = "YEAR",
                       xformla = ~ 1,
                       biters = 1000,
                       control_group = "notyettreated",
                       data = data)

dr.labor.0.4.eff <- aggte(dr.labor.0.4, type = "dynamic", na.rm = TRUE)

# Units with 0 to 9 workers
dr.labor.0.9 <- att_gt(yname = "WORKERS_0_9_PER",
                            gname = "FIRST_TREAT_YEAR_RED",
                            idname = "CO_MUN",
                            tname = "YEAR",
                            xformla = ~ 1,
                            biters = 1000,
                            control_group = "notyettreated",
                            data = data)

dr.labor.0.9.eff <- aggte(dr.labor.0.9, type = "dynamic", na.rm = TRUE)

#################################################################################
#Plotting the results
#################################################################################

# Family Farm and Gender Gap

modelos_red <- list(
  "Family farms (no weights)" = prop_af_red,
  "Family farms (weighted)" = prop_af_red_w,
  "Gender gap (no weights)" = prop_gg_red,
  "Gender gap (weighted)" = prop_gg_red_w
)

extrair_red_att <- function(nome, modelo) {
  coef <- tidy(modelo, conf.int = FALSE)
  linha <- coef %>% filter(term == "RED_TREATED:DUMMY_YEAR")
  if (nrow(linha) == 0) return(NULL)
  
  data.frame(
    Variável = nome,
    ATT = linha$estimate,
    CI_low = linha$estimate - qnorm(0.95) * linha$std.error,
    CI_high = linha$estimate + qnorm(0.95) * linha$std.error
  )
}

att_red_df <- bind_rows(
  mapply(extrair_red_att, names(modelos_red), modelos_red, SIMPLIFY = FALSE)
)

# Graph
gg.af.plot <- ggplot(att_red_df, aes(x = Variável, y = ATT)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "ATT of Bonsucro-RED on % Family Farms and Gender Gap",
       y = "ATT (90% CI)",
       x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("gg.af_plot.jpeg", plot = gg.af.plot, width = 8000, height = 6000, units = "px", dpi = 1000)

# Exports

create_att_plot <- function(model_eff, model_name, output_filename) {
  anos <- model_eff$egt
  att_egt <- model_eff$att.egt
  se <- model_eff$se.egt
  cv <- model_eff$crit.val.egt
  model_eff_df <- as_tibble(cbind(anos, att_egt, se, cv))
  
  # Create the plot
  p <- ggplot(data = model_eff_df, aes(x = anos, y = att_egt)) +
    geom_point(aes(color = ifelse(anos < 0, "Pre-Treatment", "Post-Treatment"))) +
    geom_errorbar(aes(ymin = att_egt - se * cv, ymax = att_egt + se * cv), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Years", y = "Average Treatment Effect on the Treated") +
    ggtitle(paste("", model_name)) +
    theme_ipsum(base_size = 12, base_family = "Arial", grid = "Y") +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(caption = "Dependent variable: Sugarcane and ethanol exports to the European Union.\nControl group: not yet treated municipalities with non-certified sugar producers that export to the EU.") +
    scale_x_continuous(breaks = unique(model_eff_df$anos)) +
    scale_color_manual(values = c("Pre-Treatment" = "red", "Post-Treatment" = "black")) +
    labs(color = "Period") +
    guides(color = guide_legend(title = "Period"))
  
  # Save the plot
  ggsave(output_filename, plot = p, width = 8000, height = 6000, units = "px", dpi = 1000)
}

# Create and save plots for each model
create_att_plot(dr.overall.eff, "", "att_overall.jpeg")
create_att_plot(dr.lower.eff, "", "att_lower.jpeg")
create_att_plot(dr.upper.eff, "", "att_upper.jpeg")


#Deforestation

create_att_plot_2 <- function(model_eff, model_name, output_filename) {
  anos <- model_eff$egt
  att_egt <- model_eff$att.egt
  se <- model_eff$se.egt
  cv <- model_eff$crit.val.egt
  model_eff_df <- as_tibble(cbind(anos, att_egt, se, cv))
  
  # Create the plot
  p <- ggplot(data = model_eff_df, aes(x = anos, y = att_egt)) +
    geom_point(aes(color = ifelse(anos < 0, "Pre-Treatment", "Post-Treatment"))) +
    geom_errorbar(aes(ymin = att_egt - se * cv, ymax = att_egt + se * cv), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Years", y = "Average Treatment Effect on the Treated") +
    ggtitle(paste("", model_name)) +
    theme_ipsum(base_size = 12, base_family = "Arial", grid = "Y") +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(caption = "Dependent variable: Deforestation rate.\nControl group: not yet treated municipalities with non-certified sugar producers that export to the EU.") +
    scale_x_continuous(breaks = unique(model_eff_df$anos)) +
    scale_color_manual(values = c("Pre-Treatment" = "red", "Post-Treatment" = "black")) +
    labs(color = "Period") +
    guides(color = guide_legend(title = "Period"))
  
  # Save the plot
  ggsave(output_filename, plot = p, width = 8000, height = 6000, units = "px", dpi = 1000)
}

# Create and save plots for each model
create_att_plot_2(dr.overall.deforestation.eff, "", "att_overall_deforestation.jpeg")


#Labor Allocation (≥ 4 workers)

create_att_plot_3 <- function(model_eff, model_name, output_filename) {
  anos <- model_eff$egt
  att_egt <- model_eff$att.egt
  se <- model_eff$se.egt
  cv <- model_eff$crit.val.egt
  model_eff_df <- as_tibble(cbind(anos, att_egt, se, cv))
  
  # Create the plot
  p <- ggplot(data = model_eff_df, aes(x = anos, y = att_egt)) +
    geom_point(aes(color = ifelse(anos < 0, "Pre-Treatment", "Post-Treatment"))) +
    geom_errorbar(aes(ymin = att_egt - se * cv, ymax = att_egt + se * cv), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Years", y = "Average Treatment Effect on the Treated") +
    ggtitle(paste("", model_name)) +
    theme_ipsum(base_size = 12, base_family = "Arial", grid = "Y") +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(caption = "Dependent variable: Proportion of agriculture units ≥ 4 workers.\nControl group: not yet treated municipalities with non-certified sugar producers.") +
    scale_x_continuous(breaks = unique(model_eff_df$anos)) +
    scale_color_manual(values = c("Pre-Treatment" = "red", "Post-Treatment" = "black")) +
    labs(color = "Period") +
    guides(color = guide_legend(title = "Period"))
  
  # Save the plot
  ggsave(output_filename, plot = p, width = 8000, height = 6000, units = "px", dpi = 1000)
}

# Create and save plots for each model
create_att_plot_3(dr.labor.0.4.eff, "", "att_labor_0_4.jpeg")

#Labor Allocation (≥ 9 workers)

create_att_plot_4 <- function(model_eff, model_name, output_filename) {
  anos <- model_eff$egt
  att_egt <- model_eff$att.egt
  se <- model_eff$se.egt
  cv <- model_eff$crit.val.egt
  model_eff_df <- as_tibble(cbind(anos, att_egt, se, cv))
  
  # Create the plot
  p <- ggplot(data = model_eff_df, aes(x = anos, y = att_egt)) +
    geom_point(aes(color = ifelse(anos < 0, "Pre-Treatment", "Post-Treatment"))) +
    geom_errorbar(aes(ymin = att_egt - se * cv, ymax = att_egt + se * cv), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Years", y = "Average Treatment Effect on the Treated") +
    ggtitle(paste("", model_name)) +
    theme_ipsum(base_size = 12, base_family = "Arial", grid = "Y") +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(caption = "Dependent variable: Proportion of agriculture units ≥ 9 workers.\nControl group: not yet treated municipalities with non-certified sugar producers.") +
    scale_x_continuous(breaks = unique(model_eff_df$anos)) +
    scale_color_manual(values = c("Pre-Treatment" = "red", "Post-Treatment" = "black")) +
    labs(color = "Period") +
    guides(color = guide_legend(title = "Period"))
  
  # Save the plot
  ggsave(output_filename, plot = p, width = 8000, height = 6000, units = "px", dpi = 1000)
}

# Create and save plots for each model
create_att_plot_4(dr.labor.0.9.eff, "", "att_labor_0_9.jpeg")


#Summaries of all ATTs to create comparative plot
att_objs <- list(
  "Deforestation" = dr.overall.deforestation.eff,
  "Workers 0-4" = dr.labor.0.4.eff,
  "Workers 0-9" = dr.labor.0.9.eff,
  "Exports (upper)" = dr.upper.eff,
  "Exports (lower)" = dr.lower.eff,
  "Export total" = dr.overall.eff
)

att_staggered_df <- lapply(names(att_objs), function(name) {
  obj <- att_objs[[name]]
  data.frame(
    Variável = name,
    ATT = obj$overall.att,
    CI_low = obj$overall.att - qnorm(0.95) * obj$overall.se,
    CI_high = obj$overall.att + qnorm(0.95) * obj$overall.se
  )
}) %>% bind_rows()

comp.plot <- ggplot(att_staggered_df, aes(x = Variável, y = ATT)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "ATT of Bonsucro-RED on multiple variables",
       y = "ATT (90% CI)",
       x = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("comparative_plot.jpeg", plot = comp.plot, width = 8000, height = 6000, units = "px", dpi = 1000)


##################################################################################
#APPENDIX 
##################################################################################

##################################################################################
#Summary of data for the Effects on Family Farms and Gender Gap 
#(data from the Census, only for 2006 and 2017)
##################################################################################

summary_table_census <- describe(data[, c("PROP_AF", "GENDER_GAP", "RED_TREATED", "PARTIDO_PT", "SHARE_PT", "CANA_HEC", "UNI_LOC_CANA", "UNI_LOC_ALC", "INCREMENTO")])
summary_table_census <- summary_table_census[, !colnames(summary_table_census) %in% c("vars")]
colnames(summary_table_census) <- c("N", "Mean", "St. Dev.", "Median", "Trimmed", "MAD", "Min", "Max", "Range", "Skew", "Kurtosis", "St. Error")

print(summary_table_census)

summary_table_census <- as.data.frame(t(summary_table_census))
summary_table_census <- xtable(summary_table_census)

print.xtable(summary_table_census, type = "latex")


##################################################################################
#Summary of data for the Effects on Trade and Deforestation 
##################################################################################

#Overall effect 

summary_table_eeu <- describe(data_final_eeu[, c("EXPORT_EU", "RED_TREATED")])
summary_table_eeu <- summary_table_eeu[, !colnames(summary_table_eeu) %in% c("vars")]
colnames(summary_table_eeu) <- c("N", "Mean", "St. Dev.", "Median", "Trimmed", "MAD", "Min", "Max", "Range", "Skew", "Kurtosis", "St. Error")

print(summary_table_eeu)

summary_table_eeu <- as.data.frame(t(summary_table_eeu))
summary_table_eeu <- xtable(summary_table_eeu)

print.xtable(summary_table_eeu, type = "latex")

#Lower deciles

summary_lower <- describe(lower[, c("EXPORT_EU", "RED_TREATED")])
summary_lower <- summary_lower[, !colnames(summary_lower) %in% c("vars")]
colnames(summary_lower) <- c("N", "Mean", "St. Dev.", "Median", "Trimmed", "MAD", "Min", "Max", "Range", "Skew", "Kurtosis", "St. Error")

print(summary_lower)

summary_lower <- as.data.frame(t(summary_lower))
summary_lower <- xtable(summary_lower)

print.xtable(summary_lower, type = "latex")

#Upper deciles 

summary_upper <- describe(upper[, c("EXPORT_EU", "RED_TREATED")])
summary_upper <- summary_upper[, !colnames(summary_upper) %in% c("vars")]
colnames(summary_upper) <- c("N", "Mean", "St. Dev.", "Median", "Trimmed", "MAD", "Min", "Max", "Range", "Skew", "Kurtosis", "St. Error")

print(summary_upper)

summary_upper <- as.data.frame(t(summary_upper))
summary_upper <- xtable(summary_upper)

print.xtable(summary_upper, type = "latex")


##################################################################################
#Summary of data for the Effects on Agricultura Units 
##################################################################################

summary_table_data <- describe(data[, c("WORKERS_0_4_PER", "WORKERS_0_9_PER", "RED_TREATED")])
summary_table_data <- summary_table_data[, !colnames(summary_table_data) %in% c("vars")]
colnames(summary_table_data) <- c("N", "Mean", "St. Dev.", "Median", "Trimmed", "MAD", "Min", "Max", "Range", "Skew", "Kurtosis", "St. Error")

print(summary_table_data)

summary_table_data <- as.data.frame(t(summary_table_data))
summary_table_data <- xtable(summary_table_data)

print.xtable(summary_table_data, type = "latex")

##################################################################################
#Graph of treated municipalities 
##################################################################################

#For the effects on Family Agriculture, Gender Gap and Agriculture Units 

treatment_data <- data %>%
  group_by(CO_MUN) %>%
  summarise(RED_TREATED = first(RED_TREATED)) %>%
  filter(!is.na(RED_TREATED)) %>%              # <-- exclude NAs here
  mutate(
    RED_TREATED_LABEL = ifelse(RED_TREATED == 1, "Treated", "Untreated")
  ) %>%
  count(RED_TREATED_LABEL) %>%
  ggplot(aes(x = RED_TREATED_LABEL, y = n, fill = RED_TREATED_LABEL)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Untreated" = "lightblue", "Treated" = "#003366")
  ) +
  labs(
    title = "Number of Treated vs. Untreated Municipalities",
    x = "",
    y = "Number of Municipalities",
    caption = "Note: Subset for the effects on family farms, gender gap and agriculture units."
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("treatment_data.jpeg", plot = treatment_data, width = 8000, height = 6000, units = "px", dpi = 1000)


#For the effects on trade (overall) and deforestation

treatment_eeu <- data_final_eeu %>%
  group_by(CO_MUN) %>%
  summarise(RED_TREATED = first(RED_TREATED)) %>%
  filter(!is.na(RED_TREATED)) %>%              # <-- exclude NAs here
  mutate(
    RED_TREATED_LABEL = ifelse(RED_TREATED == 1, "Treated", "Untreated")
  ) %>%
  count(RED_TREATED_LABEL) %>%
  ggplot(aes(x = RED_TREATED_LABEL, y = n, fill = RED_TREATED_LABEL)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Untreated" = "lightblue", "Treated" = "#003366")
  ) +
  labs(
    title = "Number of Treated vs. Untreated Municipalities",
    x = "",
    y = "Number of Municipalities",
    caption = "Note: Subset for the effects on exports (overall) and deforestation."
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("treatment_eeu.jpeg", plot = treatment_eeu, width = 8000, height = 6000, units = "px", dpi = 1000)


#For the effects on trade (upper)

treatment_upper <- upper %>%
  group_by(CO_MUN) %>%
  summarise(RED_TREATED = first(RED_TREATED)) %>%
  filter(!is.na(RED_TREATED)) %>%              # <-- exclude NAs here
  mutate(
    RED_TREATED_LABEL = ifelse(RED_TREATED == 1, "Treated", "Untreated")
  ) %>%
  count(RED_TREATED_LABEL) %>%
  ggplot(aes(x = RED_TREATED_LABEL, y = n, fill = RED_TREATED_LABEL)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Untreated" = "lightblue", "Treated" = "#003366")
  ) +
  labs(
    title = "Number of Treated vs. Untreated Municipalities",
    x = "",
    y = "Number of Municipalities",
    caption = "Note: Subset for the effects on exports (upper deciles)."
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("treatment_upper.jpeg", plot = treatment_upper, width = 8000, height = 6000, units = "px", dpi = 1000)


#For the effects on trade (lower)

treatment_lower <- lower %>%
  group_by(CO_MUN) %>%
  summarise(RED_TREATED = first(RED_TREATED)) %>%
  filter(!is.na(RED_TREATED)) %>%              # <-- exclude NAs here
  mutate(
    RED_TREATED_LABEL = ifelse(RED_TREATED == 1, "Treated", "Untreated")
  ) %>%
  count(RED_TREATED_LABEL) %>%
  ggplot(aes(x = RED_TREATED_LABEL, y = n, fill = RED_TREATED_LABEL)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Untreated" = "lightblue", "Treated" = "#003366")
  ) +
  labs(
    title = "Number of Treated vs. Untreated Municipalities",
    x = "",
    y = "Number of Municipalities",
    caption = "Note: Subset for the effects on exports (lower deciles)."
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("treatment_lower.jpeg", plot = treatment_lower, width = 8000, height = 6000, units = "px", dpi = 1000)


sink("session_info.txt")
sessionInfo()
sink()


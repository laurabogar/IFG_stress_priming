# Reading LI-600 data
# Adapted from Chat GPT
# 21 Oct 2024 LB

library(tidyverse)
source("read_LI-600.R")

poplar_file_path = "../data/export_2024_10_21_164809/Bogar FvFm/2024-10-15/Bogar FvFm_LMB_2024_10_15_16_28_28_1.csv"
dougfir_file_path = "../data/600N_export/Bogar FvFm 600N/2024-10-15/Bogar FvFm 600N_LMB_2024_10_15_16_24_39_1.csv"
treatments = read_csv("240910_all_plants_and_treatments.csv")

dark_fluor_pop = read_li600_data(poplar_file_path)
dark_fluor_pop$plantID = dark_fluor_pop$`Plant number`
dark_fluor_doug = read_li600_data(dougfir_file_path)
dark_fluor_doug$plantID = dark_fluor_doug$`Plant number`

treatments$plantID = str_pad(treatments$Plant, width = 3, pad = "0")
treatments$ABA = fct_recode(treatments$Tape_color, 
                            "3 wks" = "O", 
                            "2 wks" = "Y",
                            "0 wks" = "G", 
                            "None" = "B") %>%
  fct_relevel("0 wks", after = Inf)
treatments$soil = fct_recode(treatments$Soil, 
                            "Douglas fir - live" = "DE",
                            "Douglas fir - sterilized" = "DFS",
                            "Poplar - live" = "PE",
                            "Poplar - sterilized" = "PS")

pop_fluor_with_tx = left_join(dark_fluor_pop, treatments)
doug_fluor_with_tx = left_join(dark_fluor_doug, treatments)

pop_fluor_summ = pop_fluor_with_tx %>% group_by(plantID, Species, soil, ABA) %>%
  summarize(meanfvfm = mean(`Fv/Fm`))
doug_fluor_summ = doug_fluor_with_tx %>% group_by(plantID, Species, soil, ABA) %>%
  summarize(meanfvfm = mean(`Fv/Fm`))

both_fluor_summ = full_join(pop_fluor_summ, doug_fluor_summ)
both_fluor_summ = both_fluor_summ[!is.na(both_fluor_summ$Species),]
both_fluor_summ = both_fluor_summ[!is.na(both_fluor_summ$ABA),]


# trying to see everything at once
ggplot(both_fluor_summ, aes(x = Species, y = meanfvfm, fill = ABA)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.5, alpha = 0.6) + 
  labs(x = "Plant species",
       y = "Fv/Fm as of 15 Oct") +
  theme_minimal()

# just Populus
ggplot(subset(both_fluor_summ, Species == "Poplar"), aes(x = Species, y = meanfvfm, fill = Soil, shape = ABA)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.5, alpha = 0.6) + 
  labs(x = "Plant species",
       y = "Fv/Fm as of 15 Oct") +
  theme_minimal()

# just Doug fir
ggplot(subset(both_fluor_summ, Species == "Douglas fir"), aes(x = Species, y = meanfvfm, fill = Soil, shape = ABA)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.5, alpha = 0.6) + 
  labs(x = "Plant species",
       y = "Fv/Fm as of 15 Oct") +
  theme_minimal()

# Takeaways:
# Both plants did better in their own soil compared
# to the "away" soil
# ABA changes drought response in sterilized soils

### Subsetting to exclude flagged values ###

pop_fluor_with_tx_noflag = subset(pop_fluor_with_tx, is.na(remark))
doug_fluor_with_tx_noflag = subset(doug_fluor_with_tx, is.na(remark))


pop_fluor_summ = pop_fluor_with_tx_noflag %>% group_by(plantID, Species, soil, ABA) %>%
  summarize(meanfvfm = mean(`Fv/Fm`))
doug_fluor_summ = doug_fluor_with_tx_noflag %>% group_by(plantID, Species, soil, ABA) %>%
  summarize(meanfvfm = mean(`Fv/Fm`))

both_fluor_summ = full_join(pop_fluor_summ, doug_fluor_summ)
both_fluor_summ = both_fluor_summ[!is.na(both_fluor_summ$Species),]
both_fluor_summ = both_fluor_summ[!is.na(both_fluor_summ$ABA),]


# just Populus, grouped by ABA
ggplot(subset(both_fluor_summ, Species == "Poplar"), aes(x = ABA, y = meanfvfm, fill = soil)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.5, alpha = 0.6) + 
  labs(x = "ABA treatment",
       y = "Poplar Fv/Fm as of 15 Oct") +
  theme_minimal()

# just Populus, grouped by soil
ggplot(subset(both_fluor_summ, Species == "Poplar"), aes(x = soil, y = meanfvfm, fill = ABA)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.5, alpha = 0.6) + 
  labs(x = "Soil",
       y = "Poplar Fv/Fm as of 15 Oct") +
  theme_minimal()

# just Doug fir, grouped by ABA
ggplot(subset(both_fluor_summ, Species == "Douglas fir"), aes(x = ABA, y = meanfvfm, fill = soil)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.5, alpha = 0.6) + 
  labs(x = "ABA treatment",
       y = "Douglas fir Fv/Fm as of 15 Oct") +
  theme_minimal()

# just Doug fir, grouped by soil
ggplot(subset(both_fluor_summ, Species == "Douglas fir"), aes(x = soil, y = meanfvfm, fill = ABA)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.5, alpha = 0.6) + 
  labs(x = "Soil",
       y = "Douglas fir Fv/Fm as of 15 Oct") +
  theme_minimal()

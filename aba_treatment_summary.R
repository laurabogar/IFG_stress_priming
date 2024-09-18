library("tidyverse")

# Download these fresh from the Google drive to make sure it's updated if possible
treatments = read_csv("Plant IDs - Sheet1 (2).csv")
ABA = read_csv("ABA treatment flag key - Assignments (1).csv")

courtneys_record = read_csv("Bogar_Project_List_2024.csv")
courtneys_record = courtneys_record[,1:5]

together = left_join(treatments, ABA)
ABA_distribution = together %>% group_by(Species, Soil, Tape_color) %>% summarize(count = n())


write_csv(ABA_distribution, "aba_treatment_distribution.csv")

write_csv(together, "240910_all_plants_and_treatments.csv")

together[is.na(together$Tape_color),]

ABA_distribution[ABA_distribution$count <5,]

#Courtney's better data

courtneys_record$flag = case_match(courtneys_record$flag, "B" ~ "blue", "GBl" ~ "green", "GWh" ~ "green", "O" ~ "orange", "Y" ~ "yellow")

courtney_summary = courtneys_record %>% group_by(species, treatment, flag) %>% summarize(count = n())
write_csv(courtney_summary, "courtney_aba_treatment_distribution.csv")

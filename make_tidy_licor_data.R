# make_tidy_licor_data.R
#
# Builds tidy, one-row-per-plant tables of LI-600 measurements for sharing
# with collaborators (e.g. to pair with RNA-seq samples).
#
#   Inputs : raw LI-600 / LI-600N exports in ../data/ (not tracked in git)
#            240910_all_plants_and_treatments.csv (plant -> treatment key)
#   Outputs: tidydata/  (tracked in git; see tidydata/README.md)
#     populus_fvfm_by_plant.csv        dark-adapted, 15 Oct 2024
#     populus_gsw_dark_by_plant.csv    dark-adapted, 15 Oct 2024
#     populus_gsw_light_by_plant.csv   light-adapted, 18 Oct 2024
#     pseudotsuga_fvfm_by_plant.csv    dark-adapted, 4 Nov 2024
#     li600_measurements_qc.csv        every measurement, with QC columns
#
# Run from the directory that contains this script:
#   Rscript make_tidy_licor_data.R
#
# Reworked Nov 2026 from analyzing_LICOR_exploratory.R and
# "Poplar light adapted preliminary analyses.Rmd" (LB, Oct 2024).

library(tidyverse)
source("read_LI-600.R")   # provides read_li600_clean()

# ---- Settings ---------------------------------------------------------------

out_dir <- "tidydata"

# One row per raw export file.
#   condition: "dark" (dark-adapted, for Fv/Fm) or "light" (ambient light)
#   gsw_valid: FALSE where no attempt was made to get good gsw (needles)
# Not listed on purpose: the 15 Oct Pseudotsuga file (not harvest), and two
# copies of the 18 Oct light file (..._2024_10_21_16_34_30_1.csv and
# ..._2024_10_21_16_40_52_1.csv), which are byte-for-byte identical to it.
li600_files <- tribble(
  ~expected_species, ~condition, ~instrument,                 ~gsw_valid, ~path,
  "Poplar",      "dark",  "LI-600 (Bogar lab)",          TRUE,
    "../data/600_export/Bogar FvFm/2024-10-15/Bogar FvFm_LMB_2024_10_15_16_28_28_1.csv",
  "Poplar",      "light", "LI-600 (Bogar lab)",          TRUE,
    "../data/600_export/Bogar light/2024-10-18/Bogar light_LMB_2024_10_18_15_26_20_1.csv",
  "Douglas fir", "dark",  "LI-600N (IFG Placerville)",   FALSE,
    "../data/241104 Pseudotsuga Bogar FvFm 600N IFG instrument/2024-11-04/Bogar FvFm 600N_LMB_2024_11_04_15_41_19_1.csv",
  "Douglas fir", "dark",  "LI-600N (Bogar lab)",         FALSE,
    "../data/241104 Pseudosuga dark adapted FvFm Bogar instrument/2024-11-04/Bogar FvFm 600N_LMB_2024_11_04_16_24_47_1.csv"
)

treatment_file <- "240910_all_plants_and_treatments.csv"

# Readable labels for the treatment codes
aba_labels  <- c(O = "3 wks", Y = "2 wks", G = "0 wks", B = "None")
soil_labels <- c(DE  = "Douglas fir - live",
                 DFS = "Douglas fir - sterilized",
                 PE  = "Poplar - live",
                 PS  = "Poplar - sterilized")

# ---- Manual corrections (each with its reason) ------------------------------

# Fill in missing tape colors in the treatment key.
tape_color_fixes <- tribble(
  ~plantID, ~Tape_color, ~reason,
  "031",    "B",         "Blank in key; RNA metadata says no ABA (tape likely fell off)"
)

# Clear user flags judged to be mistakes.
unflag_fixes <- tribble(
  ~plantID, ~source_file,                               ~reason,
  "122",    "Bogar FvFm_LMB_2024_10_15_16_28_28_1.csv", "First readings of the session; flagged by mistake (Fo, Fm, Fv/Fm, leak all normal)"
)

# ---- QC thresholds ----------------------------------------------------------

# Fv/Fm: largest plausible value. Healthy C3 leaves top out around 0.83-0.85;
# higher values here come from readings with a near-zero Fo, not biology.
max_fvfm <- 0.85

# Fv/Fm: optional smallest Fm. NA = off. About 10% of Douglas-fir readings
# have Fm < 100 (a needle covers only part of the LI-600N aperture). They are
# kept, but flagged as low_fluor_signal in the measurement-level file.
min_Fm <- NA
low_signal_Fm <- 100   # only used for the flag column

# gsw: largest acceptable leak_pct. Values fall into two clear groups,
# ~1-2% (sealed) and ~110% (not sealed), so any cutoff between them works.
max_leak_pct <- 10

# gsw: largest acceptable deviation of rh_adj from the file's median.
# Every ~10 min the LI-600 matches its two humidity sensors; rh_adj is the
# offset applied. Most matches give rh_adj around -0.3. A few give outliers
# (-0.81, +1.55, -3 on 15 Oct; -1.4 on 18 Oct), and every gsw reading until
# the next match is shifted by a near-constant amount (e.g. all ~ -0.24 or
# all ~ +0.21 in the dark). This drops those match windows.
max_rh_adj_dev <- 0.45

# gsw is NOT filtered on sign: true gsw is small, so good readings scatter on
# both sides of zero. Dropping negatives would bias plant means upward.

# ---- Treatment key ----------------------------------------------------------

treatments <- read_csv(treatment_file, show_col_types = FALSE) %>%
  mutate(plantID = str_pad(as.character(Plant), width = 3, pad = "0")) %>%
  rows_update(select(tape_color_fixes, plantID, Tape_color), by = "plantID") %>%
  mutate(ABA  = unname(aba_labels[Tape_color]),
         soil = unname(soil_labels[Soil])) %>%
  select(plantID, Species, soil, soil_code = Soil,
         ABA, ABA_tape_color = Tape_color)

if (any(duplicated(treatments$plantID))) {
  stop("plantID is not unique in ", treatment_file)
}

# ---- Read raw LI-600 files --------------------------------------------------

keep_cols <- c("Obs#", "Date", "Time", "Plant number", "Leaf number", "remark",
               "Fo", "Fm", "Fv/Fm", "PhiPS2",
               "gsw", "leak_pct", "rh_adj", "match_time", "auto",
               "Qamb", "Tleaf", "VPDleaf")

read_one_file <- function(path, expected_species, condition, instrument,
                          gsw_valid) {
  read_li600_clean(path) %>%
    select(any_of(keep_cols)) %>%
    mutate(plantID          = str_pad(`Plant number`, width = 3, pad = "0"),
           expected_species = expected_species,
           condition        = condition,
           instrument       = instrument,
           source_file      = basename(path),
           match_time       = as.character(match_time),
           gsw              = if (gsw_valid) gsw else NA_real_,
           # how far this reading's humidity-sensor match is from typical
           rh_adj_dev       = abs(rh_adj - median(rh_adj, na.rm = TRUE)))
}

raw <- pmap(li600_files, read_one_file) %>% bind_rows()

# Apply flag corrections
raw <- raw %>%
  left_join(mutate(unflag_fixes, unflagged = TRUE) %>% select(-reason),
            by = c("plantID", "source_file")) %>%
  mutate(remark = if_else(coalesce(unflagged, FALSE), NA_character_, remark)) %>%
  select(-unflagged)

# ---- Attach treatments, with checks -----------------------------------------

unmatched <- raw %>%
  distinct(plantID, source_file) %>%
  anti_join(treatments, by = "plantID")
if (nrow(unmatched) > 0) {
  warning(nrow(unmatched), " plantID(s) are not in ", treatment_file,
          " and will be dropped:")
  print(unmatched)
}

meas <- raw %>% inner_join(treatments, by = "plantID")

# Known case: the 15 Oct Populus file also holds 15 Oct Douglas-fir readings
# (16 plants). Those are not harvest data, so they are dropped here.
wrong_species <- meas %>%
  filter(Species != expected_species) %>%
  distinct(plantID, Species, expected_species, source_file)
if (nrow(wrong_species) > 0) {
  message("\nDropping ", nrow(wrong_species), " plant(s) whose species ",
          "doesn't match the file they appear in:")
  print(wrong_species, n = Inf)
  meas <- meas %>% filter(Species == expected_species)
}

# Known case: plant 007 (Douglas fir) has no tape color in the key.
no_aba <- meas %>% filter(is.na(ABA)) %>% distinct(plantID)
if (nrow(no_aba) > 0) {
  warning("Dropping plant(s) with no ABA treatment recorded: ",
          paste(no_aba$plantID, collapse = ", "))
  meas <- meas %>% filter(!is.na(ABA))
}

# ---- QC columns -------------------------------------------------------------

meas <- meas %>%
  mutate(
    qc_no_user_flag  = is.na(remark),
    low_fluor_signal = Fm < low_signal_Fm,
    fvfm_qc_pass     = condition == "dark" & qc_no_user_flag &
                       !is.na(`Fv/Fm`) & `Fv/Fm` > 0 & `Fv/Fm` <= max_fvfm &
                       (is.na(min_Fm) | Fm >= min_Fm),
    gsw_qc_pass      = qc_no_user_flag & !is.na(gsw) &
                       !is.na(leak_pct) & leak_pct <= max_leak_pct &
                       !is.na(rh_adj_dev) & rh_adj_dev <= max_rh_adj_dev
  )

# Diagnostics: gsw by humidity-match window, so the rh_adj rule can be checked
message("\ngsw by humidity-sensor match window (Populus, unflagged, sealed):")
meas %>%
  filter(Species == "Poplar", qc_no_user_flag, leak_pct <= max_leak_pct) %>%
  group_by(condition, match_time, rh_adj) %>%
  summarise(n = n(), median_gsw = round(median(gsw), 3),
            frac_negative = round(mean(gsw < 0), 2),
            kept = all(rh_adj_dev <= max_rh_adj_dev), .groups = "drop") %>%
  print(n = Inf)

message("\nMeasurements passing QC:")
meas %>%
  group_by(Species, condition) %>%
  summarise(n_total     = n(),
            n_user_flag = sum(!qc_no_user_flag),
            n_fvfm_pass = sum(fvfm_qc_pass),
            n_gsw_pass  = sum(gsw_qc_pass), .groups = "drop") %>%
  print()

# ---- Per-plant summaries ----------------------------------------------------

id_cols <- c("plantID", "Species", "soil", "soil_code", "ABA", "ABA_tape_color")

collapse_unique <- function(x) paste(sort(unique(x)), collapse = ";")

summarise_fvfm <- function(d) {
  d %>%
    filter(fvfm_qc_pass) %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(fvfm_mean        = mean(`Fv/Fm`),
              fvfm_sd          = sd(`Fv/Fm`),
              n_measurements   = n(),
              n_low_signal     = sum(low_fluor_signal),
              measurement_date = collapse_unique(Date),
              instrument       = collapse_unique(instrument),
              .groups = "drop") %>%
    arrange(plantID)
}

summarise_gsw <- function(d) {
  d %>%
    filter(gsw_qc_pass) %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(gsw_mean         = mean(gsw),
              gsw_sd           = sd(gsw),
              n_measurements   = n(),
              mean_Qamb        = mean(Qamb),
              mean_Tleaf       = mean(Tleaf),
              mean_VPDleaf     = mean(VPDleaf),
              mean_leak_pct    = mean(leak_pct),
              measurement_date = collapse_unique(Date),
              first_time       = min(Time),
              instrument       = collapse_unique(instrument),
              .groups = "drop") %>%
    arrange(plantID)
}

pop_dark  <- meas %>% filter(Species == "Poplar", condition == "dark")
pop_light <- meas %>% filter(Species == "Poplar", condition == "light")
df_dark   <- meas %>% filter(Species == "Douglas fir", condition == "dark")

# ---- Write outputs ----------------------------------------------------------

dir.create(out_dir, showWarnings = FALSE)

write_csv(summarise_fvfm(pop_dark),
          file.path(out_dir, "populus_fvfm_by_plant.csv"))
write_csv(summarise_gsw(pop_dark),
          file.path(out_dir, "populus_gsw_dark_by_plant.csv"))
write_csv(summarise_gsw(pop_light),
          file.path(out_dir, "populus_gsw_light_by_plant.csv"))
write_csv(summarise_fvfm(df_dark),
          file.path(out_dir, "pseudotsuga_fvfm_by_plant.csv"))

# Measurement-level file with clean column names, for provenance
meas %>%
  select(plantID, Species, soil, ABA, condition, instrument, source_file,
         obs_number = `Obs#`, date = Date, time = Time,
         leaf_number = `Leaf number`, user_flag = remark,
         Fo, Fm, fvfm = `Fv/Fm`, PhiPS2, gsw, leak_pct, rh_adj, match_time,
         Qamb, Tleaf, VPDleaf,
         low_fluor_signal, fvfm_qc_pass, gsw_qc_pass) %>%
  mutate(fvfm = if_else(condition == "dark", fvfm, NA_real_),
         PhiPS2 = if_else(condition == "light", PhiPS2, NA_real_)) %>%
  arrange(Species, condition, plantID, date, time) %>%
  write_csv(file.path(out_dir, "li600_measurements_qc.csv"))

message("\nWrote tidy files to ", normalizePath(out_dir))

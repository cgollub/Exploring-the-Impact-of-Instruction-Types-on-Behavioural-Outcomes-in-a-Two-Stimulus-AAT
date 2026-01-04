#==============================================================================
#=====   Load necessary Libraries                       =======================
#==============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(scales)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(broom.mixed)
library(performance)
library(magick)




#==============================================================================
#=====   Load necessary data from pre-processing         =======================
#==============================================================================

setwd("SET/YOUR/PATH/TO/SAVED/CSV")

E1 <- readr::read_csv("experiment_1_preprocessing.csv")

E2 <- readr::read_csv("experiment_2_preprocessed.csv")

#==============================================================================
#=====   Clean data for complete similarity             =======================
#==============================================================================

E1 <- E1 %>%
  mutate(
    instruction = "implicit",
    congruence = factor(congruence, levels = c("congruent","incongruent")),
    valence    = factor(valence,    levels = c("negative","positive")),
    participant = factor(participant),
    picture_id = factor(picture_id)
  )


E2 <- E2 %>%
  mutate(
    instruction = "explicit",
    congruence = factor(congruence, levels = c("congruent","incongruent")),
    valence    = factor(valence,    levels = c("negative","positive")),
    participant = factor(participant), 
    picture_id = factor(picture_id)
  )

#=========================================================================
#===    Cleaning Data for Reaction Time                           ========
#=== -->  Filter to trials with valid RT                          ========
#===      (i.e., correct-only if incorrect were set to NA RT)     ========
#=========================================================================


E1_rt <- E1 %>%
  filter(!is.na(RT_log10)) %>%
  group_by(participant) %>%
  arrange(trial, .by_group = TRUE) %>%
  mutate(
    trial_index = trial,
    z_trial = as.numeric(scale(trial_index)),
    RT_log10 = RT_log10
  ) %>%
  ungroup()


E2_rt <- E2 %>%
  filter(!is.na(RT_log10)) %>%
  group_by(participant) %>%
  arrange(trial, .by_group = TRUE) %>%
  mutate(
    trial_index = trial,
    z_trial = as.numeric(scale(trial_index)),
    RT_log10 = RT_log10
  ) %>%
  ungroup()

# ======== Harmonize frame_side variable ========
E1_rt <- E1_rt %>%
  mutate(frame_side = factor(frame_side,
                             levels = c("left", "right")))



E2_rt <- E2_rt %>%
  mutate(frame_side = factor(ifelse(frame_side == 1, "right", "left"),
                             levels = c("left", "right")))



#==============================================================================
#=====    Getting overview of data ============================================
#==============================================================================

#===   1. Checking congruence in both Tasks =========================
# First, filter the data for each experiment and then count the congruence values

# For Implicit Task (E1)
E1_congruence_counts <- E1 %>% 
  group_by(congruence) %>% 
  summarise(count = n(),
            non_RT_na = sum(!is.na(RT_ms)),
            .groups = "drop"
  )

# For Explicit Task (E2)
E2_congruence_counts <- E2 %>% 
  group_by(congruence) %>% 
  summarise(count = n(),
            non_RT_na = sum(!is.na(RT_ms)),
            .groups = "drop"
  )

# Print out the counts
print("Experiment 1: Congruence Counts")
print(E1_congruence_counts)

print("Experiment 2: Congruence Counts")
print(E2_congruence_counts)

#====  2. counting block4 outcome: CP/NP/IP/IN =============================

# For Implicit Task 
final_by_block4 <- E1 %>%
  group_by(block4) %>%
  summarise(
    n = n(),
    non_RT_na = sum(!is.na(RT_log10)),
    .groups = "drop"
  )
print(final_by_block4)

# For Explicit Task 
final_by_block4 <- E2 %>%
  group_by(block4) %>%
  summarise(
    n = n(),
    non_RT_na = sum(!is.na(RT_log10)),
    .groups = "drop"
  )
print(final_by_block4)

#==============================================================================
#=====   Calculate mean across four blocks              =======================
#==============================================================================

# ====   Summarise mean RT and SD per group (block4) ===================
E1_group_stats <- E1%>%
  filter(!is.na(RT_ms)) %>%
  group_by(block4) %>%
  summarise(
    mean_RT = mean(RT_ms, na.rm = TRUE),
    sd_RT   = sd(RT_ms, na.rm = TRUE),
    se_RT   = sd(RT_ms, na.rm = TRUE) / sqrt(n()), 
    n       = n(),
    .groups = "drop"
  ) %>%
  mutate(experiment = "Implicit task")

E2_group_stats <- E2%>%
  filter(!is.na(RT_ms)) %>%
  group_by(block4) %>%
  summarise(
    mean_RT = mean(RT_ms, na.rm = TRUE),
    sd_RT   = sd(RT_ms, na.rm = TRUE),
    se_RT   = sd(RT_ms, na.rm = TRUE) / sqrt(n()), 
    n       = n(),
    .groups = "drop"
  ) %>%
  mutate(experiment = "Explicit task")

# Combine both
group_stats <- bind_rows(E1_group_stats, E2_group_stats)

print(group_stats)

#===============================================================================
#========      For each factor independently   =================================
#===============================================================================
# For Implicit Task
E1_cong_summary <- E1 %>%
  filter(!is.na(RT_ms)) %>%
  group_by(congruence) %>%
  summarise(mean_RT = mean(RT_ms), sd_RT = sd(RT_ms), n = n(),
            se_RT = sd_RT/sqrt(n), .groups="drop") %>%
  mutate(experiment = "Implicit")

E1_val_summary <- E1 %>%
  filter(!is.na(RT_ms)) %>%
  group_by(valence) %>%
  summarise(mean_RT = mean(RT_ms), sd_RT = sd(RT_ms), n = n(),
            se_RT = sd_RT/sqrt(n), .groups="drop") %>%
  mutate(experiment = "Implicit")

# For Explicit Task
E2_cong_summary <- E2 %>%
  filter(!is.na(RT_ms)) %>%
  group_by(congruence) %>%
  summarise(mean_RT = mean(RT_ms), sd_RT = sd(RT_ms), n = n(),
            se_RT = sd_RT/sqrt(n), .groups="drop") %>%
  mutate(experiment = "Explicit")

E2_val_summary <- E2 %>%
  filter(!is.na(RT_ms)) %>%
  group_by(valence) %>%
  summarise(mean_RT = mean(RT_ms), sd_RT = sd(RT_ms), n = n(),
            se_RT = sd_RT/sqrt(n), .groups="drop") %>%
  mutate(experiment = "Explicit")

# Print results
print("Experiment 1: RT by Congruence")
print(E1_cong_summary)

print("Experiment 1: RT by Valence")
print(E1_val_summary)

print("Experiment 2: RT by Congruence")
print(E2_cong_summary)

print("Experiment 2: RT by Valence")
print(E2_val_summary)

#===============================================================================
#==== For Instruction Types/ tasks   ===========================================
#=====  min,max,mean,std and valid trials ======================================
#===============================================================================

# Summarize for Implicit Task (with rounded values)
E1_overall_summary <- E1_rt %>%
  summarise(
    mean_RT = round(mean(RT_ms, na.rm = TRUE), 2),
    sd_RT = round(sd(RT_ms, na.rm = TRUE), 2),
    min_RT = round(min(RT_ms, na.rm = TRUE), 2),
    max_RT = round(max(RT_ms, na.rm = TRUE), 2),
    valid_trials = sum(!is.na(RT_ms)),  # Count non-NA values for valid trials
    n = n()  # Total number of trials
  )

# Summarize for Explicit task (with rounded values)
E2_overall_summary <- E2_rt %>%
  summarise(
    mean_RT = round(mean(RT_ms, na.rm = TRUE), 2),
    sd_RT = round(sd(RT_ms, na.rm = TRUE), 2),
    min_RT = round(min(RT_ms, na.rm = TRUE), 2),
    max_RT = round(max(RT_ms, na.rm = TRUE), 2),
    valid_trials = sum(!is.na(RT_ms)),  # Count non-NA values for valid trials
    n = n()  # Total number of trials
  )

# Print the summary for Experiment 1 and Experiment 2
print("Experiment 1 overall (raw RT):")
print(E1_overall_summary)

print("Experiment 2 overall (raw RT):")
print(E2_overall_summary)

#===============================================================================
#================= Plotting descriptive Analysis outcome =======================
#===============================================================================
thesis_theme <- theme(
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.x = element_text(size = 20, face = "bold"),
  axis.title.y = element_text(size = 18, face = "bold"),
  axis.line = element_line(linewidth = 0.8),
  axis.ticks = element_line(linewidth = 0.8), 
  legend.title = element_text(size = 20, face = "bold"),
  legend.text  = element_text(size = 20)
  )
#(1) Violin plots for groups 
# colors per group (optional)
cols <- c(CN="#4477AA", CP="#66CCEE", IN="#228833", IP="#EE6677")
colours_cong <-c(congruent = "#4477AA", incongruent = "#66CCEE")
colours_val <- c(negative = "#EE6677", positive = "#228833")
E1_rt <- E1_rt %>%
  mutate(
    block4 = factor(block4, levels = c("CN","CP","IN","IP"))
  )

E2_rt <- E2_rt %>%
  mutate(
    block4 = factor(block4, levels = c("CN","CP","IN","IP"))
  )

ggplot(E1_rt, aes(x = block4, y = RT_ms, fill = block4)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 0.6) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = cols) +
  labs(
    #title = "Reaction Time Distributions by Valence and Congruence (Implicit Task)",
    x = "Condition (Congruence × Valence)",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal(base_size = 20
  ) + theme(plot.title = element_text(face = "bold", hjust = 0.5),
            legend.position = "none"
  ) + thesis_theme

ggplot(E2_rt, aes(x = block4, y = RT_ms, fill = block4)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 0.6) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = cols) +
  labs(
    #title = "Reaction Time Distributions by Valence and Congruence (Explicit Task)",
    x = "Condition (Congruence × Valence)",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal(base_size = 20
  ) + theme(plot.title = element_text(face = "bold", hjust = 0.5),
            legend.position = "none"
  ) + thesis_theme

#===============================================================================
#==== Appendix plots: Additional descriptive plots =============================
#===============================================================================

#== (1) Violin plots for individual conditions 

#== A) Congruence - change for experiment (E1_rt or E2_rt)
ggplot(E2_rt, aes(x = congruence, y = RT_ms, fill = congruence)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_jitter(
    width = 0.15,
    alpha = 0.25,
    size = 0.7
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = colours_cong) +
  labs(
    #title = "Congruence Effect on Reaction Time (Explicit Task)",
    x = "Congruence",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  ) + thesis_theme

#== B) Valence - change for experiment 

ggplot(E2_rt, aes(x = valence, y = RT_ms, fill = valence)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_jitter(
    width = 0.15,
    alpha = 0.25,
    size = 0.7
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(values = colours_val) +
  labs(
    #title = "Valence Effect on Reaction Time (Explicit Task)",
    x = "Valence",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  ) + thesis_theme

#===============================================================================
#========         Learning and Fatigue            ==============================
#===============================================================================
#=== Implicit ==================================================================
E1_rt_binned <- E1_rt %>%
  mutate(trial_bin = ceiling(trial_index / 5) * 5) %>%
  group_by(trial_bin) %>%
  summarise(mean_RT = mean(RT_ms, na.rm = TRUE), .groups = "drop")

ggplot(E1_rt_binned, aes(trial_bin, mean_RT)) +
  geom_col(width = 0.9, alpha = 0.35) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +
  labs(
    #title = "Reaction Time Across Trials (Implicit Task, 5-trial bins)",
    x = "Trial (binned)",
    y = "Mean RT (ms)"
  ) +
  theme_minimal(base_size = 20
  )+ thesis_theme

#=== Explicit ==================================================================
E2_rt_binned <- E2_rt %>%
  mutate(trial_bin = ceiling(trial_index / 5) * 5) %>%
  group_by(trial_bin) %>%
  summarise(mean_RT = mean(RT_ms, na.rm = TRUE), .groups = "drop")

ggplot(E2_rt_binned, aes(trial_bin, mean_RT)) +
  geom_col(width = 0.9, alpha = 0.35) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +
  labs(
    #title = "Reaction Time Across Trials (Explicit Task, 5-trial bins)",
    x = "Trial (binned)",
    y = "Mean RT (ms)"
  ) +
  theme_minimal(base_size = 20
  )+ thesis_theme


#===============================================================================
#============     Qualitative Analysis Linear Mixed Model    ===================
#===============================================================================
#===== 1.  manual effect coding           ======================================
# Manually create ±1 coding (not the default contr.sum) ========================
# Making sure it aligns with previous work              ========================
#===============================================================================
E1_rt$congruence_coded <- ifelse(E1_rt$congruence == "congruent",  1, -1)
E1_rt$valence_coded    <- ifelse(E1_rt$valence == "positive",  1, -1)
E1_rt$frame_side_coded <- ifelse(E1_rt$frame_side == "right",  1, -1)

E2_rt$congruence_coded <- ifelse(E2_rt$congruence == "congruent",  1, -1)
E2_rt$valence_coded    <- ifelse(E2_rt$valence == "positive",  1, -1)
E2_rt$frame_side_coded <- ifelse(E2_rt$frame_side == "right",  1, -1)

#===================== Base Model LMM ==========================================

E1_mod_pm1 <- lmer(RT_log10 ~ valence_coded * congruence_coded + 
                     (1 | participant), data = E1_rt, REML = FALSE)
summary(E1_mod_pm1)

E2_mod_pm1 <- lmer(RT_log10 ~ valence_coded * congruence_coded +
                     (1 | participant) , data = E2_rt, REML = FALSE)
summary(E2_mod_pm1)


#=====     z-trial:  learning and fatigue        ===============================
E1_mod_z_trial_pm1 <- lmer(RT_log10 ~ z_trial + valence_coded * congruence_coded  +
                             (1 | participant), data = E1_rt, REML = FALSE)
summary(E1_mod_z_trial_pm1)


E2_mod__z_trial_pm1 <- lmer(RT_log10 ~ z_trial +valence_coded * congruence_coded + 
                              (1 | participant), data = E2_rt, REML = FALSE)
summary(E2_mod__z_trial_pm1)


#====== Adding picture_id and frame side =======================================
E1_mod_side_pm1 <- lmer(RT_log10 ~ z_trial + valence_coded * congruence_coded + frame_side_coded +
                          (1 | participant) + (1|picture_id), data = E1_rt, REML = FALSE)
summary(E1_mod_side_pm1)


E2_mod_side_pm1 <- lmer(RT_log10 ~ z_trial + valence_coded * congruence_coded + frame_side_coded +
                          (1 | participant) + (1|picture_id), data = E2_rt, REML = FALSE)
summary(E2_mod_side_pm1)

#==============================================================================
#===  Calcuating percentage differences between factors of LMMs   ============
#==============================================================================

# ---------- helpers ----------
# percent change for a ±1 coded contrast with log10(DV)
pct_from_log10_delta <- function(delta_log10) {
  # delta_log10 is the full contrast on log10 scale (e.g., 2*beta or a linear combo)
  (10^(delta_log10) - 1) * 100
}

# Build a tidy table of fixed-effect CIs and percent changes for main & simple effects
effect_tables <- function(mod) {
  # 1) Fixed effects with Wald 95% CI
  fe <- broom.mixed::tidy(mod, conf.int = TRUE, conf.method = "Wald") %>%
    select(term, estimate, conf.low, conf.high, std.error, p.value)
  
  # Pull needed betas
  b <- fe$estimate; names(b) <- fe$term
  # Robustly handle missing terms
  getb <- function(nm) ifelse(nm %in% names(b), b[[nm]], NA_real_)
  
  b_val   <- getb("valence_coded")
  b_cong  <- getb("congruence_coded")
  b_side  <- getb("frame_side_coded")
  b_int   <- getb("valence_coded:congruence_coded")
  
  # Helper to extract CI rows and map to % via delta = 2*beta (for main effects)
  main_effect_ci_to_pct <- function(term_name, nice_label) {
    row <- fe %>% filter(term == term_name)
    if (nrow(row) == 0) return(NULL)
    delta_hat  <- 2 * row$estimate[1]
    delta_low  <- 2 * row$conf.low[1]
    delta_high <- 2 * row$conf.high[1]
    tibble(
      effect = nice_label,
      beta = row$estimate[1],
      CI_beta_low = row$conf.low[1],
      CI_beta_high = row$conf.high[1],
      p = row$p.value[1],
      pct = pct_from_log10_delta(delta_hat),
      pct_CI_low = pct_from_log10_delta(delta_low),
      pct_CI_high = pct_from_log10_delta(delta_high)
    )
  }
  
  main_tbl <- bind_rows(
    main_effect_ci_to_pct("congruence_coded", "Main effect: Congruent vs Incongruent"),
    main_effect_ci_to_pct("valence_coded",    "Main effect: Positive vs Negative"),
    main_effect_ci_to_pct("frame_side_coded", "Main effect: Right vs Left")
  )
  
  # 2) Simple effects under the interaction (if present)
  # Congruence at Positive (valence=+1) and Negative (valence=-1):
  # delta_log10 = 2*(b_cong + b_int * valence_level)
  simple_effects <- list()
  if (!is.na(b_cong) && !is.na(b_int)) {
    delta_pos  <- 2 * (b_cong + b_int * (+1))
    delta_neg  <- 2 * (b_cong + b_int * (-1))
    simple_effects[["Congruence @ Positive"]] <- pct_from_log10_delta(delta_pos)
    simple_effects[["Congruence @ Negative"]] <- pct_from_log10_delta(delta_neg)
  }
  
  # Valence at Congruent (cong=+1) and Incongruent (cong=-1):
  # delta_log10 = 2*(b_val + b_int * congruence_level)
  if (!is.na(b_val) && !is.na(b_int)) {
    delta_cong <- 2 * (b_val + b_int * (+1))
    delta_incg <- 2 * (b_val + b_int * (-1))
    simple_effects[["Valence @ Congruent"]]    <- pct_from_log10_delta(delta_cong)
    simple_effects[["Valence @ Incongruent"]]  <- pct_from_log10_delta(delta_incg)
  }
  
  simple_tbl <- if (length(simple_effects)) {
    tibble(
      simple_effect = names(simple_effects),
      pct = unlist(simple_effects)
    )
  } else {
    tibble(simple_effect = character(0), pct = numeric(0))
  }
  
  # 3) ICCs and fit indices
  vc <- as.data.frame(VarCorr(mod))
  tot_var <- sum(vc$vcov)
  icc_tbl <- vc %>%
    transmute(
      grouping = grp,
      var = vcov,
      ICC = ifelse(tot_var > 0, vcov / tot_var, NA_real_)
    ) %>%
    distinct()
  
  fit <- tibble(
    AIC = AIC(mod),
    BIC = BIC(mod),
    REMLcrit = ifelse(lme4::isREML(mod), deviance(mod), NA_real_)
  )
  
  list(
    fixed_effects = fe,
    main_effects_pct = main_tbl,
    simple_effects_pct = simple_tbl,
    icc = icc_tbl,
    fit = fit
  )
}

# ======================== run for LMM models ========================
E1_out <- effect_tables(E1_mod_pm1) #change models accordingly 
E2_out <- effect_tables(E2_mod_pm1)

# Inspect:
E1_out$main_effects_pct
E1_out$simple_effects_pct
E1_out$icc
E1_out$fit

E2_out$main_effects_pct
E2_out$simple_effects_pct
E2_out$icc
E2_out$fit

#===============================================================================
#===== Plotting LMM outcomes        ============================================
#===============================================================================

# ======== Implicit Task =========================
preds <- ggpredict(E1_mod_pm1 , terms = c("congruence_coded", "valence_coded")) #change LMM model per interest 
preds <- preds |>
  mutate(
    RT_ms = 10^predicted,
    RT_lo = 10^conf.low,
    RT_hi = 10^conf.high,
    # Make nice labels to match your raw data plot
    congruence = factor(ifelse(x ==  1, "congruent", "incongruent"),
                        levels = c("congruent","incongruent")),
    valence    = factor(ifelse(group ==  1, "positive", "negative"),
                        levels = c("negative","positive"))
  )
preds <- preds |> filter(!is.na(valence))


p_impl__model <- ggplot(preds, aes(x = congruence, y = RT_ms, color = valence, group = valence)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = RT_lo, ymax = RT_hi),
                position = position_dodge(width = 0.5), width = 0.15, linewidth = 1.2) +
  geom_line(position = position_dodge(width = 0.5), 
            linewidth = 1.2) +
  labs(#title = "Implicit task — LMM-predicted RT with z_trial",                                 # Important: change Title accordingly 
    #subtitle = "Predicted means (back-transformed) ± 95% CI",
    x = "Congruence", y = "Predicted Reaction Time (ms)", color = "Valence") +
  theme_minimal(base_size = 20
  )+ thesis_theme
p_impl__model

# ======== Explicit Task =====================
preds <- ggpredict(E2_mod_side_pm1, terms = c("congruence_coded", "valence_coded"))
# Back-transform to milliseconds
preds <- preds |>
  mutate(
    RT_ms = 10^predicted,
    RT_lo = 10^conf.low,
    RT_hi = 10^conf.high,
    # Make nice labels to match your raw data plot
    congruence = factor(ifelse(x ==  1, "congruent", "incongruent"),
                        levels = c("congruent","incongruent")),
    valence    = factor(ifelse(group ==  1, "positive", "negative"),
                        levels = c("negative","positive"))
  )
preds <- preds |> filter(!is.na(valence))


p_model <- ggplot(preds, aes(x = congruence, y = RT_ms, color = valence, group = valence)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = RT_lo, ymax = RT_hi),
                position = position_dodge(width = 0.5), width = 0.15, linewidth = 1.2) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 1.2) +
  labs(#title = "Explicit task — LMM-predicted RT",
       #subtitle = "Predicted means (back-transformed) ± 95% CI",
       x = "Congruence", y = "Predicted Reaction Time (ms)", color = "Valence") +
  theme_minimal(base_size = 20
                )+ thesis_theme
p_model

#========================================================================
# emmeans uses asymptotic (z) tests due to large sample size (>3000 obs);
# DF corrections (KR / Satterthwaite) not applied to avoid excessive computation.
# baseline model
#========================================================================
mod <- E2_mod_pm1

# simple effects of valence within each congruence level
emm_val_by_cong <- emmeans(mod, ~ valence_coded | congruence_coded)
contrast(emm_val_by_cong, method = "revpairwise") |> summary()

# simple effects of congruence within each valence level
emm_cong_by_val <- emmeans(mod, ~ congruence_coded | valence_coded)
contrast(emm_cong_by_val, method = "revpairwise") |> summary()

#===============================================================================
#============ Learning and Fatigue trends based off of LMM =====================
#===============================================================================
# ---- helper: get smooth predictions across trial for RT ----
predict_rt_curve <- function(model, at_seq = seq(-2, 2, by = 0.1), by = NULL) {
  # if by is NULL, just marginalize over factors
  if (is.null(by)) {
    em <- emmeans(model, ~ z_trial, at = list(z_trial = at_seq))
  } else {
    # e.g., by = c("congruence","valence")
    rhs <- paste(c("z_trial", by), collapse = " * ")
    em  <- emmeans(model, as.formula(paste("~", rhs)),
                   at = list(z_trial = at_seq))
  }
  out <- as.data.frame(em)
  
  # out$emmean is on log10 scale -> back transform to milliseconds
  out <- out %>%
    mutate(
      pred_ms  = 10^(emmean),
      lcl_ms   = 10^(asymp.LCL),
      ucl_ms   = 10^(asymp.UCL)
    )
  out
}

# Example: overall RT curves (no facets)
E1_rt_curve <- predict_rt_curve(E1_mod_z_trial_pm1)
E2_rt_curve <- predict_rt_curve(E2_mod__z_trial_pm1)
#1Implicit 
ggplot(E1_rt_curve, aes(x = z_trial, y = pred_ms)) +
  geom_ribbon(aes(ymin = lcl_ms, ymax = ucl_ms), alpha = .20) +
  geom_line(size = 1) +
  labs(#title = "Implicit Task: Predicted RT over trials",
       x = "Standardized trial index (z_trial)",
       y = "Predicted RT (ms)") +
  theme_minimal(base_size = 20
  ) + thesis_theme
#2 Explicit 
ggplot(E2_rt_curve, aes(x = z_trial, y = pred_ms)) +
  geom_ribbon(aes(ymin = lcl_ms, ymax = ucl_ms), alpha = .20) +
  geom_line(size = 1) +
  labs(#title = "Explicit Task: Predicted RT over trials",
       x = "Standardized trial index (z_trial)",
       y = "Predicted RT (ms)") +
  theme_minimal(base_size = 20
  ) + thesis_theme

#===============================================================================
#===================== Adding Picture-ID  ======================================
#===============================================================================

# ===== Valence effect before vs. after adding picture ID ======================

# Extract fixed effect for valence
fe_E2_base <- tidy(E2_mod__z_trial_pm1, effects = "fixed", conf.int = TRUE) %>%
  filter(term == "valence_coded") %>%
  mutate(model = "Base (no picture ID)")

fe_E2_pic <- tidy(E2_mod_side_pm1, effects = "fixed", conf.int = TRUE) %>%
  filter(term == "valence_coded") %>%
  mutate(model = "With picture ID")

fe_E2_all <- bind_rows(fe_E2_base, fe_E2_pic)

# Order the factor to ensure correct line connection
fe_E2_all$model <- factor(fe_E2_all$model,
                          levels = c("Base (no picture ID)", "With picture ID"))

ggplot(fe_E2_all, aes(x = model, y = estimate, group = 1)) +
  geom_line(color = "black") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_text(aes(label = sprintf("%.4f", round(estimate, 4))),
            vjust = -1.2, size = 8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    x = "Model",
    y = "Estimated valence effect (log10 RT)",
    # title = "Effect of adding picture ID on the valence effect (Implicit Task)"
  ) +
  coord_cartesian(ylim = c(min(fe_E2_all$conf.low) - 0.001,
                           max(fe_E2_all$conf.high) + 0.001)) +
  theme_minimal(base_size = 20
  ) + thesis_theme

#==============================================================================
#=================   Picture Analysis (Exploration)    ========================
#==============================================================================
#downloading the images for content analysis 
img_dir  <- file.path("[ORIG]allpics_1-44pos_45-88neg")
out_dir <- file.path("Set/Path/To/Where/You/Want/It/Be/Saved")
##############################################################################
#===========  Plotting RT distribution of individual pictures ==================
################################################################################
# Get fitted values on the right scale:
.get_fitted <- function(mod) {
  if (inherits(mod, "glmerMod")) {
    fitted(mod, type = "link")  # log-odds for logistic models
  } else {
    fitted(mod)                 # response for Gaussian (your RT on log10 scale)
  }
}


# Compute picture effects as "full - no-picture-RE" and plot
plot_picture_effects_noBLUP <- function(model_full, data,
                                        picture_col = "picture_id",
                                        valence_col = "valence",
                                        center = TRUE,
                                        top_n = NULL,          # <-- NEW
                                        every_nth_label = NULL # <-- optional: label thinning
) {
  # Remove the picture random effect (assumes term is exactly (1 | picture_id))
  model_no_pic <- update(model_full, . ~ . - (1 | picture_id))
  
  df <- data %>%
    mutate(
      fitted_full  = .get_fitted(model_full),
      fitted_nopic = .get_fitted(model_no_pic),
      pic_contrib  = fitted_full - fitted_nopic
    ) %>%
    group_by(.data[[picture_col]]) %>%
    summarise(effect = mean(pic_contrib, na.rm = TRUE), .groups = "drop")
  
  if (center) {
    df <- df %>% mutate(effect = effect - mean(effect, na.rm = TRUE))
  }
  
  # Attach valence (optional)
  if (valence_col %in% names(data)) {
    val_map <- data %>%
      group_by(.data[[picture_col]]) %>%
      summarise(valence = first(.data[[valence_col]]), .groups = "drop")
    
    # Join by the picture_col name (robust)
    df <- df %>% left_join(val_map, by = setNames(picture_col, picture_col))
  } else {
    df$valence <- NA_character_
  }
  
  # Keep only the most extreme pictures (top/bottom by absolute effect)
  if (!is.null(top_n)) {
    df <- df %>% slice_max(order_by = abs(effect), n = top_n, with_ties = FALSE)
  }
  
  ylab_txt <- if (inherits(model_full, "glmerMod")) {
    "Picture contribution (link scale, e.g., log-odds)"
  } else {
    "Picture contribution (log10 RT)"
  }
  
  p <- ggplot(df,
              aes(x = reorder(as.character(.data[[picture_col]]), effect),
                  y = effect, fill = valence)) +
    geom_col(color = "black", linewidth = 0.2) +
    scale_fill_manual(
      values = c(
        "positive" = "#2ecc71",
        "negative" = "#e74c3c"
      ),
      drop = FALSE
    ) +
    coord_flip() +
    labs(x = "Picture ID",
         y = ylab_txt,
         fill = "Valence") +
    theme_minimal(base_size = 20) +
    thesis_theme
  
  # Optional: show only every nth label (useful for full plot)
  if (!is.null(every_nth_label)) {
    p <- p + scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = every_nth_label)])
  }
  
  return(p)
}



#========================= Top 40 for main body text =========================== 
# Implicit Task
p1_topbottom <- plot_picture_effects_noBLUP(
  model_full = E1_mod_side_pm1,
  data = E1_rt,
  top_n = 40
)
p1_topbottom

#Explicit Task
p2_topbottom <- plot_picture_effects_noBLUP(
  model_full = E2_mod_side_pm1,
  data = E2_rt,
  top_n = 40
)
p2_topbottom

#========================= Appendix graphs =====================================
# Compute picture effects as "full - no-picture-RE" and plot
plot_picture_effects_noBLUP_total <- function(model_full, data,
                                        picture_col = "picture_id",
                                        valence_col = "valence",
                                        center = TRUE,
                                        top_n = NULL,          # <-- NEW
                                        every_nth_label = NULL # <-- optional: label thinning
) {
  # Remove the picture random effect (assumes term is exactly (1 | picture_id))
  model_no_pic <- update(model_full, . ~ . - (1 | picture_id))
  
  df <- data %>%
    mutate(
      fitted_full  = .get_fitted(model_full),
      fitted_nopic = .get_fitted(model_no_pic),
      pic_contrib  = fitted_full - fitted_nopic
    ) %>%
    group_by(.data[[picture_col]]) %>%
    summarise(effect = mean(pic_contrib, na.rm = TRUE), .groups = "drop")
  
  if (center) {
    df <- df %>% mutate(effect = effect - mean(effect, na.rm = TRUE))
  }
  
  # Attach valence (optional)
  if (valence_col %in% names(data)) {
    val_map <- data %>%
      group_by(.data[[picture_col]]) %>%
      summarise(valence = first(.data[[valence_col]]), .groups = "drop")
    
    # Join by the picture_col name (robust)
    df <- df %>% left_join(val_map, by = setNames(picture_col, picture_col))
  } else {
    df$valence <- NA_character_
  }
  
  # Keep only the most extreme pictures (top/bottom by absolute effect)
  if (!is.null(top_n)) {
    df <- df %>% slice_max(order_by = abs(effect), n = top_n, with_ties = FALSE)
  }
  
  ylab_txt <- if (inherits(model_full, "glmerMod")) {
    "Picture contribution (link scale, e.g., log-odds)"
  } else {
    "Picture contribution (log10 RT)"
  }
  
  p <- ggplot(df,
              aes(x = reorder(as.character(.data[[picture_col]]), effect),
                  y = effect, fill = valence)) +
    geom_col(color = "black", linewidth = 0.2) +
    scale_fill_manual(
      values = c(
        "positive" = "#2ecc71",
        "negative" = "#e74c3c"
      ),
      drop = FALSE
    ) +
    coord_flip() +
    labs(x = "Picture ID",
         y = ylab_txt,
         fill = "Valence") +
    theme_minimal(base_size = 20) +
    theme(axis.text.y = element_text(size = 9))
  
  # Optional: show only every nth label (useful for full plot)
  if (!is.null(every_nth_label)) {
    p <- p + scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = every_nth_label)])
  }
  
  return(p)
}

#========================= IMPLICIT Task =======================================

p1 <- plot_picture_effects_noBLUP_total(
  model_full = E1_mod_side_pm1,
  data       = E1_rt,
  picture_col = "picture_id",
  valence_col = "valence"
)
p1

#=========================  EXPLICIT task ======================================
p2 <- plot_picture_effects_noBLUP_total(
  model_full = E2_mod_side_pm1,
  data       = E2_rt,
  picture_col = "picture_id",
  valence_col = "valence"
)
p2

#===============================================================================
#===========              Spreadsheets    ======================================
#===============================================================================

.get_fitted <- function(mod) {
  if (inherits(mod, "glmerMod")) fitted(mod, type = "link") else fitted(mod)
}

plot_picture_effects_spreadsheet <- function(model_full, data, picture_col = "picture_id",
                                        valence_col = "valence",
                                        title = "Picture-specific effects",
                                        center = TRUE,
                                        return_df = TRUE) {
  # IMPORTANT: this must match your model’s random term exactly
  model_no_pic <- update(model_full, . ~ . - (1 | picture_id))
  
  df <- data %>%
    mutate(
      fitted_full  = .get_fitted(model_full),
      fitted_nopic = .get_fitted(model_no_pic),
      pic_contrib  = fitted_full - fitted_nopic
    ) %>%
    group_by(.data[[picture_col]]) %>%
    summarise(effect = mean(pic_contrib, na.rm = TRUE), .groups = "drop")
  
  if (center) df <- df %>% mutate(effect = effect - mean(effect, na.rm = TRUE))
  
  # attach a single valence label per picture (first non-NA)
  if (valence_col %in% names(data)) {
    val_map <- data %>%
      group_by(.data[[picture_col]]) %>%
      summarise(valence = dplyr::first(na.omit(.data[[valence_col]])), .groups = "drop")
    names(val_map)[1] <- "picture_id"   # ensure join key name
    df <- df %>% left_join(val_map, by = "picture_id")
  } else {
    df$valence <- NA_character_
  }
  
  if (return_df) return(df)
  
  ylab_txt <- if (inherits(model_full, "glmerMod")) "Picture contribution (link scale)" else "Picture contribution (log10 RT)"
  ggplot(df, aes(x = reorder(as.character(.data[[picture_col]]), effect),
                 y = effect, fill = valence)) +
    geom_col() +
    coord_flip() +
    labs(title = title, x = "Picture ID", y = ylab_txt) +
    theme_minimal()
}


# =================== 1) Get the same table used by the bar plot ===============
e2_effects <- plot_picture_effects_spreadsheet(
  model_full = E2_mod_side_pm1,
  data       = E2_rt,
  picture_col = "picture_id",
  valence_col = "valence",
  title = NULL,
  center = TRUE
  # small tweak to your function: allow returning the table when title = NULL
  #return_df = TRUE
)

#========================= 2) Pick slow/fast by that effect ====================
n_top <- 12
slow_ids <- e2_effects %>% arrange(desc(effect)) %>% slice_head(n = n_top) %>% pull(picture_id)
fast_ids <- e2_effects %>% arrange(effect)      %>% slice_head(n = n_top) %>% pull(picture_id)

# ======= 3) Build a caption table to show ms (same E2_rt + same NA filters) ===
cap_tbl <- E2_rt %>%
  filter(!is.na(RT_ms), !is.na(RT_log10)) %>%
  group_by(picture_id) %>%
  summarise(n_trials = n(),
            mean_RT_ms = mean(RT_ms, na.rm = TRUE),
            .groups = "drop")

#========  4) Contact-sheet composer: join valence-from-ID and caption table ===
valence_by_id <- tibble(picture_id = sort(unique(E2_rt$picture_id))) %>%
  mutate(valence_from_id = ifelse(as.integer(as.character(picture_id)) <= 43,
                                  "positive","negative"))

compose_labeled <- function(ids, title, out_png,
                            tile_cols=4, cell_px=420, border_px=10) {
  ids <- as.integer(as.character(ids))
  imgs <- lapply(ids, function(id) {
    f <- file.path(img_dir, sprintf("image_%d.jpg", id))
    if (!file.exists(f)) return(NULL)
    
    s  <- cap_tbl      %>% dplyr::filter(picture_id == id)
    v  <- valence_by_id%>% dplyr::filter(picture_id == id) %>% dplyr::pull(valence_from_id)
    ntr <- if (nrow(s)) s$n_trials[1]   else NA_integer_
    ms  <- if (nrow(s)) s$mean_RT_ms[1] else NA_real_
    
    frame_col <- if (identical(v, "positive")) "#2ecc71" else "#e74c3c"
    img <- magick::image_read(f) |>
      magick::image_resize(sprintf("%dx%d", cell_px, cell_px)) |>
      magick::image_border(frame_col, sprintf("%dx%d", border_px, border_px))
    cap <- sprintf("#%d [%s] mean: %.0f ms | n:%d", id, v, ms, ntr)
    cap_img <- magick::image_blank(width = magick::image_info(img)$width, height = 48, color = "black") |>
      magick::image_annotate(cap, size = 24, color = "white", gravity = "center")
    magick::image_append(c(img, cap_img), stack = TRUE)
  })
  imgs <- Filter(Negate(is.null), imgs)
  if (!length(imgs)) return(invisible(NULL))
  
  m <- magick::image_montage(magick::image_join(imgs),
                             tile=paste0(tile_cols,"x"), geometry="+6+6") |>
    magick::image_border("black", "5x5") |>
    magick::image_annotate(title, size=36, color="white", boxcolor="black", gravity="north")
  
  # >>> Force a concrete format <<<
  fmt <- tools::file_ext(out_png)          # "png" or "jpg"
  m <- magick::image_convert(m, format = fmt)
  magick::image_write(m, path = out_png, format = fmt)
  
  message("Saved: ", out_png)
}

#=======  5) Now these will match the bar plot’s ranking  ======================
compose_labeled(slow_ids, "E2 – SLOWEST by model contribution",
                file.path(out_dir, "E2_slowest_by_model_contribution.png"))
compose_labeled(fast_ids, "E2 – FASTEST by model contribution",
                file.path(out_dir, "E2_fastest_by_model_contribution.png"))


#===============================================================================
#============== Adding Co-ID to LMMs ===========================================
#===============================================================================

#=============== Implicit task  ===============================================

# 1) Reconstruct focal/co pairs based on positive and negative picture IDs
E1_pairs <- E1_rt %>%
  filter(!is.na(picture_id), !is.na(picture_pos_ID), !is.na(picture_neg_ID)) %>%
  mutate(
    focal_id = picture_id,
    co_id = ifelse(picture_id == picture_pos_ID, picture_neg_ID, picture_pos_ID)
  )

# 2) Per-image summary table (reuse logic from per_image for the Implicit (E1)
per_image_E1 <- E1_rt %>%
  group_by(picture_id, valence) %>%
  summarise(
    n_trials     = n(),
    mean_RT_ms   = mean(RT_ms, na.rm = TRUE),
    mean_RT_log10 = mean(RT_log10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(valence_from_id = valence)

per_image_small_E1 <- per_image_E1 %>%
  transmute(image_id = picture_id,
            focal_meanRT = mean_RT_ms,
            focal_valence = valence_from_id)

# 3) Ensure all ID columns are numeric for consistent joining
E1_pairs <- E1_pairs %>%
  mutate(
    focal_id = as.numeric(as.character(focal_id)),
    co_id    = as.numeric(as.character(co_id))
  )

per_image_small_E1 <- per_image_small_E1 %>%
  mutate(image_id = as.numeric(as.character(image_id)))

# 4) Join focal and co-image information
partner_effects_E1 <- E1_pairs %>%
  left_join(per_image_small_E1, by = c("focal_id" = "image_id")) %>%
  left_join(per_image_small_E1 %>%
              rename(image_id = image_id,
                     partner_meanRT = focal_meanRT,
                     partner_valence = focal_valence),
            by = c("co_id" = "image_id"))

# 5) Coding and cleanup
E1_partner_effects_coded <- partner_effects_E1 %>%
  mutate(
    congruence_coded = ifelse(congruence == "congruent",  1, -1),
    valence_coded    = ifelse(valence    == "positive",   1, -1),
    frame_side_coded = ifelse(frame_side == "right",      1, -1),
    participant = factor(participant),
    focal_id    = factor(focal_id),
    co_id       = factor(co_id)
  ) %>%
  filter(!is.na(partner_meanRT)) %>%
  mutate(partner_meanRT_z = as.numeric(scale(partner_meanRT)))

# 6) Fit the LMM (same structure as Explicit Task (E2))
model_partner_E1 <- lmer(
  RT_log10 ~ z_trial + congruence_coded * valence_coded + frame_side_coded +
    partner_meanRT_z +
    (1 | participant) + (1 | focal_id) + (1 | co_id),
  data = E1_partner_effects_coded,
  REML = FALSE
)

summary(model_partner_E1)
 
#================ Explicit task (E2)  ==========================================
# 0) Make sure all ID columns exist with consistent types
E2_rt <- E2_rt %>%
  mutate(
    picture_id = as.integer(picture_id),
    left_id    = as.integer(left_id),
    right_id   = as.integer(right_id)
  )
#1.  Filter out any NA values for ID's 
E2_pairs <- E2_rt %>% filter(!is.na(left_id), !is.na(right_id))

#2. Define focal and  co-ID 
pairs <- bind_rows(
  E2_pairs %>% transmute(participant, trial,
                         focal_id = left_id,  co_id = right_id,
                         RT_ms, RT_log10, valence, congruence, z_trial, frame_side),
  E2_pairs %>% transmute(participant, trial,
                         focal_id = right_id, co_id = left_id,
                         RT_ms, RT_log10, valence, congruence, z_trial, frame_side)
)

#3.  Per-image summary table
per_image_E2 <- E2_rt %>%
  group_by(picture_id, valence) %>%
  summarise(
    n_trials     = n(),
    mean_RT_ms   = mean(RT_ms, na.rm = TRUE),
    mean_RT_log10 = mean(RT_log10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(valence_from_id = valence)



# 4. Get focal/partner means and valence-from-ID:
per_image_small <- per_image_E2 %>%
  transmute(image_id = picture_id,
            focal_meanRT = mean_RT_ms,
            focal_valence = valence_from_id)

partner_effects <- pairs %>%
  left_join(per_image_small, by = c("focal_id" = "image_id")) %>%
  left_join(per_image_small %>%
              rename(image_id = image_id,      # harmless rename for clarity
                     partner_meanRT = focal_meanRT,
                     partner_valence = focal_valence),
            by = c("co_id" = "image_id"))

# 5.  Coding and data hygiene
E2_partner_effects_coded <- partner_effects %>%
  mutate(
    congruence_coded = ifelse(congruence == "congruent",  1, -1),
    valence_coded    = ifelse(valence    == "positive",   1, -1),
    frame_side_coded = ifelse(frame_side == "right",      1, -1),
    participant = factor(participant),
    focal_id    = factor(focal_id),
    co_id       = factor(co_id)
  ) %>%
  filter(!is.na(partner_meanRT)) %>%                  # drop missing partner means
  mutate(partner_meanRT_z = as.numeric(scale(partner_meanRT)))  # scale once

# 6. Fit on the SAME response used everywhere else: RT_log10
model_partner_E2 <- lmer(
  RT_log10 ~ z_trial + congruence_coded * valence_coded + frame_side_coded +
    partner_meanRT_z +
    (1 | participant) + (1 | focal_id) + (1 | co_id),
  data = E2_partner_effects_coded,
  REML = FALSE
)
summary(model_partner_E2)

#===============================================================================
#======== Additional analysis for partner effect of above LMM analysis =========
#===============================================================================

# Function to extract partner_meanRT_z effect and convert to % change per SD
partner_effect_summary <- function(model) {
  fe <- broom.mixed::tidy(model, conf.int = TRUE, conf.method = "Wald")
  row <- fe[fe$term == "partner_meanRT_z", ]
  if (nrow(row) == 0) {
    message("No partner_meanRT_z term found.")
    return(NULL)
  }
  # Back-transform: per 1 SD change in partner mean RT (since already z-scaled)
  delta <- row$estimate
  pct_change <- (10^(delta) - 1) * 100
  pct_low <- (10^(row$conf.low) - 1) * 100
  pct_high <- (10^(row$conf.high) - 1) * 100
  tibble(
    term = "partner_meanRT_z",
    beta = delta,
    SE = row$std.error,
    CI_low = row$conf.low,
    CI_high = row$conf.high,
    p = row$p.value,
    pct_change = pct_change,
    pct_CI_low = pct_low,
    pct_CI_high = pct_high
  )
}

E1_partner <- partner_effect_summary(model_partner_E1)
E2_partner <- partner_effect_summary(model_partner_E2)

E1_partner
E2_partner


#======== Plotting the co ID outcome  ==========================================
ggplot(E2_partner_effects_coded, aes(x = partner_meanRT_z, y = RT_ms)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(aes(y = RT_ms), method = "lm", se = TRUE, color = "red") +
  labs(
    #title = "Partner Image Influence on Focal RT in Explicit Task",
    x = "Partner Image Mean RT (z-scored)",
    y = "Participant RT (ms)"
  ) +
  theme_minimal(base_size = 20
  ) + thesis_theme

#=======   creating the spreadsheets for co-ID ================================

# === 1) Compute mean RT per co_id ===========================

per_co_image_RT <- E2_partner_effects_coded %>%
  group_by(co_id, partner_valence) %>%
  summarise(
    n_trials = n(),
    mean_partnerRT = mean(RT_ms, na.rm = TRUE),          # milliseconds
    mean_partnerRT_log = mean(RT_log10, na.rm = TRUE),  # log10 scale if needed
    .groups = "drop"
  ) %>%
  arrange(mean_partnerRT)  # slower RTs at top for "disruptive"

# === 2) Pick top/bottom images ===========================

n_top <- 12
fast_co_ids <- per_co_image_RT$co_id[1:n_top]          # fastest (helpful) partners
slow_co_ids <- per_co_image_RT$co_id[(nrow(per_co_image_RT)-n_top+1):nrow(per_co_image_RT)]  # slowest (disruptive)

# === helper for frame color ===
valence_color <- function(val) {
  v <- tolower(as.character(val))
  if (is.na(v)) return("#444444")
  if (v %in% c("positive","pos","1")) return("#2ecc71")  # green
  if (v %in% c("negative","neg","0")) return("#e74c3c")  # red
  "#444444"
}

# === 3) Compose labeled contact sheet for RT ===================

compose_labeled_RT <- function(ids, title, per_tbl, out_png,
                               id_col = "co_id",
                               rt_col = "mean_partnerRT",
                               val_col = "partner_valence",
                               n_col = "n_trials",
                               tile_cols = 4, cell_px = 420, border_px = 10) {
  ids <- as.integer(as.character(ids))
  imgs <- lapply(ids, function(id) {
    f <- file.path(img_dir, sprintf("image_%d.jpg", id))
    if (!file.exists(f)) return(NULL)
    
    row <- per_tbl[per_tbl[[id_col]] == id, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    
    rt    <- row[[rt_col]][1]
    ntr   <- row[[n_col]][1]
    val   <- row[[val_col]][1]
    frame <- valence_color(val)
    
    img <- image_read(f) |>
      image_resize(sprintf("%dx%d", cell_px, cell_px)) |>
      image_border(frame, sprintf("%dx%d", border_px, border_px))
    
    cap <- sprintf("#%d [%s] RT: %.0f ms | n:%d",
                   id, val, rt, ntr)
    
    cap_img <- image_blank(width = image_info(img)$width,
                           height = 48, color = "black") |>
      image_annotate(cap, size = 24, color = "white", gravity = "center")
    
    image_append(c(img, cap_img), stack = TRUE)
  })
  
  imgs <- Filter(Negate(is.null), imgs)
  if (!length(imgs)) return(invisible(NULL))
  
  m <- image_montage(image_join(imgs),
                     tile = paste0(tile_cols, "x"),
                     geometry = "+6+6") |>
    image_border("black", "5x5") |>
    image_annotate(title, size = 36, color = "white",
                   boxcolor = "black", gravity = "north")
  
  # Save using the file's extension
  fmt <- tools::file_ext(out_png)
  m <- image_convert(m, format = fmt)
  image_write(m, path = out_png, format = fmt)
}

# === 4) Make the contact sheets for RT ===
compose_labeled_RT(
  fast_co_ids,
  "E2 – Partner Images Associated with Fastest RTs",
  per_tbl = per_co_image_RT,
  out_png = file.path(out_dir, "E2_partner_fastRT.png")
)

compose_labeled_RT(
  slow_co_ids,
  "E2 – Partner Images Associated with Slowest RTs",
  per_tbl = per_co_image_RT,
  out_png = file.path(out_dir, "E2_partner_slowRT.png")
)



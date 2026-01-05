
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
library(magick)
library(tools)

#==============================================================================
#=====   Load necessary data from preprocessing         =======================
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
#====     Cleaning Data for Accuracy                           ===========
#====  --> Build 0/1 accuracy and per-participant trial index  ===========
#=========================================================================

E1_acc <- E1 %>%
  mutate(
    acc01 = case_when(
      accuracy == TRUE  ~ 1L,
      accuracy == FALSE ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  group_by(participant) %>%
  arrange(trial, .by_group = TRUE) %>%
  mutate(
    trial_index = trial,
    z_trial = as.numeric(scale(trial_index)), 
    Experiment="Implicit Task") %>%
  ungroup() %>%
  filter(!is.na(acc01)) %>%# GLMM needs 0/1 only
  select(-accuracy)

E2_acc <- E2 %>%
  mutate(
    acc01 = case_when(
      accuracy == "correct"   ~ 1L,
      accuracy == "incorrect" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  group_by(participant) %>%
  arrange(trial, .by_group = TRUE) %>%
  mutate(
    trial_index = trial,
    z_trial = as.numeric(scale(trial_index)), 
    Experiment="Explicit Task"
  ) %>%
  ungroup() %>%
  filter(!is.na(acc01)) %>% 
  select(-accuracy)

# --- Harmonize frame_side variable ---
E1_acc <- E1_acc %>%
  mutate(frame_side = factor(frame_side,
                             levels = c("left", "right")))


E2_acc <- E2_acc %>%
  mutate(frame_side = factor(ifelse(frame_side == 1, "right", "left"),
                             levels = c("left", "right")))


#===============================================================================
# change to bind rows easier due to data format issue 
# solution: remove accuracy column and only use acc01 
# ==============================================================================
acc_all <- bind_rows(E1_acc, E2_acc)

## Helper to summarise accuracy with Wald 95% CI
summarise_acc <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_trials   = n(),
      correct_n = sum(acc01),
      acc       = correct_n / n_trials,
      mean_acc = mean(acc01),
      sd_trial = sqrt(mean_acc * (1 - mean_acc)),           # Bernoulli SD across trials
      se_trial = sd_trial / sqrt(n_trials),       # binomial SE
      lcl       = pmax(acc - 1.96 * se_trial, 0),
      ucl       = pmin(acc + 1.96 * se_trial, 1),
      .groups = "drop"
    )
}

## 2) Overall accuracy per experiment
acc_overall <- summarise_acc(acc_all, c("Experiment"))
print(acc_overall)

## 3)Accuracy actually in Thesis  
acc_participant <- acc_all %>%
  group_by(Experiment, participant) %>%
  summarise(
    acc = mean(acc01),
    n_trials = n(),
    .groups="drop"
  )
acc_participant_summary <- acc_participant %>%
  group_by(Experiment) %>%
  summarise(
    Min = min(acc) * 100,
    Max = max(acc) * 100,
    Mean = mean(acc) * 100,
    SD = sd(acc) * 100,
    Valid_Trials = sum(n_trials),
    .groups="drop"
  )
acc_participant_summary


## 4) Accuracy by VALENCE within each experiment
acc_by_valence <- acc_all %>%
  group_by(Experiment, valence) %>%
  summarise(
    n_trials = n(),
    acc = mean(acc01),
    se = sqrt(acc * (1 - acc) / n_trials),   # binomial SE
    lcl = pmax(acc - 1.96 * se, 0),
    ucl = pmin(acc + 1.96 * se, 1),
    .groups = "drop"
  )
print(acc_by_valence)

## 5) Accuracy by CONGRUENCE within each experiment
# Summarize accuracy by experiment and condition
acc_by_congruence <- acc_all %>%
  group_by(Experiment, congruence) %>%
  summarise(
    n_trials = n(),
    acc = mean(acc01),
    se = sqrt(acc * (1 - acc) / n_trials),   # binomial SE
    lcl = pmax(acc - 1.96 * se, 0),
    ucl = pmin(acc + 1.96 * se, 1),
    .groups = "drop"
  )
print(acc_by_congruence)

# 6) Accuracy within 4 conditions 
# Create a combined factor for the 4 conditions
acc_all <- acc_all %>%
  mutate(
    Condition = case_when(
      congruence == "congruent"   & valence == "positive" ~ "CP",
      congruence == "congruent"   & valence == "negative" ~ "CN",
      congruence == "incongruent" & valence == "positive" ~ "IP",
      congruence == "incongruent" & valence == "negative" ~ "IN"
    )
  )

# Summarize accuracy by experiment and condition
acc_by_condition <- acc_all %>%
  group_by(Experiment, Condition) %>%
  summarise(
    mean_acc = mean(acc01),
    sd_acc   = sd(acc01),
    se_acc   = sd(acc01)/sqrt(n()),
    lcl      = pmax(mean_acc - 1.96*se_acc, 0),
    ucl      = pmin(mean_acc + 1.96*se_acc, 1),
    .groups = "drop"
  )

#what actually displayed in thesis 
acc_participant <- acc_all %>%
  group_by(Experiment, participant) %>%
  summarise(
    acc = mean(acc01),
    n_trials = n(),
    .groups="drop"
  )

acc_participant_summary <- acc_participant %>%
  group_by(Experiment) %>%
  summarise(
    Min = min(acc) * 100,
    Max = max(acc) * 100,
    Mean = mean(acc) * 100,
    SD = sd(acc) * 100,
    Valid_Trials = sum(n_trials),
    .groups="drop"
  )

acc_participant_summary



#==============================================================================
#=======   Plotting Descriptive Analysis            ===========================
#==============================================================================
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

#=======    Accuracy across the four groups for each experiment ===============

#============  Colours for CP, CN, IP, IN   ====================================
cond_cols <- c(
  "CP" = "#1F77B4",   # blue
  "CN" = "#AEC7E8",   # light blue
  "IP" = "#2CA02C",   # green
  "IN" = "#E377C2"    # pink
)

#============ Bar plot function (adds % columns internally) ====================
plot_acc_by_condition <- function(df, cond_cols) {
  df <- df %>%
    mutate(
      acc_percent = mean_acc * 100,
      lcl_percent = lcl * 100,
      ucl_percent = ucl * 100
    )
  
  ggplot(df, aes(x = Condition, y = mean_acc, fill = Condition)) +
    geom_col(width = 0.7, colour = "black") +
    geom_errorbar(aes(ymin = lcl, ymax = ucl),
                  width = 0.15, linewidth = 0.7) +
    geom_text(
      aes(
        label = sprintf("%.1f%% (%.1f–%.1f)",
                        acc_percent, lcl_percent, ucl_percent),
        y = pmin(ucl + 0.02, 1.08)  # prevents labels going outside the plot
      ),
      size = 8
    ) +
    scale_fill_manual(values = cond_cols, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, 1.1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "Condition", y = "Accuracy (%)") +
    theme_minimal(base_size = 20) +
    thesis_theme +
    theme(legend.position = "none")
}

# Filter datasets (do NOT overwrite after adding columns)
acc_implicit  <- acc_by_condition %>% filter(Experiment == "Implicit Task")
acc_explicit  <- acc_by_condition %>% filter(Experiment == "Explicit Task")

# Make plots
p_acc_implicit <- plot_acc_by_condition(acc_implicit, cond_cols)
p_acc_explicit <- plot_acc_by_condition(acc_explicit, cond_cols)

p_acc_implicit
p_acc_explicit

#===============================================================================
#============= Bar plots of the two conditions in Appendix =====================
#===============================================================================
#1) defining colours: 

colours_val <- c(negative = "#EE6677", positive = "#228833")
colours_cong <- c("congruent"   = "dodgerblue3", "incongruent" = "#66CCEE")
# 2). Congruence plot 
plot_acc_congruence <- function(df, title_txt = NULL) {
  ggplot(df, aes(x = congruence, y = acc, fill = congruence)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.25) +
    geom_text(
      aes(label = paste0(
        scales::percent(acc, accuracy = 0.1),
        "\n[", scales::percent(lcl, accuracy = 0.1), ", ", scales::percent(ucl, accuracy = 0.1), "]"
      )),
      vjust = -0.2, size = 5
    ) +
    scale_fill_manual(values = colours_cong) +
    labs(
      x = "Congruence",
      y = "Accuracy (proportion)"
    ) +
    theme_minimal(base_size = 20) +
    thesis_theme +
    theme(legend.position = "none")
}

acc_cong_E1 <- acc_by_congruence %>% filter(Experiment == "Implicit Task")
acc_cong_E2 <- acc_by_congruence %>% filter(Experiment == "Explicit Task")

p_cong_E1 <- plot_acc_congruence(acc_cong_E1)
p_cong_E2 <- plot_acc_congruence(acc_cong_E2)

p_cong_E1
p_cong_E2

#3) Valence plot 
plot_acc_valence <- function(df) {
  ggplot(df, aes(x = valence, y = acc, fill = valence)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.25) +
    geom_text(
      aes(label = paste0(
        scales::percent(acc, accuracy = 0.1),
        "\n[", scales::percent(lcl, accuracy = 0.1), ", ", scales::percent(ucl, accuracy = 0.1), "]"
      )),
      vjust = -0.2, size = 5
    ) +
    scale_fill_manual(values = colours_val) +
    labs(x = "Valence", y = "Accuracy (proportion)") +
    theme_minimal(base_size = 20) +
    thesis_theme +
    theme(legend.position = "none")
}

#4) Plotting for the instruction types
acc_val_E1 <- acc_by_valence %>% filter(Experiment == "Implicit Task")
acc_val_E2 <- acc_by_valence %>% filter(Experiment == "Explicit Task")

p_val_E1 <- plot_acc_valence(acc_val_E1)
p_val_E2 <- plot_acc_valence(acc_val_E2)

p_val_E1
p_val_E2

#==============================================================================
#======   Learning and Fatigue       ==========================================
#==============================================================================

#1) Implicit Task
E1_acc_learning <- E1_acc %>%
  group_by(trial_index) %>%
  summarise(
    mean_acc = mean(acc01, na.rm = TRUE),
    se_acc = sd(acc01, na.rm = TRUE) / sqrt(sum(!is.na(acc01))),
    .groups = "drop"
  )

ggplot(E1_acc_learning, aes(x = trial_index, y = mean_acc)) +
  geom_line(color = "#2ca02c", size = 1) +
  geom_ribbon(aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
              alpha = 0.2, fill = "#2ca02c") +
  geom_smooth(method = "loess", span = 0.3, color = "darkorange", se = FALSE) +
  labs(
    x = "Trial Index (z-standardized)",
    y = "Mean Accuracy"
  ) +
  theme_minimal(base_size = 20) +
  thesis_theme

#2) Explicit Task
E2_acc_learning <- E2_acc %>%
  group_by(trial_index) %>%
  summarise(
    mean_acc = mean(acc01, na.rm = TRUE),
    se_acc = sd(acc01, na.rm = TRUE) / sqrt(sum(!is.na(acc01))),
    .groups = "drop"
  )

ggplot(E2_acc_learning, aes(x = trial_index, y = mean_acc)) +
  geom_line(color = "#2ca02c", size = 1) +
  geom_ribbon(aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
              alpha = 0.2, fill = "#2ca02c") +
  geom_smooth(method = "loess", span = 0.3, color = "darkorange", se = FALSE) +
  labs(
    x = "Trial Index (z-standardized)",
    y = "Mean Accuracy"
  ) +
  theme_minimal(base_size = 20)+
  thesis_theme



#==============================================================================
#============== Generalised Linear Mxed Model     =============================
#==============================================================================
#===============  manual effect coding  =======================================
# Manually create ±1 coding (not the default contr.sum)
E1_acc$congruence_coded <- ifelse(E1_acc$congruence == "congruent",  1, -1)
E1_acc$valence_coded    <- ifelse(E1_acc$valence == "positive",  1, -1)
E1_acc$frame_side_coded <- ifelse(E1_acc$frame_side == "right",  1, -1)

E2_acc$congruence_coded <- ifelse(E2_acc$congruence == "congruent",  1, -1)
E2_acc$valence_coded    <- ifelse(E2_acc$valence == "positive",  1, -1)
E2_acc$frame_side_coded <- ifelse(E2_acc$frame_side == "right",  1, -1)

# ==========================  Starter GLMM ====================================
E1_acc_glm <- glmer(acc01 ~ congruence_coded*valence_coded + (1|participant),
                    data = E1_acc, family = binomial)
E2_acc_glm <- glmer(acc01 ~ congruence_coded*valence_coded + (1|participant),
                    data = E2_acc, family = binomial)

summary(E1_acc_glm)
summary(E2_acc_glm)

#=================  adding fatigue and learning ================================
E1_acc_time <- glmer(acc01 ~ z_trial + congruence_coded*valence_coded  + (1|participant),
                     data = E1_acc, family = binomial)
E2_acc_time <- glmer(acc01 ~ z_trial + congruence_coded*valence_coded  + (1|participant),
                     data = E2_acc, family = binomial)

summary(E1_acc_time)
summary(E2_acc_time)


#================ Adding picture ID and frame side =============================

E1_acc_pic <- glmer(acc01 ~ z_trial + congruence_coded*valence_coded +frame_side_coded + (1|participant) + (1|picture_id),
                    data = E1_acc, family = binomial)
E2_acc_pic <- glmer(acc01 ~ z_trial + congruence_coded*valence_coded +frame_side_coded + (1|participant) + (1|picture_id),
                    data = E2_acc, family = binomial)

summary(E1_acc_pic)
summary(E2_acc_pic)


#===============================================================================
#=============  Plotting GLMM results         ==================================
#===============================================================================
preds_acc <- ggpredict(
  E2_acc_glm,
  terms = c("congruence_coded [-1,1]", "valence_coded [-1,1]")
) %>%
  mutate(
    # map your effect coding to readable labels
    congruence = factor(ifelse(x ==  1, "congruent",  "incongruent"),
                        levels = c("congruent", "incongruent")),
    valence    = factor(ifelse(group ==  1, "positive", "negative"),
                        levels = c("positive", "negative"))
  ) %>%
  # keep only rows where mapping succeeded
  filter(!is.na(congruence), !is.na(valence))


# Plot predicted probabilities with 95% CIs

ggplot(preds_acc,
       aes(x = congruence, y = predicted, color = valence, group = valence)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5), width = 0.15, linewidth = 1.2) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Congruence",
    y = "Predicted Accuracy",
    color = "Valence"
  ) +
  theme_minimal(base_size = 20) + 
  thesis_theme +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#===============================================================================
#============  Learning curves for accuracy (probability scale with ribbons)  ==
# ============ helper: get smooth probability curves for accuracy ==============
#===============================================================================
predict_acc_curve <- function(glmm, at_seq = seq(-2, 2, by = 0.1), by = NULL) {
  # emmeans on the logit link; type="response" gives probs and proper CIs
  if (is.null(by)) {
    em <- emmeans(glmm, ~ z_trial, at = list(z_trial = at_seq), type = "response")
  } else {
    rhs <- paste(c("z_trial", by), collapse = " * ")
    em  <- emmeans(glmm, as.formula(paste("~", rhs)),
                   at = list(z_trial = at_seq), type = "response")
  }
  out <- as.data.frame(em)  # columns: prob, SE, asymp.LCL, asymp.UCL
  names(out)[names(out) == "prob"] <- "p_correct"
  out
}

# Overall learning curves (probability scale)
E1_acc_curve <- predict_acc_curve(E1_acc_time)
E2_acc_curve <- predict_acc_curve(E2_acc_time)
#For Implicit Task
ggplot(E1_acc_curve, aes(x = z_trial, y = p_correct)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = .20) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.9, 1.0)) +
  labs(
    x = "Standardized trial index (z_trial)",
    y = "Predicted accuracy (probability)") +
  theme_minimal(base_size = 20) +
  thesis_theme
#For Explicit Task
ggplot(E2_acc_curve, aes(x = z_trial, y = p_correct)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = .20) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Standardized trial index (z_trial)",
    y = "Predicted accuracy (probability)") +
  theme_minimal(base_size = 20) + 
  thesis_theme

#===============================================================================
#============  Adding picture ID and frame side to GLMM ========================
#============ Valence effect before vs. after adding picture ID ================
#===============================================================================

fe_valence <- bind_rows(
  tidy(E1_acc_time, effects = "fixed", conf.int = TRUE) %>%
    filter(term == "valence_coded") %>%
    mutate(model = "Base (no picture ID)"),
  tidy(E1_acc_pic, effects = "fixed", conf.int = TRUE) %>%
    filter(term == "valence_coded") %>%
    mutate(model = "With picture ID")
)

# Ensure model factor order
fe_valence$model <- factor(fe_valence$model,
                           levels = c("Base (no picture ID)", "With picture ID"))

# Plot valence effect
ggplot(fe_valence, aes(x = model, y = estimate, group = 1)) +
  geom_line(color = "black") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_text(aes(label = sprintf("%.4f", round(estimate, 4))),
            vjust = -1.2, size = 8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    x = "Model",
    y = "Estimated valence effect (log-odds of correct response)"
  ) +
  coord_cartesian(ylim = c(min(fe_valence$conf.low) - 0.001,
                           max(fe_valence$conf.high) + 0.001)) +
  theme_minimal(base_size = 20)+
  thesis_theme

#==============================================================================
#======= Picture-level Analysis (Exploration) =================================
#==============================================================================

#downloading the images for content analysis 
img_dir  <- file.path("[ORIG]allpics_1-44pos_45-88neg")

# 1) Helper: fitted values on the correct scale (logit for GLMM)
.get_fitted <- function(mod) {
  if (inherits(mod, "glmerMod")) fitted(mod, type = "link") else fitted(mod)
}

# 2) Plot picture contributions for ACCURACY (full - no-picture RE), with top_n option
plot_picture_effects_noBLUP_acc <- function(model_full, data,
                                            picture_col = "picture_id",
                                            valence_col = "valence",
                                            center = TRUE,
                                            top_n = NULL,
                                            every_nth_label = NULL,
                                            axis_text_y_size = 10,  # ← NEW
                                            return_df = FALSE) {
  
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
  
  if (valence_col %in% names(data)) {
    val_map <- data %>%
      group_by(.data[[picture_col]]) %>%
      summarise(valence = first(na.omit(.data[[valence_col]])), .groups = "drop")
    df <- df %>% left_join(val_map, by = setNames(picture_col, picture_col))
  }
  
  if (!is.null(top_n)) {
    df <- df %>% slice_max(order_by = abs(effect), n = top_n, with_ties = FALSE)
  }
  
  if (return_df) return(df)
  
  p <- ggplot(df,
              aes(x = reorder(as.character(.data[[picture_col]]), effect),
                  y = effect, fill = valence)) +
    geom_col(color = "black", linewidth = 0.2) +
    scale_fill_manual(
      values = c("positive" = "#2ecc71", "negative" = "#e74c3c"),
      drop = FALSE
    ) +
    coord_flip() +
    labs(
      x = "Picture ID",
      y = "Picture contribution (logit of P(correct))",
      fill = "Valence"
    ) +
    theme_minimal(base_size = 20) +
    thesis_theme +
    theme(
      axis.text.y = element_text(size = axis_text_y_size)  # ← key line
    )
  
  if (!is.null(every_nth_label)) {
    p <- p + scale_x_discrete(
      breaks = function(x) x[seq(1, length(x), by = every_nth_label)]
    )
  }
  
  p
}#


# ==============================================================================
# ==================== MAIN BODY: top 40 =======================================
# ==============================================================================
# For Implicit Task 
p1_acc_top40 <- plot_picture_effects_noBLUP_acc(
  model_full = E1_acc_pic,
  data = E1_acc,
  top_n = 40,
  axis_text_y_size = 20   
)

# For Explicit Task
p2_acc_top40 <- plot_picture_effects_noBLUP_acc(
  model_full = E2_acc_pic,
  data = E2_acc,
  top_n = 40,
  axis_text_y_size = 20
)

p1_acc_top40
p2_acc_top40

# ==============================================================================
# ============   APPENDIX: full plots ==========================================
# ==============================================================================
# For Implicit Task
p1_acc_full <- plot_picture_effects_noBLUP_acc(
  model_full = E1_acc_pic,
  data = E1_acc,
  axis_text_y_size = 11
)

# For Explicit Task
p2_acc_full <- plot_picture_effects_noBLUP_acc(
  model_full = E2_acc_pic,
  data = E2_acc,
  axis_text_y_size = 11
)


p1_acc_full
p2_acc_full

#===============================================================================
#===========                  Spreadsheets    ==================================
#===============================================================================
# 1) Get the per-picture effect table used for ranking
e2_acc_effects <- plot_picture_effects_noBLUP_acc(E2_acc_pic, E2_acc,
                                                  picture_col="picture_id", valence_col="valence",
                                                  center=TRUE, return_df=TRUE)

# 2) Pick top/bottom pictures by effect
n_top <- 12

e2_top_pos <- e2_acc_effects %>% arrange(desc(effect)) %>% slice_head(n=n_top) %>% pull(picture_id)
e2_top_neg <- e2_acc_effects %>% arrange(effect)      %>% slice_head(n=n_top) %>% pull(picture_id)

# 3) Build caption tables with human-readable accuracy (%)
per_pic_acc_E1 <- E1_acc %>%
  group_by(picture_id) %>%
  summarise(n_trials = n(),
            mean_acc = mean(acc01), .groups="drop")
per_pic_acc_E2 <- E2_acc %>%
  group_by(picture_id) %>%
  summarise(n_trials = n(),
            mean_acc = mean(acc01), .groups="drop")

# rule-based valence for any image id if needed (or use your existing mapping)
valence_by_id_E1 <- E1_acc %>% distinct(picture_id, valence) %>%
  rename(valence_from_id = valence)
valence_by_id_E2 <- E2_acc %>% distinct(picture_id, valence) %>%
  rename(valence_from_id = valence)

valence_color <- function(v){
  vv <- tolower(as.character(v))
  if (is.na(vv)) return("#444444")
  if (vv %in% c("positive","pos","1")) return("#2ecc71")
  if (vv %in% c("negative","neg","0")) return("#e74c3c")
  "#444444"
}

find_image_file <- function(id, img_dir){
  f1 <- file.path(img_dir, sprintf("image_%d.jpg", id))
  if (file.exists(f1)) return(f1)
  f2 <- file.path(img_dir, sprintf("image_%02d.jpg", id))
  if (file.exists(f2)) return(f2)
  hits <- list.files(img_dir, pattern="\\.(jpg|jpeg|png)$", full.names=TRUE)
  hits <- hits[grepl(sprintf("(^|[^0-9])%d([^0-9]|$)", id), basename(hits))]
  if (length(hits)) return(hits[1])
  NA_character_
}

compose_labeled_acc <- function(ids,  out_png,
                                per_tbl_acc, valence_map,
                                img_dir, tile_cols=4, cell_px=420, border_px=10){
  ids <- as.integer(as.character(ids))
  imgs <- lapply(ids, function(id){
    f <- find_image_file(id, img_dir)
    if (is.na(f)) return(NULL)
    
    s  <- per_tbl_acc %>% filter(picture_id == id)
    v  <- valence_map %>% filter(picture_id == id) %>% pull(valence_from_id) %>% dplyr::first()
    ntr <- if (nrow(s)) s$n_trials[1] else NA_integer_
    acc <- if (nrow(s)) s$mean_acc[1] else NA_real_
    
    frame_col <- valence_color(v)
    img <- magick::image_read(f) |>
      magick::image_resize(sprintf("%dx%d", cell_px, cell_px)) |>
      magick::image_border(frame_col, sprintf("%dx%d", border_px, border_px))
    
    cap <- sprintf("#%d [%s] acc: %s | n:%d",
                   id, v,
                   scales::percent(acc, accuracy = 0.1),
                   ntr)
    cap_img <- magick::image_blank(width = magick::image_info(img)$width,
                                   height = 48, color = "black") |>
      magick::image_annotate(cap, size = 24, color = "white", gravity = "center")
    
    magick::image_append(c(img, cap_img), stack = TRUE)
  })
  imgs <- Filter(Negate(is.null), imgs)
  if (!length(imgs)) return(invisible(NULL))
  
  m <- magick::image_montage(magick::image_join(imgs),
                             tile=paste0(tile_cols,"x"), geometry="+6+6") |>
    magick::image_border("black", "5x5") #|>
  #magick::image_annotate(title, size=36, color="white", boxcolor="black", gravity="north")
  
  fmt <- tools::file_ext(out_png)
  m <- magick::image_convert(m, format = fmt)
  magick::image_write(m, path = out_png, format = fmt)
}

# 4) Render the four sheets (point img_dir/out_dir to save locations)
img_dir <- "[ORIG]allpics_1-44pos_45-88neg"
out_dir <- "SET/YOUR/PATH/Graphs_final"

compose_labeled_acc(e2_top_pos,
                    file.path(out_dir,"E2_acc_top_positive_New.png"), per_pic_acc_E2, valence_by_id_E2, img_dir)
compose_labeled_acc(e2_top_neg,
                    file.path(out_dir,"E2_acc_top_negative_new.png"), per_pic_acc_E2, valence_by_id_E2, img_dir)


# ==============================================================================
#=============== co-picture  ===================================================
# helper: robust integer coercion for factors/characters/numerics ==============
# ==============================================================================
# ============ Partner-image accuracy effect (clean + reusable) ================
# ============ Builds focal/co-image dataset + fits GLMM for E1 or E2  =========
# ===============================================================================
# robust integer coercion for factors/characters/numerics
safe_int <- function(x) as.integer(as.character(x))

# ==== 1) Build the partner-accuracy dataframe (works for E1 or E2) ============
build_partner_acc_df <- function(df,
                                 task = c("E1", "E2"),
                                 picture_id_col = "picture_id",
                                 valence_col = "valence",
                                 congruence_col = "congruence",
                                 frame_side_col = "frame_side",
                                 participant_col = "participant",
                                 trial_col = "trial",
                                 z_trial_col = "z_trial",
                                 acc_col = "acc01",
                                 # E1 pairing columns
                                 e1_pos_col = "picture_pos_ID",
                                 e1_neg_col = "picture_neg_ID",
                                 # E2 pairing columns
                                 e2_left_col = "left_id",
                                 e2_right_col = "right_id") {
  
  task <- match.arg(task)
  
  #============= ensure core columns exist ============
  needed <- c(picture_id_col, valence_col, congruence_col, frame_side_col,
              participant_col, trial_col, z_trial_col, acc_col)
  missing <- setdiff(needed, names(df))
  if (length(missing)) stop("Missing columns in df: ", paste(missing, collapse = ", "))
  
  #== Make a per-picture summary: mean accuracy when that picture is focal =====
  per_image <- df %>%
    mutate(picture_id_int = safe_int(.data[[picture_id_col]])) %>%
    group_by(picture_id_int) %>%
    summarise(
      focal_meanACC = mean(.data[[acc_col]], na.rm = TRUE),
      focal_valence = dplyr::first(na.omit(.data[[valence_col]])),
      .groups = "drop"
    ) %>%
    rename(image_id = picture_id_int)
  
  # ============ Build focal/co pairs depending on task ============
  if (task == "E1") {
    #============ require Implicit  pairing cols ===============================
    e1_needed <- c(e1_pos_col, e1_neg_col)
    miss_e1 <- setdiff(e1_needed, names(df))
    if (length(miss_e1)) stop("E1 requires columns: ", paste(miss_e1, collapse = ", "))
    
    pairs <- df %>%
      filter(!is.na(.data[[picture_id_col]]),
             !is.na(.data[[e1_pos_col]]),
             !is.na(.data[[e1_neg_col]])) %>%
      transmute(
        participant = .data[[participant_col]],
        trial       = .data[[trial_col]],
        z_trial     = .data[[z_trial_col]],
        acc01       = .data[[acc_col]],
        valence     = .data[[valence_col]],
        congruence  = .data[[congruence_col]],
        frame_side  = .data[[frame_side_col]],
        focal_id    = safe_int(.data[[picture_id_col]]),
        co_id       = safe_int(ifelse(.data[[picture_id_col]] == .data[[e1_pos_col]],
                                      .data[[e1_neg_col]], .data[[e1_pos_col]]))
      )
    
  } else {
    #============  Explicit Task ===============================================
    e2_needed <- c(e2_left_col, e2_right_col)
    miss_e2 <- setdiff(e2_needed, names(df))
    if (length(miss_e2)) stop("E2 requires columns: ", paste(miss_e2, collapse = ", "))
    
    df2 <- df %>%
      mutate(
        left_id_int  = safe_int(.data[[e2_left_col]]),
        right_id_int = safe_int(.data[[e2_right_col]])
      ) %>%
      filter(!is.na(left_id_int), !is.na(right_id_int))
    
    #============ Duplicate each trial so each side becomes focal once =========
    pairs <- bind_rows(
      df2 %>% transmute(
        participant = .data[[participant_col]],
        trial       = .data[[trial_col]],
        z_trial     = .data[[z_trial_col]],
        acc01       = .data[[acc_col]],
        valence     = .data[[valence_col]],
        congruence  = .data[[congruence_col]],
        frame_side  = .data[[frame_side_col]],
        focal_id    = left_id_int,
        co_id       = right_id_int
      ),
      df2 %>% transmute(
        participant = .data[[participant_col]],
        trial       = .data[[trial_col]],
        z_trial     = .data[[z_trial_col]],
        acc01       = .data[[acc_col]],
        valence     = .data[[valence_col]],
        congruence  = .data[[congruence_col]],
        frame_side  = .data[[frame_side_col]],
        focal_id    = right_id_int,
        co_id       = left_id_int
      )
    )
  }
  
  # ============  Join partner info + create coded predictors ==================
  out <- pairs %>%
    left_join(per_image, by = c("focal_id" = "image_id")) %>%
    left_join(per_image %>%
                rename(partner_meanACC = focal_meanACC,
                       partner_valence = focal_valence),
              by = c("co_id" = "image_id")) %>%
    filter(!is.na(partner_meanACC)) %>%
    mutate(
      congruence_coded   = ifelse(congruence == "congruent",  1, -1),
      valence_coded      = ifelse(valence    == "positive",   1, -1),
      frame_side_coded   = ifelse(frame_side == "right",      1, -1),
      partner_meanACC_z  = as.numeric(scale(partner_meanACC)),
      participant        = factor(participant),
      focal_id           = factor(focal_id),
      co_id              = factor(co_id)
    )
  
  out
}

# ============  Fit the partner-accuracy GLMM (one clean function call) ========
fit_partner_acc_glmm <- function(partner_df) {
  glmer(
    acc01 ~ z_trial + congruence_coded * valence_coded + frame_side_coded +
      partner_meanACC_z +
      (1 | participant) + (1 | focal_id) + (1 | co_id),
    data = partner_df,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
  )
}

# ==============================================================================
# ============ RUN: Implicit (E1) and Explicit (E2)   ==========================
# ==============================================================================

partner_acc_E1 <- build_partner_acc_df(E1_acc, task = "E1")
model_partner_acc_E1 <- fit_partner_acc_glmm(partner_acc_E1)
summary(model_partner_acc_E1)

partner_acc_E2 <- build_partner_acc_df(E2_acc, task = "E2")
model_partner_acc_E2 <- fit_partner_acc_glmm(partner_acc_E2)
summary(model_partner_acc_E2)


#=========  Plotting results of GLMM with co-ID =================================

# Get marginal effect of partner_meanACC_z
eff_partner_E1 <- ggpredict(model_partner_acc_E1, terms = "partner_meanACC_z")
eff_partner_E2 <- ggpredict(model_partner_acc_E2, terms = "partner_meanACC_z")

# Plot
ggplot(eff_partner_E1, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, colour = "black") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  labs(#title = "Effect of Co-Presented Image Performance on Focal Accuracy\n(Explicit Task)",
    x = "Partner Image Accuracy (z-scored)",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_classic(base_size = 20) +
  thesis_theme

ggplot(eff_partner_E2, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, colour = "black") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  labs(#title = "Effect of Co-Presented Image Performance on Focal Accuracy\n(Explicit Task)",
    x = "Partner Image Accuracy (z-scored)",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_classic(base_size = 20) +
  thesis_theme

# ==============================================================================
# ============ Spreadsheet for co-ID ===========================================
# ==============================================================================
# =========== 1) Average partner effect per co_id (unchanged) ============
per_co_image <- partner_acc %>%
  group_by(co_id) %>%
  summarise(
    n_trials          = n(),
    mean_partnerACC   = mean(acc01, na.rm = TRUE),          # proportion 0–1
    mean_partnerACC_z = mean(partner_meanACC_z, na.rm = TRUE),
    valence           = dplyr::first(na.omit(partner_valence)),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_partnerACC))

# ============ 2) Pick top/bottom images (unchanged) ============
n_top <- 12
helpful_co_ids    <- per_co_image$co_id[1:n_top]
disruptive_co_ids <- per_co_image$co_id[(nrow(per_co_image)-n_top+1):nrow(per_co_image)]

# ============ helper for frame color (unchanged logic) ============
valence_color <- function(val) {
  v <- tolower(as.character(val))
  if (is.na(v)) return("#444444")
  if (v %in% c("positive","pos","1")) return("#2ecc71")  # green
  if (v %in% c("negative","neg","0")) return("#e74c3c")  # red
  "#444444"
}

# ============ 3) Accuracy version of the contact-sheet composer ============
compose_labeled_acc <- function(ids, title, per_tbl, out_png,
                                id_col = "co_id",
                                acc_col = "mean_partnerACC",
                                val_col = "valence",
                                n_col = "n_trials",
                                tile_cols = 4, cell_px = 420, border_px = 10) {
  ids <- as.integer(as.character(ids))
  imgs <- lapply(ids, function(id) {
    f <- file.path(img_dir, sprintf("image_%d.jpg", id))
    if (!file.exists(f)) return(NULL)
    
    row <- per_tbl[per_tbl[[id_col]] == id, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    
    acc   <- row[[acc_col]][1]   # proportion 0–1
    ntr   <- row[[n_col]][1]
    val   <- row[[val_col]][1]
    frame <- valence_color(val)
    
    img <- image_read(f) |>
      image_resize(sprintf("%dx%d", cell_px, cell_px)) |>
      image_border(frame, sprintf("%dx%d", border_px, border_px))
    
    cap <- sprintf("#%d [%s] acc: %s | n:%d",
                   id, val, percent(acc, accuracy = 1), ntr)
    
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

# ============ 4) Make the two sheets (accuracy captions) ============
compose_labeled_acc(
  helpful_co_ids,
  "E2 – Partner Images Associated with Highest Accuracy",
  per_tbl = per_co_image,
  out_png = file.path(out_dir, "E2_partner_highACC.png")
)

compose_labeled_acc(
  disruptive_co_ids,
  "E2 – Partner Images Associated with Lowest Accuracy",
  per_tbl = per_co_image,
  out_png = file.path(out_dir, "E2_partner_lowACC.png")
)

#IMPLICIT TASK PREPROCESSING 
# 1. Fixes frame time issue 
# 2. Aligns data
# 3. Looks into Eye-tracking data 
# 4. Preprocesses Data according to Steps mentioned in README


#================= Downloading the libraries ===================================
library(R.matlab)
library(dplyr)
library(tibble)
library(tidyr)
library(lme4)
library(ggplot2)
library(cowplot)

#=================== Setting the pathway to data  =============================
setwd("SET/YOUR/PATH/Experiment 1 ")

# ==================  Load Data ===============================================
reaction_time_raw <- readMat("behavioralReactionTime.mat")[[1]]
frame_color_raw <- readMat("frameColor.mat")[[1]]
frame_side_raw <- readMat("frameSide.mat")[[1]]
valence_looked_raw <- readMat("valenceLooked.mat")[[1]]
picture_time_raw <- readMat("pictureTime.mat")[[1]]
trial_time_raw <- readMat("trialTime.mat")[[1]]
xPosition_raw <- readMat("xPositionData.mat")[[1]]
reaction_type_raw <- readMat("reactionType.mat")[[1]]
frame_time_raw <- readMat("frameTime.mat")[[1]]
fixation_cross_time_raw <- readMat("fixationCrossTime.mat")[[1]]
pictureSequenceNegative_raw <- readMat("pictureSequenceNegative.mat")[[1]]
pictureSequencePositive_raw <- readMat("pictureSequencePositive.mat")[[1]]
side_positive_raw <- readMat("sidePositive.mat")[[1]]


#================== Handling missing frame time ==============================
# Fix missing frame_time: set frame_time to NA and shift frame_time down by 1 index
for (session in seq_len(dim(reaction_time_raw)[2])) {  # 130 sessions
  participant <- ceiling(session / 2)
  n_trials <- 48
  
  trial_vec <- frame_time_raw[[session]][[1]]
  
  t <- 1
  while (t <= n_trials) {
    rt <- reaction_time_raw[t, session]
    val <- valence_looked_raw[t, session]
    
    if (is.na(rt) && is.na(val)) {
      # Insert NA at the correct position
      trial_vec <- append(trial_vec, values = NA, after = t - 1)
      
      # Set the associated behavioral variables to NA (optional, already NA)
      frame_color_raw <- replace(frame_color_raw, list = cbind(t, session), values = NA)
      frame_side_raw <- replace(frame_side_raw, list = cbind(t, session), values = NA)
    }
    
    t <- t + 1
  }
  
  # Update session list
  frame_time_raw[[session]][[1]] <- trial_vec
}

# === Add NA to picture_time_raw for each session ===
for (session in seq_len(length(picture_time_raw))) {
  # Check the number of trials for the current session (should be 40)
  n_trials <- length(picture_time_raw[[session]][[1]])
  
  # If there are fewer than 48 trials, extend with NA
  if (n_trials < 48) {
    picture_time_raw[[session]][[1]] <- c(picture_time_raw[[session]][[1]], rep(NA, 48 - n_trials))
  }
}

# === Helper Function to Extend Data ===
# Function to extend data to match target length for nested lists,
# ensuring the correct structure of lists is maintained (adding empty lists)
extend_nested_data <- function(data, target_length) {
  for (session_idx in seq_along(data)) {
    for (trial_idx in seq_along(data[[session_idx]])) {
      trial_data <- data[[session_idx]][[trial_idx]]
      
      # Ensure each trial has the correct number of elements (empty lists for new trials)
      if (length(trial_data) < target_length) {
        # Append empty lists for the missing trials
        data[[session_idx]][[trial_idx]] <- c(trial_data, rep(list(list()), target_length - length(trial_data)))
      }
    }
  }
  return(data)
}

# === Extend xPosition_raw and trial_time_raw for each session and trial ===
xPosition_raw_extended <- extend_nested_data(xPosition_raw, 48)
trial_time_raw_extended <- extend_nested_data(trial_time_raw, 48)



#======= Identify Invalid Trials (NaN in reaction_time, valence, or frame_time) ===
valid_trials <- list()

# Loop through each session (i.e., each column in the matrix for reaction_time_raw)
for (session in 1:ncol(reaction_time_raw)) {
  # Identify invalid trials where reaction_time, valence, or frame_time is NA
  invalid_trials <- is.na(frame_time_raw[, session])
  
  # Store the valid trials (non-NA)
  valid_trials[[session]] <- !invalid_trials  # TRUE for valid trials
}

# ============ Combine Blocks (Sessions 1 and 2) for Each Participant ===========
combined_reaction_time_raw <- list()
combined_valence_looked_raw <- list()
combined_frame_time_raw <- list()
combined_frame_color_raw <- list()
combined_frame_side_raw <- list() 
combined_pic_pos_raw <- list()
combined_pic_neg_raw <-list()
combined_reaction_type_raw <-list()
combined_side_pos <-list() 
combined_fixation_cross_time <-list()

combined_picture_time_raw <- list()
combined_trial_time_raw <- list()
combined_xPosition_raw <- list()

# Combine the data for each participant (sessions 1 and 2 together)
for (participant in seq_len(65)) {  # There are 65 participants
  # Get the data for the two blocks (sessions 2*participant-1 and 2*participant)
  block1_session <- 2 * participant - 1
  block2_session <- 2 * participant
  
  # Combine blocks for each participant by appending the data
  combined_reaction_time_raw[[participant]] <- c(reaction_time_raw[, block1_session], reaction_time_raw[, block2_session])
  combined_valence_looked_raw[[participant]] <- c(valence_looked_raw[, block1_session], valence_looked_raw[, block2_session])
  combined_frame_time_raw[[participant]] <- c(frame_time_raw[[block1_session]][[1]], frame_time_raw[[block2_session]][[1]])
  combined_frame_color_raw[[participant]] <- c(frame_color_raw[, block1_session], frame_color_raw[, block2_session])
  combined_frame_side_raw[[participant]] <- c(frame_side_raw[, block1_session], frame_side_raw[, block2_session])
  combined_pic_pos_raw[[participant]] <- c(pictureSequencePositive_raw[, block1_session], pictureSequencePositive_raw[, block2_session])
  combined_pic_neg_raw[[participant]] <- c(pictureSequenceNegative_raw[, block1_session], pictureSequenceNegative_raw[, block2_session])
  combined_reaction_type_raw[[participant]] <- c(reaction_type_raw[, block1_session], reaction_type_raw[, block2_session])
  combined_side_pos[[participant]] <- c(side_positive_raw[, block1_session], side_positive_raw[, block2_session])
  combined_fixation_cross_time[[participant]] <- c(fixation_cross_time_raw[, block1_session], fixation_cross_time_raw[, block2_session])
  # Combine the list-like data (picture_time_raw, trial_time_raw, xPosition_raw)
  combined_picture_time_raw[[participant]] <- c(picture_time_raw[[block1_session]][[1]], picture_time_raw[[block2_session]][[1]])
  combined_trial_time_raw[[participant]] <- c(trial_time_raw_extended[[block1_session]][[1]], trial_time_raw_extended[[block2_session]][[1]])
  combined_xPosition_raw[[participant]] <- c(xPosition_raw_extended[[block1_session]][[1]], xPosition_raw_extended[[block2_session]][[1]])
}


# ========= Recalculate Valid Trials for Combined Blocks =============================
#Calculate valid_trials for the combined dataset (96 trials per participant)
valid_trials_combined <- list()

for (participant in seq_len(65)) {
  # Combine reaction_time_raw, valence_looked_raw, frame_time_raw for each participant
  combined_frame_time <- combined_frame_time_raw[[participant]]
  
  # Identify invalid trials where reaction_time, valence, or frame_time is NA
  invalid_trials_combined <- is.na(combined_frame_time)
  
  # Store the valid trials (non-NA) for the combined data
  valid_trials_combined[[participant]] <- !invalid_trials_combined  # TRUE for valid trials
}

# =====================Remove Invalid Trials from Combined Blocks ==================
# Initialize new filtered variables for the combined blocks
filtered_reaction_time_raw <- list()
filtered_valence_looked_raw <- list()
filtered_frame_time_raw <- list()
filtered_frame_color_raw <- list() 
filtered_frame_side_raw <- list() 
filtered_pic_pos_raw <- list() 
filtered_pic_neg_raw <- list() 
filtered_reaction_type_raw <- list() 
filtered_side_pos <- list() 
filtered_fixation_cross_time <- list() 

filtered_picture_time_raw <- list()
filtered_trial_time_raw <- list()
filtered_xPosition_raw <- list()

# Loop through each participant and remove invalid trials from the combined blocks
for (participant in seq_len(65)) {
  # Apply the valid_trials_combined mask to remove invalid trials for the current participant
  filtered_reaction_time_raw[[participant]] <- combined_reaction_time_raw[[participant]][valid_trials_combined[[participant]]]
  filtered_valence_looked_raw[[participant]] <- combined_valence_looked_raw[[participant]][valid_trials_combined[[participant]]]
  filtered_frame_time_raw[[participant]] <- combined_frame_time_raw[[participant]][valid_trials_combined[[participant]]]
  filtered_frame_color_raw[[participant]] <- combined_frame_color_raw[[participant]][valid_trials_combined[[participant]]] 
  filtered_frame_side_raw[[participant]] <- combined_frame_side_raw[[participant]][valid_trials_combined[[participant]]]
  filtered_pic_pos_raw[[participant]] <- combined_pic_pos_raw[[participant]][valid_trials_combined[[participant]]] 
  filtered_pic_neg_raw[[participant]] <- combined_pic_neg_raw[[participant]][valid_trials_combined[[participant]]] 
  filtered_reaction_type_raw[[participant]] <- combined_reaction_type_raw[[participant]][valid_trials_combined[[participant]]] 
  filtered_side_pos[[participant]] <- combined_side_pos[[participant]][valid_trials_combined[[participant]]]
  filtered_fixation_cross_time[[participant]]<- combined_fixation_cross_time[[participant]][valid_trials_combined[[participant]]]
  
  
  # Apply the valid_trials_combined mask for list-like data (picture_time_raw, trial_time_raw, xPosition_raw)
  filtered_picture_time_raw[[participant]] <- combined_picture_time_raw[[participant]][valid_trials_combined[[participant]]]
  filtered_trial_time_raw[[participant]] <- combined_trial_time_raw[[participant]][valid_trials_combined[[participant]]]
  filtered_xPosition_raw[[participant]] <- combined_xPosition_raw[[participant]][valid_trials_combined[[participant]]]
}

# ===============================
# Build dataframe from filtered_* lists
# ===============================
# --- Minimal, robust dataframe from filtered_*_raw ---


# Helpers: unwrap one value from atomic or nested list/cell =====================
unwrap_num <- function(x) {
  v <- x
  while (is.list(v) && length(v) > 0) v <- v[[1]]
  v <- suppressWarnings(as.numeric(v[1]))
  ifelse(length(v) == 0 || is.na(v), NA_real_, v)
}

unwrap_int <- function(x) {
  v <- x
  while (is.list(v) && length(v) > 0) v <- v[[1]]
  v <- suppressWarnings(as.integer(v[1]))
  ifelse(length(v) == 0 || is.na(v), NA_integer_, v)
}

unwrap_move <- function(x) {
  v <- x
  while (is.list(v) && length(v) > 0) v <- v[[1]]
  v <- tolower(as.character(v[1]))
  ifelse(v %in% c("push","pull"), v, NA_character_)
}

# Build rows ===================================================================
rows <- vector("list", length = 0)
nP <- length(filtered_reaction_time_raw)

for (p in seq_len(nP)) {
  # lengths (be defensive in case any list has different length)
  lens <- c(
    length(filtered_reaction_time_raw[[p]]),
    length(filtered_valence_looked_raw[[p]]),
    length(filtered_frame_time_raw[[p]]),
    length(filtered_frame_color_raw[[p]]),
    length(filtered_frame_side_raw[[p]]),
    length(filtered_side_pos[[p]]),
    length(filtered_picture_time_raw[[p]]),
    length(filtered_fixation_cross_time[[p]]),
    length(filtered_pic_pos_raw[[p]]),
    length(filtered_pic_neg_raw[[p]]),
    length(filtered_reaction_type_raw[[p]])
  )
  nT <- min(lens[lens > 0])
  if (is.infinite(nT) || nT == 0) next
  
  for (t in seq_len(nT)) {
    RT_sec <- unwrap_num(filtered_reaction_time_raw[[p]][t])
    RT_ms  <- if (!is.na(RT_sec)) RT_sec * 1000 else NA_real_
    #RT_log10 <- if (!is.na(RT_sec) && RT_sec > 0) log10(RT_sec) else NA_real_
    
    frame_time          <- unwrap_num(filtered_frame_time_raw[[p]][t])
    picture_time        <- unwrap_num(filtered_picture_time_raw[[p]][t])
    fixation_cross_time <- unwrap_num(filtered_fixation_cross_time[[p]][t])
    
    val_num  <- unwrap_num(filtered_valence_looked_raw[[p]][t])      # 1=pos, 0=neg
    col_num  <- unwrap_num(filtered_frame_color_raw[[p]][t])         # 1=blue, 0=orange
    fside_n  <- unwrap_num(filtered_frame_side_raw[[p]][t])          # 1=right, 0=left
    spos_n   <- unwrap_num(filtered_side_pos[[p]][t])                # 1=right, 0=left
    
    valence <- if (is.na(val_num)) NA_character_ else if (val_num == 1) "positive" else "negative"
    frame_color <- if (is.na(col_num)) NA_character_ else if (col_num == 1) "blue" else "orange"
    frame_side  <- if (is.na(fside_n)) NA_character_ else if (fside_n == 1) "right" else "left"
    positive_picture_side <- if (is.na(spos_n)) NA_character_ else if (spos_n == 1) "right" else "left"
    
    picture_pos_ID <- unwrap_int(filtered_pic_pos_raw[[p]][t])
    picture_neg_ID <- unwrap_int(filtered_pic_neg_raw[[p]][t])
    
    movement <- unwrap_move(filtered_reaction_type_raw[[p]][t])
    
    # Which picture was framed this trial? (frame highlights the side it appears on)
    framed_valence <- if (is.na(frame_side) || is.na(positive_picture_side)) NA_character_
    else if (frame_side == positive_picture_side) "positive" else "negative"
    
    # Choose the actually relevant picture for this trial from valenceLooked:
    picture_id_old <- dplyr::case_when(
      valence == "positive" ~ picture_pos_ID,
      valence == "negative" ~ picture_neg_ID,
      TRUE ~ NA_integer_
    )
    
    # choose the picture that was FRAMED (instruction-relevant)
    picture_id <- dplyr::case_when(
      frame_side == positive_picture_side ~ picture_pos_ID,  # framed = positive
      frame_side != positive_picture_side ~ picture_neg_ID,  # framed = negative
      TRUE ~ NA_integer_
    )
    
    # Instructed action from color
    instructed_action <- dplyr::case_when(
      frame_color == "blue"   ~ "pull",
      frame_color == "orange" ~ "push",
      TRUE ~ NA_character_
    )
    
    # Congruence of instruction w.r.t. framed picture
    instructed_congruence <- dplyr::case_when(
      framed_valence == "positive" & instructed_action == "pull" ~ "congruent",
      framed_valence == "negative" & instructed_action == "push" ~ "congruent",
      !is.na(framed_valence) & !is.na(instructed_action) ~ "incongruent",
      TRUE ~ NA_character_
    )
    
    accuracy <-  dplyr::case_when(instructed_action == "pull" & movement == "pull" ~ "TRUE", 
                                  instructed_action == "push" & movement == "push" ~ "TRUE",
                                  instructed_action == "pull" & movement == "push" ~ "FALSE",
                                  instructed_action == "push" & movement == "pull" ~"FALSE",
                                  TRUE ~ NA_character_ )
    
    
    rows[[length(rows) + 1]] <- data.frame(
      participant = p,
      trial = t,
      block = ifelse(t <= 48, 1L, 2L),
      RT_sec, RT_ms,
      frame_time, picture_time, fixation_cross_time,
      valence, movement,
      frame_color, frame_side, positive_picture_side,
      picture_pos_ID, picture_neg_ID, picture_id, picture_id_old,
      framed_valence, instructed_action, instructed_congruence,accuracy,
      stringsAsFactors = FALSE
    )
  }
}

df_simple <- dplyr::bind_rows(rows)

# quick peek
dplyr::glimpse(df_simple)
head(df_simple, 20)


#================= Preprocessing steps=========================================


# =========================
# CONFIG
# =========================
win_sd <- 2.0
nP     <- max(df_simple$participant, na.rm = TRUE)
p80    <- sort(unique(c(1:3, (nP-3):nP)))   # first 3 + last 4 have 80 trials
denom  <- tibble(participant = sort(unique(df_simple$participant)),
                 expected_trials = ifelse(participant %in% p80, 80L, 96L))

# =========================
# Helper: instructed action
# =========================
add_instr <- function(df) {
  df %>%
    mutate(
      instructed_action = case_when(
        frame_color == "blue"   ~ "pull",
        frame_color == "orange" ~ "push",
        TRUE ~ NA_character_
      ),
      incorrect = case_when(
        is.na(instructed_action) | is.na(movement) ~ NA,
        instructed_action == movement               ~ FALSE,
        TRUE                                        ~ TRUE
      )
    )
}

# =========================
# Stage A: ORIGINAL (df_simple)
# =========================
A <- df_simple %>% add_instr()

# accuracy before RT exclusion 
acc_before <- A %>%
  summarise(accuracy_pct = 100 * mean(incorrect == FALSE, na.rm = TRUE)) %>%
  pull(accuracy_pct)

# accuracy among trials  with a known correctness label 
acc_before_known <- A %>%
  filter(incorrect %in% c(TRUE, FALSE)) %>%
  summarise(accuracy_pct = 100 * mean(!incorrect)) %>%
  pull(accuracy_pct)

# Per-participant baseline tallies
A_by_p <- A %>%
  group_by(participant) %>%
  summarise(
    available_trials = n(),
    RT_nonNA         = sum(!is.na(RT_ms)),
    correct_trials   = sum(incorrect == FALSE, na.rm = TRUE),  # regardless of RT NA
    .groups = "drop"
  ) %>%
  left_join(denom, by = "participant") %>%
  mutate(
    pct_vs_expected = 100 * RT_nonNA / expected_trials,
    pct_vs_available = 100 * RT_nonNA / pmax(available_trials, 1)
  )

# Who has <90% of expected at baseline (just highlight, do NOT remove)
A_under90_ids <- A_by_p %>% filter(pct_vs_expected < 90) %>% pull(participant)

# =========================
# Stage B: AFTER incorrect->NA  AND  RT<150ms->NA
# =========================
B <- A %>%
  mutate(
    RT_ms_after_incorrect = ifelse(incorrect == TRUE, NA_real_, RT_ms),
    lt150_flag = !is.na(RT_ms_after_incorrect) & RT_ms_after_incorrect < 150,
    RT_ms_ready = ifelse(lt150_flag, NA_real_, RT_ms_after_incorrect)
  )

B_by_p <- B %>%
  group_by(participant) %>%
  summarise(
    valid_after_invalid_150 = sum(!is.na(RT_ms_ready)),
    incorrect_n             = sum(incorrect == TRUE, na.rm = TRUE),
    lt150_n                 = sum(lt150_flag == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(denom, by = "participant") %>%
  mutate(
    perf_pct = 100 * valid_after_invalid_150 / expected_trials
  )

# Accuracy after invalid + <150 removed (on remaining analyzable trials)
acc_after <- B %>%
  filter(incorrect %in% c(TRUE, FALSE)) %>%            # keep trials with known correctness
  summarise(accuracy_pct = 100 * mean(!incorrect)) %>%
  pull(accuracy_pct)


# Participants below 90% performance NOW (highlight list & count)
B_under90_ids <- B_by_p %>% filter(perf_pct < 90) %>% pull(participant)

# =========================
# Stage C: APPLY 90% REMOVAL (for analysis)
# =========================
keep_ids <- setdiff(unique(df_simple$participant), B_under90_ids)

C <- B %>% filter(participant %in% keep_ids)

# =========================
# Stage D: WINSORIZE (no trimming)
# =========================
D <- C %>%
  group_by(participant) %>%
  mutate(
    mu  = mean(RT_ms_ready, na.rm = TRUE),
    sdv = sd(RT_ms_ready,  na.rm = TRUE),
    upper_win   = mu + win_sd * sdv,
    RT_ms_final = ifelse(!is.na(RT_ms_ready), pmin(RT_ms_ready, upper_win), NA_real_),
    was_capped  = !is.na(RT_ms_ready) & !is.na(RT_ms_final) & RT_ms_final < RT_ms_ready
  ) %>%
  ungroup() %>%
  select(-mu, -sdv, -upper_win)
################################
# Compute the global thresholds on the post-Stage-C analyzable RT
global_mu <- mean(C$RT_ms_ready, na.rm = TRUE)
global_sd <- sd(C$RT_ms_ready,   na.rm = TRUE)
upper_win_global <- global_mu + win_sd * global_sd
# (Lower tail already handled by the RT<150ms rule in Stage B)

D_global <- C %>%
  mutate(
    RT_ms_final = ifelse(!is.na(RT_ms_ready),
                         pmin(RT_ms_ready, upper_win_global),
                         NA_real_),
    was_capped  = !is.na(RT_ms_ready) & RT_ms_ready > upper_win_global
  ) %>%
  select(-incorrect, -lt150_flag)  
#===========================
# Stage D.2: Trimming for later comparison- not used 
#=============================
D_2 <- C %>%
  group_by(participant) %>%
  mutate(
    mu  = mean(RT_ms_ready, na.rm = TRUE),
    sdv = sd(RT_ms_ready,  na.rm = TRUE),
    upper_trim = mu + 2.0 * sdv,  # Trim at 2 SD
    RT_ms_clean = ifelse(RT_ms_ready > upper_trim, NA_real_, RT_ms_ready)
  ) %>%
  ungroup() %>%
  filter(!is.na(RT_ms_clean))  # Remove NA values after trimming
# =========================
# SUMMARY BLOCK (printed once)
# =========================
cat("\n================ OVERVIEW (Participants vs Trials) ================\n")
cat("Participants total:", length(unique(df_simple$participant)), "\n")
cat("Participants with 80 expected trials:", length(p80), "(IDs:", paste(p80, collapse=", "), ")\n\n")

# 1) How many trials originally in df_simple
cat("1) ORIGINAL df_simple:\n")
cat("   Total trials (rows):", nrow(A), "\n")
cat("   Trials with RT (non-NA):", sum(!is.na(A$RT_ms)), "\n")
cat("   Mean RT (ms) [raw RT_ms, non-NA]:", round(mean(A$RT_ms, na.rm = TRUE), 2),
    " | SD:", round(sd(A$RT_ms, na.rm = TRUE), 2), "\n")
cat("   Participants <90% of EXPECTED at baseline (highlight only): ",
    ifelse(length(A_under90_ids)==0, "none", paste(sort(unique(A_under90_ids)), collapse=", ")), "\n\n")

cat("   Accuracy BEFORE exclusions:", round(acc_before, 2), "%\n")


# 2) After incorrect + <150ms
cat("2) AFTER incorrect->NA and <150ms->NA:\n")
cat("   Trials remaining (non-NA RT_ms_ready):", sum(!is.na(B$RT_ms_ready)), "of", nrow(B), "rows\n")
cat("   Incorrect flagged (global):", sum(B$incorrect == TRUE, na.rm = TRUE), "\n")
cat("   <150ms flagged (global):", sum(B$lt150_flag == TRUE, na.rm = TRUE), "\n")
cat("   Accuracy on remaining analyzable trials (% correct):", round(acc_after, 2), "%\n")
cat("   Participants <90% of EXPECTED now (highlight only): ",
    ifelse(length(B_under90_ids)==0, "none", paste(sort(unique(B_under90_ids)), collapse=", ")), "\n\n")
cat("2) AFTER incorrect->NA and <150ms->NA:\n")

# ===== Global counts ============================================================
n_rows_total   <- nrow(B)
n_rt_nonNA_raw <- sum(!is.na(B$RT_ms), na.rm = TRUE)  # before either exclusion

removed_incorrect <- sum(B$incorrect == TRUE, na.rm = TRUE)
# note: lt150_flag is computed only on trials that survived incorrect step
removed_lt150     <- sum(B$lt150_flag == TRUE, na.rm = TRUE)

removed_total     <- removed_incorrect + removed_lt150
remaining_trials  <- sum(!is.na(B$RT_ms_ready), na.rm = TRUE)

cat("   Total rows (all trials): ", n_rows_total, "\n", sep = "")
cat("   RT non-NA before exclusions: ", n_rt_nonNA_raw, "\n", sep = "")
cat("   Removed due to INCORRECT: ", removed_incorrect, "\n", sep = "")
cat("   Removed due to RT < 150 ms: ", removed_lt150, "\n", sep = "")
cat("   Total removed (incorrect + <150): ", removed_total, "\n", sep = "")
cat("   Trials remaining (non-NA RT_ms_ready): ", remaining_trials, " of ", n_rows_total, "\n", sep = "")


# 3) After 90% removal (participants kept)
cat("3) AFTER 90% REMOVAL (participants kept for analysis):\n")
cat("   Kept participants:", length(keep_ids), " | Removed:", length(B_under90_ids), "\n")
cat("   Trials (rows) before:", nrow(B), " | after:", nrow(C), "\n")
cat("   Mean RT (ms) [RT_ms_ready, non-NA]:", round(mean(C$RT_ms_ready, na.rm = TRUE), 2),
    " | SD:", round(sd(C$RT_ms_ready, na.rm = TRUE), 2), "\n\n")

# 4) Winsorization change
cat("4) WINSORIZATION (cap at mean +", win_sd, "SD):\n")
cat("   Winsor-capped trials (global):", sum(D_global$was_capped, na.rm = TRUE), "\n")
cat("   Mean RT (ms) BEFORE winsor [RT_ms_ready]:", round(mean(C$RT_ms_ready, na.rm = TRUE), 2),
    " | AFTER winsor [RT_ms_final]:", round(mean(D_global$RT_ms_final, na.rm = TRUE), 2), "\n")
cat("   SD RT (ms) BEFORE winsor [RT_ms_ready]:", round(sd(C$RT_ms_ready, na.rm = TRUE), 2),
    " | AFTER winsor [RT_ms_final]:", round(sd(D_global$RT_ms_final, na.rm = TRUE), 2), "\n\n")

# 5) Overall stats for analysis (final)
cat("5) OVERALL (FINAL, post-90% + winsor):\n")
cat("   Final trials (non-NA RT_ms_final):", sum(!is.na(D_global$RT_ms_final)), "of", nrow(D_global), "rows\n")
cat("   Mean RT (ms):", round(mean(D_global$RT_ms_final, na.rm = TRUE), 2),
    " | SD:", round(sd(D_global$RT_ms_final, na.rm = TRUE), 2), "\n")
cat("   Participants in final set:", length(unique(D_global$participant)), "\n")
cat("====================================================================\n\n")

analysis_df <- D_global %>%
  mutate(
    RT_log10 = ifelse(!is.na(RT_ms_final) & RT_ms_final > 0, log10(RT_ms_final), NA_real_),
    block4 = case_when(
      instructed_congruence == "congruent"   & framed_valence == "positive" ~ "CP",
      instructed_congruence == "congruent"   & framed_valence == "negative" ~ "CN",
      instructed_congruence == "incongruent" & framed_valence == "positive" ~ "IP",
      instructed_congruence == "incongruent" & framed_valence == "negative" ~ "IN",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    participant, trial, block,
    valence = framed_valence, congruence = instructed_congruence , movement,
    block4, RT_ms = RT_ms_final, RT_log10,
    frame_time, frame_side, frame_color, picture_time, accuracy, picture_id, picture_neg_ID, picture_pos_ID
  )

# A compact per-participant table to inspect:
final_perf <- B_by_p %>%
  transmute(
    participant,
    expected_trials,
    valid_after_invalid_150,
    perf_pct = round(perf_pct, 2),
    under90 = participant %in% B_under90_ids
  )
# View(final_perf) # in RStudio; or:
# write.csv(final_perf, "participant_performance_after_invalid_150.csv", row.names = FALSE)

readr::write_csv(analysis_df, "SET/YOUR/PATH/experiment_1_preprocessing.csv")




#===============================================================================
#==   Checking which Preprocessing step for outliers works best library(lme4)  =
# Prepare the three datasets: Raw, Trimmed, and Winsorized              ========
# Standardize column names across datasets                              ========
# Ensure column consistency across datasets                             ========
#===============================================================================

# For df_raw (C dataset), assume it already has the correct column names
df_raw <- C %>%
  mutate(RT_log = log(RT_ms_ready)) %>%
  rename(
    congruence = instructed_congruence  # Make sure congruence is consistent
  )

# For D_2 dataset, ensure column names match those in df_raw (C dataset)
df_trim_mod <- D_2 %>%
  rename(
    RT_ms_ready = RT_ms_clean,         # Make sure RT_ms_ready is named the same
    congruence = instructed_congruence, # Ensure congruence is consistent
    RT_log = RT_ms_clean                # Calculate RT_log
  ) %>%
  mutate(RT_log = log(RT_ms_ready))

# For D_global dataset, ensure column names match those in df_raw (C dataset)
df_win_mod <- D_global %>%
  rename(
    RT_ms_ready = RT_ms_final,        # Make sure RT_ms_ready is named the same
    congruence = instructed_congruence, # Ensure congruence is consistent
    RT_log = RT_ms_final               # Calculate RT_log
  ) %>%
  mutate(RT_log = log(RT_ms_ready))
# Model formula (mixed model with random intercept for participant)
model_formula <- RT_log ~ valence * congruence + (1 | participant)

fit_model <- function(dat) {
  dat_use <- dat %>%
    filter(!is.na(RT_log), !is.na(congruence), !is.na(valence), !is.na(participant))  # Remove rows with any NA in key variables
  lmer(model_formula, data = dat_use, REML = TRUE)
}

# Fit models for Raw, Trimmed, and Winsorized data
mod_raw  <- fit_model(df_raw)
mod_trim <- fit_model(df_trim_mod)
mod_win  <- fit_model(df_win_mod)

# Gather diagnostics
get_diag_df <- function(mod, label) {
  tibble(
    .fitted = fitted(mod),
    .resid  = resid(mod),
    .stdres = scale(resid(mod))[,1],  # Standardized residuals
    set = label
  )
}

# Combine diagnostics from all models
diag_all <- bind_rows(
  get_diag_df(mod_raw,  "RAW"),
  get_diag_df(mod_trim, "TRIMMED (±2.0 SD removed)"),
  get_diag_df(mod_win,  "WINSORIZED (±2.0 SD capped)")
)

# Residuals vs Fitted Plot
p_resid <- ggplot(diag_all, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.35) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ set, scales = "free") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

# QQ Plot of Standardized Residuals
p_qq <- ggplot(diag_all, aes(sample = .stdres)) +
  stat_qq(alpha = 0.35) +
  stat_qq_line() +
  facet_wrap(~ set, scales = "free") +
  labs(title = "QQ Plot of Standardized Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles"
  )+ theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    strip.text   = element_text(size = 20),   # facet labels
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5)
  )


# Print the plots
print(p_resid)
print(p_qq)


#plotting them together for the tehsis 
#=Checking which Preprocessing step for outliers works best library(lme4)

## ===================== 1) Build four datasets on the same schema =====================
df_raw <- C %>%
  rename(
    congruence = instructed_congruence  # Make sure congruence is consistent
  )%>%
  transmute(participant, valence, congruence, RT = RT_ms_ready)

df_trim <- D_2 %>%
  rename(
    congruence = instructed_congruence  # Make sure congruence is consistent
  )%>%
  transmute(participant, valence, congruence, RT = RT_ms_clean)

df_win2 <- D_global %>%
  rename(
    congruence = instructed_congruence  # Make sure congruence is consistent
  )%>%
  # your existing ±2 SD winsorized result
  transmute(participant, valence, congruence, RT = RT_ms_final)

# Make a fresh ±2.5 SD winsorized version from the raw C
df_win2_5 <- C %>%
  rename(
    congruence = instructed_congruence  # Make sure congruence is consistent
  )%>%
  mutate(mu = mean(RT_ms_ready, na.rm = TRUE),
         sdv = sd(RT_ms_ready, na.rm = TRUE),
         RT_wins_2_5 = pmin(pmax(RT_ms_ready, mu - 2.5*sdv), mu + 2.5*sdv)) %>%
  transmute(participant, valence, congruence, RT = RT_wins_2_5)

## ===================== 2) Add log(RT) AFTER outlier handling =================
add_log <- function(df) df %>% mutate(RT_log = log(RT))

df_raw     <- add_log(df_raw)
df_trim    <- add_log(df_trim)
df_win2    <- add_log(df_win2)
df_win2_5  <- add_log(df_win2_5)

## ===================== 3) Fit the same mixed model to each ===================
model_formula <- RT_log ~ valence * congruence + (1 | participant)

fit_model <- function(dat) {
  dat %>%
    filter(!is.na(RT_log), !is.na(valence), !is.na(congruence), !is.na(participant)) %>%
    lmer(model_formula, data = ., REML = TRUE)
}

mod_raw    <- fit_model(df_raw)
mod_trim   <- fit_model(df_trim)
mod_win2   <- fit_model(df_win2)
mod_win2_5 <- fit_model(df_win2_5)

## ====================  4) Collect diagnostics ================================
get_diag_df <- function(mod, label){
  tibble(
    .fitted = fitted(mod),
    .resid  = resid(mod),
    .stdres = as.numeric(scale(resid(mod))),
    set     = label
  )
}

diag_all <- dplyr::bind_rows(
  get_diag_df(mod_raw,    "RAW"),
  get_diag_df(mod_trim,   "TRIMMED (±2 SD removed)"),
  get_diag_df(mod_win2,   "WINSORIZED (±2 SD capped)"),
  get_diag_df(mod_win2_5, "WINSORIZED (±2.5 SD capped)")
) %>%
  mutate(set = factor(set, levels = c("RAW",
                                      "TRIMMED (±2 SD removed)",
                                      "WINSORIZED (±2 SD capped)",
                                      "WINSORIZED (±2.5 SD capped)")))

## ================   5) Plots (compact panels) =====================
# Residuals vs Fitted
p_resid <- ggplot(diag_all, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.35, size = 1.0) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9, linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ set, ncol = 2, scales = "free_y") +
  labs(#title = "Residuals vs Fitted — Implicit Task",
    x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title   = element_text(size = 20, face = "bold"),
    strip.text   = element_text(face = "bold")
  )

# QQ plot
p_qq <- ggplot(diag_all, aes(sample = .stdres)) +
  stat_qq(alpha = 0.35, size = 1.0) +
  stat_qq_line(linewidth = 1.0) +
  facet_wrap(~ set, ncol = 2, scales = "free") +
  labs(#title = "QQ Plot of Standardized Residuals — Implicit Task",
    x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title   = element_text(size = 20, face = "bold"),
    strip.text   = element_text(face = "bold")
  )

print(p_resid)
print(p_qq)



#===============================================================================
#====================Preprocessing process plotted =============================
#===============================================================================


# ==================== helpers ===============================================
to_num <- function(x) suppressWarnings(as.numeric(unlist(x)))

# Pull vectors (ms) safely -> convert to seconds
rt1_s <- to_num(B$RT_ms_after_incorrect) / 1000    # Step 1 (seconds)
rt2_s <- to_num(C$RT_ms_ready) / 1000              # Step 2 (seconds)
rt3_s <- to_num(D_global$RT_ms_final) / 1000       # Step 3 (seconds)
rt4_log <- log10(rt3_s)                            # Step 4 (log10 seconds)

# Clean NAs / non-finite
rt1_s <- rt1_s[is.finite(rt1_s)]
rt2_s <- rt2_s[is.finite(rt2_s)]
rt4_log <- rt4_log[is.finite(rt4_log)]

# ===== MAIN PANEL: overlapping histograms for Step 1 & Step 2 =================
df_main <- rbind(
  data.frame(step = "Step 1: After incorrect->NA  AND  RT<150ms->NA", RT_s = rt1_s),
  data.frame(step = "Step 2: Apply 90% removal", RT_s = rt2_s), 
  data.frame(step = "Step 3: Winsorisation", RT_s = rt3_s)
)


main_plot <- ggplot(df_main, aes(x = RT_s, fill = step)) +
  geom_histogram(position = "identity", alpha = 0.45, bins = 100, color = NA) +
  # optional density overlay scaled to counts (comment out if you don't want it)
  geom_density(aes(y = after_stat(count)), alpha = 0.15, adjust = 1) +
  scale_fill_manual(values = c("#1f77b4", "#17becf", "#ff7f0e")) + # Added 3rd color for Step 3
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.25)) +
  labs(
    #title = "Distribution of Behavioural Reaction Times Across Preprocessing Steps",
    x = "Reaction time (s)",
    y = "Frequency",
    fill = NULL
  ) +
  theme_minimal(base_size = 20
  ) +theme(legend.position = "bottom", 
           plot.title = element_text(size = 20),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 20), 
           legend.text = element_text (size = 18)
  )


# ==== INSET: histogram for Step 4 (log10 seconds) =============================
df_inset <- data.frame(RT_log10_s = rt4_log)

inset_plot <- ggplot(df_inset, aes(x = RT_log10_s)) +
  geom_histogram(bins = 35, fill = "#20b2aa", alpha = 0.7, color = NA) +
  # KDE on log-scale histogram (optional)
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(-0.8, 0.8)) +
  labs(
    x = expression(log[10]*"(Reaction time in seconds)"),
    y = "Frequency",
    title = "Step 4: log10 transform (of winsorised RTs)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )

# =====  COMBINE: draw inset inside main ========================================
# Position uses relative canvas coordinates: (x, y, width, height)
combined <- ggdraw(main_plot) +
  draw_plot(inset_plot, x = 0.52, y = 0.48, width = 0.43, height = 0.43)

combined

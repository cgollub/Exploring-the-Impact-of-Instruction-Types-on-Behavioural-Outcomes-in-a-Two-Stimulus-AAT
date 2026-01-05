#EXPLICIT TASK PREPROCESSING 
# 1. Aligns data
# 2. Looks into Eye-tracking data 
# 3. Preprocesses Data according to Steps mentioned in README


#================= Downloading the libraries ===================================
library(R.matlab)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(cowplot)
library(zoo)
library(lme4)
library(broom.mixed)




#=================== Setting the pathway  where data is stored  ================
setwd("Set/Your/Path//Experiment 2")

# ==================  Load Data ===============================================#

reaction_time_raw    <- readMat("behavioralReactionTime.mat")[[1]]      # 146 x 88
valence_looked_raw   <- readMat("valenceLooked.mat")[[1]]               # 146 x  88
picture_time_raw     <- readMat("pictureTimes.mat")[[1]]                # list len 146, each numeric[88]: [[146]][[1]][[88]]
trial_time_raw       <- readMat("trialTime.mat")[[1]]                   # list len 146, each list of 88 trials
xPosition_raw        <- readMat("xPositionData.mat")[[1]]               # 1 x 73 cell; each cell list of 88 trials
frame_time_raw       <- readMat("frameTimes.mat")[[1]]
frame_side_raw       <- readMat("frameSide.mat")[[1]] 
picture_sequence_raw <- readMat("pictureSequence.mat")[[1]] 
fixation_cross_time_raw <- readMat("fixationCrossTime.mat")[[1]] 

# In Matlab: loaded block_order.mat and reactionType.mat files,
#access the variables within them, convert them from string arrays
# to cell arrays (for compatibility with R),
#and save them as separate .mat files in v6 format 
#for compatibility with external tools like R.
#Additionally, we also convert them back to string arrays 
#and save those versions in separate .mat files.
#loaded here 
block_order_raw <-readMat("NEWEST_blockOrder_converted.mat") [[1]]
reaction_type_raw <- readMat("NEWEST_reactionType_converted.mat")[[1]]

block_order <- unlist(lapply(block_order_raw, function(x) x[[1]]))  # Flatten the list into a vector
reactionType <- unlist(lapply(reaction_type_raw, function(x) x[[1]]))  # Flatten the list into a vector

#=======  reassigning reaction type  to the corresponding participants =========
# ===  create an empty list to store filtered reaction types per participant ==
#===============================================================================
combined_reaction_type_raw <- list()

# Loop to assign the unlisted data to participants (1 to 73) 
for (participant in seq_len(73)) {  # There are 73 participants
  start_idx <- (participant - 1) * 176 + 1
  end_idx <- participant * 176
  combined_reaction_type_raw[[participant]] <- reactionType[start_idx:end_idx]
}
########
# Initialize the list for storing combined block_order for each participant
combined_block_order_raw <- list()

# Combine the data for each participant (sessions 1 and 2 together for block_order)
for (participant in seq_len(73)) {  # There are 73 participants
  # Get the block order for this participant (session 1 and session 2)
  block1_order <- block_order[ 2 * participant - 1]  # Session 1 (first block)
  block2_order <- block_order[ 2 * participant ]  # Session 2 (second block)
  
  # Assign block order for each session (A or B) to the corresponding trials
  # The first 88 trials of block 1 will have block1_order, and the first 88 trials of block 2 will have block2_order
  combined_block_order_raw[[participant]] <- c(rep(block1_order, 88), rep(block2_order, 88))
}

# === Identify Invalid Trials (NaN in reaction_time, valence, or frame_time) ===
valid_trials <- list()

# Loop through each session (i.e., each column in the matrix for reaction_time_raw)
for (session in 1:ncol(reaction_time_raw)) {
  # Identify invalid trials where reaction_time, valence, or frame_time is NA
  invalid_trials <- is.na(reaction_time_raw[, session]) | 
    is.na(valence_looked_raw[, session]) | 
    is.na(frame_time_raw[, session])
  
  # Store the valid trials (non-NA)
  valid_trials[[session]] <- !invalid_trials  # TRUE for valid trials
}

# === Combine Blocks (Sessions 1 and 2) for Each Participant ===
combined_reaction_time_raw <- list()
combined_valence_looked_raw <- list()
combined_frame_time_raw <- list()
combined_frame_side_raw <- list()

combined_picture_time_raw <- list()
combined_trial_time_raw <- list()
combined_xPosition_raw <-list()

# Combine the data for each participant (sessions 1 and 2 together)
for (participant in seq_len(73)) {  # There are 65 participants
  # Get the data for the two blocks (sessions 2*participant=1 and 2*participant)
  block1_session <- 2 * participant - 1
  block2_session <- 2 * participant
  
  # Combine blocks for each participant by appending the data
  combined_reaction_time_raw[[participant]] <- c(reaction_time_raw[, block1_session], reaction_time_raw[, block2_session])
  combined_valence_looked_raw[[participant]] <- c(valence_looked_raw[, block1_session], valence_looked_raw[, block2_session])
  combined_frame_time_raw[[participant]] <- c(frame_time_raw[[block1_session]][[1]], frame_time_raw[[block2_session]][[1]])
  combined_frame_side_raw[[participant]] <- c(frame_side_raw[,block1_session], frame_side_raw[,block2_session])
  # Combine the list-like data (picture_time_raw, trial_time_raw, xPosition_raw)
  combined_picture_time_raw[[participant]] <- c(picture_time_raw[[block1_session]][[1]], picture_time_raw[[block2_session]][[1]])
  combined_trial_time_raw[[participant]] <- c(trial_time_raw[[block1_session]][[1]], trial_time_raw[[block2_session]][[1]])
  combined_xPosition_raw[[participant]] <- c(xPosition_raw[[block1_session]][[1]], xPosition_raw[[block2_session]][[1]])
  
}
# ============== Recalculate Valid Trials for Combined Blocks ==================

valid_trials_combined <- list()

for (participant in seq_len(73)) {
  # Combine reaction_time_raw, valence_looked_raw, frame_time_raw for each participant
  combined_reaction_time <- combined_reaction_time_raw[[participant]]
  combined_valence_looked <- combined_valence_looked_raw[[participant]]
  combined_frame_time <- combined_frame_time_raw[[participant]]
  combined_frame_side <- combined_frame_side_raw[[participant]]
  
  # Identify invalid trials where reaction_time, valence, or frame_time is NA
  invalid_trials_combined <- is.na(combined_reaction_time) | 
    is.na(combined_valence_looked) | 
    is.na(combined_frame_time)
  
  # Store the valid trials (non-NA) for the combined data
  valid_trials_combined[[participant]] <- !invalid_trials_combined  # TRUE for valid trials
}


#========= Build per=participant left/right picture ID vectors =================

combined_left_id_raw  <- vector("list", 73)
combined_right_id_raw <- vector("list", 73)

for (participant in seq_len(73)) {
  s1 <- 2 * participant - 1
  s2 <- 2 * participant
  
  left_s1  <- picture_sequence_raw[, 1, s1]
  right_s1 <- picture_sequence_raw[, 2, s1]
  left_s2  <- picture_sequence_raw[, 1, s2]
  right_s2 <- picture_sequence_raw[, 2, s2]
  
  combined_left_id_raw[[participant]]  <- c(left_s1,  left_s2)
  combined_right_id_raw[[participant]] <- c(right_s1, right_s2)
}

#=====================================================================================================
#========== Next Steps only needed if eye data of interest too                  =====================
## ====== BUILD FILTERED LISTS  for plotting eye=data (align-within-session, then concatenate) ======
#=====================================================================================================
# Helper: extract + align ONE session for participant p, session index s
extract_session_aligned <- function(p, s,
                                    reaction_time_raw, valence_looked_raw,
                                    frame_time_raw, picture_time_raw,
                                    trial_time_raw, xPosition_raw,
                                    combined_reaction_type_raw, combined_block_order_raw,
                                    picture_sequence_raw, frame_side_raw) {
  
  rt <- reaction_time_raw[, s]
  vl <- valence_looked_raw[, s]
  ft <- frame_time_raw[[s]][[1]]
  pt <- picture_time_raw[[s]][[1]]
  
  n <- min(length(rt), length(vl), length(ft), length(pt), 88L)
  rt <- rt[1:n]; vl <- vl[1:n]; ft <- ft[1:n]; pt <- pt[1:n]
  
  left_ids  <- picture_sequence_raw[, 1, s][1:n]
  right_ids <- picture_sequence_raw[, 2, s][1:n]
  fs        <- frame_side_raw[, s][1:n]
  
  mask <- !is.na(rt) & !is.na(vl) & !is.na(ft) & !is.na(pt) & !is.na(fs)
  
  rt <- rt[mask]; vl <- vl[mask]; ft <- ft[mask]; pt <- pt[mask]
  left_ids  <- left_ids[mask]; right_ids <- right_ids[mask]; fs <- fs[mask]
  
  # trial time (list) and xPosition (list of vectors) aligned & masked
  tt_all <- trial_time_raw[[s]][[1]][1:n]
  tt <- tt_all[mask]
  
  subj <- p
  xp_cell <- xPosition_raw[[subj]][[1]]
  if (length(xp_cell) == 176) {
    xp_block <- if (s %% 2 == 1) xp_cell[1:88] else xp_cell[89:176]
    xp <- xp_block[1:n][mask]
  } else if (length(xp_cell) == 88) {
    if (s %% 2 == 1) {
      xp <- xp_cell[1:n][mask]
    } else {
      warning(sprintf("Subject %d has only 88 xPosition trials; filling session 2 with NA.", subj))
      xp <- replicate(sum(mask), list(NA), simplify = FALSE)
    }
  } else {
    warning(sprintf("Subject %d xPosition has unexpected length=%d; filling NA.", subj, length(xp_cell)))
    xp <- replicate(sum(mask), list(NA), simplify = FALSE)
  }
  
  # reaction type & block order (176) → slice 88, then mask
  rt_all_types <- combined_reaction_type_raw[[p]]
  bo_all       <- combined_block_order_raw[[p]]
  if (length(rt_all_types) == 176 && length(bo_all) == 176) {
    if (s %% 2 == 1) {
      typ <- rt_all_types[1:88][1:n][mask]
      boo <- bo_all[1:88][1:n][mask]
    } else {
      typ <- rt_all_types[89:176][1:n][mask]
      boo <- bo_all[89:176][1:n][mask]
    }
  } else {
    half <- floor(min(length(rt_all_types), length(bo_all)) / 2)
    if (s %% 2 == 1) {
      typ <- rt_all_types[1:half][1:n][mask]
      boo <- bo_all[1:half][1:n][mask]
    } else {
      rng <- (half + 1):(2 * half)
      typ <- rt_all_types[rng][1:n][mask]
      boo <- bo_all[rng][1:n][mask]
    }
  }
  
  # Return everything that is later concatenate, with consistent names
  list(
    rt   = rt,
    vl   = vl,
    ft   = ft,
    pt   = pt,
    tt   = tt,          
    xp   = xp,         
    fs   = fs,
    lid  = left_ids,
    rid  = right_ids,
    rtype = typ,        
    bord  = boo
  )
}


# Build aligned, filtered trials for each participant by concatenating their two sessions
filtered_reaction_time_raw   <- vector("list", 73)
filtered_valence_looked_raw  <- vector("list", 73)
filtered_frame_time_raw      <- vector("list", 73)
filtered_picture_time_raw    <- vector("list", 73)
filtered_trial_time_raw      <- vector("list", 73)
filtered_xPosition_raw       <- vector("list", 73)
filtered_reaction_type_raw   <- vector("list", 73)
filtered_block_order_raw      <- vector("list", 73)

for (p in seq_len(73)) {
  s1 <- 2*p - 1
  s2 <- 2*p
  
  A <- extract_session_aligned(
    p, s1,
    reaction_time_raw, valence_looked_raw,
    frame_time_raw, picture_time_raw,
    trial_time_raw, xPosition_raw,
    combined_reaction_type_raw, combined_block_order_raw,
    picture_sequence_raw, frame_side_raw
  )
  
  B <- extract_session_aligned(
    p, s2,
    reaction_time_raw, valence_looked_raw,
    frame_time_raw, picture_time_raw,
    trial_time_raw, xPosition_raw,
    combined_reaction_type_raw, combined_block_order_raw,
    picture_sequence_raw, frame_side_raw
  )
  
  # Concatenate sessions in order
  filtered_reaction_time_raw[[p]]  <- c(A$rt,   B$rt)
  filtered_valence_looked_raw[[p]] <- c(A$vl,   B$vl)
  filtered_frame_time_raw[[p]]     <- c(A$ft,   B$ft)
  filtered_picture_time_raw[[p]]   <- c(A$pt,   B$pt)
  filtered_trial_time_raw[[p]]     <- c(A$tt,   B$tt)
  filtered_xPosition_raw[[p]]      <- c(A$xp,   B$xp)
  filtered_reaction_type_raw[[p]]  <- c(A$rtype, B$rtype)
  filtered_block_order_raw[[p]]    <- c(A$bord, B$bord)
}

#===============================================================================
##=========    Plotting based off of Experiment 1  =============================
#===============================================================================

plot_trial_eye_trace_new <- function(participant_id, trial_number,
                                     filtered_reaction_time_raw,
                                     filtered_valence_looked_raw,
                                     filtered_frame_time_raw,
                                     filtered_picture_time_raw,
                                     filtered_trial_time_raw,
                                     filtered_xPosition_raw,
                                     velocity_threshold = 5, dead_zone = 50, min_fix_dur_ms = 50,
                                     smooth_window = 5, show_blinks = TRUE){
  
  # Get the data for the given participant and trial number
  reaction_time <- filtered_reaction_time_raw[[participant_id]][trial_number]
  valence <- filtered_valence_looked_raw[[participant_id]][trial_number]
  frame_time <- filtered_frame_time_raw[[participant_id]][trial_number]
  picture_time <- filtered_picture_time_raw[[participant_id]][trial_number]
  
  # Check if the data exists and is valid
  if (is.na(picture_time) || is.na(frame_time) || is.na(reaction_time)) return(NULL)
  
  # Calculate reaction times and relative time
  RT_ms <- reaction_time * 1000
  reaction_time_absolute <- frame_time + RT_ms
  rt_relative <- reaction_time_absolute - picture_time
  
  # Extract time and position data
  timeVec <- unlist(filtered_trial_time_raw[[participant_id]][trial_number])  # Unlist the trial time data
  xPos <- unlist(filtered_xPosition_raw[[participant_id]][trial_number])  # Unlist the xPosition data
  
  # Align time
  aligned_time <- timeVec - picture_time
  
  # Remove NA values
  valid <- !is.na(xPos) & !is.na(aligned_time)
  xPos <- xPos[valid]
  aligned_time <- aligned_time[valid]
  
  # Check if enough data is available
  if (length(xPos) < 2) return(NULL)
  
  # Smooth the data
  #xPos <- zoo::rollmean(xPos, k = smooth_window, fill = NA)
  aligned_time <- aligned_time[1:length(xPos)]
  
  # Calculate velocity
  dx <- diff(xPos)
  dt <- diff(aligned_time)
  dt <- diff(aligned_time)
  sample_interval <- median(dt, na.rm = TRUE)
  
  # Then define both thresholds
  low_velocity_threshold <- 5
  min_fix_samples <- max(5, ceiling(min_fix_dur_ms / sample_interval))
  
  velocity <- dx / dt
  velocity_full <- c(NA, velocity)
  velocity_threshold <- mean(abs(velocity), na.rm = TRUE) + 0.5 * sd(abs(velocity), na.rm = TRUE)
  
  # Define thresholds and variables
  low_velocity_threshold <- 5
  min_saccade_samples <- 5  #potentially chnage to 7 or 8 for more robust detection 
  #min_fix_samples <- max(5, ceiling(min_fix_dur_ms / median(dt, na.rm = TRUE)))
  
  # Identify saccades
  valid_velocity <- aligned_time[-1] > 0 & abs(velocity) > velocity_threshold
  high_velocity_streaks <- zoo::rollapply(valid_velocity, width = min_saccade_samples,
                                          FUN = function(x) mean(x) > 0.8,
                                          fill = NA, align = "left")
  streak_idx <- which(high_velocity_streaks)
  
  if (length(streak_idx) > 0) {
    saccade_start_idx <- max(1, streak_idx[1] - floor(min_saccade_samples / 2))
    saccade_time_computed <- aligned_time[saccade_start_idx]
  }
  
  screen_center <- 960
  left_bounds <- c(200, 920)     # pixels
  right_bounds <- c(1000, 1720)  # pixels
  
  #sample_interval <- median(dt, na.rm = TRUE)
  start_time_fixation_window <- max(0, saccade_time_computed, na.rm = TRUE) #+ 10
  end_time_fixation_window <- rt_relative
  
  # Identify stable fixations
  screen_center <- 960
  # sample_interval <- median(dt, na.rm = TRUE)
  frame_relative_time <- frame_time - picture_time
  
  # Define analysis window
  valid_indices <- which(aligned_time > 0 & aligned_time < frame_relative_time)
  x_post <- xPos[valid_indices]
  t_post <- aligned_time[valid_indices]
  velocity_post <- velocity_full[valid_indices]
  
  
  # Determine side: left or right (outside dead zone only)
  is_left <- x_post < (screen_center - dead_zone)
  is_right <- x_post > (screen_center + dead_zone)
  
  # Check for continuous stable gaze on one side
  is_stable <- abs(velocity_post) < low_velocity_threshold
  left_fixation_mask <- is_stable & is_left
  right_fixation_mask <- is_stable & is_right
  
  # Detect stable fixations using run=length encoding
  fixation_time <- NA
  side_fixated <- NA
  
  detect_fixation_in_box <- function(xPos, tVec, box_bounds, min_samples = 5) {
    in_box <- xPos >= box_bounds[1] & xPos <= box_bounds[2] & 
      (xPos < (screen_center = dead_zone) | xPos > (screen_center + dead_zone))
    
    sustained <- zoo::rollapply(
      in_box, width = min_samples,
      FUN = function(x) mean(x) > 0.8,
      fill = NA, align = "left"
    )
    
    idx <- which(sustained)
    if (length(idx) > 0) {
      # Return the center of the stable fixation window
      fixation_start_idx <- idx[1]
      fixation_end_idx <- fixation_start_idx + min_samples - 1
      
      if (fixation_end_idx > length(tVec)) {
        fixation_end_idx <- length(tVec)
      }
      
      fixation_mid_idx <- round((fixation_start_idx + fixation_end_idx) / 2)
      fixation_time <- tVec[fixation_mid_idx]
      
      return(fixation_time)
    } else {
      return(NA)
    }
  }
  
  left_fix_time <- detect_fixation_in_box(x_post, t_post, left_bounds, min_fix_samples)
  right_fix_time <- detect_fixation_in_box(x_post, t_post, right_bounds, min_fix_samples)
  
  if (!is.na(left_fix_time) && !is.na(right_fix_time)) {
    fixation_time <- min(left_fix_time, right_fix_time)
    side_fixated <- ifelse(left_fix_time < right_fix_time, "left", "right")
  } else if (!is.na(left_fix_time)) {
    fixation_time <- left_fix_time
    side_fixated <- "left"
  } else if (!is.na(right_fix_time)) {
    fixation_time <- right_fix_time
    side_fixated <- "right"
  }
  

  # Plot the results
  plot(aligned_time, xPos, type = "l", col = "black", lwd = 2,
       xlab = "Time (ms, aligned to picture onset)", ylab = "X Position (in Pixels)", main = paste("P", participant_id, "- T", trial_number))
  lines(aligned_time, xPos, col = "blue4", lty = 3)
  abline(v = 0, col = "green", lty = 2)
  if (!is.na(frame_time)) abline(v = frame_time - picture_time, col = "darkorange", lty = 2)
  if (!is.na(saccade_time_computed)) abline(v = saccade_time_computed, col = "purple", lty = 2)
  if (!is.na(fixation_time)) abline(v = fixation_time, col = "brown3", lty = 2)
  if (!is.na(rt_relative)) abline(v = rt_relative, col = "seagreen", lty = 2)
  
  abline(h = screen_center, col = "black", lty = 3)
  abline(h = screen_center - dead_zone, col = "gray", lty = 2)
  abline(h = screen_center + dead_zone, col = "gray", lty = 2)
  
  legend("topright", legend = c("Raw", "Picture", "Frame", "Saccade", "Fixation", "Reaction"),
         col = c("black", "green", "darkorange", "purple", "brown3", "dodgerblue", "seagreen"),
         lty = c(1, 3, 2, 2, 2, 2, 2), cex = 0.8)
  
  cat("\n=== Trial Timeline Check ===\n")
  cat("Participant:", participant_id, "Trial:", trial_number, "\n")
  cat("  picture_time:", picture_time, "\n")
  cat("  frame_time:", frame_time, "\n")
  cat("  rt_relative:", rt_relative, "\n")
  cat("  saccade_time:", saccade_time_computed, "\n")
  cat("  fixation_time:", fixation_time, "\n")
  cat("reaction_time: ", round(RT_ms), "\n")
  cat("reaction_time to pic time: ", round(rt_relative), "\n")
  cat("reaction_time to frame time: ", round(reaction_time_absolute), "\n")
  cat("============================\n")
  
  
}

# Example: Plot for Participant 35, Trial 7
plot_trial_eye_trace_new(
  participant_id = 35,      #change number for participant
  trial_number = 7,         #change number for trial number
  filtered_reaction_time_raw = filtered_reaction_time_raw,
  filtered_valence_looked_raw = filtered_valence_looked_raw,
  filtered_frame_time_raw = filtered_frame_time_raw,
  filtered_picture_time_raw = filtered_picture_time_raw,
  filtered_trial_time_raw = filtered_trial_time_raw,
  filtered_xPosition_raw = filtered_xPosition_raw
)

#==================================================================================
#===     Continue here if Eye-data not of interest      ===========================
#==================================================================================
#====== creating a data frame with all the data for a better look at the data  ====
#=========== UNFILTERED WITH NA VALUES            =================================
#==================================================================================

# Initialize the list to store filtered data
combined_data_list <- list()

# Loop through each participant
for (p in 1:73) {
  # Get the total number of trials for this participant (after combining blocks)
  total_trials <- length(combined_reaction_time_raw[[p]])
  
  # Get the block order for the current participant (should be a vector)
  block_order <- combined_block_order_raw[[p]]
  
  for (t in 1:total_trials) {  # Each trial for participant p
    # Convert reaction time from seconds to milliseconds
    RT_sec <- combined_reaction_time_raw[[p]][t]
    RT_ms  <- if (!is.na(RT_sec)) RT_sec * 1000 else NA
    
    # Convert valence to readable labels (positive/negative)
    val_raw <- combined_valence_looked_raw[[p]][t]
    valence <- if (is.na(val_raw)) NA else if (val_raw == 1) "positive" else "negative"
    
    # Get the frame and picture times
    frame_time   <- combined_frame_time_raw[[p]][t]
    picture_time <- combined_picture_time_raw[[p]][t]
    
    # Inside your `for (p in 1:73) { ... for (t in 1:total_trials) { ... } }` loop:
    
    # Get side-specific picture IDs for this trial
    left_id  <- combined_left_id_raw[[p]][t]
    right_id <- combined_right_id_raw[[p]][t]
    
    # Get frame side for this trial (1=RIGHT, 0=LEFT)
    frame_side <- combined_frame_side_raw[[p]][t]
    
    # Choose the picture that was framed
    picture_id <- if (!is.na(frame_side)) {
      ifelse(frame_side == 1, right_id, left_id)
    } else {
      NA_integer_
    }
    
    
    # Get reaction type from the corresponding data
    reaction_type <- combined_reaction_type_raw[[p]][t]  # unchanged from original
    
    # Assign congruence based on block order and trial number within combined data
    if (block_order[t] == "A") {  # Check block order for each trial
      if (t <= 44) {
        congruence <- "congruent"
      } else if (t <= 88) {
        congruence <- "incongruent"
      } else if (t <= 132) {
        congruence <- "congruent"
      } else {
        congruence <- "incongruent"
      }
    } else { if (block_order[t] == "B"){
      if (t <= 44) {
        congruence <- "incongruent"
      } else if (t <= 88) {
        congruence <- "congruent"
      } else if (t <= 132) {
        congruence <- "incongruent"
      } else {
        congruence <- "congruent"
      }
    }
    }
    
    # Calculate accuracy based on the combination of congruence, valence, and reaction_type
    accuracy <- ifelse(
      (congruence == "congruent"   & valence == "positive" & reaction_type == "pull") |
        (congruence == "congruent"   & valence == "negative" & reaction_type == "push") |
        (congruence == "incongruent" & valence == "negative" & reaction_type == "pull") |
        (congruence == "incongruent" & valence == "positive" & reaction_type == "push"),
      "correct", "incorrect"
    )
    
    # Store the trial data in the filtered data list
    combined_data_list[[length(combined_data_list) + 1]] <- data.frame(
      participant = p,
      trial       = t,
      valence     = valence,
      RT_sec      = RT_sec,
      RT_ms       = RT_ms,
      accuracy    = accuracy,
      frame_time  = frame_time,
      picture_time= picture_time,
      block_order = block_order[t],  # Ensure correct block order per trial
      reaction_type = reaction_type,
      congruence  = congruence,
      frame_side   = frame_side,
      left_id      = left_id,
      right_id     = right_id,
      picture_id   = picture_id,
      stringsAsFactors = FALSE
    )
  }
}



# Combine the list of filtered data frames into a single data frame
combined_df_all <- dplyr::bind_rows(combined_data_list)

# View the first few rows of the filtered data
head(combined_df_all, n= 100)

filtered_combined_df_all <- combined_df_all%>% 
  filter(!is.na(RT_ms))
filtered_combined_df_all


#===============================================================================
#===     Preprocessing                                  ========================
#===============================================================================

# =========================
# CONFIG (Exp 2)
# =========================
n_subjects_claimed <- 73
expected_trials_per_participant <- 176L
win_sd <- 2

# Denominator table: everyone 176
participants_vec <- sort(unique(filtered_combined_df_all$participant))
denom <- tibble(
  participant = participants_vec,
  expected_trials = expected_trials_per_participant
)

# =========================
# Harmonize column names / values
# =========================
DF0 <- filtered_combined_df_all %>%
  mutate(
    picture_id    = as.integer(picture_id),
    valence       = tolower(as.character(valence)),
    congruence    = tolower(as.character(congruence)),
    accuracy      = tolower(as.character(accuracy)),
    reaction_type = tolower(as.character(reaction_type)),
    RT_ms         = RT_ms
  )

# Quick sanity notes
cat("Participants present:", length(participants_vec),
    "| Claimed:", n_subjects_claimed, "\n")
cat("Any participant with rows != ", expected_trials_per_participant, "?\n", sep = "")
print(
  DF0 %>% count(participant, name = "rows") %>%
    mutate(flag = rows != expected_trials_per_participant) %>%
    filter(flag)
)

# =========================
# Stage A: ORIGINAL
# =========================
A <- DF0 %>%
  mutate(
    incorrect = case_when(
      is.na(accuracy)           ~ NA,          # unknown
      accuracy == "incorrect"   ~ TRUE,
      accuracy == "correct"     ~ FALSE,
      TRUE ~ NA
    )
  )

A_by_p <- A %>%
  group_by(participant) %>%
  summarise(
    available_trials = n(),
    RT_nonNA         = sum(!is.na(RT_ms)),
    correct_trials   = sum(incorrect == FALSE, na.rm = TRUE),
    baseline_accuracy_pct = 100 * mean(incorrect == FALSE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(denom, by = "participant") %>%
  mutate(
    pct_vs_expected  = 100 * RT_nonNA / expected_trials,
    pct_vs_available = 100 * RT_nonNA / pmax(available_trials, 1)
  )

A_under90_ids <- A_by_p %>%
  filter(pct_vs_expected < 90) %>%
  pull(participant)

# =========================
# Stage B: incorrect->NA and RT<150ms->NA
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
  mutate(perf_pct = 100 * valid_after_invalid_150 / expected_trials)

acc_after <- B %>%                       # accuracy among analyzable trials
  filter(!is.na(RT_ms_ready)) %>%
  summarise(accuracy_pct = 100 * mean(incorrect == FALSE, na.rm = TRUE)) %>%
  pull(accuracy_pct)

B_under90_ids <- B_by_p %>%
  filter(perf_pct < 90) %>% pull(participant)

# =========================
# Stage C: apply 90% removal
# =========================
keep_ids <- setdiff(participants_vec, B_under90_ids)
C <- B %>% filter(participant %in% keep_ids)

# =========================
# Stage D: winsorize (no trimming)
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
############################
# Stage D.2: winsorization globally 
# =========================
# Stage D: winsorize globally (no per-subject grouping)
# One common cap at global mean + win_sd * global SD
# =========================

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
  select(-incorrect, -lt150_flag)  # optional: drop helper flags if you like

#===========================
# Stage D.2: Trimming for later comparison- not used 
#=============================
D_2 <- C %>%
  group_by(participant) %>%
  mutate(
    mu  = mean(RT_ms_ready, na.rm = TRUE),
    sdv = sd(RT_ms_ready,  na.rm = TRUE),
    upper_trim = mu + 2* sdv,  # Trim at 2 SD
    RT_ms_clean = ifelse(RT_ms_ready > upper_trim, NA_real_, RT_ms_ready)
  ) %>%
  ungroup() %>%
  filter(!is.na(RT_ms_clean))  # Remove NA values after trimming
# =========================
# SUMMARY (one block)
# =========================
cat("\n================ EXPERIMENT 2 SUMMARY ================\n")
cat("Participants present:", length(participants_vec), " | Expected:", n_subjects_claimed, "\n")
cat("Expected trials per participant:", expected_trials_per_participant, "\n\n")

cat("1) ORIGINAL (filtered_combined_df_all):\n")
cat("   Total trials (rows):", nrow(A), "\n")
cat("   Trials with RT (non-NA):", sum(!is.na(A$RT_ms)), "\n")
cat("   Mean RT (ms) [raw RT_ms]:", round(mean(A$RT_ms, na.rm = TRUE), 2),
    " | SD:", round(sd(A$RT_ms, na.rm = TRUE), 2), "\n")
cat("   Baseline accuracy (mean across participants):",
    round(mean(A_by_p$baseline_accuracy_pct, na.rm = TRUE), 2), "%\n")
cat("   <90% of expected at baseline (highlight only): ",
    ifelse(length(A_under90_ids)==0, "none", paste(sort(unique(A_under90_ids)), collapse=", ")), "\n\n")

cat("2) AFTER incorrect->NA and <150ms->NA:\n")
cat("   Trials remaining (non-NA RT_ms_ready):", sum(!is.na(B$RT_ms_ready)), "of", nrow(B), "\n")
cat("   Incorrect flagged (global):", sum(B$incorrect == TRUE, na.rm = TRUE), "\n")
cat("   <150ms flagged (global):", sum(B$lt150_flag == TRUE, na.rm = TRUE), "\n")
cat("   Accuracy on analyzable trials:", round(acc_after, 2), "%\n")
cat("   <90% of expected now (highlight only): ",
    ifelse(length(B_under90_ids)==0, "none", paste(sort(unique(B_under90_ids)), collapse=", ")), "\n\n")
cat("2) AFTER incorrect->NA and <150ms->NA:\n")

# --- Global counts ------------------------------------------------------------
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


cat("3) AFTER 90% removal:\n")
cat("   Kept participants:", length(keep_ids), " | Removed:", length(B_under90_ids), "\n")
cat("   Trial rows before:", nrow(B), " | after:", nrow(C), "\n")
cat("   Mean RT (ms) [RT_ms_ready]:", round(mean(C$RT_ms_ready, na.rm = TRUE), 2),
    " | SD:", round(sd(C$RT_ms_ready, na.rm = TRUE), 2), "\n\n")

cat("4) Winsorization (cap at mean +", win_sd, "SD):\n")
cat("   Winsor-capped trials :", sum(D$was_capped, na.rm = TRUE), "\n")
cat("   Winsor-capped trials (global):", sum(D_global$was_capped, na.rm = TRUE), "\n")

cat("   Mean RT BEFORE winsor:", round(mean(C$RT_ms_ready, na.rm = TRUE), 2),
    " | AFTER winsor:", round(mean(D$RT_ms_final, na.rm = TRUE), 2),
    " | AFTER winsor global:", round(mean(D_global$RT_ms_final, na.rm = TRUE), 2), "\n")

cat("   SD BEFORE winsor:", round(sd(C$RT_ms_ready, na.rm = TRUE), 2),
    " | AFTER winsor:", round(sd(D$RT_ms_final, na.rm = TRUE), 2), 
    " | AFTER winsor global:", round(sd(D_global$RT_ms_final, na.rm = TRUE), 2), "\n\n")

cat("5) OVERALL (final set):\n")
cat("   Final trials (non-NA RT_ms_final):", sum(!is.na(D$RT_ms_final)), "of", nrow(D), "\n")
cat("   Mean RT (ms):", round(mean(D$RT_ms_final, na.rm = TRUE), 2),
    " | SD:", round(sd(D$RT_ms_final, na.rm = TRUE), 2), "\n")
cat("   Participants in final set:", length(unique(D$participant)), "\n")
cat("=======================================================\n\n")

# =========================
# Final analysis frame
# =========================
analysis_df2 <- D_global %>%
  mutate(
    RT_log10 = ifelse(!is.na(RT_ms_final) & RT_ms_final > 0, log10(RT_ms_final), NA_real_),
    block4 = case_when(
      congruence == "congruent"   & valence == "positive" ~ "CP",
      congruence == "congruent"   & valence == "negative" ~ "CN",
      congruence == "incongruent" & valence == "positive" ~ "IP",
      congruence == "incongruent" & valence == "negative" ~ "IN",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    participant, trial, block_order,
    valence, congruence, reaction_type,
    block4, RT_ms = RT_ms_final, RT_log10,
    frame_time, frame_side, left_id, right_id, picture_time, accuracy, picture_id
  )

# Optional per-participant table you can export
final_perf <- B_by_p %>%
  transmute(
    participant,
    expected_trials,
    valid_after_invalid_150,
    perf_pct = round(perf_pct, 2),
    under90 = participant %in% B_under90_ids
  )

######saving the outcome 
# Save the preprocessed dataset for Experiment 2
readr::write_csv(analysis_df2, "SET/Your/Path//experiment_2_preprocessed.csv")

#===============================================================================
#====================Preprocessing process plotted =============================
#===============================================================================

# --- helpers -------------------------------------------------------------
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

# --- MAIN PANEL: overlapping histograms for Step 1 & Step 2 --------------
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

# --- INSET: histogram for Step 4 (log10 seconds) -------------------------
df_inset <- data.frame(RT_log10_s = rt4_log)

inset_plot <- ggplot(df_inset, aes(x = RT_log10_s)) +
  geom_histogram(bins = 35, fill = "#20b2aa", alpha = 0.7, color = NA) +
  # KDE on log-scale histogram (optional)
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(-0.8, 0.8)) +
  labs(
    x = expression(log[10]*"(Reaction time in seconds)"),
    y = "Frequency",
    title = "Step 4: log10 transform ",
    subtitle ="(of winsorised RTs)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )

# --- COMBINE: draw inset inside main -------------------------------------
# Position uses relative canvas coordinates: (x, y, width, height)
combined <- ggdraw(main_plot) +
  draw_plot(inset_plot, x = 0.59, y = 0.48, width = 0.43, height = 0.43)

combined

#==============================================================================
#== Checking which Preprocessing step for outliers works best library(lme4) ====
#==============================================================================

## ---------- 1) Build four datasets on the same schema ----------
df_raw <- C %>%
  transmute(participant, valence, congruence, RT = RT_ms_ready)

df_trim <- D_2 %>%
  transmute(participant, valence, congruence, RT = RT_ms_clean)

df_win2 <- D_global %>%  # your existing ±2 SD winsorized result
  transmute(participant, valence, congruence, RT = RT_ms_final)

# Make a fresh ±2.5 SD winsorized version from the raw C
df_win2_5 <- C %>%
  mutate(mu = mean(RT_ms_ready, na.rm = TRUE),
         sdv = sd(RT_ms_ready, na.rm = TRUE),
         RT_wins_2_5 = pmin(pmax(RT_ms_ready, mu - 2.5*sdv), mu + 2.5*sdv)) %>%
  transmute(participant, valence, congruence, RT = RT_wins_2_5)

## ---------- 2) Add log(RT) AFTER outlier handling ----------
add_log <- function(df) df %>% mutate(RT_log = log(RT))

df_raw     <- add_log(df_raw)
df_trim    <- add_log(df_trim)
df_win2    <- add_log(df_win2)
df_win2_5  <- add_log(df_win2_5)

## ---------- 3) Fit the same mixed model to each ----------
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

## ---------- 4) Collect diagnostics ----------
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

## ---------- 5) Plots (compact panels) ----------
# Residuals vs Fitted
p_resid <- ggplot(diag_all, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.35, size = 1.0) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9, linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ set, ncol = 2, scales = "free_y") +
  labs(#title = "Residuals vs Fitted — Explicit Task",
    x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title   = element_text(size = 14, face = "bold"),
    strip.text   = element_text(face = "bold")
  )

# QQ plot
p_qq <- ggplot(diag_all, aes(sample = .stdres)) +
  stat_qq(alpha = 0.35, size = 1.0) +
  stat_qq_line(linewidth = 1.0) +
  facet_wrap(~ set, ncol = 2, scales = "free") +
  labs(#title = "QQ Plot of Standardized Residuals — Explicit Task",
    x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title   = element_text(size = 20, face = "bold"),
    strip.text   = element_text(face = "bold")
  )

print(p_resid)
print(p_qq)



## (Optional) Save compact wide images:
# ggsave("explicit_resid_vs_fitted_all.png", p_resid, width = 12, height = 4, dpi = 300)
# ggsave("explicit_qq_all.png",            p_qq,    width = 12, height = 4, dpi = 300)


#===============================================================================
#======================  PLOTS FOR PREPROCESSING ===============================
#===============================================================================

#===== Participant Performance before and after  ==============================

# --- 0) Denominator: everyone has 176 expected trials -----------------
participants_all <- sort(unique(filtered_combined_df_all$participant))
denom_tbl <- tibble(
  participant = participants_all,
  expected_trials = 176L
)

# (Optional) sanity: ensure RT column name
DF <- filtered_combined_df_all %>%
  mutate(
    RT_ms      = if (!"RT_ms" %in% names(.)) RT_Ms else RT_ms,
    accuracy   = tolower(as.character(accuracy)),
    picture_id = picture_id
  )

#===== 1) BASELINE performance (raw) ============================================
# Valid at baseline = non-NA RT_ms (no correctness / <150ms rules yet)
perf_before <- DF %>%
  group_by(participant) %>%
  summarise(valid_trials = sum(!is.na(RT_ms)), .groups = "drop") %>%
  right_join(denom_tbl, by = "participant") %>%
  mutate(
    valid_trials = replace_na(valid_trials, 0L),
    percentage   = 100 * valid_trials / expected_trials,
    stage        = "Before (raw)", 
    picture_id = picture_id
  )

# ====  2) AFTER exclusions: incorrect -> NA; RT < 150ms -> NA =================
df_after <- DF %>%
  mutate(
    incorrect     = case_when(
      is.na(accuracy)          ~ NA,
      accuracy == "incorrect"  ~ TRUE,
      accuracy == "correct"    ~ FALSE,
      TRUE ~ NA
    ),
    RT_ms_interm  = ifelse(incorrect == TRUE, NA_real_, RT_ms),
    RT_ms_ready   = ifelse(!is.na(RT_ms_interm) & RT_ms_interm < 150, NA_real_, RT_ms_interm)
  )

perf_after <- df_after %>%
  group_by(participant) %>%
  summarise(valid_trials = sum(!is.na(RT_ms_ready)), .groups = "drop") %>%
  right_join(denom_tbl, by = "participant") %>%
  mutate(
    valid_trials = replace_na(valid_trials, 0L),
    percentage   = 100 * valid_trials / expected_trials,
    picture_id = picture_id,
    stage        = "After (incorrect + <150ms)"
  )

#===== 3) Combine & plot ========================================================
perf_both <- bind_rows(perf_before, perf_after) %>%
  mutate(participant = factor(participant, levels = participants_all))

gg <- ggplot(perf_both,
             aes(x = participant, y = percentage, fill = stage)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 90, color = "red", linewidth = 0.7) +
  labs(title = "Explicit Task — Participant Performance",
       x = "Participant ID", y = "Percentage of Expected Trials (%)", fill = NULL) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
print(gg)

#======== Label only AFTER bars that are <90% ==================================
below90 <- perf_after %>% filter(percentage < 90)
if (nrow(below90) > 0) {
  gg + geom_text(
    data = below90,
    aes(x = factor(participant, levels = participants_all),
        y = percentage, label = sprintf("%.1f", percentage)),
    position = position_nudge(x = 0.00, y = 2),
    size = 3
  ) -> gg_lab
  print(gg_lab)
}
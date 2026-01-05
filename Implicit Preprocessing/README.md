# Implicit Task Preprocessing (Experiment 1)
This folder contains the preprocessing pipeline for the **Implicit Task**
The preprocessing was implemented in R and follows the protocol provided by Aitana Grasso-Cladera. 

## Files in this folder

- `Implicit_Preprocessing_Steps.R`  
  Main preprocessing script that loads the raw MATLAB output, repairs timing inconsistencies, aligns behavioural and eye-tracking data, and produces the final trial-level dataset used for analysis.

- `README_Experiment 1_Attention and Action Task.docx`  
  Original preprocessing protocol and methodological instructions provided by the supervisor.  
  This document served as the conceptual guideline for the preprocessing steps implemented in R.


My script implements these steps in code and additionally includes diagnostic summaries/plots to verify that preprocessing behaved as expected.

> **Important:** Raw data (MATLAB `.mat` files) are not included in this repository due to privacy/ethics and data sharing restrictions.


## What the preprocessing script does (high level)

### 1) Loads raw MATLAB outputs
The script reads the behavioural and timing variables from MATLAB `.mat` files, including:
- reaction times, reaction type (push/pull)
- frame timing (`frameTime`) and frame characteristics (side, color)
- picture timing, trial timing, fixation cross timing
- picture IDs for positive/negative stimuli and the side of the positive stimulus
- eye-tracking x-position/trial-time structures (loaded and aligned for inspection)

### 2) Fixes the frame-time missingness issue
Some sessions contain missing trials that cause `frameTime` to be misaligned with the behavioural matrices.

The script detects trials where behavioural values are missing (e.g., RT and looked-valence both `NA`) and:
- inserts `NA` at the correct position in the session’s `frameTime` vector,
- ensures `pictureTime` is padded to a consistent length,
- extends list-structured variables (e.g., `xPositionData`, `trialTime`) to match the expected number of trials.

### 3) Combines blocks and removes invalid trials
Each participant has two sessions (two blocks). The script:
- concatenates block 1 and block 2 into a single per-participant sequence,
- creates a validity mask based on missing `frameTime`,
- removes trials marked invalid across behavioural and timing variables,
- carries the same removals through list-like timing/gaze structures to keep alignment.

### 4) Builds a clean trial-level dataframe for analysis
A trialwise dataframe is created with one row per trial and participant, including:
- RT (seconds + milliseconds), key timing variables, and picture IDs
- derived variables that define the experimental condition and task meaning:
  - **framed_valence** (whether the framed stimulus is positive or negative)
  - **instructed_action** from frame color (blue = pull, orange = push)
  - **instructed_congruence** (congruent vs incongruent based on valence × action)
  - **accuracy** (whether movement matches instructed action)
  - `block4` condition labels (CP/CN/IP/IN)

The script uses the framed side and the side of the positive picture to determine which picture is instruction-relevant on each trial.

### 5) Applies preprocessing/exclusion rules for RT analysis
The pipeline then applies the core RT preprocessing steps:

- Marks incorrect trials as `NA` for RT analysis
- Marks RTs < 150 ms as invalid (`NA`)
- Computes per-participant performance (% valid trials vs expected trials) and flags participants below **90%** valid-trial performance
- Excludes low-performance participants from the analysis dataset
- Applies **winsorisation** to cap extreme RTs (upper tail) using a global mean + *k* SD rule (k defined in the script)

A trimmed version is also created for comparison, but the final dataset exported for analysis uses the winsorised RTs.

### 6) Produces diagnostic summaries and plots
To document preprocessing effects, the script prints:
- trial counts before/after each preprocessing stage
- counts of removed trials by reason (incorrect, too fast)
- participant-level performance summaries (expected vs valid)
- mean and SD of RT before and after winsorisation

It also generates:
- residual/QQ diagnostics comparing raw vs trimmed vs winsorised variants (mixed-effects model on log RT)
- histograms of RT distributions across preprocessing stages (+ log-transform inset)

### 7) Exports the final preprocessed dataset
The final analysis dataset is written to a CSV (path set manually in the script), containing:
- participant, trial, block
- valence and congruence defined relative to the **framed** stimulus (instruction-relevant)
- RT_ms and RT_log10 (log10 of final RT)
- timing variables (frame_time, picture_time, fixation_cross_time)
- stimulus information (picture_id, pos/neg IDs), frame_side, frame_color
- accuracy

Output file name used in the script:
- `experiment_1_preprocessing.csv`

---

## How to run
1. Place the MATLAB `.mat` files for Experiment 1 in a local folder.
2. In the script, set:
   - `setwd("SET/YOUR/PATH/Experiment 1")`
   - the export path for the final CSV.
3. Run the script top-to-bottom in R.

---

## Notes / limitations
- The repository contains **code and documentation**, not raw participant data.
- Eye-tracking-related structures are loaded and aligned to support inspection, but the exported CSV focuses on behavioural outcomes + timing needed for downstream analyses.
- Folder names and file paths are currently manual; for reproducibility, these can be converted to relative paths if needed.




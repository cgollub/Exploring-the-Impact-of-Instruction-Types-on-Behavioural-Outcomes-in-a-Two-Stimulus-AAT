# Explicit Task Preprocessing (Experiment 2)

This folder contains the preprocessing code for **Experiment 2 (Explicit task)** from my bachelor thesis:

**“Exploring the Impact of Instruction Types on Behavioural Outcomes in a Two-Stimulus Approach-Avoidance Task”**

The goal of this preprocessing pipeline is to convert the raw MATLAB exports into a clean trial-level dataset that can be used for reaction time and accuracy analyses in R (mixed-effects models and descriptive plots).

---
The preprocessing was implemented in R and follows the protocol provided by Aitana Grasso-Cladera. 

## Files in this folder

- `Explicit_Preprocessing_Steps.R`  
  Main preprocessing script that loads the raw MATLAB output, repairs timing inconsistencies, aligns behavioural and eye-tracking data, and produces the final trial-level dataset used for analysis.

- `README_Experiment 2_Attention and Action Task.docx`  
  Original preprocessing protocol and methodological instructions provided by the supervisor.  
  This document served as the conceptual guideline for the preprocessing steps implemented in R.


My script implements these steps in code and additionally includes diagnostic summaries/plots to verify that preprocessing behaved as expected.

> **Important:** Raw data (MATLAB `.mat` files) are not included in this repository due to privacy/ethics and data sharing restrictions.

---
## What this script does (high-level)

### 1) Load and align raw MATLAB files
The script loads behavioural timing and stimulus information from `.mat` files, including:
- reaction times
- valence labels
- frame times and frame side
- trial timing and eye-tracking-related time series (optional)
- picture IDs (left/right)
- reaction types and block order (converted from MATLAB string format)

Because some MATLAB outputs are stored as nested lists/cell arrays, the script first converts them into R-friendly formats and aligns them across trials.

### 2) Combine blocks into one continuous sequence per participant
Each participant completed two sessions/blocks (88 trials each).  
The script concatenates session 1 + session 2 into **176 total trials per participant**, keeping trial order intact.

### 3) Determine the framed picture ID per trial
For each trial, both a left and right image are shown.  
Using `frame_side` (0 = left, 1 = right), the script selects the framed image:

- `picture_id` = framed image  
- `left_id`, `right_id` = both presented image IDs

### 4) Compute congruence and accuracy
Congruence is derived from the block order (`A` vs `B`) and trial number, following the experiment design.  
Accuracy is computed based on the mapping of:
- `congruence × valence × reaction_type`

The result is a trial-level variable:
- `accuracy` ∈ {correct, incorrect}

### 5) Optional: eye-tracking alignment and plotting (exploratory)
If eye-tracking inspection is needed, the script contains helper functions to:
- align gaze time-series to picture onset
- compute rough saccade/fixation timing
- plot single-trial eye traces for inspection

This step is exploratory and does not alter the behavioural preprocessing output.

---

## Behavioural preprocessing steps (reaction time cleaning)

After building the combined trial dataframe, the script applies the same RT-cleaning logic used in the thesis:

### Stage A — Baseline overview
- Keep all trials with available RTs  
- Summarise baseline accuracy and completeness per participant

### Stage B — Remove invalid responses
- Set RT to `NA` if the trial is incorrect
- Set RT to `NA` if RT < 150 ms

### Stage C — Participant exclusion rule
- Participants with < 90% valid trials (after Stage B) are excluded from analysis

### Stage D — Winsorisation (outlier handling)
- RTs are winsorised using a global cap:  
  **cap = global_mean + (2 × global_SD)**  
- (A trimming alternative is included for comparison but not used in the thesis)

---

## Output
The script writes a cleaned behavioural dataset to:

- `experiment_2_preprocessed.csv`

Key columns include:
- `participant`, `trial`, `block_order`
- `valence`, `congruence`, `reaction_type`, `accuracy`
- `RT_ms`, `RT_log10`
- `frame_time`, `frame_side`, `picture_time`
- `left_id`, `right_id`, `picture_id`




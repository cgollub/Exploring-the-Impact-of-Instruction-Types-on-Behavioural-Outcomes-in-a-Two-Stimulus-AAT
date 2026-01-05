# Behavioural Analysis 

This folder contains the behavioural analysis scripts used in the Thesis. 

The analyses focus on Reaction Time (RT) and accuracy measured derived from the 
two Approach-Avoidance Tasks (AAT)

-----
## Notes 
- Scripts assume preprocessed data generated in the preprocessing files
- Both scripts need the csv files saved from the preprocessing outcome 
  E1 refers to the Implicit Task (from Experiment 1)
  E2 refers to the Explicit Task (from Experiment 2)

- For picture-level exploration local access to stimulus picture files required (not included in repo) 
- Output files are generated locally and are not tracked via Git. 
------

## Files 

### RT_analysis_overleaf. R 
  - Imports preprocessed datasets for the Implicit and Explicit Task 
    and harmonises factor coding 
  - Filters to valid RT trials, adds trial index and standardised trial covariate 
    for learning/fatigue modelling 
  - Computes descriptove summaries (counts and mean/SD/SE) by condition
    and by congruence/valence and produces plots 
  - Fits linear mixed-effects models on RT_log10 with extensions
    including z_trial, frame side, and random intercepts for picture ID
    and visualises outcomes 
  - Back-transforms log10 effects into percentage RT differences and visualises
    model predictions; runs emmeans simple-effects contrasts 
  - Exploratory picture-level and co-image analyses 
  
## Accurac_analysis_overleaf.R
  - Imports preprocessed datasets for the Implicit and Explicit Task 
    and harmonises factor coding 
  - Constructs a binary accuracy variable acc01 (0/1)
  - Filters to valid 0/1 trials, adds trial index and standardised trial covariate 
    for learning/fatigue modelling
  - Combines both experiments into one dataset to compute accuracy summaries
    (Overall accuracy with Wald-style 95% CIs, min/max/mean/SD) by condition
    and by congruence/valence and produces plots 
  - Fits generalised linear mixed-effects models on accuracy with extensions
    including z_trial, frame side, and random intercepts for picture ID 
    and visualises outcomes 
  - Exploratory picture-level and co-image analyses 
  
  
## Software 
- R (version 4.2.2)
- Required packages are listed at the top of each script 


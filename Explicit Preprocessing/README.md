# README file for Attention and Action Task, Experiment 2
This information corresponds to the dataset for Task 3.
In this dataset, the information from both blocks has been merged, and the test trials have been removed. This gives a total of 176 trials per participant. 
No participants have been excluded from the analysis yet.

# Preprocessing
1. Extraction of information regarding specific events (e.g., task start, picture presentation).
2. A filter was applied to remove implausible spikes.
3. Data samples around signal loss were set to NaN.
4. A median filter was applied to smooth the data and emphasize large amplitude saccades.
5. Voloh et al., 2020. algorithm was applied to detect saccades based on velocity.
6. Data was epoched in 1500 ms windows (-500 to 1500).

Most of the datasets have the structure 88 x 146 (trials x sessions).
Subjects with incomplete sessions were already removed.
●	behavioralReactionTime.mat = reaction time from the joystick. This reaction time is relative to frame onset, since the subjects are required to react to the frame color. Here, some trials are “NaN”. They correspond to the trials where the frame did not appear.
●	blockOrder.mat = string indicating the block order for each session. A means that the subject was presented first with a CONGRUENT block and then the INCONGRUENT block. B means that the subject was presented first with an INCONGRUENT block and then a CONGRUENT block.
●	fixationCrossTime.mat = time, in eye-tracking units, on which the fixation cross appeared on each trial.
●	frameSide.mat = matrix indicating the side on which the frame was presented on each trial. 1 means RIGHT and 0 means LEFT.
●	frameTimes.mat =  time, in eye-tracking units, on which the frame appeared on each trial.
●	pictureSequence.mat is a 3D matrix containing the IDs of the pictures presented on each trial. To understand this matrix:
○	The value (1,1,1) corresponds to the first trial, the picture on the LEFT, for session 1 of subject one.
○	The value (1,2,1) corresponds to the first trial, the picture on the RIGHT, for session 1 of subject one.
●	pictureTimes.mat = time, in eye-tracking units, on which the picture appeared on each trial.
●	reactionType.mat = type of reaction generated for every subject on every trial, it is a string that can either be “push” or “pull”.
●	trialTime.mat = timeline for each trial from picture onset to next fixation cross onset.
●	valenceLooked.mat = matrix indicating the valence of the picture looked at on every trial. 1 means that the subject looked at a POSITIVE picture, 0 means that the subject looked at a NEGATIVE picture. Here, some trials are “NaN”. They correspond to the trials where the frame did not appear.
●	xPositionData.mat = position of the eye, for every trial from picture onset to the next fixation cross onset. This is based on the dominant eye of the subject.

Note: You have two variables that indicate which trials you should not consider because the frame was not presented. Make sure to use this information for all your analyses.


# README file for Attention and Action Task, Experiment 1
This information corresponds to the dataset for Task 3.
In this dataset, the information from both blocks has been merged, and the test trials have been removed. This gives a total of 96 trials per participant. 
Note: the first 3 participants have a smaller number of trials (n = 80). The same occurs for the last 5.
No participants have been excluded from the analysis yet.

# Preprocessing
1. Extraction of information regarding specific events (e.g., task start, picture presentation).
2. A filter was applied to remove implausible spikes.
3. Data samples around signal loss were set to NaN.
4. A median filter was applied to smooth the data and emphasize large amplitude saccades.
5. Voloh et al., 2020. algorithm was applied to detect saccades based on velocity.
6. Data was epoched in 1500 ms windows (-500 to 1500).

Most of the datasets have the structure 48 x 130 (trials x sessions).
Subjects with incomplete sessions were already removed.
●	behavioralReactionTime.mat = reaction time from the joystick. This reaction time is relative to frame onset, since the subjects are required to react to the frame color. Here, some trials are “NaN”. They correspond to the trials where the frame did not appear.
●	fixationCrossTime.mat = time, in eye-tracking units, on which the fixation cross appeared on each trial.
●	frameColor.mat = matrix indicating the color of the frame presented on each trial. 1 means BLUE and 0 means ORANGE.
●	frameSide.mat = matrix indicating the side on which the frame was presented on each trial. 1 means RIGHT and 0 means LEFT.
●	frameTime.mat =  time, in eye-tracking units, on which the frame appeared on each trial.
●	pictureSequenceNegative.mat = indicates the ID of the negative picture presented on each trial.
●	pictureSequencePositive.mat = indicates the ID of the positive picture presented on each trial.
●	pictureTime.mat = time, in eye-tracking units, on which the picture appeared on each trial.
●	reactionType.mat = type of reaction generated for every subject on every trial, it is a string that can either be “push” or “pull”.
●	sidePositive.mat = matrix indicating on which side of the screen the positive picture was presented. 1 means that the positive picture was presented on the RIGHT side, 0 means that the negative picture was presented on the LEFT side.
●	trialTime.mat = timeline for each trial from picture onset to next fixation cross onset.
●	valenceLooked.mat = matrix indicating the valence of the picture looked at on every trial. 1 means that the subject looked at a POSITIVE picture, 0 means that the subject looked at a NEGATIVE picture. Here, some trials are “NaN”. They correspond to the trials where the frame did not appear.
●	xPositionData.mat = position of the eye, for every trial from picture onset to the next fixation cross onset. This is based on the dominant eye of the subject.

Note: You have two variables that indicate which trials you should not consider because the frame was not presented. Make sure to use this information for all your analyses.

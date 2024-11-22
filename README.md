Face-looking as a real-time process in mind-mindedness
====

## Overview
This repository includes the R codes and dataset for the following paper:

Yamamoto, H., Sunahara, N., & Kanakogi, Y. (in press). Face-looking as a real-time process in mind-mindedness: Timely coordination between mothers' gaze on infants' faces and mind-related comments.

## Description
This repository consists of 4 folders and 3 R scripts. Some R scripts output figures to the folder 'Figure', and output CSV files to the folder 'Result' or 'Randomization'.

A simple folder structure is shown below:

- Data
  - ParticipantInformation.csv
  - gaze.csv
  - comments.csv
- Figure
  - Figure2.jpg
  - Figure3.jpg
  - FigureS1.jpg
  - FigureS2.jpg
- Randomization
  - df_effect_rnd_2.5s.csv
- Result 
  - wsize_2s.csv
  - wsize_2.5s.csv
  - wsize_3s.csv
- R scripts
  - 01_DescripticveStatistics_GLM.R
  - 02_GLMM.R
  - 03_Randomization.R

Note that the file `03_Randomization.R` needs a long time to be processed. We placed CSV file `df_effect_rnd_2.5s.csv` as the results of the R script in the folder `Randomization`.

## Data Structure
- ParticipantInformation.csv
  - This csv file is used with `01_DescripticveStatistics_GLM.R`. 

| Column Name     | Variable               | Explanation                                                 |
|-----------------|------------------------|-------------------------------------------------------------|
| ParticipantID   | quantitative (integer) | Participant IDs                                             |
| Month           | quantitative (integer) | Infants' age in months                                      |
| partcipant_name | qualitative            | Participant names saved in an eye-tracking software         |
| recording_name  | qualitative            | Recording names saved in an eye-tracking software           |
| Total_comment   | quantitative (integer) | Total number of comments of participants                    |
| mind_comment    | quantitative (integer) | Number of mind-related comments of participants             |
| app_comment     | quantitative (integer) | Number of appropriate mind-related comments of participants |


- gaze.csv
  - This csv file is used with all R files.

| Column Name       | Variable              | Explanation                                                                                |
|-------------------|-----------------------|--------------------------------------------------------------------------------------------|
| participant_name  | qualitative           | Participant names saved in an eye-tracking software                                        |
| recording_name    | qualitative           | Recording names saved in an eye-tracking software                                          |
| image_name        | qualitative           | Image names extracted from caregivers' first-person perspective videos                     |
| frame_id          | quantitative          | Frame number of an image in caregivers' first-person perspective videos                    |
| movie_timestamp   | qualitative           | Movie timestamp of an image (msec)                                                         |
| gaze_timestamp    | quantitative          | Gaze timestamp of an image (msec)                                                          |
| gaze              | quantitative (binary) | Whether caregivers' pupils were detected by an eye-tracker (detected = 1; failed = 0)      |
| MM                | quantitative          | Caregivers' verbal mind-mindedness                                                         |
| FaceInView        | logical               | Whether infants' faces were in their caregivers' field of view (presence = 1; absence = 0) |
| FaceLooking       | logical               | Whether caregivers looked at their infants' faces (face-looking = 1; not face-looking = 0) |

- comments.csv
  - This csv file is used with `02_GLMM.R` and `03_Randomization.R`.

| Column Name       | Variable               | Explanation                                                                 |
|-------------------|------------------------|-----------------------------------------------------------------------------|
| category          | quantitative (binary)  | Type of a comment (appropriate mind-related comment = 1; other comment = 0) |
| comment_timestamp | quantitative           | Movie timestamp of a comment (microseconds)                                 |
| participant_name  | qualitative            | Participant names saved in an eye-tracking software                         |
| id                | quantitative (integar) | The sequential number of a row                                              |


## Software & Package Versions
- RStudio: 2024.09.01+394
- R: 4.4.1 
- tidyverse: 2.0.0
- lubridate: 1.9.3
- here: 1.0.1
- lme4: 1.1.35.5
- car: 3.1.3
- broom.mixed: 0.2.9.6
- parameters: 0.23.0
- modelbased: 0.8.9

## The Author of this README File
- [Hiroki Yamamoto](https://github.com/dororo1225)
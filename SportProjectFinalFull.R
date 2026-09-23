# NFL Offensive Strategy Analysis
# Research Question:
# How does offensive strategy change when NFL teams are winning vs. losing?

# More specifically:
# When a team is trailing by 7+ points,
# how does its play selection change compared
# with when it is leading or tied?

# Data: NFL play-by-play data
# Seasons analyzed: 2021–2024

# -------------------------
# 1. Load Packages
# -------------------------

#Install package
install.packages("nflreadr")
library(nflreadr)

#load the 2024 data
pbp <- load_pbp(2024)

#Explore the data
dim(pbp)

#This gave us the names of the 372 variables
names(pbp)

#Shows the first few plays in dataset
head(pbp)

#Step 2: Create analysis dataset
#Keep only run and pass plays
offensive_plays <- pbp[
  pbp$play_type %in% c("run", "pass"),
]

#Check the new dataset
dim(offensive_plays)

#Step 3: Create Game Situation Categories
#Turning numerical score_differential into categories easier to analyze
#Going to create:
#Leading
#Tied
#Trailing
#Trailing by 7+
#Trailing by 14+

offensive_plays$situation <- ifelse(
  offensive_plays$score_differential >= 1,
  "Leading",
  ifelse(
    offensive_plays$score_differential == 0,
    "Tied",
    ifelse(
      offensive_plays$score_differential <= -14,
      "Trailing by 14+",
      ifelse(
        offensive_plays$score_differential <= -7,
        "Trailing by 7+",
        "Trailing"
      )
    )
  )
)

#Count the number of plays in each game situation
table(offensive_plays$situation, useNA = "ifany")

#Step 4: Calculate Run/Pass Rates

#Count run and pass plays within each situation
play_counts <- table(
  offensive_plays$situation,
  offensive_plays$play_type
)

#Convert counts into percentages within each situation
play_percentages <- prop.table(
  play_counts,
  margin = 1
) * 100

#Round percentages to 2 decimal places
round(play_percentages, 2)
  
#Step 5: Create a Results Table

results <- as.data.frame(play_percentages)

#Rename the columns
names(results) <- c(
  "Situation",
  "Play_Type",
  "Percentage"
)

#Round percentages
results$Percentage <- round(results$Percentage, 2)

#View the results
results

#Cleaner table that we can use for chart

#Put game situations in logical order
results$Situation <- factor(
  results$Situation,
  levels = c(
    "Leading",
    "Tied",
    "Trailing",
    "Trailing by 7+",
    "Trailing by 14+"
  )
)

#View the organized results
results

#Step 6: Create the Run/Pass Chart

#Create visualization

install.packages("ggplot2")
library(ggplot2)

ggplot(
  results,
  aes(
    x = Situation,
    y = Percentage,
    fill = Play_Type
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "NFL Offensive Play Selection by Game Situation",
    x = "Game Situation",
    y = "Percentage of Offensive Plays",
    fill = "Play Type"
  ) +
  theme_minimal()

#Step 7: Calculate offensive efficiency

efficiency <- aggregate(
  cbind(yards_gained, epa) ~ situation + play_type,
  data = offensive_plays,
  FUN = mean,
  na.rm = TRUE
)

#View efficiency results

efficiency

#Step 8:  Round efficiency results

efficiency$yards_gained <- round(
  efficiency$yards_gained,
  2
)

efficiency$epa <- round(
  efficiency$epa,
  3
)

#View cleaned efficiency table
efficiency

#Step 9: Create the Efficiency Chart
#Visualize average yards gained

#Put game situations in the correct order
efficiency$situation <- factor(
  efficiency$situation,
  levels = c(
    "Leading",
    "Tied",
    "Trailing",
    "Trailing by 7+",
    "Trailing by 14+"
  )
)

ggplot(
  efficiency,
  aes(
    x = situation,
    y = yards_gained,
    fill = play_type
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Yards Gained by Game Situation",
    x = "Game Situation",
    y = "Average Yards Gained",
    fill = "Play Type"
  ) +
  theme_minimal()
  
#Step 10: Does Strategy Change During the Game?

#Question: When a team is trailing by 7+ points, does its run/pass strategy change as the game progresses?
#10A : Create quarter groups

offensive_plays$quarter_group <- ifelse(
  offensive_plays$qtr == 1,
  "Q1",
  ifelse(
    offensive_plays$qtr == 2,
    "Q2",
    ifelse(
      offensive_plays$qtr == 3,
      "Q3",
      ifelse(
        offensive_plays$qtr == 4,
        "Q4",
        "Other"
      )
    )
  )
)

#Check quarter groups
table(offensive_plays$quarter_group)
  
#Step 11 : Focus on teams trailing by 7+  
#Question: When teams are trailing by 7+ points, how does their run/pass selection change throughout the game?

#Analyze teams trailing by 7+

trailing_7 <- offensive_plays[
  offensive_plays$situation == "Trailing by 7+",
]

#Count run and pass plays by quarter

table(
  trailing_7$quarter_group,
  trailing_7$play_type
)
  
#Step 12: Calculate Run/Pass Percentages by Quarter   

trailing_7_percentages <- prop.table(
  table(
    trailing_7$quarter_group,
    trailing_7$play_type
  ),
  margin = 1
) * 100

round(trailing_7_percentages, 2)
  
#Step 13:  Create the Timing Chart

#Create timing results table
timing_results <- as.data.frame(
  trailing_7_percentages
)

names(timing_results) <- c(
  "Quarter",
  "Play_Type",
  "Percentage"
)

timing_results$Percentage <- round(
  timing_results$Percentage,
  2
)

#View timing results
timing_results

#Put quarters in the correct order

timing_results$Quarter <- factor(
  timing_results$Quarter,
  levels = c("Q1", "Q2", "Q3", "Q4")
)

#Create timing chart
ggplot(
  timing_results,
  aes(
    x = Quarter,
    y = Percentage,
    fill = Play_Type
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Offensive Play Selection When Trailing by 7+",
    x = "Quarter",
    y = "Percentage of Offensive Plays",
    fill = "Play Type"
  ) +
  theme_minimal()

#Step 14: Multiple-Season Analysis
#Does the pattern we found in 2024 also appear in other NFL seasons?

#14A: Load multiple NFL seasons
pbp_2021 <- load_pbp(2021)
pbp_2022 <- load_pbp(2022)
pbp_2023 <- load_pbp(2023)

#14B: Create a function for seasonal analysis
analyze_season <- function(data, year) {
  #Keep only run and pass plays
  offensive <- data[
    data$play_type %in% c("run", "pass") &
    !is.na(data$score_differential),
  ]
  
#Create game situation 
  offensive$situation <- ifelse(
    offensive$score_differential >= 1,
    "Leading",
    ifelse(
      offensive$score_differential == 0,
      "Tied",
      ifelse(
        offensive$score_differential <= -14,
        "Trailing by 14+",
        ifelse(
          offensive$score_differential <= -7,
          "Trailing by 7+",
          "Trailing"
        )
      )
    )
  )
  
#Calculate play percentages

 percentages <- prop.table(
    table(
      offensive$situation,
      offensive$play_type
    ),
    margin = 1
  ) * 100

#Return the trailing-by-7+ passing percentage  
 
 result <- percentages["Trailing by 7+", "pass"]
  
  data.frame(
    Season = year,
    Trailing_7_Pass_Percentage = round(result, 2)
  )
}

#14C: Analyze each season

season_results <- rbind(
  analyze_season(pbp_2021, 2021),
  analyze_season(pbp_2022, 2022),
  analyze_season(pbp_2023, 2023),
  analyze_season(pbp_2024, 2024)
)

#View results
season_results

#Step 15: Final Results Table
#Create final results table

final_results <- data.frame(
  Situation = c(
    "Leading",
    "Tied",
    "Trailing",
    "Trailing by 7+",
    "Trailing by 14+"
  ),
  Pass_Percentage = c(
    play_percentages["Leading", "pass"],
    play_percentages["Tied", "pass"],
    play_percentages["Trailing", "pass"],
    play_percentages["Trailing by 7+", "pass"],
    play_percentages["Trailing by 14+", "pass"]
  ),
  Run_Percentage = c(
    play_percentages["Leading", "run"],
    play_percentages["Tied", "run"],
    play_percentages["Trailing", "run"],
    play_percentages["Trailing by 7+", "run"],
    play_percentages["Trailing by 14+", "run"]
  )
)

#Round percentages
final_results$Pass_Percentage <- round(
  final_results$Pass_Percentage,
  2
)

final_results$Run_Percentage <- round(
  final_results$Run_Percentage,
  2
)

#View final results
final_results

#Step 16: Calculate Key Changes
#Calculate change in passing from leading to trailing by 14+

pass_change <- 
  final_results$Pass_Percentage[
    final_results$Situation == "Trailing by 14+"
  ] -
  final_results$Pass_Percentage[
    final_results$Situation == "Leading"
  ]

pass_change 

#Calculate Q1 to Q4 passing increase when trailing by 7+

q1_pass <- timing_results$Percentage[
  timing_results$Quarter == "Q1" &
  timing_results$Play_Type == "pass"
]

q4_pass <- timing_results$Percentage[
  timing_results$Quarter == "Q4" &
  timing_results$Play_Type == "pass"
]

q4_increase <- q4_pass - q1_pass

q4_increase

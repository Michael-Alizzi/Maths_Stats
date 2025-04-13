# Pull all functions
source('~/Maths_Stats/Packages.R')
source('~/Maths_Stats/Probability Functions.R')
source('~/Maths_Stats/Random Variable Functions.R')
source('~/Maths_Stats/Distribution Functions.R')
source('~/Maths_Stats/Transformations.R')
source('~/Maths_Stats/Random Samples.R')
source('~/Maths_Stats/Confidence Intervals and Hypothesis Tests.R')
source('~/Maths_Stats/Supervised and Unsupervised Machine Learning.R')
options(scipen = 999)

# Function metadata
attributes(condition_entropy_funct)

# Create the dataset as a data frame
play_tennis_data <- data.frame(
  Day = paste0("D", 1:14),
  Outlook = c("Sunny", "Sunny", "Overcast", "Rain", "Rain", "Rain", "Overcast",
              "Sunny", "Sunny", "Rain", "Sunny", "Overcast", "Overcast", "Rain"),
  Temperature = c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool",
                  "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Mild"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal",
               "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Wind = c("Weak", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong",
           "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong"),
  PlayTennis = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes",
                 "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No"),
  stringsAsFactors = FALSE
)



Tennis <- list()

for (i in colnames(play_tennis_data)[-1]) {
  
  TennisFeatures <- play_tennis_data %>%
    group_by(.data[[i]]) %>%
    summarise(
      Count = n(),
      Proportion = n() / nrow(play_tennis_data), 
      .groups = "drop"
    )
  
  Tennis[[{{i}}]] <- TennisFeatures
  
}

Tennis

TennisConditional <- list()

for (i in colnames(play_tennis_data)[-c(1, 6)]) {
  TennisFeatures <- play_tennis_data %>%
    group_by(PlayTennis, .data[[i]]) %>%
    summarise(
      Count = n(),
      Proportion = n() / nrow(play_tennis_data),
      .groups = "drop"
    )
  
  TennisConditional[[i]] <- TennisFeatures
}

round(entropy_funct(Tennis$PlayTennis %>%
                      filter(PlayTennis == "Yes") %>%
                      pull(Proportion)
                    , 
                    Tennis$PlayTennis %>%
                      filter(PlayTennis == "No") %>%
                      pull(Proportion)
) -
  condition_entropy_data(play_tennis_data, 'Wind', 'PlayTennis', 'Yes'), 4)

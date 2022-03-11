# Want to assess the likely hood of a whiff of better whiff percentage based strictly on the characteristics of the pitch type
# Long-term end goal of the project is to eventually develop a pitcher arsenal score.
library(tidyverse)
library(readr)
library(randomForest)
library(Boruta)
library(caret)
library(ggpubr)

data <- read_csv("draft_league_updated.csv") # Draft League data through 7/18
hawkeye <- read_csv("hawkeye.csv") # Hawkeye data from first two Draft League games played at Citizens Bank Park

common_columns <- intersect(colnames(data), colnames(hawkeye))

draft_league <- rbind(subset(data, select = common_columns), subset(hawkeye, select = common_columns))

# Can remove data and hawkeye data frames after joining the two files together
rm(data, hawkeye)

# Start looking at the different pitch calls, tagged and auto pitch types 
draft_league %>%
  select(taggedpitchtype) %>%
  unique()

draft_league %>%
  select(pitchcall) %>%
  unique()

# Create pitch mix function - curious about what certain players throw - based on what I saw when player was pitching in game
pitch_mix <- function(player){
  draft_league %>%
    filter(pitcher == player) %>%
    group_by(taggedpitchtype) %>%
    summarize('Total Pitches' = n())
}

pitch_mix("Ayers, Cole") 

# Add a description column to the dataset - this will make it easier to subset data later as well make the data frame similar to MLB data for comparison later
draft_league <- draft_league %>%
  mutate(description = case_when(
    pitchcall == "StrikeSwinging" ~ "Whiff",
    pitchcall == "StrikeCalled" ~ "StrikeCalled",
    pitchcall == "BallCalled" ~ "BallCalled",
    pitchcall == "BallinDirt" ~ "BallinDirt",
    pitchcall == "BallIntentional" ~ "BallIntentional",
    pitchcall == "HitByPitch" ~ "HitByPitch",
    pitchcall == "FoulBall" ~ "FoulBall",
    pitchcall == "InPlay" ~ "InPlay",
    pitchcall %in% c("StrikeSwinging", "FoulBall", "InPlay") ~ "Swing"
  ))

# Start to look at the total pitches pitchers have thrown over the course of the summer
pitches_by_pitcher <- draft_league %>%
  group_by(pitcher) %>%
  summarize('total_pitches' = length(taggedpitchtype)) %>%
  arrange(desc(total_pitches))


# Create second data frame joined with total pitches for each pitcher - its also good practice to create a copy just incase an error is made
dl_2 <- draft_league %>%
  left_join(pitches_by_pitcher, by = "pitcher")

# Continue to aggregate the data
pitches_per_group <- dl_2 %>%
  filter(taggedpitchtype != "Undefined") %>%
  group_by(pitcher, taggedpitchtype) %>%
  summarize('pitches_thrown' = length(taggedpitchtype))
# I decided to take out undefined from the dataset as it would've been more trouble to figure out the correct pitch - its being treated like an na value

total_pitches <- pitches_per_group %>%
  left_join(pitches_by_pitcher, by = "pitcher")

# Create third data frame and normalize LHP data to align with the rest of the data set before more data manipulation
dl_3 <- dl_2 %>%
  inner_join(total_pitches %>% select(pitcher, taggedpitchtype, pitches_thrown)) %>%
  mutate(hb = ifelse(pitcherthrows == "Left", horzbreak*-1, horzbreak),
         release_side = ifelse(pitcherthrows == "Left", relside*-1, relside))
# Creating three different data sets prevents losing data over errors made, and still have the original dataset if I had to back track. 

# Create the Whiff Variables for modeling 
whiff <- dl_3 %>%
  filter(description %in% c("StrikeCalled", "Whiff", "FoulBall", "InPlay")) %>%
  group_by(pitcher, taggedpitchtype, description) %>%
  summarize(total = n()) %>%
  spread(description, total) %>%
  left_join(total_pitches, by = c("pitcher", "taggedpitchtype")) %>% 
  replace_na(list(FoulBall=0,StrikeCalled=0,Whiff=0,InPlay=0)) %>% # Replace NA Values with 0, models don't like NA values lol
  mutate(pitch_pct = 100 * round((pitches_thrown / total_pitches),digits = 3),
         swing = sum(FoulBall) + sum(Whiff) + sum(InPlay),
         whiff_pct = round(Whiff / swing, digits = 3)) %>% 
  replace_na(list(swing = 0)) %>%
  filter(taggedpitchtype != "Undefined") 

# Create separate pitch profiles - combining sinkers with fastballs, cutters with sl and splitters with changeups to get consistent with 4 major pitch sub type groups
fb_profile <- dl_3 %>%
  filter(taggedpitchtype %in% c("Fastball", "Sinker")) %>%
  group_by(pitcher, taggedpitchtype) %>%
  summarize(total = length(taggedpitchtype),
            avg_velo = mean(relspeed, na.rm = T),
            avg_spin = mean(spinrate, na.rm = T),
            avg_hb = mean(hb, na.rm = T),
            avg_vb = mean(inducedvertbreak, na.rm = T),
            avg_ext = mean(extension, na.rm = T),
            avg_vaa = mean(vertapprangle, na.rm = T),
            avg_axis = mean(spinaxis, na.rm = T),
            avg_release_height = mean(relheight, na.rm = T),
            avg_release_side = mean(release_side, na.rm = T)) %>%
  left_join(whiff %>% select(pitcher, taggedpitchtype, whiff_pct), by = c("pitcher", "taggedpitchtype")) %>%
  replace_na(list(whiff_pct=0))


# CH Profile
ch_profile <- dl_3 %>%
  filter(taggedpitchtype %in% c("ChangeUp", "Splitter")) %>%
  group_by(pitcher, taggedpitchtype) %>%
  summarize(total = length(taggedpitchtype),
            avg_velo = mean(relspeed, na.rm = T),
            avg_spin = mean(spinrate, na.rm = T),
            avg_hb = mean(hb, na.rm = T),
            avg_vb = mean(inducedvertbreak, na.rm = T),
            avg_ext = mean(extension, na.rm = T),
            avg_vaa = mean(vertapprangle, na.rm = T),
            avg_axis = mean(spinaxis, na.rm = T),
            avg_release_height = mean(relheight, na.rm = T),
            avg_release_side = mean(release_side, na.rm = T)) %>%
  left_join(whiff %>% select(pitcher, taggedpitchtype, whiff_pct), by = c("pitcher", "taggedpitchtype")) %>%
  replace_na(list(whiff_pct=0))


# SL profile
sl_profile <- dl_3 %>%
  filter(taggedpitchtype == "Slider") %>%
  group_by(pitcher, taggedpitchtype) %>%
  summarize(total = length(taggedpitchtype),
            avg_velo = mean(relspeed, na.rm = T),
            avg_spin = mean(spinrate, na.rm = T),
            avg_hb = mean(hb, na.rm = T),
            avg_vb = mean(inducedvertbreak, na.rm = T),
            avg_ext = mean(extension, na.rm = T),
            avg_vaa = mean(vertapprangle, na.rm = T),
            avg_axis = mean(spinaxis, na.rm = T),
            avg_release_height = mean(relheight, na.rm = T),
            avg_release_side = mean(release_side, na.rm = T)) %>%
  left_join(whiff %>% select(pitcher, taggedpitchtype, whiff_pct), by = c("pitcher", "taggedpitchtype")) %>%
  replace_na(list(whiff_pct=0))


# CB profile
cb_profile <- dl_3 %>%
  filter(taggedpitchtype == "Curveball") %>%
  group_by(pitcher, taggedpitchtype) %>%
  summarize(total = length(taggedpitchtype),
            avg_velo = mean(relspeed, na.rm = T),
            avg_spin = mean(spinrate, na.rm = T),
            avg_hb = mean(hb, na.rm = T),
            avg_vb = mean(inducedvertbreak, na.rm = T),
            avg_ext = mean(extension, na.rm = T),
            avg_vaa = mean(vertapprangle, na.rm = T),
            avg_axis = mean(spinaxis, na.rm = T),
            avg_release_height = mean(relheight, na.rm = T),
            avg_release_side = mean(release_side, na.rm = T)) %>%
  left_join(whiff %>% select(pitcher, taggedpitchtype, whiff_pct), by = c("pitcher", "taggedpitchtype")) %>%
  replace_na(list(whiff_pct=0))

# Now time to determine thresholds for each pitch profile
# The thresholds I have chosen are predetermined I picked 100 for FB as they are the most thrown pitch in the league, then 50 for the other sub groups
# I ended up only looking at FB was due to the small sample sizes for breaking ball and off speed pitches 
fb_profile <- fb_profile %>%
  filter(total >= 100)

# Now its time for feature selection with bortua
fb_variables <- as.formula(whiff_pct ~ avg_velo + avg_spin + avg_hb + avg_vb + avg_ext + avg_vaa + avg_axis + avg_release_height + 
                             avg_release_side)

fb_bortua <- Boruta(fb_variables, data = fb_profile)

plot(fb_bortua)
print(fb_bortua)

# The variables that are determined important are spinrate, velocity, and vertical break. Tentative attributes are axis and vaa I am going to throw them in 
# I am still going to include horizontal break as it is still apart of the pitch profile and individual pitch shape
# Time for train and test split
set.seed(2021) # Setting the seed helps with reproducing the data
train_test_split <- function(data, size = 0.6, train = TRUE){
  nrow_data <- nrow(data)
  total_rows <- nrow_data * size
  train_samp <- 1: total_rows
  if(train == TRUE) {
    return(data[train_samp, ])
  } else {
      return(data[-train_samp, ])
    }
}

fb_train <- train_test_split(fb_profile, size = 0.6, train = TRUE)
fb_test <- train_test_split(fb_profile, size = 0.6, train = FALSE)
nrow(fb_profile) * 0.6 #sanity check to make sure the function worked correctly 
# Reason for 60/40 test and train split is the smaller sample size of data working with, better for testing accuracy

# Use random forest model to create xWhiff
fb_forest_model <- randomForest(whiff_pct ~ avg_velo + avg_spin + avg_vb + avg_hb + avg_vaa + avg_axis,
                                data = fb_train)

summary(fb_forest_model) # Summary of the model
varImpPlot(fb_forest_model) # Imp plot is used to see which variables random forest model deemed important for xWhiff, Velocity, Spin Axis, and Vertical Break were the top 3 most important
plot(fb_forest_model) # Plotting the model allows use to see the different trees used and the error rate

fb_forest_pred <- predict(fb_forest_model, newdata = fb_test) # Test data is used to test the accuracy and validity of the model
fb_pred_all <- predict(fb_forest_model, newdata = fb_profile) # After testing with the test data, predict xWhiff with collected performance data

fb_forest_rmse <- RMSE(fb_forest_pred, fb_test$whiff_pct)
fb_rmse_all <- RMSE(fb_pred_all, fb_profile$whiff_pct)

# Copy FB profile with Whiff results into a new dataframe 
fb_profile$xWhiff <- round(fb_pred_all, 3)

fb_profile_final <- fb_profile %>%
  group_by(pitcher, taggedpitchtype) %>%
  select(pitcher, taggedpitchtype, whiff_pct, xWhiff) %>%
  arrange(desc(xWhiff))

# Visualize xWhiff to Whiff Percentage
ggscatter(fb_profile_final, x = "whiff_pct", y = "xWhiff",
          add = "reg.line", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "pearson",
          xlab = "Whiff Percentage", ylab = "Expected Whiff Percentage") 

rmse <- (fb_rmse_all)
std <- (sd(fb_profile$whiff_pct))
results <- data.frame(fb_rmse = rmse, fb_sd = std)
view(results) # The RMSE is lower than the SD, that is a win lol - definitely need to make improvements 

# Leaderboard for xWhiff
xWhiff_Leaders <- fb_profile_final %>%
  arrange(desc(xWhiff)) %>%
  head(10)
view(xWhiff_Leaders)

# Over Performers - xWhiff
xWhiff_overperformance <- fb_profile_final %>%
  arrange(desc(whiff_pct)) %>%
  head(10)
view(xWhiff_overperformance)

# Underperformers 
xWhiff_underperform <- fb_profile_final %>%
  arrange(whiff_pct) %>%
  head(10)
view(xWhiff_underperform)

# xWhiff percentile rankings - fun to look at, maybe in the future grade xWhiff with z-scores for each pitcher in stead of a plus ranking for whiff and xWhiff?
percentile <- function(x) trunc(rank(x))/length(x)

fb_profile_final$whiff_percentile <- round(percentile(fb_profile_final$whiff_pct),2)
fb_profile_final$xWhiff_percentile <- round(percentile(fb_profile_final$xWhiff),2)

# Create FB Profile Percentile Ranks - this will help us explain why guys may be over performing or under performing with xWhiff
# Limitations of this is limited data set and talent level within the MLB Draft League, as well the model was trained on this years data. Test with second half data and possibly fall college data.  
# Another limitation is using tagged pitch types, there are going to be human error with tagging pitches based on what the tagger thinks the pitch is which may skew some results. 
# Next steps are to improve the model, figure out a better method for xWhiff (possibly whiff plus), make MLB comps (to help with MLB comps derive VAA with statcast data using the pitch trajectories or maybe a regression model see what correlates with VAA and use the coefficients?)
# Another step for the future is xWhiff with the autopitch type and see the xWhiff difference from what the TM tagger thinks the pitch is and what the TM machine thinks the pitch is. 
# Reason for modeling with FB only is due to the fact in the first half in the Draft League not many pitchers threw enough breaking balls and off speed to qualify 

pitch_data <- read.csv("savant_data.csv")
# Keep only 7 pitch types (the 7 most common)
pitch_data <- pitch_data[pitch_data$pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),]
View(pitch_data)
pd_right <- pitch_data[pitch_data$p_throws=="R",]
pd_left <- pitch_data[pitch_data$p_throws=="L",]


#########################################
## Looking at pitch selection
ggplot(pitch_data, aes(x = pitch_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))

#Pitch selection based on handedness
ggplot(pd_right, aes(x = pitch_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))

ggplot(pd_left, aes(x = pitch_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))

#Pitch selection based on count
ggplot(group_by(pitch_data, balls), aes(x = pitch_type, y = ..count../sum(..count..))) + geom_bar() + facet_grid(~balls)
#########################################




#########################################
#pitch type vs launch speed
type_velo <- pitch_data[,c("pitch_type", "launch_speed", "release_speed")]
type_velo <- type_velo[type_velo$launch_speed!="null" & type_velo$pitch_type!="null",]
type_velo$launch_speed <- as.numeric(as.character(type_velo$launch_speed))
type_velo$release_speed <- as.numeric(as.character(type_velo$release_speed))
type_velo <- type_velo %>% group_by(pitch_type) %>% 
  summarise(launch_velo = mean(launch_speed),  pitch_speed = mean(release_speed, na.rm=TRUE), num_pitches = n())
#type_velo <- type_velo[type_velo$num_pitches>100,] #Take only common pitches
View(type_velo)
ggplot(type_velo, aes(x = pitch_speed, y = launch_velo)) +
  geom_smooth(method = "lm", se = FALSE) + geom_text(aes(label = pitch_type)) +
  labs(title = "Exit Velocity off the Bat for each Pitch Type", x = "Pitch Speed (MPH)", y = "Exit Velocity (MPH)")
summary(lm(launch_velo~pitch_speed, data = type_velo))
## R^2 = .8025, b_1 = .15, p = .00636. This is significant
#########################################



#########################################
## release speed vs effective speed based on pitch type
pitch_diff <- pitch_data[,c("pitch_type", "release_speed", "effective_speed")]
pitch_diff <- pitch_diff[pitch_diff$release_speed!="null" & 
                           pitch_diff$pitch_type!="null" & pitch_diff$effective_speed!="null",]
pitch_diff$effective_speed <- as.numeric(as.character(pitch_diff$effective_speed))
pitch_diff$release_speed <- as.numeric(as.character(pitch_diff$release_speed))
pitch_diff <- pitch_diff %>% group_by(pitch_type) %>% 
  summarise(pitch_speed = mean(release_speed, na.rm=TRUE), effective_speed = mean(effective_speed),
            diff = mean(effective_speed - release_speed), num_pitches = n())
View(pitch_diff)
ggplot(pitch_diff, aes(x = pitch_type, y = diff)) + geom_bar(stat = "identity")
# conclusion: All pitches are slower, but off speed pitches tend to look slower comparatively

## spin rate vs horizontal/vertical movement for different pitches
ggplot(pd_right, aes(x = as.numeric(as.character(release_spin_rate)),
                       y = as.numeric(as.character(pfx_x)), color = pitch_type)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Horizontal Movement of Pitches vs Spin Rate (righties)",
       x = "Spin Rate (RPM)", y = "Horizontal Movement (inches)", color = "Pitch Type")
ggplot(pd_right, aes(x = as.numeric(as.character(release_spin_rate)),
                     y = as.numeric(as.character(pfx_z)), color = pitch_type)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Vertical Movement of Pitches vs Spin Rate (righties)",
       x = "Spin Rate (RPM)", y = "Vertical Movement (inches)", color = "Pitch Type")

#Horizontal vs vertical by pitch type
ggplot(pd_right, aes(x = as.numeric(as.character(pfx_x)),
                     y = as.numeric(as.character(pfx_z)), color = pitch_type)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Horizontal/Vertical Movement of Pitches (righties)",
       x = "Horizontal Movement (inches)", y = "Vertical Movement (inches)", color = "Pitch Type")


#########################################

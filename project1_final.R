pitch_data <- read.csv("savant_data.csv")
# Keep only 7 pitch types (the 7 most common)
pitch_data <- pitch_data[pitch_data$pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),]
pitch_data$launch_speed <- as.numeric(as.character(pitch_data$launch_speed))
pitch_data$release_speed <- as.numeric(as.character(pitch_data$release_speed))
pitch_data$effective_speed <- as.numeric(as.character(pitch_data$effective_speed))
pitch_data$release_spin_rate <- as.numeric(as.character(pitch_data$release_spin_rate))
pitch_data$pfx_x <- as.numeric(as.character(pitch_data$pfx_x))
pitch_data$pfx_z <- as.numeric(as.character(pitch_data$pfx_z))
pd_right <- pitch_data[pitch_data$p_throws=="R",]
pd_left <- pitch_data[pitch_data$p_throws=="L",]
#########################################
#THEME
Static_Poster_Theme <- theme_bw() + 
  theme(text = element_text(face = "bold", 
                            color = "steelblue4"),
        legend.background = element_rect(color = "black", 
                                         size = 0.3),
        axis.text = element_text(size = 10, 
                                 color = "red4", 
                                 face = "plain"),
        axis.title = element_text(size = 12),
        legend.text = element_text(color = "red4", 
                                   face = "plain"),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(face = "italic", 
                                     color = "red4", 
                                     size = 10),
        plot.caption = element_text(face = "italic", 
                                    color = "red4", 
                                    size = 8))
#########################################

#########################################
## Looking at pitch selection
ggplot(pitch_data, aes(x = pitch_type, fill = pitch_type)) + Static_Poster_Theme + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  labs(title = "4 Seam Fastball is Most Common Pitch Selected",
       subtitle = "About 50% fastballs, 20% sliders, 10% curve/changeup",
       x = "Pitch Type", y = "Proportion", fill = "Pitch Type")
ggsave("pitch_selection.png", dpi = 300)

#########################################




#########################################
#pitch type vs launch speed
type_velo <- pitch_data[,c("pitch_type", "launch_speed", "release_speed")]
type_velo <- type_velo[type_velo$launch_speed!="null" & type_velo$pitch_type!="null",]
type_velo <- type_velo %>% group_by(pitch_type) %>% 
  summarise(launch_velo = mean(launch_speed),  pitch_speed = mean(release_speed, na.rm=TRUE), num_pitches = n())
ggplot(type_velo, aes(x = pitch_speed, y = launch_velo)) +
  geom_smooth(method = "lm", se = FALSE, color = "red4") +
  geom_text(aes(label = pitch_type)) + Static_Poster_Theme +
  labs(title = "Correlation Between Pitch Speed, Exit Velocity",
       subtitle = "R-squared = .8025, slope = .15, p-value = .006",  x = "Pitch Speed (MPH)",
       y = "Exit Velocity (MPH)")
ggsave("pitch_speed_vs_exit_velo.png", dpi = 300)
summary(lm(launch_velo~pitch_speed, data = type_velo))
## R^2 = .8025, b_1 = .15, p = .00636. This is significant
#########################################



#########################################
## spin rate vs horizontal/vertical movement for different pitches
ggplot(sample_n(pd_right, 1000), aes(x = release_spin_rate,
                       y = pfx_x, color = pitch_type)) +
  geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE) + Static_Poster_Theme +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(title = "More Spin Makes Off-Speed Pitches Break More",
       subtitle = "Fastballs don't appear to have this same property",
       x = "Spin Rate (RPM)", y = "Horizontal Movement (inches)", color = "Pitch Type")
ggsave("srate_vs_horizmovement.png", dpi = 300)

#########################################

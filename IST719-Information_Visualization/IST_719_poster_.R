player_total <- read.csv("C:\\Users\\daum1\\Downloads\\archive (11)\\players\\players_total.csv")

library(ggplot2)
library(tidyverse)
library(forcats)



average_age <- mean(passing$Age)

# average age of the teams

str(player_total)
summary(player_total)


ggplot(team, aes(x = Squad, y = Age, fill = Squad)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_hline(yintercept = average_age, color = "red", linetype = "dashed", size = 1.5) +
  labs(title = "Bar Plot of Team Members' Ages", y = "Age", x = "Squad") +
  theme_minimal() +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  annotate("text", x = 1, y = average_age, label = paste("Average Age: ", average_age), vjust = -1.5) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0, 50)) 


# horizantal 

ggplot(team, aes(x = Squad, y = Age, fill = Squad)) +
     geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
     coord_flip() +  
     geom_hline(yintercept = average_age, color = "red", linetype = "dashed", size = 1.5) +
     labs(title = "Bar Plot of Team Members' Ages", y = "Age", x = "Squad") +
     theme_minimal() +
     theme(text = element_text(size=12)) +
     annotate("text", x = 1, y = average_age, label = paste("Average Age: ", average_age), vjust = -1.5) +
     scale_y_continuous(breaks = c(0, 15,30), limits = c(0, 40)) 

# sort it by age / it can be used as one of my plot
team <- team %>%
  mutate(Squad = fct_reorder(Squad, Age))

ggplot(team, aes(x = Squad, y = Age)) +
  geom_bar(stat = "identity", color = "black", fill = "lightgreen", show.legend = FALSE) +
  coord_flip() +  # Flip the bars to be horizontal
  geom_hline(yintercept = average_age, color = "red", linetype = "dashed", size = 1.5) +  # Add a horizontal line for the average age
  labs(title = "Squad's Ages", x = "Squad", y = "Age") +  
  theme_minimal() 

######################################################################################################################

# world map for the player

#install.packages("rnaturalearth", dependencies=TRUE)
#install.packages("rnaturalearthdata")

library(rnaturalearth)

#erase first three letter, now every nation information is written in three letter.

defensive <- defensive %>%
  mutate(
    Nation = substring(Nation, 4), 
    Nation = gsub(" ", "", Nation) 
  )



# Load world countries data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Count the number of players per country if needed
nation_counts <- defensive %>%
  group_by(Nation) %>%
  summarise(Count = n(), .groups = 'drop')

# Merge with world map data
world_data <- world %>%
  left_join(nation_counts, by = c("iso_a3" = "Nation"))

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

# plot 1 map
p <- ggplot(data = world_data) +
  geom_sf(aes(fill = Count), color = "white") +  # Fill countries based on the count of players
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "gray", guide = "colorbar", name = "Player Count") +
  labs(title = "Number of Players per Country",
       subtitle = "Data based on defensive$Nation",
       caption = "Source: defensive$Nation") +
  theme_minimal() +
  theme(
    #plot.background = element_rect(fill = "#034694", color = NA),
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")  
  ) +
  coord_sf() +
  plain

# Print the plot
print(p)
###############################################################################################
#players country distribution bar graph(top 20) plot2

nation_counts <- table(player_total$Nation)

# Convert the table to a dataframe
nation_counts_df <- as.data.frame(nation_counts)

# Rename the columns appropriately
names(nation_counts_df) <- c("Nation", "Count")

nation_counts_df$Nation <- sub("^.{3}\\s*", "", nation_counts_df$Nation)
#filter countries top 10 without england

nation_counts_without_eng <- nation_counts_df[nation_counts_df$Nation != "ENG", ]

top_nation_counts <- nation_counts_without_eng %>%
  arrange(desc(Count)) %>%  # Sort by Count in descending order
  slice_head(n = 20)

top_nation_counts <- top_nation_counts %>%
  mutate(Nation = factor(Nation, levels = Nation[order(-Count)]))


#Where players from? - national
ggplot(top_nation_counts, aes(x = Nation, y = Count, fill = Nation)) +
  geom_bar(stat = "identity", color = "black") +  
  labs(title = "Top 20 Countries withour England", x = "Nation", y = "Count") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "right") +  
  scale_fill_viridis_d() 

##############
# bubble chart
library(packcircles)
library(ggplot2)

packing <- circleProgressiveLayout(sqrt(top_nation_counts$Count), sizetype='radius')


top_nation_counts <- cbind(top_nation_counts, x = packing$x, y = packing$y, radius = packing$r)

# Convert the center + radius into coordinates of a circle
dat.gg <- circleLayoutVertices(packing, npoints=50)

custom_colors <- c("#340040", "#F2055C", "#07F2F2", "#05F26C", "#EAF205")

# Make the plot
library(colorspace)
total_groups <- length(unique(dat.gg$id))
expanded_colors <- colorRampPalette(custom_colors)(total_groups)

ggplot() +
  geom_polygon(data = dat.gg, aes(x = x, y = y, group = id, fill = as.factor(id)), colour = "black", alpha = 0.6) +
  geom_text(data = top_nation_counts, aes(x = x, y = y, label = Nation), size = 4, check_overlap = TRUE) +
  scale_fill_manual(values = expanded_colors) +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed()



###########################################################################################
# player distribution
# Used just one position indicated infront if player can play multi position

player_total_one_position <- player_total
player_total_one_position$Pos <- sub(",.*", "", player_total$Pos)

library(treemap)

# Aggregate data to count the number of players by position
position_counts <- player_total_one_position %>%
  group_by(Pos) %>%
  summarise(Count = n(), .groups = 'drop')  

position_counts$Label <- paste(position_counts$Pos, position_counts$Count)


color_palette <- colorRampPalette(c("#00ff85", "#38003C"))(length(unique(position_counts$Count)))

treemap(position_counts,
        index = c("Label"),  
        vSize = "Count",  
        title = "Number of Players by Position",
        fontsize.labels = 12,
        fontcolor.labels = "white",
        border.col = "white",
        palette = color_palette
)

library(treemapify)
library(ggfittext)



ggplot(position_counts, aes(area = Count, fill = Count, label = Label)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE, fontface = "bold", 
                    aes(label = paste(Label)), size = 15) + 
  scale_fill_gradient(low = "#d4b7d6", high = "#340040", name = "Player Count") +
  ggtitle("Number of Players by Position") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


############################################################################################
# multi-demensional
# Matches Played by Position and Age


player_total_one_position$AgeGroup <- cut(player_total_one_position$Age, breaks=c(15, 20, 25, 30, 35, 40), labels=c("16-20", "21-25", "26-30", "31-35", "36-40"))





###########################################################################################
# Poition, match_played_age Plot (plot)

# Define colors
custom_colors <- c("#340040", "#F2055C", "#07F2F2", "#05F26C", "#EAF205")


ggplot(player_total_one_position, aes(x = Pos, y = Match_played, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  labs(title = "Matches Played by Position and Age Group", x = "Position", y = "Matches Played") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  theme_minimal() +
  scale_y_continuous(limits = c(0, 40)) 


#########################################################################################

# total player points based on the important factor

player_point <- player_total[c("Player", "Pos", "Squad", "Tackle_Won", "Ball_Blocked", "Intercept",
                                "Clearances",
                               "Assist", "KeyPas", "Goals", "Tot_Shot", "Shot_On_Target")] #total_pass, Tackle_Intercept


player_point$goal_assist_keypass <- 3000*player_point$Goals + 2000*player_point$Assist + 1000*player_point$KeyPas





#normalize the data set

numeric_columns <- sapply(player_point, is.numeric)

# Normalize the numeric columns to range 0-1
point_normalized <- as.data.frame(lapply(player_point[, numeric_columns], function(x) (x - min(x)) / (max(x) - min(x))))

#different normalization Z-score
# player_point_normalized <- as.data.frame(lapply(player_point[, numeric_columns], function(x) (x - mean(x)) / sd(x)))


# Rename columns to indicate normalization
names(point_normalized) <- paste(names(point_normalized), "normalized", sep = "_")

player_point$normalized_point <- rowSums(point_normalized, na.rm = TRUE)

#leave only one position
player_point$Pos <- sub(",.*", "", player_point$Pos)


#top 10 fw / Mohamed Salah is the best player
top_fw_players <- player_point %>%
  filter(Pos == "FW") %>%
  arrange(desc(normalized_point)) %>%
  slice(1:10) 

ggplot(top_fw_players, aes(x = reorder(Player, normalized_point), y = normalized_point)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "Normalized Points for Forwards",
       x = "Player",
       y = "Normalized Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#top 10 mf / Bruno Femandes was the best
top_mf_players <- player_point %>%
  filter(Pos == "MF") %>%
  arrange(desc(normalized_point)) %>%
  slice(1:10) 

ggplot(top_mf_players, aes(x = reorder(Player, normalized_point), y = normalized_point)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "Normalized Points for Mid",
       x = "Player",
       y = "Normalized Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#top 10 df  James Tarkowski
top_df_players <- player_point %>%
  filter(Pos == "DF") %>%
  arrange(desc(normalized_point)) %>%
  slice(1:10) 

ggplot(top_df_players, aes(x = reorder(Player, normalized_point), y = normalized_point)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "Normalized Points for Defenders",
       x = "Player",
       y = "Normalized Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Top 20 players
top_players <- player_point[order(player_point$normalized_point, decreasing = TRUE), ][1:20,]

ggplot(top_players, aes(y = reorder(Player, -normalized_point), x = normalized_point)) +
  geom_bar(stat = "identity", fill = "#945798") +  
  labs(title = "Top 10 Players by Normalized Points",
       x = "Normalized Points",  
       y = "Player") +  
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))


ggplot(top_players, aes(x = reorder(Player, normalized_point), y = normalized_point)) +
  geom_bar(stat = "identity", fill = "steelblue") +  
  labs(title = "Top 10 Players by Normalized Points",
       x = "Player",
       y = "Normalized Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# making radar graph from the best player 


#install.packages("fmsb")
library(fmsb)
player_point <- player_total[c("Player", "Pos", "Squad", "Tackle_Won", "Ball_Blocked", "Intercept",
                               "Clearances",
                               "Assist", "KeyPas", "Goals", "Tot_Shot", "Shot_On_Target")] #total_pass, Tackle_Intercept


player_point$Goals <- 3000*player_point$Goals
player_point$Assist <- 2000*player_point$Assist 
player_point$KeyPas <- 1000*player_point$KeyPas


# point_normalized <- as.data.frame(lapply(player_point[, numeric_columns], function(x) (x - min(x)) / (max(x) - min(x))))

numeric_columns <- names(player_point[sapply(player_point, is.numeric)])


normalized_columns <- as.data.frame(lapply(player_point[, numeric_columns], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}))


player_point_normalized <- cbind(player_point[, !names(player_point) %in% numeric_columns], normalized_columns)



#################################
# Filtering the data for Mohamed Salah with normalized

library(fmsb)

player_point_normalized

# Best FW Salah
Salah <- player_point_normalized[player_point_normalized$Player == "Mohamed Salah", c("Goals", "Assist", "KeyPas", "Shot_On_Target", "Tackle_Won", "Ball_Blocked")]
Salah

#FW_avg
forwards_data <- player_point_normalized[player_point_normalized$Pos == "FW", ]

average_values_FW <- colMeans(forwards_data[c("Goals", "Assist", "KeyPas", "Shot_On_Target", "Tackle_Won", "Ball_Blocked")], na.rm = TRUE)

print(average_values_FW)

#Best MF Bruno Femanades
Fernandes <- player_point_normalized[player_point_normalized$Player == "Bruno Fernandes", c("Goals", "Assist", "KeyPas", "Shot_On_Target", "Tackle_Won", "Ball_Blocked")]
Fernandes


#MF_avg
forwards_data <- player_point_normalized[player_point_normalized$Pos == "MF", ]

average_values_MF <- colMeans(forwards_data[c("Goals", "Assist", "KeyPas", "Shot_On_Target", "Tackle_Won", "Ball_Blocked")], na.rm = TRUE)

print(average_values_MF)


#Best DF James Tarkowski
Tarkowski <- player_point_normalized[player_point_normalized$Player == "James Tarkowski", c("Goals", "Assist", "KeyPas", "Shot_On_Target", "Tackle_Won", "Ball_Blocked")]
Tarkowski

#MF_avg
denfender_data <- player_point_normalized[player_point_normalized$Pos == "DF", ]

average_values_DF <- colMeans(denfender_data[c("Goals", "Assist", "KeyPas", "Shot_On_Target", "Tackle_Won", "Ball_Blocked")], na.rm = TRUE)

print(average_values_DF)
 




############################################################################




salah <- data.frame(Goals = c(1,0,0.527778,0.11146273),
                   Assist = c(1,0,0.75,0.11827532),
                   KeyPas = c(1,0,0.5462185,0.13360281),
                   Shot_On_Target = c(1,0,0.8333333,0.20452414),
                   Tackle_Won = c(1,0,0.1445783,0.09768187),
                   Ball_Blocked = c(1,0,0.2121212,0.11929421),
                   row.names = c("max","min","Mohamed Salah","Forward Average")
                    )
# Setting column names for axis labels
colnames(salah) <- c("Goal", "Assist", "Key Pass", "Shooting", "Tackle", "Block")


colors_fill <- c(scales::alpha("#945798", 0.3),
                 scales::alpha("lightgreen", 0.5))

colors_line <- c(scales::alpha("purple", 0.6),
                scales::alpha("green", 0.6))

radarchart(salah,
           seg=6,
           title="Mohamed Salah VS Other Forwards",
           pfcol =  colors_fill,
           pcol = colors_line,
           plwd=4,
           plty = c(1, 1))

legend(x=0.6,
       y=1.35,
       legend = rownames(salah[-c(1,2),]),
       bty = "n", pch = 20, col = colors_line, cex = 1.2, pt.cex = 3) 

###########################################################################################
# MF


Femanades <- data.frame(Goals = c(1,0,0.2222222,0.04645323),
                    Assist = c(1,0,0.5,0.09922316),
                    KeyPas = c(1,0,1 ,0.14076817),
                    Shot_On_Target = c(1,0,0.5925926,0.10274116),
                    Tackle_Won = c(1,0,0.5060241,0.20556803),
                    Ball_Blocked = c(1,0,0.3737374,0.18558466),
                    row.names = c("max","min","Bruno Fernandes","Midfielder Average")
)
# Setting column names for axis labels
colnames(Femanades) <- c("Goal", "Assist", "Key Pass", "Shooting", "Tackle", "Block")


colors_fill <- c(scales::alpha("#945798", 0.3),
                 scales::alpha("lightgreen", 0.5))

colors_line <- c(scales::alpha("purple", 0.6),
                 scales::alpha("green", 0.6))

radarchart(Femanades,
           seg=6,
           title="Bruno Fernandes VS Other Midfielders",
           pfcol =  colors_fill,
           pcol = colors_line,
           plwd=4,
           plty = c(1, 1))

legend(x=0.6,
       y=1.35,
       legend = rownames(Femanades[-c(1,2),]),
       bty = "n", pch = 20, col = colors_line, cex = 1.2, pt.cex = 3) 
#############################################################################################

Tarkowski <- data.frame(Goals = c(1,0,0.02777778,0.01552707),
                        Assist = c(1,0,0.0625,0.04551282),
                        KeyPas = c(1,0,0.1260504 ,0.07153631),
                        Shot_On_Target = c(1,0,0.1666667,0.04074074),
                        Tackle_Won = c(1,0,0.313253,0.19925857),
                        Ball_Blocked = c(1,0,1,0.20637141),
                        row.names = c("max","min","James Tarkowski","Defender Average")
)
# Setting column names for axis labels
colnames(Tarkowski) <- c("Goal", "Assist", "Key Pass", "Shooting", "Tackle", "Block")


colors_fill <- c(scales::alpha("#945798", 0.3),
                 scales::alpha("lightgreen", 0.5))

colors_line <- c(scales::alpha("purple", 0.6),
                 scales::alpha("green", 0.6))

radarchart(Tarkowski,
           seg=6,
           title="James Tarkowski VS Other Defenders",
           pfcol =  colors_fill,
           pcol = colors_line,
           plwd=4,
           plty = c(1, 1))

legend(x=0.6,
       y=1.35,
       legend = rownames(Tarkowski[-c(1,2),]),
       bty = "n", pch = 20, col = colors_line, cex = 1.2, pt.cex = 3) 


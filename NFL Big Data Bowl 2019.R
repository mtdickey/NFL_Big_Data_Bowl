setwd("D:/NFL_Big_Data_Bowl")

library(tidyverse)
library(tidyselect)
library(gganimate)
library(cowplot)
library(dtwclust)
library(plotly)

### Download all files to drive
# for(i in 1:length(unique(games.sum$gameId))){
#   file.tracking <- paste0("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_", unique(games.sum$gameId)[i], ".csv")
#   tracking.example <- read_csv(file.tracking)
#   write.csv(tracking.example, file = paste0("tracking_gameId_", unique(games.sum$gameId)[i], ".csv"), row.names = F)
# }

### For isolating one play
## "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100110.csv"

## Game summary info
# "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
file.game <- "D:/NFL_Big_Data_Bowl/Data Provided/games.csv"
games.sum <- read_csv(file.game) 

## Play summary info
# "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
file.plays <- "D:/NFL_Big_Data_Bowl/Data Provided/plays.csv"
plays.sum <- read_csv(file.plays) 

## Play summary info
# "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
file.players <- "D:/NFL_Big_Data_Bowl/Data Provided/players.csv"
players.sum <- read_csv(file.players) 

### Chargers games 
chargers.games <- unique(plays.sum$gameId[which(plays.sum$possessionTeam=="LAC")])
for(i in 1:length(chargers.games)){
  if(i == 1){
    tracking <- read_csv(paste0("D:/NFL_Big_Data_Bowl/Data Provided/tracking_gameId_", 
                                chargers.games[i]
                                ,".csv"))
  } else{
    tracking.new <- read_csv(paste0("D:/NFL_Big_Data_Bowl/Data Provided/tracking_gameId_", 
                                chargers.games[i]
                                ,".csv"))
    tracking <- rbind(tracking, tracking.new)
  }
}
rm(tracking.new)

### Merged all together
tracking.merged <- tracking %>% inner_join(games.sum) %>% inner_join(plays.sum) %>% left_join(players.sum)

#example.play <- tracking.merged %>% filter(playId == 1363 & gameId == 2017100110)
pass.arrive.time.ex <- 58
example.play <- tracking.merged %>% filter(playId == 3567 & gameId == 2017100110 & frame.id <= pass.arrive.time.ex)

example.play %>% select(playDescription) %>% slice(1)


## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play <- ggplot() +
  geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                      colour = team, group = nflId, pch = team, size = team)) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber, group = nflId), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  geom_path(data = (example.play #%>% filter(nflId == "2540154")
                    ), aes(x = xmax-y, y = x, alpha = ifelse(nflId == 2540154 & !is.na(nflId), "yes","no")), size = 1, color = "red") +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_alpha_manual(values = c("yes" = .8, "no" = 0), guide = FALSE) + 
  scale_colour_manual(values = c("#004c55", "#654321", "#0073cf"),
                      guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_reveal(frame.id)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
#play.length.ex <- length(unique(example.play$frame.id))
animate(animate.play, fps = 10, nframe = pass.arrive.time.ex) #play.length.ex)


#### Time series clustering example on 1 play ####
### Isolate the movement from ball snap to pass arriving for all receivers
ball.snap.time <- as.integer((example.play %>% filter(event == "ball_snap") %>% summarise(time = min(frame.id)))[1])
pass.arrive.time <- as.integer((example.play %>% filter(event == "pass_arrived") %>% summarise(time = min(frame.id)))[1])
routes <- example.play %>% filter(PositionAbbr %in% c("WR", "TE") & frame.id >= ball.snap.time & frame.id <= pass.arrive.time)
route.start.positions <- routes %>% filter(frame.id == ball.snap.time & PositionAbbr %in% c("WR", "TE")) %>%
                                    group_by(nflId, displayName) %>%
                                    summarise(x.start = min(x),
                                              y.start = min(y))
routes <- routes %>% inner_join(route.start.positions, by = c("nflId", "displayName"))
routes$x.trans <- routes$x - routes$x.start
routes$y.trans <- routes$y - routes$y.start
routes$y.trans.abs <- abs(routes$y - routes$y.start)
routes %>% ggplot(aes(x = x.trans, y = y.trans)) + geom_line(aes(color = displayName), size = 1) + theme_grey()
routes %>% ggplot(aes(x = x.trans, y = y.trans.abs)) + geom_line(aes(color = displayName), size = 1) + theme_grey()


### Multivariate timeseries clustering using dynamic time warping package (dtwclust)
routesMV <- list(Keenan = matrix(data = c(routes$x.trans[which(routes$displayName == "Keenan Allen")],
                                          routes$y.trans.abs[which(routes$displayName == "Keenan Allen")]), 
                                 nrow = length(which(routes$displayName == "Keenan Allen")), ncol = 2),
                 Tyrell = matrix(data = c(routes$x.trans[which(routes$displayName == "Tyrell Williams")],
                                          routes$y.trans.abs[which(routes$displayName == "Tyrell Williams")]), 
                                 nrow = length(which(routes$displayName == "Hunter Henry")), ncol = 2),
                 Hunter = matrix(data = c(routes$x.trans[which(routes$displayName == "Hunter Henry")],
                                          routes$y.trans.abs[which(routes$displayName == "Hunter Henry")]), 
                                 nrow = length(which(routes$displayName == "Hunter Henry")), ncol = 2),
                 Sean   = matrix(data = c(routes$x.trans[which(routes$displayName == "Sean McGrath")],
                                          routes$y.trans.abs[which(routes$displayName == "Sean McGrath")]), 
                                 nrow = length(which(routes$displayName == "Sean McGrath")), ncol = 2))
# Multivariate series provided as a list of matrices, using GAK distance
mvc <- tsclust(routesMV, k = 2L, distance = "gak", seed = 8675309L)
# Note how the variables of each series are appended one after the other in the plot
plot(mvc, labels = list(nudge_x = -10, nudge_y = 1))  ## x then abs y

### Keenan and Tyrell's routes are put into group 2 (longer routes), while Hunter/Sean are put into short screen/blocking routes

#### Use Time series clustering on all chargers plays ####
### Plays of interest: chargers with the ball, pass play, no penalties/sacks.  
# Focus on time from snap to ball delivered to receiver.
chargers.passes <- plays.sum %>% filter(gameId %in% chargers.games &
                                       isPenalty == F &
                                       possessionTeam == "LAC" &
                                       PassResult %in% c("C", "I", "IN"))


chargers.tracking.merged <- tracking %>% inner_join(games.sum) %>% inner_join(chargers.passes) %>% inner_join(players.sum)
chargers.tracking.receivers <- chargers.tracking.merged %>% filter(PositionAbbr %in% c("WR", "TE"))
chargers.tracking.defense <- chargers.tracking.merged %>% filter(team != "LAC")

### Get location of the ball with tracking data
ball.tracking <- tracking %>% inner_join(games.sum) %>% inner_join(chargers.passes) %>% left_join(players.sum) %>% filter(is.na(nflId))


### Get ball snap/pass arrive times to determine which frames to focus on,
ball.snap.times <- chargers.tracking.receivers %>% filter(event == "ball_snap") %>% group_by(playId, gameId) %>% summarise(ball.snap.time = min(frame.id))
pass.arrive.times <- chargers.tracking.receivers %>% filter(event == "pass_arrived") %>% group_by(playId, gameId) %>% summarise(pass.arrive.time = min(frame.id))
chargers.tracking.receivers <- chargers.tracking.receivers %>% inner_join(pass.arrive.times) %>% inner_join(ball.snap.times)
routes <- chargers.tracking.receivers %>% filter(frame.id >= ball.snap.time & frame.id <= pass.arrive.time)

### Get the position on the field when the ball was snapped
route.start.positions <- routes %>% filter(frame.id == ball.snap.time) %>%
  group_by(nflId, gameId, displayName, playId) %>%
  summarise(x.start = min(x),
            y.start = min(y))
routes <- routes %>% inner_join(route.start.positions, by = c("nflId", "gameId", "displayName", "playId"))

### Get the location of the ball when the pass arrived to determine who was targeted
ball.location.arrive <- ball.tracking %>% inner_join(pass.arrive.times, by = c("playId" = "playId", "gameId" = "gameId",
                                                                               "frame.id" = "pass.arrive.time")) %>% 
                        mutate(ball.x = x,
                               ball.y = y) %>%
                        select(one_of("ball.x","ball.y","playId","gameId", "frame.id"))
routes <- routes %>% left_join(ball.location.arrive)

target.distances <- routes %>% filter(event == "pass_arrived") %>% 
            mutate(ball.distance = sqrt( (x-ball.x)^2 + (y-ball.y)^2 )) %>% 
            group_by(playId, gameId, frame.id) %>% summarise(min.ball.distance = min(ball.distance))

routes <- routes %>% left_join(target.distances)
receivers.targets <- routes %>% mutate(ball.distance = sqrt( (x-ball.x)^2 + (y-ball.y)^2 )) %>%
           mutate(target_ind = ifelse(ball.distance == min.ball.distance & ball.distance < 5, 1, 0)) %>%
           group_by(nflId, gameId, playId) %>% summarise(target_ind = max(target_ind, na.rm = T))
receivers.targets$target_ind[which(receivers.targets$target_ind == -Inf)] <- 0
routes <- routes %>% left_join(receivers.targets)

### Get defense's average x starting positions at the ball snap 
defense.start.positions <- chargers.tracking.defense %>% filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarise(defense.x.start = mean(x),
            defense.y.start = mean(y))

routes <- routes %>% inner_join(defense.start.positions)

### Transform X and Y differences so that: 
##    x is relative to yards past the line of scrimmage and
##    y is yards from the nearest sideline
routes$x.trans <- ifelse(routes$defense.x.start < routes$x.start, -1*(routes$x - routes$x.start), routes$x - routes$x.start)
routes$y.trans <- ifelse(routes$y.start < 26.65,  ### if they're on the bottom side of the field
                         routes$y - 0,  ### use the value of y to determine distance from bottom sideline
                         53.3 - routes$y) ## otherwise, determine distance to top sideline by subtracting y from 53.3
#routes$y.trans.abs <- abs(routes$y - routes$y.start)

### Get a grid for the new layout
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 53.3), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)


### Plot it
routes %>% arrange(frame.id) %>% ggplot(aes(x = y.trans, y = x.trans)) + 
  annotate("rect", xmin = 0, xmax = 53.3, ymin = -10, ymax = -5, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = -5, ymax = 0, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 0, ymax = 5, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 5, ymax = 10, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 10, ymax = 15, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 15, ymax = 20, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 20, ymax = 25, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 25, ymax = 30, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 30, ymax = 35, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 35, ymax = 40, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 40, ymax = 45, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 45, ymax = 50, fill = "#74c476", alpha = .5, color = "black") +
  geom_path(aes(group = paste0(displayName, " ", gameId, " ", playId)), size = 1, alpha = .2) + 
  scale_color_manual() +
  scale_y_continuous(breaks = seq(-10,50,by=10)) +
  labs(x = "\nYards from Closest Sideline at Start of Play",
       y = "Yards Downfield from LOS\n",
       title = "Chargers Pass Routes - 2017 Season, Weeks 1-6\n") + 
  guides(color = F) + 
  theme(line = element_blank(),
        axis.title = element_text(size = 12))


### Multivariate timeseries clustering using dynamic time warping package (dtwclust)

routes <- routes %>% mutate(routeId = paste0(nflId, "-", gameId,"-", playId)) 
## Adding routeId variable: there are 664 unique routes that we will need stored in a named list of matrices

### Get each route into matrix form and store in a list with the "RouteID" as the name
routes_ts_mv <- list()
for(route in unique(routes$routeId)){
  mat <- matrix(data = c(routes$x.trans[which(routes$routeId == route)],
                         routes$y.trans[which(routes$routeId == route)]), 
                nrow = length(which(routes$routeId == route)), ncol = 2)
  routes_ts_mv[[route]] <- mat
}

### Multivariate series provided as a list of matrices, using GAK distance
mvc <- tsclust(routes_ts_mv, k = 15L, distance = "gak", seed = 8675309L) ## 10 route types
# Store results in a data frame and join to routes df
clust_df <- data.frame(cluster = mvc@cluster, routeId = names(mvc@datalist), stringsAsFactors = F)
routes <- routes %>% inner_join(clust_df)

routes <- routes %>% mutate(cluster_name = ifelse(cluster == 1, paste0(cluster, ": ",  "Straight (5-15 yds)"),
                                           ifelse(cluster == 2, paste0(cluster, ": ",  "Mixed (15-30 yds)"),
                                           ifelse(cluster == 3, paste0(cluster, ": ",  "Straight (5-10 yds)"),
                                           ifelse(cluster == 4, paste0(cluster, ": ",  "Cross (5-15 yds)"),
                                           ifelse(cluster == 5, paste0(cluster, ": ",  "Outs (10-20 yds)"),
                                           ifelse(cluster == 6, paste0(cluster, ": ",  "Cross (10-20 yds)"),
                                           ifelse(cluster == 7, paste0(cluster, ": ",  "Fades (10-20 yds)"),
                                           ifelse(cluster == 8, paste0(cluster, ": ",  "Slants (5-15 yds)"),
                                           ifelse(cluster == 9, paste0(cluster, ": ",  "Mixed (5-15 yds)"),
                                           ifelse(cluster == 10, paste0(cluster, ": ", "Fades (5-15 yds)"),
                                           ifelse(cluster == 11, paste0(cluster, ": ", "Cross (5-15 yds)"),
                                           ifelse(cluster == 12, paste0(cluster, ": ", "Deep Fades (20-50 yds)"),
                                           ifelse(cluster == 13, paste0(cluster, ": ", "Straight (5-15 yds)"),
                                           ifelse(cluster == 14, paste0(cluster, ": ", "Curls (5-10 yds)"),
                                           ifelse(cluster == 15, paste0(cluster, ": ", "Sideline Cross (10-20 yds)"), NA
                                           ))))))))))))))))

routes$cluster_name_f <- factor(routes$cluster_name, levels=c("1: Straight (5-15 yds)", "2: Mixed (15-30 yds)", "3: Straight (5-10 yds)",
                                                              "4: Cross (5-15 yds)", "5: Outs (10-20 yds)", "6: Cross (10-20 yds)", 
                                                              "7: Fades (10-20 yds)", "8: Slants (5-15 yds)", "9: Mixed (5-15 yds)",
                                                              "10: Fades (5-15 yds)", "11: Cross (5-15 yds)", "12: Deep Fades (20-50 yds)",
                                                              "13: Straight (5-15 yds)", "14: Curls (5-10 yds)", "15: Sideline Cross (10-20 yds)"),
                                labels=c("1: Straight (5-15 yds)", "2: Mixed (15-30 yds)", "3: Straight (5-10 yds)",
                                         "4: Cross (5-15 yds)", "5: Outs (10-20 yds)", "6: Cross (10-20 yds)", 
                                         "7: Fades (10-20 yds)", "8: Slants (5-15 yds)", "9: Mixed (5-15 yds)",
                                         "10: Fades (5-15 yds)", "11: Cross (5-15 yds)", "12: Deep Fades (20-50 yds)",
                                         "13: Straight (5-15 yds)", "14: Curls (5-10 yds)", "15: Sideline Cross (10-20 yds)"))

routes %>% arrange(frame.id) %>% ggplot(aes(x = y.trans, y = x.trans)) + facet_wrap(~cluster_name_f, ncol = 5) +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = -10, ymax = -5, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = -5, ymax = 0, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 0, ymax = 5, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 5, ymax = 10, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 10, ymax = 15, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 15, ymax = 20, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 20, ymax = 25, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 25, ymax = 30, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 30, ymax = 35, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 35, ymax = 40, fill = "#74c476", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 40, ymax = 45, fill = "#005a32", alpha = .5, color = "black") +
  annotate("rect", xmin = 0, xmax = 53.3, ymin = 45, ymax = 50, fill = "#74c476", alpha = .5, color = "black") +
  geom_path(aes(group = paste0(displayName, " ", gameId, " ", playId)), size = 1, alpha = .2) + 
  scale_color_manual() +
  scale_y_continuous(breaks = seq(-10,50,by=10)) +
  labs(x = "\nYards from Closest Sideline at Start of Play",
       y = "Yards Downfield from LOS\n",
       title = "Chargers Pass Routes - 2017 Season, Weeks 1-6\n",
       subtitle = "Clustered into 15 Route Types") + 
  guides(color = F) + 
  theme(line = element_blank(),
        axis.title = element_text(size = 12),
        plot.subtitle = element_text(size = 13, hjust = 0.5))


### Success rates of route types
routes %>% filter(target_ind == 1) %>%
  group_by(routeId, cluster_name, PassResult, PlayResult, down, yardsToGo) %>%
  summarise(complete_ind = max(ifelse(PassResult == "C", 1, 0))) %>% 
  group_by(cluster_name) %>% 
  summarise(cnt = length(unique(routeId)),
            complete_pct = mean(complete_ind),
            avg_yds = mean(PlayResult),
            success_rate = mean(
                             ifelse(down == 3 & PlayResult < yardsToGo, 0,
                             ifelse(down == 3 & PlayResult >= yardsToGo, 1,
                             ifelse(down == 2 & PlayResult < yardsToGo/2, 0,
                             ifelse(down == 2 & PlayResult >= yardsToGo/2, 1,
                             ifelse(down == 1 & PlayResult < yardsToGo/3, 0,
                             ifelse(down == 1 & PlayResult >= yardsToGo/3, 1,
                             ifelse(down == 4 & PlayResult < yardsToGo, 0,
                             ifelse(down == 4 & PlayResult >= yardsToGo, 1, NA))))))))
                           )) %>% 
  arrange(-success_rate)


### Success rates of route types AND Receiver
routes %>% filter(target_ind == 1) %>%
  group_by(routeId, cluster_name, displayName, PassResult, PlayResult, down, yardsToGo) %>%
  summarise(complete_ind = max(ifelse(PassResult == "C", 1, 0))) %>% 
  group_by(cluster_name, displayName) %>% 
  summarise(cnt = length(unique(routeId)),
            complete_pct = mean(complete_ind),
            avg_yds = mean(PlayResult),
            success_rate = mean(
              ifelse(down == 3 & PlayResult < yardsToGo, 0,
              ifelse(down == 3 & PlayResult >= yardsToGo, 1,
              ifelse(down == 2 & PlayResult < yardsToGo/2, 0,
              ifelse(down == 2 & PlayResult >= yardsToGo/2, 1,
              ifelse(down == 1 & PlayResult < yardsToGo/3, 0,
              ifelse(down == 1 & PlayResult >= yardsToGo/3, 1,
              ifelse(down == 4 & PlayResult < yardsToGo, 0,
              ifelse(down == 4 & PlayResult >= yardsToGo, 1, NA))))))))
            )) %>% filter(cnt > 2) %>%
  arrange(-success_rate)


### Target rates of route types AND Receiver
routes %>% 
  group_by(routeId, cluster_name, displayName, PassResult, PlayResult, down, yardsToGo, target_ind) %>%
  summarise(complete_ind = max(ifelse(PassResult == "C", 1, 0))) %>% 
  group_by(cluster_name, displayName) %>% 
  summarise(cnt = length(unique(routeId)),
            target_cnt = sum(ifelse(target_ind == 1, 1, 0)),
            complete_pct = mean(ifelse(complete_ind == 1, 1, 0), na.rm = T),
            avg_yds = mean(ifelse(target_ind == 1, PlayResult, NA), na.rm = T),
            success_rate = mean(
              ifelse(down == 3 & PlayResult < yardsToGo & target_ind == 1, 0,
              ifelse(down == 3 & PlayResult >= yardsToGo & target_ind == 1, 1,
              ifelse(down == 2 & PlayResult < yardsToGo/2 & target_ind == 1, 0,
              ifelse(down == 2 & PlayResult >= yardsToGo/2 & target_ind == 1, 1,
              ifelse(down == 1 & PlayResult < yardsToGo/3 & target_ind == 1, 0,
              ifelse(down == 1 & PlayResult >= yardsToGo/3 & target_ind == 1, 1,
              ifelse(down == 4 & PlayResult < yardsToGo & target_ind == 1, 0,
              ifelse(down == 4 & PlayResult >= yardsToGo & target_ind == 1, 1, NA)))))))), na.rm = T),
            target_rate = mean(target_ind)
            ) %>% filter(target_cnt > 2) %>%
  arrange(-success_rate)



### Which receiver ran the most types of routes?
routes %>% group_by(displayName) %>% summarise(route_cnt = length(unique(cluster_name))) %>% arrange(-route_cnt)
routes %>% group_by(displayName) %>% summarise(route_cnt = length(unique(routeId))) %>% arrange(-route_cnt)
### Top 3 receivers in routes had all 15 route types
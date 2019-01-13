setwd("D:/NFL_Big_Data_Bowl")

library(tidyverse)
library(gganimate)
library(cowplot)
library(dtwclust)

### Download all files to drive
for(i in 1:length(unique(games.sum$gameId))){
  file.tracking <- paste0("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_", unique(games.sum$gameId)[i], ".csv")
  tracking.example <- read_csv(file.tracking)
  write.csv(tracking.example, file = paste0("tracking_gameId_", unique(games.sum$gameId)[i], ".csv"), row.names = F)
}

### For isolating one play
file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017100110.csv"
tracking.example <- read_csv(file.tracking)

## Game summary info
file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

## Play summary info
file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays) 

## Play summary info
file.players <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
players.sum <- read_csv(file.players) 

### Merged all together
tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) %>% inner_join(players.sum)

example.play <- tracking.example.merged %>% filter(playId == 1363)

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
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_colour_manual(values = c("#004c55", "#654321",
                                 "#0073cf"),
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
  transition_time(frame.id)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))
animate(animate.play, fps = 10, nframe = play.length.ex)


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
library(tidyverse)
library(WinProbability)

## Assumes Retrosheet Play-by-Play files are in current directory
d2018 <- compute.runs.expectancy(2018)
d2017 <- compute.runs.expectancy(2017)
d2016 <- compute.runs.expectancy(2016)
d2015 <- compute.runs.expectancy(2015)
d2014 <- compute.runs.expectancy(2014)
d2013 <- compute.runs.expectancy(2013)
d2012 <- compute.runs.expectancy(2012)
d2011 <- compute.runs.expectancy(2011)
d2010 <- compute.runs.expectancy(2010)
d2009 <- compute.runs.expectancy(2009)

data <- rbind(d2009, d2010, d2011, d2012, d2013,
              d2014, d2015, d2016, d2017, d2018)
data <- compute.win.probs(data)

## Calculating average WE Swing
WE_swing <- mean(abs(data$WPA))

WPA_table <- data %>%
  select(INN_CT, BAT_HOME_ID, STATE, AWAY_SCORE_CT, HOME_SCORE_CT, WPA) %>%
  mutate(SCORE_DIFF = HOME_SCORE_CT - AWAY_SCORE_CT)

## Computing LI for different situations
LI_table <- WPA_table %>%
  group_by(INN_CT, BAT_HOME_ID, STATE, SCORE_DIFF) %>%
  summarize(Count = n(), LI = mean(abs((WPA))) / WE_swing)

## -------------------------------------------------------------------------

## Download 2018 Steamer projections
steamer18 <- "http://www.steamerprojections.com/images/projections/steamer_hitters_2018_ros_preseason_final.csv"
pre2018 <- read_csv(steamer18) ##%>% select(steamerid, firstname, lastname, PA, wRC)

## Calculate coefficients needed
Lg_R_PA <- sum(d2018$RUNS.SCORED) / sum(d2018$BAT_EVENT_FL)
AL_wRC <- 10995/91924
NL_wRC <- 10757/88080
MLB_wRC <- (10995+10757)/(91924+88080)

## Identify Player's League
AL <- c("HOU","LAA","DET","TOR","OAK","BOS","TEX","TBA",
        "KCA","SEA","CLE","MIN","CHA","BAL","NYA")
NL <- c("MIL","ARI","SDN","MIA","CIN","SFN","PHI","SLN",
        "LAN","NYN","PIT","ATL","COL","WAS","CHN")

pre2018 <- pre2018 %>%
  mutate(Lg = ifelse(Team %in% AL, 'AL',
                     ifelse(Team %in% NL, 'NL', 'MLB')),
         Lg_wRC = ifelse(Lg == 'AL', AL_wRC,
                         ifelse(Lg == 'NL', NL_wRC, MLB_wRC)))

## Calculate Weighted Runs Created Plus
pre2018 <- pre2018 %>%
  mutate(wRC_plus = ((wRAA/PA + Lg_R_PA) + 
                       (Lg_R_PA - (BasicPF * Lg_R_PA))) / Lg_wRC * 100)

## -------------------------------------------------------------------------

## Importing Steamer Projections
projected <- pre2018 %>%
  mutate(Name = paste(firstname, lastname)) %>%
  select(steamerid, Name, Team, bats, wRC_plus)
  
play2018 <- read_csv("play2018.csv") %>% select(Name, Team, PA, playerid)
play2018$playerid <- as.character(play2018$playerid)

projected <- left_join(projected, play2018,
                       by = c('steamerid' = 'playerid'))

## Identify 2018 Rookies
different <- projected %>% filter(is.na(PA), nchar(steamerid) > 6)

'%not%' <- Negate('%in%')
changed <- play2018 %>% filter(playerid %not% projected$steamerid)

## Changed team during season
changed$Team[changed$Name == 'Rosell Herrera'] <- "Reds"
changed$Team[changed$Name == 'Johnny Field'] <- "Rays"
changed$Team[changed$Name == 'Austin Meadows'] <- "Pirates"
changed$Team[changed$Name == 'Billy McKinney'] <- "Yankees"
changed$Team[changed$Name == 'John Andreoli'] <- "Mariners"

## Changed team before season
changed$Team[changed$Name == 'Abiatal Avelino'] <- "Yankees"
changed$Team[changed$Name == 'Michael Perez'] <- "Diamondbacks"
changed$Team[changed$Name == 'Joe Hudson'] <- "Reds"

## Changed name
changed$Name[changed$Name == 'Ronald Acuna Jr.'] <- "Ronald Acuna"
changed$Name[changed$Name == 'Lourdes Gurriel Jr.'] <- "Lourdes Gurriel"
changed$Name[changed$Name == 'Cedric Mullins II'] <- "Cedric Mullins"
changed$Name[changed$Name == 'Adolis Garcia'] <- "Jose Adolis Garcia"
changed$Name[changed$Name == 'Jose Fernandez'] <- "Jose Miguel Fernandez"

## Join with projections
teams <- data.frame(team_name = c(unique(play2018$Team)),
                    initial = c("PIT","SFN","CHA","TOR","OAK","KCA",
                                "ATL","LAA","HOU","BOS",NA,"MIL","LAN",
                                "CLE","NYN","PHI","WAS","ARI","SLN",
                                "CIN","TEX","NYA","BAL","CHN","MIN",
                                "COL","MIA","DET","SEA","SDN","TBA"))

changed <- left_join(changed, teams, by = c("Team" = "team_name"))

changed <- left_join(changed, different,
                     by = c("Name" = "Name.x", "initial" = "Team.x"))

## One player without projections
still_diff <- changed %>% filter(is.na(wRC_plus))

changed$bats[changed$Name == 'Corban Joseph'] <- "L"
changed$wRC_plus[changed$Name == 'Corban Joseph'] <- 54

## Merge back to original dataset
changed <- changed %>% select(playerid, Name, bats, wRC_plus)
projected <- projected %>% filter(!is.na(PA)) %>%
  select(steamerid, Name.x, bats, wRC_plus)
names(projected) <- c("playerid","Name","bats","wRC_plus")

projected <- rbind(projected, changed)
projected$playerid <- as.double(projected$playerid)

## -------------------------------------------------------------------------

## Importing platoon splits data
career <- read_csv("career.csv") %>% select(Name, PA, 'wRC+', playerid)
names(career) <- c("Name","PA","wRC","playerid")

vsRight <- read_csv("vsRight.csv") %>% select(Name, PA, 'wRC+', playerid)
names(vsRight) <- c("Name","PA_R","wRC_R","playerid")

vsLeft <- read_csv("vsLeft.csv") %>% select(Name, PA, 'wRC+', playerid)
names(vsLeft) <- c("Name","PA_L","wRC_L","playerid")

platoon <- inner_join(vsRight, vsLeft, by = c("Name", "playerid"))
career <- right_join(career, platoon, by = c("Name", "playerid"))
career <- career %>% mutate(split_R = wRC_R - wRC, split_L = wRC_L - wRC)
splits <- career %>% select(playerid, PA, split_R, PA_R, split_L, PA_L)

## Add splits data to projections
proj_split <- left_join(projected, splits, by = "playerid")

## Separating players at 1000 career PA
thousand <- proj_split %>% filter(PA_L >= 1000)
less_1k <- proj_split %>% filter(PA_L < 1000 | is.na(PA))

## League platoon splits
vsLasL <- read_csv("vsLasL.csv") %>% select(Season, 'wRC+')
vsRasL <- read_csv("vsRasL.csv") %>% select(Season, 'wRC+')
vsLasR <- read_csv("vsLasR.csv") %>% select(Season, 'wRC+')
vsRasR <- read_csv("vsRasR.csv") %>% select(Season, 'wRC+')

asLeft <- left_join(vsLasL, vsRasL, by = 'Season')
names(asLeft) <- c("Season", "wRC_L", "wRC_R")
asLeft <- asLeft %>%
  mutate(bats = 'L', split_R = wRC_R - 100, split_L = wRC_L - 100)

asRight <- left_join(vsLasR, vsRasR, by = 'Season')
names(asRight) <- c("Season", "wRC_L", "wRC_R")
asRight <- asRight %>%
  mutate(bats = 'R', split_R = wRC_R - 100, split_L = wRC_L - 100)

Lg_split <- rbind(asLeft, asRight) %>% group_by(bats) %>%
  summarize(split_L = round(mean(split_L), 1),
            split_R = round(mean(split_R), 1))

less_1k$split_R[less_1k$bats == 'R'] <- Lg_split$split_R[Lg_split$bats == 'R']
less_1k$split_L[less_1k$bats == 'R'] <- Lg_split$split_L[Lg_split$bats == 'R']
less_1k$split_R[less_1k$bats == 'L'] <- Lg_split$split_R[Lg_split$bats == 'L']
less_1k$split_L[less_1k$bats == 'L'] <- Lg_split$split_L[Lg_split$bats == 'L']
less_1k$split_R[less_1k$bats == 'B'] <- 0
less_1k$split_L[less_1k$bats == 'B'] <- 0

## Merge data back together
proj_split <- rbind(thousand, less_1k) %>%
  mutate(wRC_R = wRC_plus + split_R, wRC_L = wRC_plus + split_L)

## Add Retrosheet ID to data
ID_Map <- read_csv("http://crunchtimebaseball.com/master.csv")

fg_retro <- ID_Map %>% select(fg_id, retro_id) %>% filter(nchar(fg_id) < 6)
fg_retro$fg_id <- as.double(fg_retro$fg_id)

proj_split <- left_join(proj_split, fg_retro, by = c("playerid" = "fg_id"))

## -------------------------------------------------------------------------

d2018 <- compute.win.probs(d2018)

## Calculate Leverage Index for 2018 Season
d2018 <- d2018 %>% mutate(SCORE_DIFF = HOME_SCORE_CT - AWAY_SCORE_CT)
d2018 <- left_join(d2018, LI_table, 
                   by = c("INN_CT","BAT_HOME_ID","STATE","SCORE_DIFF"))

## Add players' projected wRC+
d2018 <- left_join(d2018, proj_split, by = c("BAT_ID" = "retro_id"))

## Add pitchers' wRC+
d2018$wRC_plus[is.na(d2018$playerid)] <- -20
d2018$wRC_R[is.na(d2018$playerid)] <- -20
d2018$wRC_L[is.na(d2018$playerid)] <- -20

## -------------------------------------------------------------------------

d2018 <- d2018 %>% filter(BAT_EVENT_FL == TRUE) %>%
  arrange(BAT_HOME_ID, GAME_ID, INN_CT) %>%
  mutate(wRC_p3 = NA, wRC_R3 = NA, wRC_L3 = NA)


for (n in 1:nrow(d2018)){
  if (n %in% c(nrow(d2018), nrow(d2018)-1)){
    d2018$wRC_p3[n] = (d2018$wRC_plus[n] + d2018$wRC_plus[n-8] + d2018$wRC_plus[n-7]) / 3
    d2018$wRC_R3[n] = (d2018$wRC_R[n] + d2018$wRC_R[n-8] + d2018$wRC_R[n-7]) / 3
    d2018$wRC_L3[n] = (d2018$wRC_L[n] + d2018$wRC_L[n-8] + d2018$wRC_L[n-7]) / 3
  } else if (d2018$EVENT_ID[n] > d2018$EVENT_ID[n+2]){
    d2018$wRC_p3[n] = (d2018$wRC_plus[n] + d2018$wRC_plus[n-8] + d2018$wRC_plus[n-7]) / 3
    d2018$wRC_R3[n] = (d2018$wRC_R[n] + d2018$wRC_R[n-8] + d2018$wRC_R[n-7]) / 3
    d2018$wRC_L3[n] = (d2018$wRC_L[n] + d2018$wRC_L[n-8] + d2018$wRC_L[n-7]) / 3
  } else {
    d2018$wRC_p3[n] = (d2018$wRC_plus[n] + d2018$wRC_plus[n+1] + d2018$wRC_plus[n+2]) / 3
    d2018$wRC_R3[n] = (d2018$wRC_R[n] + d2018$wRC_R[n+1] + d2018$wRC_R[n+2]) / 3
    d2018$wRC_L3[n] = (d2018$wRC_L[n] + d2018$wRC_L[n+1] + d2018$wRC_L[n+2]) / 3
  }
}

## Calculate LI Plus of each PA
d2018 <- d2018 %>%
  mutate(LI_plus = LI * wRC_plus / 100, LI_R = LI * wRC_R / 100,
         LI_L = LI * wRC_L /100, LI_p3 = LI * wRC_p3 / 100,
         LI_R3 = LI * wRC_R3 / 100, LI_L3 = LI * wRC_L3 /100)

d2018 <- d2018 %>% mutate(hand_1 = LI_L - LI_R, hand_3 = LI_L3 - LI_R3)

d2018 <- d2018 %>% 
  mutate_at(vars(RUNS.STATE, RUNS.NEW.STATE, RUNS.VALUE, P.OLD,
                 P.NEW, WPA, LI, wRC_plus, wRC_R, wRC_L, wRC_p3,
                 wRC_R3, wRC_L3, LI_plus, LI_R, LI_L, LI_p3,
                 LI_R3, LI_L3, hand_1, hand_3), funs(round(., 2)))

## ------------------------------------------------------------------------

## Looking at average LI & LI+ during a game
d2018 <- d2018 %>% mutate(TeamPA = EVENT_ID)

for (n in 2:nrow(d2018)) {
  if (d2018$TeamPA[n] > d2018$TeamPA[n-1]){
    d2018$TeamPA[n] = d2018$TeamPA[n-1] + 1
  } else {d2018$TeamPA[n] = 1}
}

TeamPA <- d2018 %>% group_by(TeamPA) %>%
  summarize(LI = mean(LI), LI_plus = mean(LI_plus),
            LI_R3 = mean(LI_R3), LI_L3 = mean(LI_L3), N = n())

ggplot(TeamPA, aes(TeamPA, LI)) + geom_line() + xlim(0, 40) +
  xlab("Team Plate Appearance in Game") + ylim(0, 1.5) + ylab("LI") +
  ggtitle("Average LI by Team PA in Game")

ggplot(TeamPA, aes(TeamPA, LI_plus)) + geom_line() + xlim(0, 40) +
  xlab("Team Plate Appearance in Game") + ylim(0, 1.5) + ylab("LI+") +
  ggtitle("Average LI+ by Team PA in Game")

## Separating data by League
d2018 <- d2018 %>% mutate(HOME_TEAM_ID = substr(GAME_ID, 1, 3),
                          Lg = ifelse(HOME_TEAM_ID %in% NL, 'NL', 'AL'))

TeamPA_Lg <- d2018 %>% group_by(TeamPA, Lg) %>%
  summarize(LI_plus = mean(LI_plus),
            LI_R3 = mean(LI_R3), LI_L3 = mean(LI_L3), N = n())

ggplot(TeamPA_Lg, aes(TeamPA, LI_plus, color = Lg)) + geom_line() +
  xlim(0, 40) + xlab("Team Plate Appearance in Game") + ylim(-0.25, 1.5) +
  ylab("LI+") + ggtitle("Average LI+ by Team PA in Game")

## ------------------------------------------------------------------------

rays <- d2018 %>% filter(HOME_TEAM_ID == 'TBA' & BAT_HOME_ID == 0
                         | AWAY_TEAM_ID == 'TBA' & BAT_HOME_ID == 1)

ggplot(d2018, aes(hand_3, LI_plus)) + geom_point(alpha = 0.2) +
  ylim(0, 10) + ylab("LI+") + xlab("3-Batter Preferred Handedness") +
  labs(title = "LI+ & Handedness distribution of MLB, 2018",
       subtitle = "Positive = use RHP; Negative = use LHP")

ggplot(rays, aes(hand_3, LI_plus)) +
  geom_point(alpha = 0.2) + ylim(0, 10) + xlim(-1.5, 1.5) +
  labs(title = "LI+ & Handedness distribution of the Rays, 2018",
       subtitle = "Positive = use RHP; Negative = use LHP",
       x = "3-Batter Preferred Handedness", y = "LI+")

library(gghighlight)

## Sergio Romo
gghighlight_point(rays, aes(hand_3, LI_plus),
                  PIT_ID == 'romos001', colour = 'blue',
                  unhighlighted_colour = ggplot2::alpha('black', 0.2),
                  use_group_by = FALSE, use_direct_label = FALSE) +
  labs(title = "LI+ & Handedness distribution of the Rays, 2018",
       subtitle = "Positive = use RHP; Negative = use LHP",
       caption = "Blue Dots = Sergio Romo",
       x = "3-Batter Preferred Handedness", y = "LI+") +
  xlim(-1.5, 1.5) + ylim(0, 10)

## Jose Alvarado
gghighlight_point(rays, aes(hand_3, LI_plus),
                  PIT_ID == 'alvaj004', colour = 'blue',
                  unhighlighted_colour = ggplot2::alpha('black', 0.2),
                  use_group_by = FALSE, use_direct_label = FALSE) +
  labs(title = "LI+ & Handedness distribution of the Rays, 2018",
       subtitle = "Positive = use RHP; Negative = use LHP",
       caption = "Blue Dots = Jose Alvarado",
       x = "3-Batter Preferred Handedness", y = "LI+") +
  xlim(-1.5, 1.5) + ylim(0, 10)

## Chaz Roe
gghighlight_point(rays, aes(hand_3, LI_plus),
                  PIT_ID == 'roe-c001', colour = 'blue',
                  unhighlighted_colour = ggplot2::alpha('black', 0.2),
                  use_group_by = FALSE, use_direct_label = FALSE) +
  labs(title = "LI+ & Handedness distribution of the Rays, 2018",
       subtitle = "Positive = use RHP; Negative = use LHP",
       caption = "Blue Dots = Chaz Roe",
       x = "3-Batter Preferred Handedness", y = "LI+") +
  xlim(-1.5, 1.5) + ylim(0, 10)

## Alex Colome
gghighlight_point(rays, aes(hand_3, LI_plus),
                  PIT_ID == 'coloa001', colour = 'blue',
                  unhighlighted_colour = ggplot2::alpha('black', 0.2),
                  use_group_by = FALSE, use_direct_label = FALSE) +
  labs(title = "LI+ & Handedness distribution of the Rays, 2018",
       subtitle = "Positive = use RHP; Negative = use LHP",
       caption = "Blue Dots = Alex Colome",
       x = "3-Batter Preferred Handedness", y = "LI+") +
  xlim(-1.5, 1.5) + ylim(0, 10)

## Ryne Stanek
gghighlight_point(rays, aes(hand_3, LI_plus),
                  PIT_ID == 'stanr002', colour = 'blue',
                  unhighlighted_colour = ggplot2::alpha('black', 0.2),
                  use_group_by = FALSE, use_direct_label = FALSE) +
  labs(title = "LI+ & Handedness distribution of the Rays, 2018",
       subtitle = "Positive = use RHP; Negative = use LHP",
       caption = "Blue Dots = Ryne Stanek",
       x = "3-Batter Preferred Handedness", y = "LI+") +
  xlim(-1.5, 1.5) + ylim(0, 10)

## ------------------------------------------------------------------------

## Import preseason pitcher projections
pre18_pitch <- read_csv("http://www.steamerprojections.com/images/projections/steamer_pitchers_2018_ros_preseason_final.csv")
pre18_pitch <- pre18_pitch %>%
  mutate(Name = paste(firstname, lastname)) %>%
  select(-firstname, -lastname)

## Identify every pitcher for 2018 Rays
rays_pitch <- read_csv("rays_pitcher.csv") %>% select(Name)

rays_pitch <- left_join(rays_pitch, pre18_pitch, by = "Name")
name_retro <- ID_Map %>% select(fg_name, retro_id)
rays_pitch <- left_join(rays_pitch, name_retro, by = c("Name" = "fg_name")) %>%
  filter(retro_id != "robed003") %>% mutate(ERA_m = round(`ERA-`, 1))

## Add data for Venters (not projected)
rays_pitch$ERA_m[rays_pitch$retro_id == 'ventj001'] <- 89
rays_pitch$Throws[rays_pitch$retro_id == 'ventj001'] <- 'L'

## Separate by starters and relievers
rays_SP <- rays_pitch %>% filter(start_percent != 0 & IP > 50) %>%
  select(Name, retro_id, Throws, ERA_m)
rays_RP <- rays_pitch %>% filter(Name %not% rays_SP$Name) %>%
  select(Name, retro_id, Throws, ERA_m) %>% filter(!is.na(ERA_m))

## Calculated average LI & LI+ (of every PA)
avg_LI <- rays %>% group_by(PIT_ID) %>%
  summarize(pLI = round(mean(LI), 2),
            pLI_p = round(mean(LI_plus), 2),
            pLI_p3 = round(mean(LI_p3), 2))

## Calculate LI & LI+ when entering game
gm_LI <- rays %>% group_by(PIT_ID, GAME_ID) %>%
  summarize(LI = first(LI), LI_p = first(LI_plus), LI_p3 = first(LI_p3)) %>%
  group_by(PIT_ID) %>% summarize(App = n(), gmLI = round(mean(LI), 2),
                                 gmLI_p = round(mean(LI_p), 2),
                                 gmLI_p3 = round(mean(LI_p3), 2))

## Calculate batter's wRC+ when entering game
gm_wRC <- rays %>% group_by(PIT_ID, GAME_ID) %>%
  summarize(wRC = first(wRC_plus), wRC3 = first(wRC_p3)) %>%
  group_by(PIT_ID) %>% summarize(wRC_plus = round(mean(wRC), 2),
                                 wRC_p3 = round(mean(wRC3), 2))

rays_RP <- left_join(rays_RP, gm_LI, by = c("retro_id" = "PIT_ID"))
rays_RP <- left_join(rays_RP, avg_LI, by = c("retro_id" = "PIT_ID"))
rays_RP <- left_join(rays_RP, gm_wRC, by = c("retro_id" = "PIT_ID"))

## Graphing gmLI+ & pLI+ vs. projected ERA-
ggplot(rays_RP, aes(ERA_m, gmLI_p3, label = Name)) + geom_point() +
  geom_text(aes(label = Name), hjust = 0.5, vjust = 1.5) + xlim(80, 125) +
  ylim(0.2, 1.7) + geom_smooth(method = lm, se = FALSE) +
  labs(title = "Projected ERA- vs. gmLI+ of Rays relievers, 2018",
       x = "ERA- (projected by Steamer)", y = "gmLI+")

ggplot(rays_RP, aes(ERA_m, pLI_p, label = Name)) + geom_point() +
  geom_text(aes(label = Name), just = 0.5, vjust = 1.5) + xlim(80, 125) +
  ylim(0, 2.2) + geom_smooth(method = lm, se = FALSE) +
  labs(title = "Projected ERA- vs. pLI+ of Rays relievers, 2018",
       x = "ERA-", y = "pLI+", caption = "ERA- projected by Steamer")

## Graphing projected wRC+ vs. projected ERA-
ggplot(rays_RP, aes(ERA_m, wRC, label = Name)) + geom_point() +
  geom_text(aes(label = Name), hjust = 0.5, vjust = 1.5) + xlim(80, 125) +
  ylim(80, 110) + geom_smooth(method = lm, se = FALSE) +
  labs(title = "Projected ERA- vs. batter wRC+ of Rays relievers, 2018",
       x = "ERA- (projected by Steamer)", y = "wRC+ of first batter faced")

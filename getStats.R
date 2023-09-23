library(tidyverse)
library(toOrdinal)

# Colors for spraycharts
spraychartcols <- c("Home Run" = "red",
          "Contact Out" = "green",
          "Double" = "blue",
          "Triple" = "cyan",
          "Single" = "purple",
          "Error" = "yellow")

# Colors for pitches
pitchcols <- c("Four Seam Fastball" = "red",
          "Two Seam Fastball" = "orange",
          "Cutter" = "hotpink",
          "Slider" = "springgreen3",
          "Curveball" = "navy",
          "Changeup" = "magenta4",
          "Splitter" = "yellow",
          "Forkball" = "olivedrab3",
          "Knuckleball" = "black",
          "Sinker" = "cyan4",
          "Slurve" = "royalblue",
          "Knuckle Curve" = "slateblue1",
          "Split Change" = "purple",
          "Circle Change" = "maroon1")

# Read in FlightScope Data
master <- read.csv("Master.csv", encoding="UTF-8")

# Read in current active rosters
activeroster <- read.csv("2022 Active Rosters.csv", encoding="UTF-8")

# Read in batter game logs
batterlogs <- read.csv("2022BatterGameLogs.csv", encoding="UTF-8")
batterlogs[is.na(batterlogs)] = 0

batterlogs$Position <- str_replace_all(batterlogs$Position, c(
  'H' = 'Pinch Hitter',
  'R' = 'Pinch Runner',
  '2' = 'Catcher',
  '3' = 'First Base',
  '4' = 'Second Base',
  '5' = 'Third Base',
  '6' = 'Shortstop',
  '7' = 'Left Field',
  '8' = 'Center Field',
  '9' = 'Right Field',
  '0' = 'Designated Hitter'
))

as.Date(batterlogs$Date, format = "%m/%d/%y") -> batterlogs$Date

# Read in pitcher game logs
pitcherlogs <- read.csv("2022PitcherGameLogs.csv", encoding="UTF-8") %>%
  mutate(IP = (as.integer(IP) + ((IP %% 1) * (10/3))))
pitcherlogs[is.na(pitcherlogs)] = 0

as.Date(pitcherlogs$Date, format = "%m/%d/%y") -> pitcherlogs$Date

# Renaming columns with weird characters
colnames(master)[34] <- 'VerticalLaunch'
colnames(master)[35] <- 'HorizontalLaunch'

as.Date(master$Date) -> master$Date

# Renaming team names
master$Pitcher.Team <- str_replace_all(master$Pitcher.Team, c(
  "2022 Beavers" = "BB",
  "2022 Diamond Hoppers" = "DH",
  "2022 Unicorns" = "UU",
  "2022 Woolly Mammoths" = "WM"
))
master$Batter.Team <- str_replace_all(master$Batter.Team, c(
  "2022 Beavers" = "BB",
  "2022 Diamond Hoppers" = "DH",
  "2022 Unicorns" = "UU",
  "2022 Woolly Mammoths" = "WM"
))

# Adding column for First Pitch of PA
master <- mutate(master, FirstPitch = 0)
for (i in 1:nrow(master)) {
  if (i == 1) {
    master$FirstPitch[i] <- 1
  }
  else {
    if (master$Batter[i - 1] != master$Batter[i]) {
      master$FirstPitch[i] <- 1
    }
  }
}

# Fixing the strikezone
master <- mutate(master, 
                 Pitch.Strike.Zone...Offset..ft. = Pitch.Strike.Zone...Offset..ft. + 1.60,
                 Hit_X = ifelse(((pi/180) * HorizontalLaunch) > 0,
                                sin(((pi/180) * HorizontalLaunch)) * Hit.Carry.Distance..ft.,
                                -sin(abs(((pi/180) * HorizontalLaunch))) * Hit.Carry.Distance..ft.),
                 Hit_Y = cos((pi/180) * HorizontalLaunch) * Hit.Carry.Distance..ft.)

# Adding columns for stats
master <- mutate(master, 
                 PA = ifelse((Pitch.Call == "Contact Out") | (Pitch.Call == "Called Strikeout") 
                             | (Pitch.Call == "Swinging Strikeout") | (Pitch.Call == "Single")
                             | (Pitch.Call == "Double") | (Pitch.Call == "Triple")
                             | (Pitch.Call == "Home Run") | (Pitch.Call == "Error")
                             | (Pitch.Call == "Hit by Pitch") | (Pitch.Call == "Walk"), 1, 0),
                 AB = ifelse((Pitch.Call == "Contact Out") | (Pitch.Call == "Called Strikeout") 
                             | (Pitch.Call == "Swinging Strikeout") | (Pitch.Call == "Single")
                             | (Pitch.Call == "Double") | (Pitch.Call == "Triple")
                             | (Pitch.Call == "Home Run") | (Pitch.Call == "Error"), 1, 0),
                 H = ifelse((Pitch.Call == "Single") | (Pitch.Call == "Double") 
                            | (Pitch.Call == "Triple") | (Pitch.Call == "Home Run"), 1, 0),
                 `1B` = ifelse(Pitch.Call == "Single", 1, 0),
                 `2B` = ifelse(Pitch.Call == "Double", 1, 0),
                 `3B` = ifelse(Pitch.Call == "Triple", 1, 0),
                 HR = ifelse(Pitch.Call == "Home Run", 1, 0),
                 BB = ifelse(Pitch.Call == "Walk", 1, 0),
                 SO = ifelse(Pitch.Call == "Called Strikeout" | Pitch.Call == "Swinging Strikeout", 1, 0),
                 HBP = ifelse(Pitch.Call == "Hit by Pitch", 1, 0),
                 E = ifelse(Pitch.Call == "Error", 1, 0),
                 InPlay = ifelse((Pitch.Call == "Contact Out") | (Pitch.Call == "Single")
                                 | (Pitch.Call == "Double") | (Pitch.Call == "Triple")
                                 | (Pitch.Call == "Home Run") | (Pitch.Call == "Error"), 1, 0),
                 Tracked = ifelse(!is.na(Hit.Ball.Speed..mph.) & Pitch.Call != 'Foul Ball', 1, 0),
                 HardHit = ifelse(Hit.Ball.Speed..mph. >= 85, 1, 0),
                 GB = ifelse(VerticalLaunch < 10, 1, 0),
                 LD = ifelse(VerticalLaunch >= 10 & VerticalLaunch < 25, 1, 0),
                 FB = ifelse(VerticalLaunch >= 25 & VerticalLaunch <= 50, 1, 0),
                 PU = ifelse(VerticalLaunch > 50, 1, 0),
                 Pull = ifelse(((Batter.Handedness == "R") 
                               & (((pi/180) * HorizontalLaunch) <= -0.25)
                               | (Batter.Handedness == "L") 
                               & (((pi/180) * HorizontalLaunch) >= 0.25))
                               & Tracked == 1, 1, 0),
                 Straight = ifelse(((pi/180) * HorizontalLaunch) > -0.25 
                                   & ((pi/180) * HorizontalLaunch) < 0.25
                                   & Tracked == 1, 1, 0),
                 Oppo = ifelse(((Batter.Handedness == "L") 
                               & (((pi/180) * HorizontalLaunch) <= -0.25)
                               | (Batter.Handedness == "R") 
                               & (((pi/180) * HorizontalLaunch) >= 0.25))
                               & Tracked == 1, 1, 0),
                 InZone = ifelse((Pitch.Strike.Zone...Offset..ft. > -1.41667) &
                                   (Pitch.Strike.Zone...Offset..ft. < 1.41667) &
                                   (Pitch.Strike.Zone...Height..ft. > 1.35) &
                                   (Pitch.Strike.Zone...Height..ft. < 3.45), 1, 0),
                 Strike = ifelse((Pitch.Call == "Contact Out") | (Pitch.Call == "Called Strikeout")
                                 | (Pitch.Call == "Swinging Strikeout") | (Pitch.Call == "Single")
                                 | (Pitch.Call == "Double") | (Pitch.Call == "Triple")
                                 | (Pitch.Call == "Home Run") | (Pitch.Call == "Called Strike")
                                 | (Pitch.Call == "Swinging Strike") | (Pitch.Call == "Error")
                                 | (Pitch.Call == "Foul Ball"), 1, 0),
                 Swing = ifelse((Pitch.Call == "Contact Out") | (Pitch.Call == "Swinging Strikeout") 
                                | (Pitch.Call == "Single") | (Pitch.Call == "Double") 
                                | (Pitch.Call == "Triple") | (Pitch.Call == "Home Run")
                                | (Pitch.Call == "Swinging Strike") | (Pitch.Call == "Error")
                                | (Pitch.Call == "Foul Ball"), 1, 0),
                 Contact = ifelse((Pitch.Call == "Contact Out") | (Pitch.Call == "Error") 
                                  | (Pitch.Call == "Single") | (Pitch.Call == "Double") 
                                  | (Pitch.Call == "Triple") | (Pitch.Call == "Home Run")
                                  | (Pitch.Call == "Foul Ball"), 1, 0),
                 Whiff = ifelse((Pitch.Call == "Swinging Strikeout") | (Pitch.Call == "Swinging Strike"), 1, 0),
                 Called = ifelse((Pitch.Call == "Called Strikeout") | (Pitch.Call == "Called Strike"), 1, 0),
                 PitCat = ifelse(Pitch.Type == "Four Seam Fastball" | Pitch.Type == "Two Seam Fastball" |
                                   Pitch.Type == "Cutter" | Pitch.Type == "Sinker", "Fastball", 
                                 ifelse(Pitch.Type == "Curveball" | Pitch.Type == "Slider" |
                                          Pitch.Type == "Slurve" | Pitch.Type == "Knuckle Curve", 
                                        "Breaking Ball", ifelse(
                                          Pitch.Type == "Changeup" | Pitch.Type == "Splitter" |
                                            Pitch.Type == "Forkball" | Pitch.Type == "Split Change" |
                                            Pitch.Type == "Circle Change", "Changeup", "Other"
                                        )))
)

# Defining Barrels
df <- filter(master, Pitch.Call != "Foul Ball")
barreldef <- read.csv("Barrel Definition.csv", encoding="UTF-8") %>%
  mutate(MPHUp = MPH + 1)
barrels <- data.frame(matrix(ncol = ncol(master), nrow = 0))
for (i in 1:nrow(barreldef)) {
  df %>% filter(Hit.Ball.Speed..mph. >= barreldef$MPH[i]) %>%
    filter(Hit.Ball.Speed..mph. < barreldef$MPHUp[i]) %>%
    filter(VerticalLaunch >= barreldef$Low.Angle[i]) %>%
    filter(VerticalLaunch <= barreldef$High.Angle[i]) -> group
  barrels <- rbind(barrels, group)
}
barrels %>%
  group_by(Batter) %>%
  summarise(Barrels = n()) -> barrels
data.frame(barrels) -> barrels
rownames(barrels) <- barrels[,1]

# Defining expected stats
master %>% 
  drop_na(Hit.Ball.Speed..mph.) %>% 
  filter(Pitch.Call != '' & Pitch.Call != 'Foul Ball') -> tracked
tracked %>%
  mutate(
    ExitVeloZone = paste0(as.integer(Hit.Ball.Speed..mph. / 5) * 5, '-', 
                          (as.integer(Hit.Ball.Speed..mph. / 5) + 1) * 5),
    LAZone = paste0(as.integer(VerticalLaunch / 5) * 5, '-', 
                    (as.integer(VerticalLaunch / 5) + 1) * 5)
  ) -> tracked

tracked %>%
  group_by(ExitVeloZone, LAZone) %>%
  summarise(
    Num = n(),
    AVG = sum(H) / sum(AB),
    SLG = (sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / sum(AB)
  ) %>% 
  mutate(
    Zones = paste(ExitVeloZone, LAZone)
  ) %>%
  drop_na(AVG) %>%
  as.data.frame() -> xDefins
rownames(xDefins) <- xDefins$Zones
xDefins <- select(xDefins, -Zones)

master %>%
  mutate(
    ExitVeloZone = ifelse(!is.na(Hit.Ball.Speed..mph.), paste0(as.integer(Hit.Ball.Speed..mph. / 5) * 5, '-', 
                                                               (as.integer(Hit.Ball.Speed..mph. / 5) + 1) * 5), "None"),
    LAZone = ifelse(!is.na(Hit.Ball.Speed..mph.), paste0(as.integer(VerticalLaunch / 5) * 5, '-', 
                                                         (as.integer(VerticalLaunch / 5) + 1) * 5), "None"),
    Zones = paste(ExitVeloZone, LAZone)
  ) -> master

master %>%
  mutate(
    xH = ifelse(Zones == 'None', H, xDefins[Zones, 4]),
    xH = ifelse(is.na(xH), H, xH),
    xTB = ifelse(Zones == 'None', NA, xDefins[Zones, 5]),
    xTB = ifelse(is.na(xTB), ifelse(`1B` == 1, 1, 
                                    ifelse(`2B` == 1, 2, 
                                           ifelse(`3B` == 1, 3, 
                                                  ifelse(HR == 1, 4, 0)))), xTB)
  ) -> master

# Creating variables for league-wide OBP and SLG
LGOBP <- (sum(batterlogs$H) + sum(batterlogs$BB) + sum(batterlogs$HBP)) / 
  (sum(batterlogs$SF) + sum(batterlogs$BB) + sum(batterlogs$HBP) + sum(batterlogs$AB))
LGSLG <- ((sum(batterlogs$H) - sum(batterlogs$X2B) - sum(batterlogs$X3B) - sum(batterlogs$HR)) + 
            2 * sum(batterlogs$X2B) + 3 * sum(batterlogs$X3B) + 
            4 * sum(batterlogs$HR)) / sum(batterlogs$AB)

# Calculating Basic Hitter Stats
basichitting <- batterlogs %>%
  group_by(Player) %>%
  summarise(Team = last(Team),
            G = n(),
            PA = sum(AB) + sum(BB) + sum(HBP) + sum(SF),
            AB = sum(AB),
            R = sum(R),
            H = sum(H),
            `2B` = sum(X2B),
            `3B` = sum(X3B), 
            HR = sum(HR), 
            RBI = sum(RBI), 
            SB = sum(SB), 
            CS = sum(CS), 
            BB = sum(BB), 
            SO = sum(SO), 
            HBP = sum(HBP), 
            SF = sum(SF),
            AVG = round(H / AB, 3),
            OBP = round((H + BB + HBP) / PA, 3),
            SLG = round(((H - `2B` - `3B` - HR) + 2 * `2B` + 3 * `3B` + 4 * HR) / AB,3), 
            OPS = OBP + SLG,
            `OPS+` = round((OBP / LGOBP + SLG / LGSLG - 1) * 100),
            `BB %` = round(BB / PA * 100, 1),  
            `SO %` = round(SO / PA * 100, 1), 
            BABIP = round((H - HR) / (AB - HR - SO - SF), 3)
  ) %>%
  as.data.frame()
basichitting <- basichitting[basichitting$Player %in% activeroster$Name, ]

# Calculating Advanced Hitter Stats
advhitting <- master %>%
  group_by(Batter) %>%
  summarise(Team = last(Batter.Team),
            `Pitches Seen` = n(), 
            `Pitches / AB` = round(n() / sum(AB),1),
            `Batted Balls` = sum(InPlay),  
            `Batted Balls Tracked` = sum(Tracked),
            `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
            `Max Exit Velocity` = round(max(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
            `Average LA` = round(mean(VerticalLaunch, na.rm = TRUE), 1),
            `Average Distance` = round(mean(Hit.Carry.Distance..ft., na.rm = TRUE)),
            `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / `Batted Balls Tracked`, 1),
            xAVG = round(sum(xH) / sum(AB), 3),
            xSLG = round(sum(xTB) / sum(AB), 3)
  ) %>%
  mutate(Barrels = ifelse(Batter %in% rownames(barrels), barrels[Batter, 2], 0),
         `Barrel %` = round(100 * Barrels / `Batted Balls Tracked`, 1)) %>%
  as.data.frame() %>%
  rename(c('Player' = 'Batter'))
advhitting <- advhitting[advhitting$Player %in% activeroster$Name, ]

# Calculating Plate Discipline Hitter Stats
pdhitting <- master %>%
  group_by(Batter) %>%
  summarise(Team = last(Batter.Team),
            `Zone %` = round(100 * sum(InZone, na.rm = TRUE) / n(), 1), 
            `Strike %` = round(100 * sum(Strike) / n(), 1), 
            `Swing %` = round(100 * sum(Swing) / n(), 1), 
            `O-Swing %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                                / sum(InZone == 0, na.rm = TRUE), 1),
            `Z-Swing %` = round(100 * sum(InZone == 1 & Swing == 1, na.rm = TRUE) 
                                / sum(InZone, na.rm = TRUE), 1),
            `Contact %` = round(100 * sum(Contact) / sum(Swing), 1),
            `O-Contact %` = round(100 * sum(InZone == 0 & Contact == 1, na.rm = TRUE) 
                                  / sum(InZone == 0 & Swing == 1, na.rm = TRUE), 1), 
            `Z-Contact %` = round(100 * sum(InZone == 1 & Contact == 1, na.rm = TRUE) 
                                  / sum(InZone == 1 & Swing == 1, na.rm = TRUE), 1),
            `FP-Strike %` = round(100 * sum(FirstPitch == 1 & Strike == 1) / sum(FirstPitch), 1),
            `Sw-Strike %` = round(100 * sum(Whiff) / n(), 1),
            `C-Strike %` = round(100 * sum(Called) / n(), 1),
            `C-Sw-Strike %` = round(100 * (sum(Whiff) + sum(Called)) / n(), 1)) %>%
  as.data.frame() %>%
  rename(c('Player' = 'Batter'))
pdhitting <- pdhitting[pdhitting$Player %in% activeroster$Name, ]

# Calculating Batted Ball Profiles for Hitters
bbhitting <- master %>%
  group_by(Batter) %>%
  summarise(Team = last(Batter.Team),
            `GB %` = round(100 * sum(GB, na.rm = TRUE) / sum(Tracked), 1),
            `LD %` = round(100 * sum(LD, na.rm = TRUE) / sum(Tracked), 1),
            `FB %` = round(100 * sum(FB, na.rm = TRUE) / sum(Tracked), 1),
            `PU %` = round(100 * sum(PU, na.rm = TRUE) / sum(Tracked), 1),
            `Pull %` = round(100 * sum(Pull, na.rm = TRUE) / sum(Tracked), 1),
            `Straight %` = round(100 * sum(Straight, na.rm = TRUE) / sum(Tracked), 1),
            `Oppo %` = round(100 * sum(Oppo, na.rm = TRUE) / sum(Tracked), 1)) %>%
  as.data.frame()
bbhitting <- bbhitting[bbhitting$Batter %in% activeroster$Name, ]

# Creating variables for league-wide ERA and FIP constant
LGERA <- 9 * sum(pitcherlogs$ER) / sum(pitcherlogs$IP)
LGFIPConst <- LGERA - ((13 * sum(pitcherlogs$HR)) + (3 * (sum(pitcherlogs$BB) + sum(pitcherlogs$HBP))) - (2 * sum(pitcherlogs$SO))) / sum(pitcherlogs$IP)

# Calculating Basic Pitching Stats
basicpitching <- pitcherlogs %>%
  group_by(Player) %>%
  summarise(
    Team = last(Team),
    G = n(),
    GS = sum(GS),
    W = sum(W),
    L = sum(L),
    S = sum(S),
    IP = sum(IP),
    H = sum(H),
    `2B` = sum(X2B),
    `3B` = sum(X3B),
    HR = sum(HR),
    R = sum(R),
    ER = sum(ER),
    BB = sum(BB),
    SO = sum(SO),
    HBP = sum(HBP),
    ERA = round(9 * ER / IP, 2),
    WHIP = round((BB + H) / IP, 3),
    FIP = round((((13 * HR)+(3 * (BB + HBP)) - (2 * SO)) / IP) + LGFIPConst, 2),
    `H/9` = round(9 * H / IP, 1),
    `HR/9` = round(9 * HR / IP, 1),
    `BB/9` = round(9 * BB / IP, 1),
    `SO/9` = round(9 * SO / IP, 1),
    `SO/BB` = round(ifelse(BB > 0, SO / BB, 999), 1),
    AB = sum(AB),
    BF = sum(BF),
    `Opp AVG` = round(H / AB, 3),
    `Opp SLG` = round(((H - `2B` - `3B` - HR) + 2 * `2B` + 3 * `3B` + 4 * HR) / AB, 3), 
    `ERA+` = round(100 * LGERA / ERA),
    IP = as.integer(sum(IP)) + ((sum(IP) %% 1) * (3 / 10))
  ) %>%
  as.data.frame() %>%
  select(-c(AB,BF))
basicpitching <- basicpitching[basicpitching$Player %in% activeroster$Name, ]

# Calculating Advanced Pitching Stats
advpitching <- master %>%
  group_by(Pitcher) %>%
  summarise(
    Team = last(Pitcher.Team),
    `Pitches` = n(), 
    `Pitches / BF` = round(n() / sum(PA),1),
    `Batted Balls` = sum(InPlay),  
    `Batted Balls Tracked` = sum(Tracked),
    `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Max Exit Velocity` = round(max(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Average LA` = round(mean(VerticalLaunch, na.rm = TRUE), 1),
    `Average Distance` = round(mean(Hit.Carry.Distance..ft., na.rm = TRUE)),
    `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / `Batted Balls Tracked`, 1),
    xAVG = round(sum(xH) / sum(AB), 3),
    xSLG = round(sum(xTB) / sum(AB), 3)
  ) %>%
  as.data.frame() %>%
  rename(c('Player' = 'Pitcher'))
advpitching <- advpitching[advpitching$Player %in% activeroster$Name, ]

# Calculating Plate Discipline Pitching Stats
pdpitching <- master %>%
  group_by(Pitcher) %>%
  summarise(
    Team = last(Pitcher.Team),
    `Zone %` = round(100 * sum(InZone, na.rm = TRUE) / n(), 1), 
    `Strike %` = round(100 * sum(Strike) / n(), 1), 
    `Swing %` = round(100 * sum(Swing) / n(), 1), 
    `O-Swing %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone == 0, na.rm = TRUE), 1),
    `Z-Swing %` = round(100 * sum(InZone == 1 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone, na.rm = TRUE), 1),
    `Contact %` = round(100 * sum(Contact) / sum(Swing), 1),
    `O-Contact %` = round(100 * sum(InZone == 0 & Contact == 1, na.rm = TRUE) 
                          / sum(InZone == 0 & Swing == 1, na.rm = TRUE), 1), 
    `Z-Contact %` = round(100 * sum(InZone == 1 & Contact == 1, na.rm = TRUE) 
                          / sum(InZone == 1 & Swing == 1, na.rm = TRUE), 1),
    `FP-Strike %` = round(100 * sum(FirstPitch == 1 & Strike == 1) / sum(FirstPitch), 1),
    `Sw-Strike %` = round(100 * sum(Whiff) / n(), 1),
    `C-Strike %` = round(100 * sum(Called) / n(), 1),
    `C-Sw-Strike %` = round(100 * (sum(Whiff) + sum(Called)) / n(), 1)
  ) %>%
  as.data.frame() %>%
  rename(c('Player' = 'Pitcher'))
pdpitching <- pdpitching[pdpitching$Player %in% activeroster$Name, ]

# Calculating Batted Balls Profiles for Pitchers
bbpitching <- master %>%
  group_by(Pitcher) %>%
  summarise(
    Team = last(Pitcher.Team),
    `GB %` = round(100 * sum(GB, na.rm = TRUE) / sum(Tracked), 1),
    `LD %` = round(100 * sum(LD, na.rm = TRUE) / sum(Tracked), 1),
    `FB %` = round(100 * sum(FB, na.rm = TRUE) / sum(Tracked), 1),
    `PU %` = round(100 * sum(PU, na.rm = TRUE) / sum(Tracked), 1),
    `Pull %` = round(100 * sum(Pull, na.rm = TRUE) / sum(Tracked), 1),
    `Straight %` = round(100 * sum(Straight, na.rm = TRUE) / sum(Tracked), 1),
    `Oppo %` = round(100 * sum(Oppo, na.rm = TRUE) / sum(Tracked), 1)
  ) %>%
  as.data.frame()
bbpitching <- bbpitching[bbpitching$Pitcher %in% activeroster$Name, ]

# Pitching and hitting stats by team
basicteamhitting <- batterlogs %>%
  group_by(Team) %>%
  summarise(PA = sum(AB) + sum(BB) + sum(HBP) + sum(SF),
            AB = sum(AB),
            R = sum(R),
            H = sum(H),
            `2B` = sum(X2B),
            `3B` = sum(X3B), 
            HR = sum(HR), 
            RBI = sum(RBI), 
            SB = sum(SB), 
            CS = sum(CS), 
            BB = sum(BB), 
            SO = sum(SO), 
            HBP = sum(HBP), 
            SF = sum(SF),
            AVG = round(H / AB, 3),
            OBP = round((H + BB + HBP) / PA, 3),
            SLG = round(((H - `2B` - `3B` - HR) + 2 * `2B` + 3 * `3B` + 4 * HR) / AB,3), 
            `OPS+` = round((OBP / LGOBP + SLG / LGSLG - 1) * 100),
            `BB %` = round(BB / PA * 100, 1),  
            `SO %` = round(SO / PA * 100, 1), 
            BABIP = round((H - HR) / (AB - HR - SO - SF), 3)
  ) %>%
  as.data.frame()

teambarrels <- advhitting %>%
  group_by(Team) %>%
  summarise(
    Barrels = sum(Barrels)
  )

advteamhitting <- master %>%
  group_by(Batter.Team) %>%
  summarise(`Pitches Seen` = n(), 
            `Pitches / AB` = round(n() / sum(AB),1),
            `Batted Balls` = sum(InPlay),  
            `Batted Balls Tracked` = sum(Tracked),
            `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
            `Max Exit Velocity` = round(max(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
            `Average LA` = round(mean(VerticalLaunch, na.rm = TRUE), 1),
            `Average Distance` = round(mean(Hit.Carry.Distance..ft., na.rm = TRUE)),
            `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / `Batted Balls Tracked`, 1),
            xAVG = round(sum(xH) / sum(AB), 3),
            xSLG = round(sum(xTB) / sum(AB), 3)
  ) %>%
  merge(teambarrels, by.x = 'Batter.Team', by.y = 'Team') %>%
  mutate(`Barrel %` = round(100 * Barrels / `Batted Balls Tracked`, 1)) %>%
  as.data.frame() %>%
  rename(c('Team' = 'Batter.Team'))

pdteamhitting <- master %>%
  group_by(Batter.Team) %>%
  summarise(`Zone %` = round(100 * sum(InZone, na.rm = TRUE) / n(), 1), 
            `Strike %` = round(100 * sum(Strike) / n(), 1), 
            `Swing %` = round(100 * sum(Swing) / n(), 1), 
            `O-Swing %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                                / sum(InZone == 0, na.rm = TRUE), 1),
            `Z-Swing %` = round(100 * sum(InZone == 1 & Swing == 1, na.rm = TRUE) 
                                / sum(InZone, na.rm = TRUE), 1),
            `Contact %` = round(100 * sum(Contact) / sum(Swing), 1),
            `O-Contact %` = round(100 * sum(InZone == 0 & Contact == 1, na.rm = TRUE) 
                                  / sum(InZone == 0 & Swing == 1, na.rm = TRUE), 1), 
            `Z-Contact %` = round(100 * sum(InZone == 1 & Contact == 1, na.rm = TRUE) 
                                  / sum(InZone == 1 & Swing == 1, na.rm = TRUE), 1),
            `FP-Strike %` = round(100 * sum(FirstPitch == 1 & Strike == 1) / sum(FirstPitch), 1),
            `Sw-Strike %` = round(100 * sum(Whiff) / n(), 1),
            `C-Strike %` = round(100 * sum(Called) / n(), 1),
            `C-Sw-Strike %` = round(100 * (sum(Whiff) + sum(Called)) / n(), 1)) %>%
  as.data.frame() %>%
  rename(c('Team' = 'Batter.Team'))

bbteamhitting <- master %>%
  group_by(Batter.Team) %>%
  summarise(`GB %` = round(100 * sum(GB, na.rm = TRUE) / sum(Tracked), 1),
            `LD %` = round(100 * sum(LD, na.rm = TRUE) / sum(Tracked), 1),
            `FB %` = round(100 * sum(FB, na.rm = TRUE) / sum(Tracked), 1),
            `PU %` = round(100 * sum(PU, na.rm = TRUE) / sum(Tracked), 1),
            `Pull %` = round(100 * sum(Pull, na.rm = TRUE) / sum(Tracked), 1),
            `Straight %` = round(100 * sum(Straight, na.rm = TRUE) / sum(Tracked), 1),
            `Oppo %` = round(100 * sum(Oppo, na.rm = TRUE) / sum(Tracked), 1)) %>%
  as.data.frame() %>%
  rename(c('Team' = 'Batter.Team'))

basicteampitching <- pitcherlogs %>%
  group_by(Team) %>%
  summarise(
    GS = sum(GS),
    W = sum(W),
    L = sum(L),
    S = sum(S),
    IP = sum(IP),
    H = sum(H),
    `2B` = sum(X2B),
    `3B` = sum(X3B),
    HR = sum(HR),
    R = sum(R),
    ER = sum(ER),
    BB = sum(BB),
    SO = sum(SO),
    HBP = sum(HBP),
    ERA = round(9 * ER / IP, 2),
    WHIP = round((BB + H) / IP, 3),
    FIP = round((((13 * HR)+(3 * (BB + HBP)) - (2 * SO)) / IP) + LGFIPConst, 2),
    `H/9` = round(9 * H / IP, 1),
    `HR/9` = round(9 * HR / IP, 1),
    `BB/9` = round(9 * BB / IP, 1),
    `SO/9` = round(9 * SO / IP, 1),
    `SO/BB` = round(ifelse(BB > 0, SO / BB, 999), 1),
    AB = sum(AB),
    BF = sum(BF),
    `Opp AVG` = round(H / AB, 3),
    `Opp SLG` = round(((H - `2B` - `3B` - HR) + 2 * `2B` + 3 * `3B` + 4 * HR) / AB, 3), 
    `ERA+` = round(100 * LGERA / ERA),
    IP = as.integer(sum(IP)) + ((sum(IP) %% 1) * (3 / 10))
  ) %>%
  as.data.frame() %>%
  select(-c(AB,BF))

advteampitching <- master %>%
  group_by(Pitcher.Team) %>%
  summarise(
    `Pitches` = n(), 
    `Pitches / BF` = round(n() / sum(PA),1),
    `Batted Balls` = sum(InPlay),  
    `Batted Balls Tracked` = sum(Tracked),
    `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Max Exit Velocity` = round(max(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Average LA` = round(mean(VerticalLaunch, na.rm = TRUE), 1),
    `Average Distance` = round(mean(Hit.Carry.Distance..ft., na.rm = TRUE)),
    `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / `Batted Balls Tracked`, 1),
    xAVG = round(sum(xH) / sum(AB), 3),
    xSLG = round(sum(xTB) / sum(AB), 3)
  ) %>%
  as.data.frame() %>%
  rename(c('Team' = 'Pitcher.Team'))

pdteampitching <- master %>%
  group_by(Pitcher.Team) %>%
  summarise(
    `Zone %` = round(100 * sum(InZone, na.rm = TRUE) / n(), 1), 
    `Strike %` = round(100 * sum(Strike) / n(), 1), 
    `Swing %` = round(100 * sum(Swing) / n(), 1), 
    `O-Swing %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone == 0, na.rm = TRUE), 1),
    `Z-Swing %` = round(100 * sum(InZone == 1 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone, na.rm = TRUE), 1),
    `Contact %` = round(100 * sum(Contact) / sum(Swing), 1),
    `O-Contact %` = round(100 * sum(InZone == 0 & Contact == 1, na.rm = TRUE) 
                          / sum(InZone == 0 & Swing == 1, na.rm = TRUE), 1), 
    `Z-Contact %` = round(100 * sum(InZone == 1 & Contact == 1, na.rm = TRUE) 
                          / sum(InZone == 1 & Swing == 1, na.rm = TRUE), 1),
    `FP-Strike %` = round(100 * sum(FirstPitch == 1 & Strike == 1) / sum(FirstPitch), 1),
    `Sw-Strike %` = round(100 * sum(Whiff) / n(), 1),
    `C-Strike %` = round(100 * sum(Called) / n(), 1),
    `C-Sw-Strike %` = round(100 * (sum(Whiff) + sum(Called)) / n(), 1)
  ) %>%
  as.data.frame() %>%
  rename(c('Team' = 'Pitcher.Team'))

bbteampitching <- master %>%
  group_by(Pitcher.Team) %>%
  summarise(
    `GB %` = round(100 * sum(GB, na.rm = TRUE) / sum(Tracked), 1),
    `LD %` = round(100 * sum(LD, na.rm = TRUE) / sum(Tracked), 1),
    `FB %` = round(100 * sum(FB, na.rm = TRUE) / sum(Tracked), 1),
    `PU %` = round(100 * sum(PU, na.rm = TRUE) / sum(Tracked), 1),
    `Pull %` = round(100 * sum(Pull, na.rm = TRUE) / sum(Tracked), 1),
    `Straight %` = round(100 * sum(Straight, na.rm = TRUE) / sum(Tracked), 1),
    `Oppo %` = round(100 * sum(Oppo, na.rm = TRUE) / sum(Tracked), 1)
  ) %>%
  as.data.frame() %>%
  rename(c('Team' = 'Pitcher.Team'))

platoonhitting <- master %>%
  group_by(Batter, Pitcher.Handedness) %>%
  summarise(
    Team = last(Batter.Team),
    PA = sum(PA),
    AB = sum(AB),
    H = sum(H),
    BB = sum(BB),
    SO = sum(SO),
    AVG = round(H / AB, 3),
    OBP = round((H + BB + sum(HBP)) / PA, 3),
    SLG = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
    `OPS+` = round((OBP / LGOBP + SLG / LGSLG - 1) * 100),
    `BB %` = round(BB / PA * 100, 1),  
    `SO %` = round(SO / PA * 100, 1),
    `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
    `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                      / sum(InZone == 0, na.rm = TRUE), 1),
    `Whiff %` = round(100 * sum(Whiff) / n(), 1)
  ) %>%
  rename(c("Pitcher Hand" = "Pitcher.Handedness")) %>%
  as.data.frame()
platoonhitting <- platoonhitting[platoonhitting$Batter %in% activeroster$Name, ]

pitchtypehitting <- master %>%
  group_by(Batter, PitCat) %>%
  summarise(
    Team = last(Batter.Team),
    Seen = n(),
    `Seen %` = n(),
    PA = sum(PA),
    AB = sum(AB),
    H = sum(H),
    BB = sum(BB),
    SO = sum(SO),
    AVG = round(H / AB, 3),
    OBP = round((H + BB + sum(HBP)) / PA, 3),
    SLG = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
    `OPS+` = round((OBP / LGOBP + SLG / LGSLG - 1) * 100),
    `BB %` = round(BB / PA * 100, 1),  
    `SO %` = round(SO / PA * 100, 1),
    `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
    `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                      / sum(InZone == 0, na.rm = TRUE), 1),
    `Whiff %` = round(100 * sum(Whiff) / n(), 1)
  ) %>%
  mutate(
    `Seen %` = round(100 * `Seen %` / sum(`Seen %`), 1)
  ) %>%
  filter(PitCat != "Other") %>%
  rename(c("Pitch Type" = "PitCat")) %>%
  as.data.frame()
pitchtypehitting <- pitchtypehitting[pitchtypehitting$Batter %in% activeroster$Name, ]

hranks <- data.frame(
  Player = basichitting$Player,
  Team = basichitting$Team,
  G = toOrdinal(rank(-1 * basichitting$G, ties.method = 'min')),
  PA = toOrdinal(rank(-1 * basichitting$PA, ties.method = 'min')),
  AB = toOrdinal(rank(-1 * basichitting$AB, ties.method = 'min')),
  R = toOrdinal(rank(-1 * basichitting$R, ties.method = 'min')),
  H = toOrdinal(rank(-1 * basichitting$H, ties.method = 'min')),
  `2B` = toOrdinal(rank(-1 * basichitting$`2B`, ties.method = 'min')),
  `3B` = toOrdinal(rank(-1 * basichitting$`3B`, ties.method = 'min')),
  HR = toOrdinal(rank(-1 * basichitting$HR, ties.method = 'min')),
  RBI = toOrdinal(rank(-1 * basichitting$RBI, ties.method = 'min')),
  SB = toOrdinal(rank(-1 * basichitting$SB, ties.method = 'min')),
  CS = toOrdinal(rank(basichitting$CS, ties.method = 'min')),
  BB = toOrdinal(rank(-1 * basichitting$BB, ties.method = 'min')), 
  SO = toOrdinal(rank(basichitting$SO, ties.method = 'min')),
  HBP = toOrdinal(rank(-1 * basichitting$HBP, ties.method = 'min')),
  SF = toOrdinal(rank(-1 * basichitting$SF, ties.method = 'min')),
  AVG = toOrdinal(rank(-1 * basichitting$AVG, ties.method = 'min')),
  OBP = toOrdinal(rank(-1 * basichitting$OBP, ties.method = 'min')),
  SLG = toOrdinal(rank(-1 * basichitting$SLG, ties.method = 'min')), 
  OPS = toOrdinal(rank(-1 * basichitting$OPS, ties.method = 'min')),
  `OPS+` = toOrdinal(rank(-1 * basichitting$`OPS+`, ties.method = 'min')),
  `BB %` = toOrdinal(rank(-1 * basichitting$`BB %`, ties.method = 'min')),
  `SO %` = toOrdinal(rank(basichitting$`SO %`, ties.method = 'min')),
  BABIP = toOrdinal(rank(-1 * basichitting$BABIP, ties.method = 'min')),
  `Pitches Seen` = toOrdinal(rank(-1 * advhitting$`Pitches Seen`, ties.method = 'min')), 
  `Pitches / AB` = toOrdinal(rank(-1 * advhitting$`Pitches / AB`, ties.method = 'min')),
  `Batted Balls` = toOrdinal(rank(-1 * advhitting$`Batted Balls`, ties.method = 'min')),  
  `Batted Balls Tracked` = toOrdinal(rank(-1 * advhitting$`Batted Balls Tracked`, ties.method = 'min')),
  `Average Exit Velocity` = toOrdinal(rank(-1 * advhitting$`Average Exit Velocity`, ties.method = 'min')),
  `Max Exit Velocity` = toOrdinal(rank(-1 * advhitting$`Max Exit Velocity`, ties.method = 'min')),
  `Average LA` = toOrdinal(rank(-1 * advhitting$`Average LA`, ties.method = 'min')),
  `Average Distance` = toOrdinal(rank(-1 * advhitting$`Average Distance`, ties.method = 'min')),
  `Hard Hit %` = toOrdinal(rank(-1 * advhitting$`Hard Hit %`, ties.method = 'min')),
  xAVG = toOrdinal(rank(-1 * advhitting$xAVG, ties.method = 'min')),
  xSLG = toOrdinal(rank(-1 * advhitting$xSLG, ties.method = 'min')),
  Barrels = toOrdinal(rank(-1 * advhitting$Barrels, ties.method = 'min')),
  `Barrel %` = toOrdinal(rank(-1 * advhitting$`Barrel %`, ties.method = 'min')),
  `Zone %` = toOrdinal(rank(pdhitting$`Zone %`, ties.method = 'min')),
  `Strike %` = toOrdinal(rank(pdhitting$`Strike %`, ties.method = 'min')),
  `Swing %` = toOrdinal(rank(pdhitting$`Swing %`, ties.method = 'min')),
  `O-Swing %` = toOrdinal(rank(pdhitting$`O-Swing %`, ties.method = 'min')),
  `Z-Swing %` = toOrdinal(rank(-1 * pdhitting$`Z-Swing %`, ties.method = 'min')),
  `Contact %` = toOrdinal(rank(-1 * pdhitting$`Contact %`, ties.method = 'min')),
  `O-Contact %` = toOrdinal(rank(-1 * pdhitting$`O-Contact %`, ties.method = 'min')),
  `Z-Contact %` = toOrdinal(rank(-1 * pdhitting$`Z-Contact %`, ties.method = 'min')),
  `FP-Strike %` = toOrdinal(rank(pdhitting$`FP-Strike %`, ties.method = 'min')),
  `Sw-Strike %` = toOrdinal(rank(pdhitting$`Sw-Strike %`, ties.method = 'min')),
  `C-Strike %` = toOrdinal(rank(pdhitting$`C-Strike %`, ties.method = 'min')),
  `C-Sw-Strike %` = toOrdinal(rank(pdhitting$`C-Sw-Strike %`, ties.method = 'min'))
)
colnames(hranks) <- c(colnames(basichitting), colnames(advhitting)[-c(1:2)], colnames(pdhitting)[-c(1:2)])

platoonpitching <- master %>%
  group_by(Pitcher, Batter.Handedness) %>%
  summarise(
    Team = last(Pitcher.Team),
    BF = sum(PA),
    AB = sum(AB),
    H = sum(H),
    BB = sum(BB),
    SO = sum(SO),
    WHIP = round((BB + H) / ((AB - H - sum(E)) / 3), 3),
    FIP = round((((13 * sum(HR)) + (3 * (BB + sum(HBP))) - (2 * SO)) / ((AB - H - sum(E)) / 3))
                + LGFIPConst, 2),
    `H/9` = round(9 * H / ((AB - H - sum(E)) / 3), 1),
    `HR/9` = round(9 * sum(HR) / ((AB - sum(HR) - sum(E)) / 3), 1),
    `BB/9` = round(9 * BB / ((AB - BB - sum(E)) / 3), 1),  
    `SO/9` = round(9 * SO / ((AB - SO - sum(E)) / 3), 1),
    `Opp AVG` = round(H / AB, 3),
    `Opp SLG` = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
    `Opp OPS` = `Opp AVG` + `Opp SLG`,
    `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
    `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                      / sum(InZone == 0, na.rm = TRUE), 1),
    `Whiff %` = round(100 * sum(Whiff) / n(), 1)
  ) %>%
  rename(c("Batter Hand" = "Batter.Handedness")) %>%
  as.data.frame()
platoonpitching <- platoonpitching[platoonpitching$Pitcher %in% activeroster$Name, ]

pitchtypepitching <- master %>%
  filter(Pitch.Type != "") %>%
  group_by(Pitcher, Pitch.Type) %>%
  summarise(
    Team = last(Pitcher.Team),
    BF = sum(PA),
    AB = sum(AB),
    H = sum(H),
    BB = sum(BB),
    SO = sum(SO),
    WHIP = round((BB + H) / ((AB - H - sum(E)) / 3), 3),
    FIP = round((((13 * sum(HR)) + (3 * (BB + sum(HBP))) - (2 * SO)) / ((AB - H - sum(E)) / 3))
                + LGFIPConst, 2),
    `H/9` = round(9 * H / ((AB - H - sum(E)) / 3), 1),
    `HR/9` = round(9 * sum(HR) / ((AB - H - sum(E)) / 3), 1),
    `BB/9` = round(9 * BB / ((AB - H - sum(E)) / 3), 1),  
    `SO/9` = round(9 * SO / ((AB - H - sum(E)) / 3), 1),
    `Opp AVG` = round(H / AB, 3),
    `Opp SLG` = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
    `Opp OPS` = `Opp AVG` + `Opp SLG`,
    `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
    `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
    `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                      / sum(InZone == 0, na.rm = TRUE), 1),
    `Whiff %` = round(100 * sum(Whiff) / n(), 1)
  ) %>%
  rename(c("Pitch Type" = "Pitch.Type")) %>%
  as.data.frame()
pitchtypepitching <- pitchtypepitching[pitchtypepitching$Pitcher %in% activeroster$Name, ]

pranks <- data.frame(
  Player = basicpitching$Player,
  Team = basicpitching$Team,
  G = toOrdinal(rank(-1 * basicpitching$G, ties.method = 'min')),
  GS = toOrdinal(rank(-1 * basicpitching$GS, ties.method = 'min')),
  W = toOrdinal(rank(-1 * basicpitching$W, ties.method = 'min')),
  L = toOrdinal(rank(basicpitching$L, ties.method = 'min')),
  S = toOrdinal(rank(-1 * basicpitching$S, ties.method = 'min')),
  IP = toOrdinal(rank(-1 * basicpitching$IP, ties.method = 'min')),
  H = toOrdinal(rank(basicpitching$H, ties.method = 'min')),
  `2B` = toOrdinal(rank(basicpitching$`2B`, ties.method = 'min')),
  `3B` = toOrdinal(rank(basicpitching$`3B`, ties.method = 'min')),
  HR = toOrdinal(rank(basicpitching$HR, ties.method = 'min')),
  R = toOrdinal(rank(basicpitching$R, ties.method = 'min')),
  ER = toOrdinal(rank(basicpitching$ER, ties.method = 'min')),
  BB = toOrdinal(rank(basicpitching$BB, ties.method = 'min')),
  SO = toOrdinal(rank(-1 * basicpitching$SO, ties.method = 'min')),
  HBP = toOrdinal(rank(basicpitching$HBP, ties.method = 'min')),
  ERA = toOrdinal(rank(basicpitching$ERA, ties.method = 'min')),
  WHIP = toOrdinal(rank(basicpitching$WHIP, ties.method = 'min')),
  FIP = toOrdinal(rank(basicpitching$FIP, ties.method = 'min')),
  `H/9` = toOrdinal(rank(basicpitching$`H/9`, ties.method = 'min')),
  `HR/9` = toOrdinal(rank(basicpitching$`HR/9`, ties.method = 'min')),
  `BB/9` = toOrdinal(rank(basicpitching$`BB/9`, ties.method = 'min')),
  `SO/9` = toOrdinal(rank(-1 * basicpitching$`SO/9`, ties.method = 'min')),
  `SO/BB` = toOrdinal(rank(-1 * basicpitching$`SO/BB`, ties.method = 'min')),
  `Opp AVG` = toOrdinal(rank(basicpitching$`Opp AVG`, ties.method = 'min')),
  `Opp SLG` = toOrdinal(rank(basicpitching$`Opp SLG`, ties.method = 'min')), 
  `ERA+` = toOrdinal(rank(-1 * basicpitching$`ERA+`, ties.method = 'min')),
  `Pitches` = toOrdinal(rank(-1 * advpitching$`Pitches`, ties.method = 'min')), 
  `Pitches / BF` = toOrdinal(rank(advpitching$`Pitches / BF`, ties.method = 'min')),
  `Batted Balls` = toOrdinal(rank(-1 * advpitching$`Batted Balls`, ties.method = 'min')),  
  `Batted Balls Tracked` = toOrdinal(rank(-1 * advpitching$`Batted Balls Tracked`, ties.method = 'min')),
  `Average Exit Velocity` = toOrdinal(rank(advpitching$`Average Exit Velocity`, ties.method = 'min')),
  `Max Exit Velocity` = toOrdinal(rank(advpitching$`Max Exit Velocity`, ties.method = 'min')),
  `Average LA` = toOrdinal(rank(advpitching$`Average LA`, ties.method = 'min')),
  `Average Distance` = toOrdinal(rank(advpitching$`Average Distance`, ties.method = 'min')),
  `Hard Hit %` = toOrdinal(rank(advpitching$`Hard Hit %`, ties.method = 'min')),
  xAVG = toOrdinal(rank(advpitching$xAVG, ties.method = 'min')),
  xSLG = toOrdinal(rank(advpitching$xSLG, ties.method = 'min')),
  `Zone %` = toOrdinal(rank(-1 * pdpitching$`Zone %`, ties.method = 'min')),
  `Strike %` = toOrdinal(rank(-1 * pdpitching$`Strike %`, ties.method = 'min')),
  `Swing %` = toOrdinal(rank(-1 * pdpitching$`Swing %`, ties.method = 'min')),
  `O-Swing %` = toOrdinal(rank(-1 * pdpitching$`O-Swing %`, ties.method = 'min')),
  `Z-Swing %` = toOrdinal(rank(pdpitching$`Z-Swing %`, ties.method = 'min')),
  `Contact %` = toOrdinal(rank(pdpitching$`Contact %`, ties.method = 'min')),
  `O-Contact %` = toOrdinal(rank(pdpitching$`O-Contact %`, ties.method = 'min')),
  `Z-Contact %` = toOrdinal(rank(pdpitching$`Z-Contact %`, ties.method = 'min')),
  `FP-Strike %` = toOrdinal(rank(-1 * pdpitching$`FP-Strike %`, ties.method = 'min')),
  `Sw-Strike %` = toOrdinal(rank(-1 * pdpitching$`Sw-Strike %`, ties.method = 'min')),
  `C-Strike %` = toOrdinal(rank(-1 * pdpitching$`C-Strike %`, ties.method = 'min')),
  `C-Sw-Strike %` = toOrdinal(rank(-1 * pdpitching$`C-Sw-Strike %`, ties.method = 'min'))
)
colnames(pranks) <- c(colnames(basicpitching), colnames(advpitching)[-c(1:2)], colnames(pdpitching)[-c(1:2)])

pitchmetricpitching <- master %>%
  filter(Pitch.Type != "") %>%
  group_by(Pitcher, Pitch.Type) %>%
  summarise(
    Team = last(Pitcher.Team),
    `Thrown %` = n(),
    `Average Velocity` = round(median(Pitch.Speed..mph.), 1),
    `Max Velocity` = round(max(Pitch.Speed..mph.), 1),
    `Average Spin Rate` = round(median(Pitch.Spin..rpm.), 1),
    `Max Spin Rate` = round(max(Pitch.Spin..rpm.), 1),
    `Swinging Strike %` = round(100 * sum(Whiff) / n(), 1)
  ) %>%
  mutate(`Thrown %` = round(100 * `Thrown %` / sum(`Thrown %`), 1)) %>%
  rename(c(`Pitch Type` = 'Pitch.Type')) %>%
  arrange(desc(`Thrown %`)) %>%
  as.data.frame()
pitchmetricpitching <- pitchmetricpitching[pitchmetricpitching$Pitcher %in% activeroster$Name, ]

#Function for generating stats for split tool
gethsplits <- function(df) {
  
  df %>%
    group_by(Batter) %>%
    summarise(
      PA = sum(PA),
      AB = sum(AB),
      H = sum(H),
      `2B` = sum(`2B`),
      `3B` = sum(`3B`),
      HR = sum(HR),
      BB = sum(BB),
      SO = sum(SO),
      AVG = round(H / AB, 3),
      OBP = round((H + BB + sum(HBP)) / PA, 3),
      SLG = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
      `OPS+` = round((OBP / LGOBP + SLG / LGSLG - 1) * 100),
      `BB %` = round(BB / PA * 100, 1),  
      `SO %` = round(SO / PA * 100, 1),
      `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
      `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
      xAVG = round(sum(xH) / sum(AB), 3),
      xSLG = round(sum(xTB) / sum(AB), 3),
      `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone == 0, na.rm = TRUE), 1),
      `Whiff %` = round(100 * sum(Whiff) / n(), 1)
    ) %>%
    as.data.frame() -> basic
  basic <- basic[basic$Batter %in% activeroster$Name, ]
  
  return(basic)
  
}

getthsplits <- function(df) {
  
  df %>%
    group_by(Batter.Team) %>%
    summarise(
      PA = sum(PA),
      AB = sum(AB),
      H = sum(H),
      `2B` = sum(`2B`),
      `3B` = sum(`3B`),
      HR = sum(HR),
      BB = sum(BB),
      SO = sum(SO),
      AVG = round(H / AB, 3),
      OBP = round((H + BB + sum(HBP)) / PA, 3),
      SLG = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
      `OPS+` = round((OBP / LGOBP + SLG / LGSLG - 1) * 100),
      `BB %` = round(BB / PA * 100, 1),  
      `SO %` = round(SO / PA * 100, 1),
      `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
      `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
      xAVG = round(sum(xH) / sum(AB), 3),
      xSLG = round(sum(xTB) / sum(AB), 3),
      `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone == 0, na.rm = TRUE), 1),
      `Whiff %` = round(100 * sum(Whiff) / n(), 1)
    ) %>%
    rename(c(
      'Team' = 'Batter.Team'
    )) %>%
    as.data.frame() -> basic
  
  return(basic)
  
}

getpsplits <- function(df) {
  
  df %>%
    group_by(Pitcher) %>%
    summarise(
      BF = sum(PA),
      AB = sum(AB),
      H = sum(H),
      `2B` = sum(`2B`),
      `3B` = sum(`3B`),
      BB = sum(BB),
      SO = sum(SO),
      WHIP = round((BB + H) / ((AB - H - sum(E)) / 3), 3),
      FIP = round((((13 * sum(HR)) + (3 * (BB + sum(HBP))) - (2 * SO)) / ((AB - H - sum(E)) / 3))
                  + LGFIPConst, 2),
      `H/9` = round(9 * H / ((AB - H - sum(E)) / 3), 1),
      `HR/9` = round(9 * sum(HR) / ((AB - sum(HR) - sum(E)) / 3), 1),
      `BB/9` = round(9 * BB / ((AB - BB - sum(E)) / 3), 1),  
      `SO/9` = round(9 * SO / ((AB - SO - sum(E)) / 3), 1),
      `Opp AVG` = round(H / AB, 3),
      `Opp SLG` = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
      `Opp OPS` = `Opp AVG` + `Opp SLG`,
      `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
      `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
      xAVG = round(sum(xH) / sum(AB), 3),
      xSLG = round(sum(xTB) / sum(AB), 3),
      `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone == 0, na.rm = TRUE), 1),
      `Whiff %` = round(100 * sum(Whiff) / n(), 1)
    ) %>%
    as.data.frame() -> basic
  basic <- basic[basic$Pitcher %in% activeroster$Name, ]
  
  return(basic)
  
}

gettpsplits <- function(df) {
  
  df %>%
    group_by(Pitcher.Team) %>%
    summarise(
      BF = sum(PA),
      AB = sum(AB),
      H = sum(H),
      `2B` = sum(`2B`),
      `3B` = sum(`3B`),
      BB = sum(BB),
      SO = sum(SO),
      WHIP = round((BB + H) / ((AB - H - sum(E)) / 3), 3),
      FIP = round((((13 * sum(HR)) + (3 * (BB + sum(HBP))) - (2 * SO)) / ((AB - H - sum(E)) / 3))
                  + LGFIPConst, 2),
      `H/9` = round(9 * H / ((AB - H - sum(E)) / 3), 1),
      `HR/9` = round(9 * sum(HR) / ((AB - sum(HR) - sum(E)) / 3), 1),
      `BB/9` = round(9 * BB / ((AB - BB - sum(E)) / 3), 1),  
      `SO/9` = round(9 * SO / ((AB - SO - sum(E)) / 3), 1),
      `Opp AVG` = round(H / AB, 3),
      `Opp SLG` = round((sum(`1B`) + 2 * sum(`2B`) + 3 * sum(`3B`) + 4 * sum(HR)) / AB, 3), 
      `Opp OPS` = `Opp AVG` + `Opp SLG`,
      `Average Exit Velocity` = round(mean(Hit.Ball.Speed..mph., na.rm = TRUE), 1),
      `Hard Hit %` = round(100 * sum(HardHit, na.rm = TRUE) / sum(Tracked), 1),
      xAVG = round(sum(xH) / sum(AB), 3),
      xSLG = round(sum(xTB) / sum(AB), 3),
      `Chase %` = round(100 * sum(InZone == 0 & Swing == 1, na.rm = TRUE) 
                        / sum(InZone == 0, na.rm = TRUE), 1),
      `Whiff %` = round(100 * sum(Whiff) / n(), 1)
    ) %>%
    rename(c(
      'Team' = 'Pitcher.Team'
    )) %>%
    as.data.frame() -> basic
  
  return(basic)
  
}

# Heat Maps
league_ba <- sum(master$H) / sum(master$AB)

zone_xmin = -1.41667
zone_xmax = 1.41667
zone_ymin = 1.5
zone_ymax = 3.6 

zone_xleftmid = zone_xmin + ((zone_xmax - zone_xmin) / 3)
zone_xrightmid = zone_xmax - ((zone_xmax - zone_xmin) / 3)
zone_ybotmid = zone_ymin + ((zone_ymax - zone_ymin) / 3)
zone_ytopmid = zone_ymax - ((zone_ymax - zone_ymin) / 3)

zone_xmid = 0
zone_ymid = (zone_ymax + zone_ymin) / 2
xwidth = zone_xmax - zone_xrightmid
yheight = zone_ymax - zone_ytopmid

master %>%
  mutate(
    zone_1_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xmin &
        Pitch.Strike.Zone...Offset..ft. < zone_xleftmid &
        Pitch.Strike.Zone...Height..ft. > zone_ytopmid &
        Pitch.Strike.Zone...Height..ft. < zone_ymax &
        AB == 1, 1, 0
    ),
    zone_2_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xleftmid &
        Pitch.Strike.Zone...Offset..ft. < zone_xrightmid &
        Pitch.Strike.Zone...Height..ft. > zone_ytopmid &
        Pitch.Strike.Zone...Height..ft. < zone_ymax &
        AB == 1, 1, 0
    ),
    zone_3_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xrightmid &
        Pitch.Strike.Zone...Offset..ft. < zone_xmax &
        Pitch.Strike.Zone...Height..ft. > zone_ytopmid &
        Pitch.Strike.Zone...Height..ft. < zone_ymax &
        AB == 1, 1, 0
    ),
    zone_4_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xmin &
        Pitch.Strike.Zone...Offset..ft. < zone_xleftmid &
        Pitch.Strike.Zone...Height..ft. > zone_ybotmid &
        Pitch.Strike.Zone...Height..ft. < zone_ytopmid &
        AB == 1, 1, 0
    ),
    zone_5_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xleftmid &
        Pitch.Strike.Zone...Offset..ft. < zone_xrightmid &
        Pitch.Strike.Zone...Height..ft. > zone_ybotmid &
        Pitch.Strike.Zone...Height..ft. < zone_ytopmid &
        AB == 1, 1, 0
    ),
    zone_6_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xrightmid &
        Pitch.Strike.Zone...Offset..ft. < zone_xmax &
        Pitch.Strike.Zone...Height..ft. > zone_ybotmid &
        Pitch.Strike.Zone...Height..ft. < zone_ytopmid &
        AB == 1, 1, 0
    ),
    zone_7_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xmin &
        Pitch.Strike.Zone...Offset..ft. < zone_xleftmid &
        Pitch.Strike.Zone...Height..ft. > zone_ymin &
        Pitch.Strike.Zone...Height..ft. < zone_ybotmid &
        AB == 1, 1, 0
    ),
    zone_8_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xleftmid &
        Pitch.Strike.Zone...Offset..ft. < zone_xrightmid &
        Pitch.Strike.Zone...Height..ft. > zone_ymin &
        Pitch.Strike.Zone...Height..ft. < zone_ybotmid &
        AB == 1, 1, 0
    ),
    zone_9_AB = ifelse(
      Pitch.Strike.Zone...Offset..ft. > zone_xrightmid &
        Pitch.Strike.Zone...Offset..ft. < zone_xmax &
        Pitch.Strike.Zone...Height..ft. > zone_ymin &
        Pitch.Strike.Zone...Height..ft. < zone_ybotmid &
        AB == 1, 1, 0
    ),
    zone_1_H = ifelse(
      zone_1_AB == 1 & H == 1, 1, 0
    ),
    zone_2_H = ifelse(
      zone_2_AB == 1 & H == 1, 1, 0
    ),
    zone_3_H = ifelse(
      zone_3_AB == 1 & H == 1, 1, 0
    ),
    zone_4_H = ifelse(
      zone_4_AB == 1 & H == 1, 1, 0
    ),
    zone_5_H = ifelse(
      zone_5_AB == 1 & H == 1, 1, 0
    ),
    zone_6_H = ifelse(
      zone_6_AB == 1 & H == 1, 1, 0
    ),
    zone_7_H = ifelse(
      zone_7_AB == 1 & H == 1, 1, 0
    ),
    zone_8_H = ifelse(
      zone_8_AB == 1 & H == 1, 1, 0
    ),
    zone_9_H = ifelse(
      zone_9_AB == 1 & H == 1, 1, 0
    ),
    dead_zone_1_AB = ifelse(
      (Pitch.Strike.Zone...Height..ft. > zone_ymid & Pitch.Strike.Zone...Offset..ft. < zone_xmin) | 
        (Pitch.Strike.Zone...Height..ft. > zone_ymax & Pitch.Strike.Zone...Offset..ft. < zone_xmid), 1, 0
    ),
    dead_zone_2_AB = ifelse(
      (Pitch.Strike.Zone...Height..ft. > zone_ymid & Pitch.Strike.Zone...Offset..ft. > zone_xmax) | 
        (Pitch.Strike.Zone...Height..ft. > zone_ymax & Pitch.Strike.Zone...Offset..ft. > zone_xmid), 1, 0
    ),
    dead_zone_3_AB = ifelse(
      (Pitch.Strike.Zone...Height..ft. < zone_ymid & Pitch.Strike.Zone...Offset..ft. < zone_xmin) | 
        (Pitch.Strike.Zone...Height..ft. < zone_ymin & Pitch.Strike.Zone...Offset..ft. < zone_xmid), 1, 0
    ),
    dead_zone_4_AB = ifelse(
      (Pitch.Strike.Zone...Height..ft. < zone_ymid & Pitch.Strike.Zone...Offset..ft. > zone_xmax) | 
        (Pitch.Strike.Zone...Height..ft. < zone_ymin & Pitch.Strike.Zone...Offset..ft. > zone_xmid), 1, 0
    ),
    dead_zone_1_H = ifelse(
      dead_zone_1_AB == 1 & H == 1, 1, 0
    ),
    dead_zone_2_H = ifelse(
      dead_zone_2_AB == 1 & H == 1, 1, 0
    ),
    dead_zone_3_H = ifelse(
      dead_zone_3_AB == 1 & H == 1, 1, 0
    ),
    dead_zone_4_H = ifelse(
      dead_zone_4_AB == 1 & H == 1, 1, 0
    )
  ) -> masterAB

masterAB %>%
  drop_na(zone_1_AB) -> masterAB

masterAB %>%
  group_by(Batter) %>%
  summarise(
    zone_1_AVG = sum(zone_1_H) / sum(zone_1_AB),
    zone_2_AVG = sum(zone_2_H) / sum(zone_2_AB),
    zone_3_AVG = sum(zone_3_H) / sum(zone_3_AB),
    zone_4_AVG = sum(zone_4_H) / sum(zone_4_AB),
    zone_5_AVG = sum(zone_5_H) / sum(zone_5_AB),
    zone_6_AVG = sum(zone_6_H) / sum(zone_6_AB),
    zone_7_AVG = sum(zone_7_H) / sum(zone_7_AB),
    zone_8_AVG = sum(zone_8_H) / sum(zone_8_AB),
    zone_9_AVG = sum(zone_9_H) / sum(zone_9_AB),
    dead_zone_1_AVG = sum(dead_zone_1_H) / sum(dead_zone_1_AB),
    dead_zone_2_AVG = sum(dead_zone_2_H) / sum(dead_zone_2_AB),
    dead_zone_3_AVG = sum(dead_zone_3_H) / sum(dead_zone_3_AB),
    dead_zone_4_AVG = sum(dead_zone_4_H) / sum(dead_zone_4_AB)
  ) -> hitter_avgs

hitter_avgs[is.na(hitter_avgs)] <- league_ba

zonePlot <- data.frame(
  x = rep(c(zone_xmin, zone_xleftmid, zone_xrightmid), 3),
  y = rep(c(zone_ymin, zone_ybotmid, zone_ytopmid), each = 3),
  w = rep(c(xwidth), 9),
  h = rep(c(yheight), 9)
)

zonePlot[10,] <- c(zone_xmin - xwidth, zone_ymid, zone_xmid - (zone_xmin - xwidth), (zone_ymax - zone_ymid) + yheight)
zonePlot[11,] <- c(zone_xmid, zone_ymid, zone_xmid - (zone_xmin - xwidth), (zone_ymax - zone_ymid) + yheight)
zonePlot[12,] <- c(zone_xmin - xwidth, zone_ymin - yheight, zone_xmid - (zone_xmin - xwidth), (zone_ymax - zone_ymid) + yheight)
zonePlot[13,] <- c(zone_xmid, zone_ymin - yheight, zone_xmid - (zone_xmin - xwidth), (zone_ymax - zone_ymid) + yheight)

zonePlot %>%
  slice(10:13, 1:9) -> zonePlot

activeroster %>%
  arrange(Name) -> activeroster
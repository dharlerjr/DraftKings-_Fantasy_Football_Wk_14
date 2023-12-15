
# Load Team Images -------------------------------------------------------------

sPath = "C:/Users/David/OneDrive/Documents/R/ff-analysis/images/team logos"

ARI_img <- paste0(sPath, 'ARI.png')
ATL_img <- paste0(sPath, 'ATL.png')
BAL_img <- paste0(sPath, 'BAL.png')
BUF_img <- paste0(sPath, 'BUF.png')
CAR_img <- paste0(sPath, 'CAR.png')
CHI_img <- paste0(sPath, 'CHI.png')
CIN_img <- paste0(sPath, 'CIN.png')
CLE_img <- paste0(sPath, 'CLE.png')
DAL_img <- paste0(sPath, 'DAL.png')
DEN_img <- paste0(sPath, 'DEN.png')
DET_img <- paste0(sPath, 'DET.png')
GB_img <- paste0(sPath, 'GB.png')
HOU_img <- paste0(sPath, 'HOU.png')
IND_img <- paste0(sPath, 'IND.png')
JAC_img <- paste0(sPath, 'JAC.png')
KC_img <- paste0(sPath, 'KC.png')
LAC_img <- paste0(sPath, 'LAC.png')
LAR_img <- paste0(sPath, 'LAR.png')
LV_img <- paste0(sPath, 'LV.png')
MIA_img <- paste0(sPath, 'MIA.png')
MIN_img <- paste0(sPath, 'MIN.png')
NE_img <- paste0(sPath, 'NE.png')
NO_img <- paste0(sPath, 'NO.png')
NYG_img <- paste0(sPath, 'NYG.png')
NYJ_img <- paste0(sPath, 'NYJ.png')
PHI_img <- paste0(sPath, 'PHI.png')
PIT_img <- paste0(sPath, 'PIT.png')
SEA_img <- paste0(sPath, 'SEA.png')
SF_img <- paste0(sPath, 'SF.png')
TB_img <- paste0(sPath, 'TB.png')
TEN_img <- paste0(sPath, 'TEN.png')
WAS_img <- paste0(sPath, 'WAS.png')

teamImages <- c(ARI_img, ATL_img, BAL_img, BUF_img, CAR_img, CHI_img, CIN_img, 
                CLE_img, DAL_img, DEN_img, DET_img, GB_img, HOU_img, IND_img, 
                JAC_img, KC_img, LAC_img, LAR_img, LV_img, MIA_img, MIN_img, 
                NE_img, NO_img, NYG_img, NYJ_img, PHI_img, PIT_img, SEA_img,
                SF_img, TB_img, TEN_img, WAS_img)

# Set directory ----------------------------------------------------------------

setwd("C:/Users/David/OneDrive/Documents/R/ff-analysis/dfs-dk/wk-14-analysis")

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(ggimage)
library(ggrepel)
library(stringr)
library(png)

# Read in data -----------------------------------------------------------------

salaries <- read.csv("data/DKSalaries-Wk-14.csv")
contest <- read.csv("data/DK-Triple-Option-Contest.csv")

# Select columns ---------------------------------------------------------------

salaries <- salaries %>% select(Name, Position, TeamAbbrev, Salary, AvgPointsPerGame)
contest <- contest %>% select(Player, X.Drafted, FPTS) 

# Rename Columns ---------------------------------------------------------------

salaries <- salaries %>% rename("Player" = "Name", "Avg" = "AvgPointsPerGame", 
                                "Team" = "TeamAbbrev")
contest <- contest %>% rename("Rostership" = "X.Drafted")

# Clean Contest DF -------------------------------------------------------------

contest$Rostership <- str_sub(contest$Rostership, end = -2)
contest$Rostership = as.numeric(as.character(contest$Rostership)) 
contest <- contest %>% filter(Rostership > 0)

view(salaries)
view(contest)

# Join DFs ---------------------------------------------------------------------

playersDF <- contest %>% left_join(salaries, by = "Player")
playersDF <- playersDF %>% select(Player, Team, Position, FPTS, 
                         Avg, Salary, Value, Rostership)

# Add New Columns: Value & Performance -----------------------------------------

playersDF <- playersDF %>% mutate(Value = round(FPTS / (Salary / 1000), 2))
playersDF <- playersDF %>% mutate(Performance = round(FPTS - Avg, 2))

playersDF <- playersDF %>% select(Player, Team, Position, Salary, FPTS, 
                                  Avg, Performance, Value, Rostership)

# Account for Injured Players, Benched Players ---------------------------------

playersDF <- playersDF %>% filter(FPTS > 0)

view(playersDF)

# Note: While the majority of players who scored 0 fantasy points were likely
# injured or on the bench, there is a small chance that a handful of players 
# played the entire game and still did not score any fantasy points. To account 
# for this in the future, we would have to examine injury reports and depth 
# charts to determine exactly which players who scored 0 fantasy points need to 
# remain in our dataset. For the purpose of efficiency, however, in this analysis
# we will remove all players who scored 0 fantasy points from our Players 
# Dataframe. 

# Calculate summary statistics by position -------------------------------------

playersDF %>% group_by(Position) %>% 
  summarise("MeanPoints" = round(mean(FPTS), 2), 
            "MedianPoints" = round(median(FPTS), 2), 
            "MaxPoints" = max(FPTS), 
            "MeanValue" = round(mean(Value), 2), 
            "MedianValue" = round(median(Value), 2), 
            "MaxValue" = max(Value)) %>%
  arrange(desc(MedianPoints))

# Create Box Plots for Points by Position --------------------------------------

playersDF %>% 
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: DK Points by Position",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Create Box Plots for Salaries by Position ------------------------------------

playersDF %>%
  ggplot(mapping = aes(x = Position, y = Salary)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Salary by Position",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Salary") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Create Box Plots for Value by Position ---------------------------------------

playersDF %>%
  ggplot(mapping = aes(x = Position, y = Value)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Value by Position",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Create Scatterplots for Points vs. Salary, by Position

playersDF %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point() +
  facet_wrap( ~ factor(Position, levels = c("QB", "RB", "WR", "TE", "DST"))) + 
  labs(title = "Week 14: DK Points vs. Salary, by Position",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Points vs. Salary: QBs ----------------------------------------

playersDF %>% filter(Position == "QB") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  labs(title = "Week 14: DK Points vs. Salary - QBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Points vs. Salary: RBs ----------------------------------------

playersDF %>% filter(Position == "RB") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3) +
  labs(title = "Week 14: DK Points vs. Salary - RBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Points vs. Salary: WRs ----------------------------------------

playersDF %>% filter(Position == "WR") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3) +
  labs(title = "Week 14: DK Points vs. Salary - WRs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Points vs. Salary: TEs ----------------------------------------

playersDF %>% filter(Position == "TE") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3) +
  labs(title = "Week 14: DK Points vs. Salary - TEs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Points vs. Salary: DSTs ---------------------------------------

playersDF %>% filter(Position == "DST") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  labs(title = "Week 14: DK Points vs. Salary - DSTs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Create Scatterplots for Value vs. Salary, by Position -------------------------

playersDF %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point() +
  facet_wrap( ~ factor(Position, levels = c("QB", "RB", "WR", "TE", "DST"))) + 
  labs(title = "Week 14: Value vs. Salary, by Position",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Value vs. Salary: QBs -----------------------------------------

playersDF %>% filter(Position == "QB") %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 7300, y = 2.5, label = "20-Point Threshold", size = 3.75) +
  labs(title = "Week 14: Value vs. Salary - QBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Value vs. Salary: RBs -----------------------------------------

playersDF %>% filter(Position == "RB") %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 4500, y = 5.1, label = "20-Point Threshold", size = 3.5) +
  labs(title = "Week 14: Value vs. Salary - RBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Value vs. Salary: WRs -----------------------------------------

playersDF %>% filter(Position == "WR") %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 3900, y = 6.1, label = "20-Point Threshold", size = 3.5) +
  labs(title = "Week 14: Value vs. Salary - WRs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Value vs. Salary: TEs -----------------------------------------

playersDF %>% filter(Position == "TE") %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 7250, y = 3.5, label = "20-Point Threshold", size = 3.5) +
  labs(title = "Week 14: Value vs. Salary - TEs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Scatterplot for Value vs. Salary: DSTs -----------------------------------------

playersDF %>% filter(Position == "DST") %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point() + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  stat_function(fun = function(x) (15000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 2600, y = 6.25, label = "15-Point Threshold", size = 3.5) +
  labs(title = "Week 14: Value vs. Salary - DSTs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Rostership Analysis ----------------------------------------------------------

playersDF %>% summarise("MedianRostership" = round(median(Rostership), 2))

playersDF %>% group_by(Position) %>% 
  summarise("MedianRostership" = round(median(Rostership), 2)) %>%
  arrange(desc(MedianRostership))

view(playersDF)

# Rostership by Position -------------------------------------------------------

playersDF %>%
  ggplot(mapping = aes(x = Position, y = Rostership)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Rostership by Position",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Rostership") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Create HighOwned & LowOwned DFs ----------------------------------------------

quantile(playersDF$Rostership)
quantile(playersDF$Rostership, probs = 0.9)

nrow(playersDF %>% filter(Rostership >= 10))

highOwned <- playersDF %>% filter(Rostership >= 10)
lowOwned <- playersDF %>% filter(Rostership < 10)

ownership <- list(High = highOwned, 
                  Low = lowOwned)

playersDF <- ownership %>% bind_rows(.id = "Ownership")
playersDF <- playersDF %>% select(Player, Team, Position, FPTS, 
                                  Avg, Salary, Value, Rostership, Ownership)

view(playersDF)

# Ownership: Calculate summary statistics by Position & Ownership --------------

playersDF %>% group_by(Position, Ownership) %>% 
  summarise("MeanPoints" = round(mean(FPTS), 2), 
            "MedianPoints" = round(median(FPTS), 2), 
            "MaxPoints" = max(FPTS), 
            "MeanValue" = round(mean(Value), 2), 
            "MedianValue" = round(median(Value), 2), 
            "MaxValue" = max(Value)) %>%
  arrange(Ownership, desc(MaxPoints))

# Ownership: Create Side-by-Side Box Plots for Points by Position --------------

playersDF %>%
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ Ownership) + 
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: DK Points by Position, by Ownership",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Highly Owned: Create Box Plots for Points by Position ------------------------

highOwned %>%
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  ylim(0, 40) +
  labs(title = "Week 14: DK Points by Position, Highly Owned Players (>= 10%)",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Low Owned: Create Box Plots for Points by Position ---------------------------

lowOwned %>%
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  ylim(0, 40) +
  labs(title = "Week 14: DK Points by Position, Low Owned Players (< 10%)",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Ownership: Create Side-by-Side Box Plots for Salary by Position --------------

playersDF %>%
  ggplot(mapping = aes(x = Position, y = Salary)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ Ownership) + 
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Salary by Position, by Ownership",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Salary") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Highly Owned: Create Box Plots for Salaries by Position ----------------------

highOwned %>%
  ggplot(mapping = aes(x = reorder(Position, -Salary), y = Salary)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Salary by Position, Highly Owned Players",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Salary") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Low Owned: Create Box Plots for Salaries by Position -------------------------

lowOwned %>%
  ggplot(mapping = aes(x = reorder(Position, -Salary), y = Salary)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Salary by Position, Low Owned Players",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Salary") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Ownership: Create Side-by-Side Box Plots for Value by Position ---------------

playersDF %>%
  ggplot(mapping = aes(x = Position, y = Value)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ Ownership) + 
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Value by Position, by Ownership",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Highly Owned: Create Box Plots for Value by Position -------------------------

highOwned %>%
  ggplot(mapping = aes(x = reorder(Position, -Value), y = Value)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Value by Position, Highly Owned Players",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Low Owned: Create Box Plots for Value by Position ----------------------------

lowOwned %>%
  ggplot(mapping = aes(x = reorder(Position, -Value), y = Value)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Value by Position, Low Owned Players",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Create Box Plots for Points by Position, Color by Ownership ------------------

playersDF %>%
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(mapping = aes(color = Ownership), alpha = 0.75) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: DK Points by Position Colored by Ownership",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Create Top Scores DF ---------------------------------------------------------

qbs <- playersDF %>% filter(Position == "QB")
rbs <- playersDF %>% filter(Position == "RB")
wrs <- playersDF %>% filter(Position == "WR")
tes <- playersDF %>% filter(Position == "TE")
dsts <- playersDF %>% filter(Position == "DST")

topQB <- qbs %>% slice_max(FPTS, n = 1)
topRBs <- rbs %>% slice_max(FPTS, n = 3)
topWRs <- wrs %>% slice_max(FPTS, n = 4)
topTEs <- tes %>% slice_max(FPTS, n = 2)
topDST <- dsts %>% slice_max(FPTS, n = 1)

topScoresDF <- bind_rows(topQB, topRBs, topWRs, topTEs, topDST)
view(topScoresDF)

# Scatterplot of Top Scores by Position ----------------------------------------

topScoresDF %>%
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_point(alpha = 0.5) +
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  ylim(10, 40) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: DK Points by Position, Top Scores",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top 12 Analysis --------------------------------------------------------------

top12QBs <- qbs %>% slice_max(FPTS, n = 12)
top12RBs <- rbs %>% slice_max(FPTS, n = 12)
top12WRs <- wrs %>% slice_max(FPTS, n = 12)
top12TEs <- tes %>% slice_max(FPTS, n = 12)
top12DSTs <- dsts %>% slice_max(FPTS, n = 12)

top12DF <- bind_rows(top12QBs, top12RBs, top12WRs, top12TEs, top12DSTs)
view(top12DF)

# Top12: Calculate summary statistics by position ------------------------------

top12DF %>% group_by(Position) %>% 
  summarise("MeanPoints" = round(mean(FPTS), 2), 
            "MedianPoints" = round(median(FPTS), 2), 
            "MaxPoints" = max(FPTS), 
            "MeanValue" = round(mean(Value), 2), 
            "MedianValue" = round(median(Value), 2), 
            "MaxValue" = max(Value)) %>%
  arrange(desc(MedianPoints))

# Top12: Create Box Plots for Points by Position -------------------------------

top12DF %>%
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: DK Points by Position, Top 12",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Create Box Plots for Points by Position, Color by Ownership -----------

top12DF %>%
  ggplot(mapping = aes(x = reorder(Position, -FPTS), y = FPTS)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(mapping = aes(color = Ownership), alpha = 0.75) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: DK Points by Position Colored by Ownership, Top 12",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Points by Position, Color by Ownership ----------------

top12DF %>%
  ggplot(mapping = aes(x = Position, y = FPTS)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: DK Points by Position Colored by Ownership, Top 12",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("DK Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Create Box Plots for Salaries by Position -----------------------------

top12DF %>%
  ggplot(mapping = aes(x = Position, y = Salary)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Salary by Position, Top 12",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Salary") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Create Box Plots for Value by Position --------------------------------

top12DF %>%
  ggplot(mapping = aes(x = Position, y = Value)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_discrete(limits = c("QB", "RB", "WR", "TE", "DST")) +
  labs(title = "Week 14: Value by Position, Top 12",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Position") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Create Scatterplots for Points vs. Salary, by Position

top12DF %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point() +
  facet_wrap( ~ factor(Position, levels = c("QB", "RB", "WR", "TE", "DST"))) + 
  labs(title = "Week 14: Points vs. Salary, by Position, Top 12",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Points vs. Salary, Color by Ownership: QBs -------------

top12DF %>% filter(Position == "QB") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  ylim(10, 40) +
  labs(title = "Week 14: Points vs. Salary, Top 12 QBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Points vs. Salary, Color by Ownership: RBs -------------

top12DF %>% filter(Position == "RB") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  ylim(15, 30) +
  labs(title = "Week 14: Points vs. Salary, Top 12 RBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Points vs. Salary, Color by Ownership: WRs -------------

top12DF %>% filter(Position == "WR") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  ylim(10, 40) +
  labs(title = "Week 14: Points vs. Salary, Top 12 WRs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Points vs. Salary, Color by Ownership: TEs -------------

top12DF %>% filter(Position == "TE") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  ylim(5, 35) +
  labs(title = "Week 14: Points vs. Salary, Top 12 TEs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Points vs. Salary, Color by Ownership: DSTs ------------

top12DF %>% filter(Position == "DST") %>%
  ggplot(mapping = aes(x = Salary, y = FPTS)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 3.5) +
  ylim(5, 25) +
  labs(title = "Week 14: Points vs. Salary, Top 12 DSTs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Points") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Create Scatterplots for Value vs. Salary, by Position -------------------------

top12DF %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point() +
  facet_wrap( ~ factor(Position, levels = c("QB", "RB", "WR", "TE", "DST"))) + 
  labs(title = "Week 14: Value vs. Salary, by Position",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Value vs. Salary: QBs -----------------------------------------

top12QBs %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 4) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 7600, y = 3, label = "20-Point Threshold", size = 4) +
  labs(title = "Week 14: Value vs. Salary, Top 12 QBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Value vs. Salary: RBs -----------------------------------------

top12RBs %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 4) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 8500, y = 2.5, label = "20-Point Threshold", size = 4) +
  labs(title = "Week 14: Value vs. Salary, Top 12 RBs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Value vs. Salary: WRs -----------------------------------------

top12WRs %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 4) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 5000, y = 4.75, label = "20-Point Threshold", size = 4) +
  labs(title = "Week 14: Value vs. Salary, Top 12 WRs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Value vs. Salary: TEs -----------------------------------------

top12TEs %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 4) +
  stat_function(fun = function(x) (20000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 7250, y = 3.25, label = "20-Point Threshold", size = 4) +
  labs(title = "Week 14: Value vs. Salary, Top 12 TEs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))

# Top12: Scatterplot for Value vs. Salary: DSTs -----------------------------------------

top12DSTs %>%
  ggplot(mapping = aes(x = Salary, y = Value)) + 
  geom_point(mapping = aes(color = Ownership)) + 
  geom_text_repel(mapping = aes(label = Player), size = 4) +
  stat_function(fun = function(x) (15000 / x), inherit.aes = FALSE, 
                linewidth = .75, alpha = 0.8) +
  annotate("label", x = 2600, y = 6.25, label = "15-Point Threshold", size = 4) +
  labs(title = "Week 14: Value vs. Salary, Top 12 DSTs",
       caption = "By: David Harler Jr. | Data from: Draftkings Daily Fantasy") +
  xlab("Salary") + ylab("Value") + 
  theme(plot.title = element_text(size = 16), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(8, 10, 8, 10, "pt"))










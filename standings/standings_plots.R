# Weekly standings Plots
library(dplyr)
library(ggplot2)

# Functions ---------------------------------------------------------------

WeekFinder <- function(curtime) {
  # Define when NFL season starts
  start <- lubridate::dmy("04-09-14")

  # Today's date
  today <- lubridate::ymd(curtime)

  # Calculate which Week of the season we're in
  return(cur.week <- lubridate::week(today) - lubridate::week(start))
}

# Scrape Data -------------------------------------------------------------
library(XML)

# Define current week
cur.week <- WeekFinder(Sys.Date())

# Grab espn standings page
bad.rows <- c(1, 2, 7, 12, 17, 22, 23, 28, 33, 38)
stands <- readHTMLTable("http://espn.go.com/nfl/standings",
                        which = 1,
                        header = FALSE,
                        skip.rows = bad.rows,
                        stringsAsFactors = FALSE)

 write.csv(stands, paste0("data/week", cur.week), row.names = FALSE)

# Load data ---------------------------------------------------------------
library(quatts)

# Load team info file
tinfo <- read.csv("../team_info.csv", stringsAsFactors = FALSE)

# Load standings data
sdat <- dataloader("data/", "*.csv") %>%
          select(city.name = V1,
                 win  = V2,
                 loss = V3,
                 tie  = V4,
                 winp = V5)

# Add Week Counter
sdat$week <- expand.grid(1:32, 1:cur.week)$Var2

# Find Division Ranks
recs <- sdat %>%
          left_join(., tinfo, by = "city.name") %>%
          group_by(week, conference, division) %>%
          mutate(
            drank = rank(winp, ties.method = "min") * -1)

# Plots -------------------------------------------------------------------

ggplot(filter(recs, conference == "NFC", division == "West"),
              aes(week, drank, color = team.name, label = team.name))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  geom_text(data = filter(recs, conference == "NFC", division == "West",week == cur.week),
            hjust = -.2)+
  scale_x_continuous(limits = c(1, 16), breaks = seq(1, 16, 2))+
  scale_color_discrete(guide = FALSE)+
  scale_y_reverse(labels = 1:4)

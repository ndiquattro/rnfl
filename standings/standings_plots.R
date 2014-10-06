# Weekly standings Plots
library(dplyr)
library(ggplot2)

# Scrape Data -------------------------------------------------------------
library(XML)

# Grab espn standings page
bad.rows <- c(1, 2, 7, 12, 17, 22, 23, 28, 33, 38)
stands <- readHTMLTable("http://web.archive.org/web/20140910045622/http://espn.go.com/nfl/standings",
                        which = 4,
                        header = FALSE,
                        skip.rows = bad.rows,
                        stringsAsFactors = FALSE)

 write.csv(stands, "data/week1.csv", row.names = FALSE)

# Load data ---------------------------------------------------------------
library(quatts)

tinfo <- read.csv("../team_info.csv", stringsAsFactors = FALSE)

sdat <- dataloader("data/", "*.csv") %>%
          select(city.name = V1,
                 win  = V2,
                 loss = V3,
                 tie  = V4,
                 winp = V5)

# Add Week Counter
cur.week <- (dim(sdat)[1] / 32)
sdat$week <- expand.grid(1:32, 1:cur.week)$Var2

# Add team info
sdat <- left_join(sdat, tinfo, by = "city.name")

# Find Division Ranks
dat <- sdat %>%
        group_by(week, conference, division) %>%
        mutate(
          drank = rank(winp, ties.method = "max") * -1)



# Plots -------------------------------------------------------------------

ggplot(filter(dat, conference == "NFC", division == "West"),
              aes(week, drank, color = team.name, label = team.name))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  geom_text(data = filter(dat, conference == "NFC", division == "West",week == cur.week),
            hjust = -.2)+
  scale_x_continuous(limits = c(1, 16), breaks = seq(1, 16, 2))+
  scale_color_discrete(guide = FALSE)+
  scale_y_reverse(labels = 1:4)

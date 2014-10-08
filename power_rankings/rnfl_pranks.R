# Visualization for r/NFL Power Rankings
# Fall - 2014

# Load Libraries
library(dplyr)
library(ggplot2)
library(quatts)

# Functions ---------------------------------------------------------------

DataGrabber <- function(url) {
  # Takes a published google spreadsheet and puts it into a local data-frame
  # Important: MUST edit URL to have "output=csv"
  # Args:
  #   url: spreadsheet URL
  # Returns:
  #   sdat: a data.frame

  # Get spreadsheet raw dat
  gdat <- RCurl::getURL(url)

  # Read into a dataframe
  sdat <- read.csv(text = gdat, header=TRUE, stringsAsFactors=FALSE)
}

WeekFinder <- function(curtime) {
  # Define when NFL season starts
  start <- lubridate::dmy("04-09-14")

  # Today's date
  today <- lubridate::ymd(curtime)

  # Calculate which Week of the season we're in
  return(cur.week <- lubridate::week(today) - lubridate::week(start))
}

# Make text file of latest week -------------------------------------------
library(XML)
library(jsonlite)

# Define Current week
cur.week <- WeekFinder(Sys.Date())

# Initial comment grab
jurl <- 'http://www.reddit.com/r/nfl/new/.json'
coms <- fromJSON(jurl)

# While loop until the rankings have posted
while (sum(coms$data$children$data$author == "NFLPowerRankers") == 0) {
  # Display Time
  print(Sys.time())

  # Wait for a bit
  Sys.sleep(10)

  # Get recent comments in r/NFL
  coms <- fromJSON(jurl)
}

# Announce when rankings have been posted.
system("say power rankings have been posted!")

# Find which index has rankings
useridx <- which(coms$data$children$data$author == "NFLPowerRankers")

# Pull self text and clean up HTML
stxt <- coms$data$children$data$selftext_html[[useridx]]
  stxt <- gsub("&lt;", "<", stxt)
  stxt <- gsub("&gt;", ">", stxt)
  stxt <- gsub("&amp;", "&", stxt)

# Save thread ID for posting to reddit
thread.id <- coms$data$children$data$id[[useridx]]

# Find gdoc link
doclink <- as.character(
             getNodeSet(htmlParse(stxt),
                        "//a[contains(@href, 'docs.google.com')]/@href")[[1]])
  # replace html with csv
  doclink <- gsub("html", "csv", doclink)

# Grab data
gdat <- DataGrabber(doclink) %>%
          select(team = Teams, med.scr = Median, mn.scr = Mean.,
                 sd.scr = Standard.Deviation)

# Write to file
fname <- paste0("data/week", cur.week, ".csv")
write.csv(gdat, fname, row.names = FALSE)

# Load Data ---------------------------------------------------------------

# Load Ranks
rawdat <- dataloader("data", "week*", header = TRUE)
  # Add week column
  rawdat$week <- expand.grid(1:32, 0:cur.week)$Var2

# Load Colors
team.colors <- read.csv("nfl_colors.csv", stringsAsFactors = FALSE)
  # Make color pallette
  tcols <- team.colors$color1
  names(tcols) <- team.colors$team

# Load team info
tinfo <- read.csv("../team_info.csv", stringsAsFactors = FALSE)
  # Change names to work with this script
  names(tinfo) <- c("city", "team", "conf", "div")

# Assign to pranks data
pranks <- rawdat %>%
            left_join(., tinfo, by = "team") %>%
            mutate(
              cd = paste(conf, div, sep=" "),
              conf = factor(conf, c("NFC", "AFC")),
              div = factor(div, c("North", "East", "South", "West")) ) %>%
            group_by(week) %>%
            mutate(
              ovrank = rank(med.scr, ties.method = "first"),
              quantrnk = cut(mn.scr, quantile(mn.scr), 1:4, TRUE)) %>%
            group_by(week, cd) %>%
            mutate(
              divrank = factor(rank(med.scr, ties.method = "first")) ) %>%
            group_by(week, conf) %>%
            mutate(
              conf.mn = mean(mn.scr) )

# Current Week Plots ------------------------------------------------------

# All Teams
oranks.plot <-
  ggplot(filter(pranks, week == cur.week),
         aes(x = ovrank, y = ovrank, color = factor(quantrnk),
            xmin = ovrank - sd.scr, xmax = ovrank + sd.scr, label = team))+
  geom_errorbarh()+
  #geom_point(size = 3)+
  geom_text(aes(x = (ovrank - sd.scr) - 2.5))+
  geom_text(aes(label = ovrank),color = "black")+
  scale_y_reverse(labels = NULL)+
  scale_x_continuous(limits = c(-3, 33))+
  labs(x = NULL, y = paste0("Mean Power Rankings - Week ", cur.week),
       color = "Quantiles")+
  scale_color_discrete(labels = c("1st", "2nd", "3rd", "4th"), guide = F)

# Mean Ranks of each Division
mndiv.plot <-
  ggplot(filter(pranks, week == cur.week), aes(cd, mn.scr, color = conf))+
    stat_summary(fun.y=mean, fun.ymin = min, fun.ymax = max, geom="pointrange",
                 size = 2)+
    geom_segment(aes(x = 0, xend = 9, y = conf.mn, yend = conf.mn,
                     color = conf), linetype = 2)+
    scale_y_reverse()+
    scale_color_manual(values = c("blue", "red"))+
    scale_x_discrete(labels = c("East", "North", "South", "West", "East",
                                "North", "South", "West"))+
    labs(y = paste0("Division Mean Power Rankings - Week ", cur.week),
         x = NULL, color = "Conference")

# By Division
bydiv.plot <-
  ggplot(filter(pranks, week == cur.week),
         aes(x = reorder(team, mn.scr), y = mn.scr, color = divrank,
             ymin = mn.scr - sd.scr, ymax = mn.scr + sd.scr,
             label = abbreviate(team)))+
    geom_linerange(alpha = .8)+
    geom_text(fontface = "bold")+
    facet_wrap(conf ~ div, scales = "free_x", nrow = 2, ncol = 4)+
    scale_y_reverse()+
    scale_x_discrete(breaks = NULL)+
    scale_color_discrete(guide = FALSE)+
    labs(y = paste0("Power Rankings - Week ", cur.week), x = NULL)

# History Plots -----------------------------------------------------------

# Make path and directory for saving images
spath <- paste0("images/week", cur.week)
dir.create(spath, showWarnings = FALSE)

# Overall History plot
hplot <-
  ggplot(filter(pranks, week == cur.week | week == cur.week-1),
                aes(week, ovrank, ymin = mn.scr - sd.scr,
                     ymax = mn.scr + sd.scr, color = team, label = team))+
    geom_point(size = 4)+
    geom_line(size = 2)+
    geom_text(data = pranks[pranks$week == cur.week, ],
            hjust = -.2, fontface = "bold")+
    scale_x_continuous(limits = c(cur.week-1, cur.week+.5),
                       breaks = seq(cur.week-1:cur.week))+
    scale_y_reverse()+
    scale_color_manual(values = tcols, guide = FALSE)+
    labs(x = "Week", y = "Power Ranking")

# History plot by Division Plot
HistPlot <- function(div){
  cplot <- ggplot(pranks[pranks$cd == div, ],
                  aes(x = week,
                      y = mn.scr,
                      ymin = mn.scr - sd.scr,
                      ymax = mn.scr + sd.scr,
                      color = team,
                      label = team))+
          geom_line()+
          geom_pointrange(size = 1, position = "dodge")+
          geom_text(data = pranks[pranks$cd == div & pranks$week == cur.week, ],
                    hjust = -.2, fontface = "bold")+
          scale_y_reverse()+
          scale_color_manual(values = tcols, guide = FALSE)+
          scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, 2))+
          labs(x = "Week", y = "Power Ranking", title = div)

  # Save plot
   filen <- paste0(spath,"/", div, ".png")
   ggsave(filen, cplot, height = 4, width = 8)
}

# Make plot for each division
hplots <- lapply(unique(pranks$cd), HistPlot)

# Save Plots --------------------------------------------------------------

# Save Current week plots
ggsave(paste0(spath, "/overall_ranks.png"), oranks.plot, height = 6, width = 10)
ggsave(paste0(spath, "/div_mean_ranks.png"), mndiv.plot, height = 4, width = 8)
ggsave(paste0(spath, "/divranks.png"), bydiv.plot, height = 4, width = 8)

# Save overall history plot
ggsave(paste0(spath, "/overall_hist1.png"), hplot, height = 10, width = 5)

# Upload images to imgur --------------------------------------------------
library(imguR)

# Load API Keys
keys <- read.csv("../../apikeys.txt", stringsAsFactors = FALSE, row.names = 1)

# Make auth token
itoken <- imgur_login(client = keys["imgur", "client"],
                      secret = keys["imgur", "secret"],
                      cache = TRUE)

# Create this week's ablum
album <- create_album(title = paste("Week", cur.week, sep=" "), token = itoken)

# Upload images
pups <- lapply(list.files(spath, full.names = TRUE),
               upload_image, album = album$id, token = itoken)

# Pull out links
ilinks <- data.frame(links = unlist(lapply(pups, function(x) x$link)))

# Generate Reddit Comment -------------------------------------------------

# Function to make markdown link
linkr <- function(txt, url){
  paste0("* [", txt, "](", url, ")")
  #debug <- "* [Click this](http://imgur.com)"
}

# Generate body text
ctxt <- c("# Power Ranking PLOTS!",
          "By Division, by Conference, with History.",
          " ",
          linkr("Album Link", paste0("http://i.imgur.com/a/", album$id)),
          " ",
          "#### League level plots",
          linkr("Overall Rankings", ilinks$links[12]),
          linkr("Change Plot", ilinks$links[11]),
          linkr("Division Mean Ranks, error bars are min/max rank within division, dotted lines are conference means", ilinks$links[5]),
          linkr("Ranks by Division", ilinks$links[6]),
          " ",
          "#### Division level history plots",
          " ",
          "## AFC",
          linkr("North", ilinks$links[2]),
          linkr("East", ilinks$links[1]),
          linkr("South", ilinks$links[3]),
          linkr("West", ilinks$links[4]),
          " ",
          "## NFC",
          linkr("North", ilinks$links[8]),
          linkr("East", ilinks$links[7]),
          linkr("South", ilinks$links[9]),
          linkr("West", ilinks$links[10]),
          " ",
          "_Error bars are +/- 1 standard deviation, except where noted_"
          )

# Print markdown code
writeLines(ctxt)

# Post comment to reddit --------------------------------------------------
library(httr)

# Grab oauth token
rtoken <- content(
            POST("https://ssl.reddit.com/api/v1/access_token",
              body = list(
                grant_type = "password",
                username = keys["reddit", "user"],
                password = keys["reddit", "pass"]
              ),
              encode = "form",
              authenticate(keys["reddit", "client"], keys["reddit", "secret"])
            ))

# Make the post
ahead <- paste(rtoken$token_type, rtoken$access_token, sep = " ")  # make auth

POST("https://oauth.reddit.com/api/comment",
      body = list(
        text = paste0(ctxt, collapse = "\n"),
        thing_id = paste0("t3_", thread.id),
        api_type = "json"),
      add_headers(Authorization = ahead),
      user_agent("PowerRanks Uploader by box_plot"))

system("say Comment posted!")

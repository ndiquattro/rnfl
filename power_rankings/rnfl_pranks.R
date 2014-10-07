# Visualization for r/NFL Power Rankings

# Load Libraries
library(dplyr)
library(ggplot2)

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

DataLoader <- function(folder, expr) {
  # Loads files within a folder that matches a regular expression and combines
  # them into one data frame.
  # Args:
  #   folder: path to folder that contains data files.
  #   expr: regular expression that filters which files are loaded.
  # Returns:
  #   datf: a data.frame

  # Find files
  file.list <- list.files(folder, expr, full.names=TRUE)

  # Make read.csv function
  Reader <- function(files){
    read.csv(files, header=TRUE, stringsAsFactors=FALSE)
  }

  # Apply Reader to file.list
  datf <- do.call("rbind", lapply(file.list, Reader))
}

# Make text file of latest week -------------------------------------------
# library(XML)
# library(RCurl)
# library(jsonlite)
#
# # Initial comment grab
# coms <- fromJSON(getURL('http://www.reddit.com/r/nfl/new/.json'))
#
# # While loop until the rankings have posted
# while (sum(coms$data$children$data$author == "NFLPowerRankers") == 0) {
#   # Display Time
#   print(Sys.time())
#
#   # Wait for a bit
#   Sys.sleep(30)
#
#   # Get recent comments in r/NFL
#   coms <- fromJSON(getURL('http://www.reddit.com/r/nfl/new/.json'))
# }
#
# # Find which index has rankings
# useridx <- which(coms$data$children$data$author == "NFLPowerRankers")
#
# # Pull self text and title
# stxt <- coms$data$children$data$selftext_html[[useridx]]
#   stxt <- gsub("&lt;", "<", stxt)
#   stxt <- gsub("&gt;", ">", stxt)
#   stxt <- gsub("&amp;", "&", stxt)
#
# # Pull current week out of post title
# cur.week <- coms$data$children$data$title[[useridx]]
# cur.week <- max(unique(as.numeric(unlist(cur.week))), na.rm = TRUE)
#
# # Parse HTML
# parsed <- htmlParse(stxt)
#
# # Find gdoc link
# doclink <- as.character(getNodeSet(parsed, "//a[contains(@href, 'docs.google.com')]/@href")[[1]])
#   # replace html
#   doclink <- gsub("html", "csv", doclink)
#
# # Grab data
# gdat <- DataGrabber(doclink) %>%
#           select(team = Teams, med.scr =Median, mn.scr = Mean.,
#                  sd.scr = Standard.Deviation)
#
# # Write to file
# fname <- paste0("data/week", cur.week, ".csv")
# write.csv(gdat, fname, row.names = FALSE)

# Load Data ---------------------------------------------------------------

# Load Ranks
rawdat <- DataLoader("data", "week*")
  # Add Week Counter
  cur.week <- (dim(rawdat)[1] / 32) - 1
  rawdat$week <- expand.grid(1:32, 0:cur.week)$Var2

# Load Colors
team.colors <- read.csv("nfl_colors.csv", stringsAsFactors = FALSE)
  # Make color pallette
  tcols <- team.colors$color1
  names(tcols) <- team.colors$team

# Define division relationships
nfc.n <- c("Packers", "Lions", "Bears", "Vikings")
nfc.e <- c("Cowboys", "Eagles", "Giants", "Redskins")
nfc.s <- c("Saints", "Falcons", "Panthers", "Buccaneers")
nfc.w <- c("49ers", "Seahawks", "Rams", "Cardinals")

afc.n <- c("Bengals", "Browns", "Steelers", "Ravens")
afc.e <- c("Patriots", "Dolphins", "Jets", "Bills")
afc.s <- c("Texans", "Jaguars", "Titans", "Colts")
afc.w <- c("Raiders", "Broncos", "Chargers", "Chiefs")

# Find quantiles
quants <- quantile(seq(1,32,1))

# Assign to pranks data
pranks <-
  rawdat %>%
    mutate(
      conf = ifelse(team %in% c(nfc.n, nfc.e, nfc.s, nfc.w), "NFC", "AFC"),
      div = ifelse(team %in% c(nfc.w, afc.w), "West", NA),
      div = ifelse(team %in% c(nfc.e, afc.e), "East", div),
      div = ifelse(team %in% c(nfc.n, afc.n), "North", div),
      div = ifelse(team %in% c(nfc.s, afc.s), "South", div),
      cd = paste(conf, div, sep=" "),
      conf = factor(conf, c("NFC", "AFC")),
      div = factor(div, c("North", "East", "South", "West")) ) %>%
      group_by(week) %>%
      mutate(
        quantrnk = ifelse(mn.scr < quants[2], 1, 99),
        quantrnk = ifelse(mn.scr > quants[2] & mn.scr < quants[3], 2, quantrnk),
        quantrnk = ifelse(mn.scr > quants[3] & mn.scr < quants[4], 3, quantrnk),
        quantrnk = ifelse(mn.scr > quants[4], 4, quantrnk),
        ovrank = rank(mn.scr) )%>%
      group_by(week, cd) %>%
      mutate(
        divrank = factor(rank(mn.scr)) ) %>%
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

# Make path for saving images
spath <- paste0("images/week", cur.week)

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
   #filen <- paste0(spath,"/", div, ".png")
   #ggsave(filen, cplot, height = 4, width = 8)
}

# Make plot for each division
hplots <- lapply(unique(pranks$cd), HistPlot)

# Save Plots --------------------------------------------------------------

# # Save Current week plots
# ggsave(paste0(spath, "/overall_ranks.png"), oranks.plot, height = 6, width = 10)
# ggsave(paste0(spath, "/div_mean_ranks.png"), mndiv.plot, height = 4, width = 8)
# ggsave(paste0(spath, "/divranks.png"), bydiv.plot, height = 4, width = 8)
#
# # Save overall history plot
# ggsave(paste0(spath, "/overall_hist1.png"), hplot, height = 10, width = 5)


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
ilinks <- data.frame(names = plots,
                     links = unlist(lapply(pups, function(x) x$link)))

# Generate Reddit Comment -------------------------------------------------

# Function to make markdown link
linkr <- function(txt, url){
  paste0("* [", txt, "](", url, ")")
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

# Auth with Reddit
client_id <- keys["reddit", "client"]
secret <- keys["reddit", "secret"]

postForm("https://ssl.reddit.com/api/v1/access_token?grant_type=password",
         username = keys["reddit", "user"],
         password = keys["reddit", "pass"],
         .opts = list(u = paste(keys["reddit", "client"],
                                      keys["reddit", "secret"],
                                      sep = ":"))
         )

library(httr)
req <- POST("https://ssl.reddit.com/api/v1/access_token",
             body = list(
               grant_type = "password",
               username = keys["reddit", "user"],
               password = keys["reddit", "pass"]
             ),
             encode = "form",
             authenticate(keys["reddit", "client"], keys["reddit", "secret"])
        )

rtoken <- content(req)

test <- GET("https://oauth.reddit.com/api/v1/me",
     add_headers(Authorization = paste("bearer",
                                       rtoken$access_token, sep = " ")),
     user_agent("PowerRanks Uploader by box_plot")
     )

test <- GET("https://oauth.reddit.com/api/v1/me",
            add_headers(Authorization = paste("bearer",
                                              rtoken$access_token, sep = " ")),
            user_agent("PowerRanks Uploader by box_plot")
)

pmakec <- POST("https://oauth.reddit.com/api/comment",
               body = list(
                 text = "Hello World",
                 thing_id = "t3_2ibwp8",
                 api_type = "json"),
               add_headers(Authorization = paste("bearer",
                                                 rtoken$access_token, sep = " ")),
               user_agent("PowerRanks Uploader by box_plot")
               )
content(pmakec)


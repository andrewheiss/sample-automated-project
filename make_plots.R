#----------------
# Load packages
#----------------
# This is the long-winded way to do this, but it guarantees that the packages 
# will be installed if they aren't already
pkgs <- c("dplyr", "ggplot2")
invisible(lapply(pkgs, function(x) if(!is.element(x, installed.packages()[, 1]))
  install.packages(x, repos=c(CRAN = "http://cran.rstudio.com"))))
invisible(suppressPackageStartupMessages(lapply(pkgs, library, character.only=TRUE)))


#------------
# Load data
#------------
olympic.medals <- read.csv("Data/olympic-medals.csv")

# Years
winter <- c(2002, 2006, 2010)
summer <- c(2000, 2004, 2008, 2012)


#------------------
# Winter olympics
#------------------
# Get an ordered list of the top medal earners
top.winter <- olympic.medals %>% 
  filter(Year %in% winter) %>%
  group_by(Country) %>% 
  summarize(Total = sum(Total)) %>% 
  arrange(desc(Total))

# Look at total medal counts for top 6 countries
top.winter.year <- olympic.medals %>%
  filter(Year %in% winter, Country %in% top.winter$Country[1:6]) %>%
  group_by(Country, Year) %>%
  summarize(Total = sum(Total)) %>%
  ungroup() %>%
  mutate(Year = factor(Year, levels=rev(winter))) %>%
  mutate(Country = factor(Country, levels=rev(top.winter$Country[1:6]), ordered=TRUE))

# Plot everything
p <- ggplot(top.winter.year, aes(x=Country, y=Total, fill=Year))
winter.medals <- p + geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("#0A0D27", "#636675", "#ACADC2")) + 
  labs(x=NULL, y="Total medals won") + 
  theme_bw(8) + coord_flip()

# Save the plot
ggsave(winter.medals, filename="Output/winter_medals.pdf", width=7, height=4, units="in")


#-----------------------
# Summer olympics too!
#-----------------------
# Get order of medal counts
top.summer <- olympic.medals %>% 
  filter(Year %in% summer) %>%
  group_by(Country) %>% 
  summarize(Total = sum(Total)) %>% 
  arrange(desc(Total))

# Look at total medal counts for top 6 countries
top.summer.year <- olympic.medals %>%
  filter(Year %in% summer, Country %in% top.summer$Country[1:6]) %>%
  group_by(Country, Year) %>%
  summarize(Total = sum(Total)) %>%
  ungroup() %>%
  mutate(Year = factor(Year, levels=rev(summer))) %>%
  mutate(Country = factor(Country, levels=rev(top.summer$Country[1:6]), ordered=TRUE))

# Plot everything
p <- ggplot(top.summer.year, aes(x=Country, y=Total, fill=Year))
summer.medals <- p + geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("#C7C100", "#236DFF", "#86ADFF", "#CC2714")) + 
  labs(x=NULL, y="Total medals won") + 
  theme_bw(8) + coord_flip()

# Save the plot
ggsave(summer.medals, filename="Output/summer_medals.pdf", width=7, height=4, units="in")

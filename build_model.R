#-------------------------
# Load packages and data
#-------------------------
pkgs <- c("dplyr", "stargazer")
invisible(lapply(pkgs, function(x) if(!is.element(x, installed.packages()[, 1]))
  install.packages(x, repos=c(CRAN = "http://cran.rstudio.com"))))
invisible(suppressPackageStartupMessages(lapply(pkgs, library, character.only=TRUE)))

olympic.medals <- read.csv("Data/olympic-medals.csv")


#-----------------------------
# Model total ~ age + season
#-----------------------------
model.data <- olympic.medals %>%
  mutate(Season = factor(ifelse(Year %in% c(2002, 2006, 2010), "Winter", "Summer"))) %>%
  group_by(Country, Year, Season) %>%
  summarize(Total = sum(Total), Age = mean(Age, na.rm=TRUE))

model <- lm(Total ~ Age + Season, data=model.data)
# summary(model)


# Save model output
stargazer(model, type="html", out="Output/medal_model.docx")

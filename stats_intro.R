## Introduction to Statistics


# Loading data
# Needed to -> install.packages
library(tidyverse)
library(here)
library(janitor)
library(kableExtra)

darwin <- read_csv(here("data","darwin.csv"))


# Checking
glimpse(darwin)
head(darwin)
colnames(darwin)


# Data cleaning
darwin <- janitor::clean_names(darwin)
darwin %>% 
  duplicated() %>%
  sum()
summary(darwin)


# Visualisation
darwin %>%
  ggplot(aes(x=type,
             y=height))+
  geom_point()


# Comparing groups
darwin %>% group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))


# Turning compared group into object
darwin_summary <- darwin %>% group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))


#Summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()


# kable extra functions
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position ="left")


## Part 2 - Differences

# Pivoting data + creating new column with self height subtracted from cross height
darwin_wide <- darwin %>% 
  pivot_wider(names_from = "type", values_from = "height") %>%
  mutate(difference = Cross - Self)


# Calculating mean difference and variance (as SD)
diff_summary <- darwin_wide %>%
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

diff_summary <- diff_summary %>% mutate(se = sd/sqrt(n))
# Summary sentence = the average difference in height was 2.62 ± 1.22 inches (mean ± SE)

## Normal distribution
# Mean = centre/peak of bell curve, SD = how long tails are/width of bell
# Large SD = wide + squat bellcurve, small SD = narrow bell curve with tall peak

# Bell curve data creation
x <- seq(-4, 4, length=100)
# vec of values showing height of prob distribution for each x value
y <- dnorm(x)

#plot x ann y as scatterplot
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "Axis X", ylab = "Axis Y")
axis(1, at = -3:3, labels = c("-3s", "-2s","-1s","mean","1s", "2s","3s"))
# Confidence intervals
lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI
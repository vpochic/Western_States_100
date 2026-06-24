### Western States Endurance Run: temperature and percent of finishers ###
## Author: V. POCHIC
# Last modif: 2026/06/24

## Description ####

# The goal here is to answer the following question:
# Does temperature influence the percentage of finishers
# at the Western States 100-mile Endurance Race (WSER)?

# (And, as a side quest, how did the percentage of finishers evolve since the
# first edition in 1974?)

# For this, we will use a dataset that is displayed publicly on the WSER
# website, that I modified slightly to make it easier to handle and more
# consistent. I made sure that the numbers are strictly the same.
# Url of the WSER website: https://www.wser.org/

## Packages ####

library(tidyverse)
library(RColorBrewer)
library(cmocean)
library(mgcv)
library(ggnewscale)

####------------------------------------------------------------------------####
## Import and curate data ####

WSER_data <- read.csv2('Data/WSER_temperature_finishers_data.csv',
                       header = TRUE, fileEncoding = 'ISO-8859-1')

# First of all, we will compute the temperature in proper degrees celsius, 
# because we're serious scientists and won't work with f*cking Fahrenheit...
WSER_data <- WSER_data %>%
  # To convert F into C, we have to apply the relation C = (F-32)*(5/9)
  # My God is the Fahrenheit scale stupid...
  mutate(Temp_high_C = (Temp_high_F-32)*(5/9)) %>%
  mutate(Temp_low_C = (Temp_low_F-32)*(5/9)) %>%
  # Compute the temperature range
  mutate(Temp_range = Temp_high_C-Temp_low_C) %>%
  # Get the date in Date format
  mutate(Date = dmy(Date)) %>%
  mutate(Year = year(Date)) %>%
  mutate(First_man_time = hms(First_man_time)) %>%
  mutate(First_woman_time = hms(First_woman_time))

## Simple plots ####

# Ok, for a start, let's plot the percentage of finishers as a time series,
# with the maximum temperature as color scale

ggplot(WSER_data) +
  geom_point(aes(x = Date, y = Finish_percent, fill = Temp_high_C),
             size = 3.5, alpha = .8, color = 'grey10', 
             stroke = .05, shape = 21) +
  # color scales
  scale_fill_distiller(palette = 'RdBu', direction = -1) +
  # Labels
  labs(title = 'Percent of finishers at the Western States',
       x = 'Year', y = NULL,
       fill = 'Temperature (°C): ') +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                  linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# Interesting! Several things to see here. 
# First, there are a few points with
# either really low or really high values on the y-scale, in the 70s. This is
# because in the first 4 editions, there were only 1, 1, 1 and 14 runners! The
# 5th edition in 1978 had only 63.

# So, we're going to exclude these points from our analysis, as they are clearly
# outliers regarding the number of runners. We'll start with the 1979 edition,
# which had 143 runners. Nowadays, there are consistently 370 runners on the
# start line each year.

WSER_data_2 <- WSER_data %>%
  filter(N_runners > 100)

# Note: this also filters out the 2 cancelled editions, in 2008 because of
# forest fires and 2020 because of the COVID-19 pandemic.

# Let's plot the same graph with the new data!
ggplot(WSER_data_2) +
  geom_point(aes(x = Date, y = Finish_percent, fill = Temp_high_C),
             size = 3.5, alpha = .8, color = 'grey10', 
             stroke = .05, shape = 21) +
  # color scales
  scale_fill_distiller(palette = 'RdBu', direction = -1) +
  # y-axis limits
  scale_y_continuous(limits = c(25,100)) +
  # Labels
  labs(title = 'Percent of finishers at the Western States',
       x = 'Year', y = NULL,
       fill = 'Temperature (°C): ') +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                    linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# Ok nice!
# Let's see the percentage of finishers as a function of temperature now
ggplot(WSER_data_2) +
  geom_point(aes(x = Temp_high_C, y = Finish_percent, fill = Temp_high_C),
             size = 3.5, alpha = .8, color = 'grey10', 
             stroke = .05, shape = 21) +
  # color scales
  scale_fill_distiller(palette = 'RdBu', direction = -1) +
  # x- and y-axis limits
  scale_y_continuous(limits = c(25,100)) +
  scale_x_continuous(limits = c(15,45)) +
  # Labels
  labs(title = 'Percent of finishers at the Western States',
       x = 'Temperature (°C)', y = NULL,
       fill = 'Temperature (°C): ') +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                    linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# We can guess there's something, but the relation is not clear-cut.
# Instead of colouring the dots by temperature, let's color them by year on this
# graph.
# While we're at it, we'll also compute the decade, this will come at handy 
# later. We'll do that with a function:
floor_decade = function(value){ return(value - value %% 10) }

WSER_data_2 <- WSER_data_2 %>%
  mutate(Year = year(Date)) %>%
  mutate(Decade = floor_decade(Year))

ggplot(WSER_data_2) +
  geom_point(aes(x = Temp_high_C, y = Finish_percent, fill = Year),
             size = 3.5, alpha = .8, color = 'grey10', 
             stroke = .05, shape = 21) +
  # color scales
  scale_fill_cmocean(name = 'haline') +
  # x- and y-axis limits
  scale_y_continuous(limits = c(25,100)) +
  scale_x_continuous(limits = c(15,45)) +
  # Labels
  labs(title = 'Percent of finishers at the Western States',
       x = 'Temperature (°C)', y = NULL,
       fill = 'Year: ') +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                    linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# There is a clear trend that we already saw in the first graph: recent editions
# tend to have higher percentages of finishers. That's just the average level of
# runners becoming higher and higher as time goes by.

# So, we will need to disentangle the effects of temperature and time.

####------------------------------------------------------------------------####
### Generalised Linear Model ####

# One way to estimate the effects of both parameters (time and temperature) is
# generalised linear models, or GLMs.

# Fortunately for us, my colleague and friend Bede Davies did a whole bunch of
# tutorials for doing GLMs in R!
# https://bedeffinianrowedavies.com/statisticstutorials/introductionglms

# First we need to transform our response variable (percentage of finishers) so
# it follows a Beta distribution. This is very easy, we just have to divide it
# by 100 so it's bound between 0 and 1.
WSER_data_2 <- WSER_data_2 %>%
  mutate(Finish_freq = Finish_percent/100)

# Now we call the GLM.
# The formula means we want to predict the frequency of finishers based on the
# maximum temperature recorded in Auburn AND the Year, AND the interaction
# between the 2.
glm_WSER <- gam(Finish_freq ~ Temp_high_C*Year,
                data = WSER_data_2,
                # Specifying a beta distribution of the variable
                family = betar(link="logit"))

summary(glm_WSER)

# Let's look at what the model tells us in detail.

## Diagnostic plots ####

### Checking the model
ModelOutputs<-data.frame(Fitted=fitted(glm_WSER),
                         Residuals=resid(glm_WSER))

# We're gonna make our own qq plot with colors identifying sites
# We base it on the structure of the model
qq_data <- glm_WSER$model
# then we add the values of fitted and residuals
qq_data <- bind_cols(qq_data, ModelOutputs)

# And (qq-)plot
qqplot_custom <- ggplot(qq_data) +
  stat_qq(aes(sample=Residuals), alpha = .7) +
  stat_qq_line(aes(sample=Residuals)) +
  theme_classic() +
  labs(y="Sample Quantiles",x="Theoretical Quantiles")

qqplot_custom

# It's quite ok, the data fall reasonably along the 1:1 line, even if there's
# some deviation.

# We can do the same for residuals vs fitted
RvFplot_custom <- ggplot(qq_data)+
  geom_point(aes(x=Fitted,y=Residuals), 
             alpha = .7) +
  theme_classic() +
  labs(y="Residuals",x="Fitted Values")

RvFplot_custom

# No apparent structure in the data, so it's ok.

# And let's do one last diagnostic plot with histogram of residuals
HistRes_custom <- ggplot(qq_data, aes(x = Residuals))+
  geom_histogram(binwidth = 1)+
  theme_classic() +
  labs(x='Residuals', y = 'Count')

HistRes_custom

# The residuals are approximately centered around 0, so this is ok. It's a bit
# skewed towards the negatives but nothing too bad.

## Model predictions ####

# Now that we've checked that our model is ok, we can start analysing its
# results. Here, we will want to use the model's formula to predict values in
# a given range of years and temperatures, and see how it compares to the true
# data.

# Let's predict with the GLM
NewData_1 <- expand_grid(
  # We create a Year range that goes from our first year (1979) 
  # to present (2026)
  Year=seq(min(WSER_data_2$Year), 
           max(WSER_data_2$Year)+1),
  # We also add the temperature data, with a range that goes from 18°C to 42°C
  # (to the maximum of recorded temperatures)which roughly correspond to the
  # min/max in the dataset), with a step of 0.5
  Temp_high_C=seq(18, 42, by = .5))

# And now we predict with our model based on this blank dataset
Pred <- predict(glm_WSER, NewData_1, se.fit=TRUE, type="response")

# Let's compute the equivalent of 95% confidence intervals
WSER_model <- NewData_1 %>% 
  mutate(response=Pred$fit,
         se.fit=Pred$se.fit,
         Upr=response+(se.fit*1.96),
         Lwr=response-(se.fit*1.96))

# But now, how do we plot that?

### Model plots ####

# First, let's try to just plot the percentage of finishers as a function of 
# temperature.
# Because we modeled both variables and their interaction, we will plot this for
# 5 years, representative of the 5 decades of our dataset.

# Let's define a nice color palette
palette_sierra_5years <- c('#759AD9', '#225E6C', '#B2A078', 
                           '#6E7005', '#0C2802')

ggplot(subset(WSER_model, Year %in% c(1983,1991,2003,2013,2024))) +
  # a shaded area for the confidence interval
  geom_ribbon(aes(x = Temp_high_C,
                  # (We multiply by 100 to transform frequences 
                  # into percentages.)
                  ymax = Upr*100,
                  ymin = Lwr*100, group = as_factor(Year),
                  fill = as_factor(Year)),
              alpha=0.25) +
  # a line for the model fit
  geom_line(aes(x = Temp_high_C, y = response*100,
                group = as_factor(Year), color = as_factor(Year)),
             linewidth = 1.5, alpha = .8) +
  # color scales
  scale_color_discrete(palette = palette_sierra_5years) +
  scale_fill_discrete(palette = palette_sierra_5years) +
  # x- and y-axis limits
  scale_y_continuous(limits = c(40,100)) +
  scale_x_continuous(limits = c(18,42)) +
  # Labels
  labs(title = 'Model: % finishers at WSER',
       subtitle = 'depending on temperature',
       fill = 'Modeled year: ', color = 'Modeled year: ',
       x = 'Temperature (°C)', y = NULL,) +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                    linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# Great! What about plotting some true data on top?

ggplot(subset(WSER_model, Year %in% c(1983,1991,2003,2013,2024))) +
  
  # a shaded area for the confidence interval
  geom_ribbon(aes(x = Temp_high_C,
                  # (We multiply by 100 to transform frequences 
                  # into percentages.)
                  ymax = Upr*100,
                  ymin = Lwr*100, group = as_factor(Year),
                  fill = Year),
              alpha=0.45) +
  # a line for the model fit
  geom_line(aes(x = Temp_high_C, y = response*100,
                group = as_factor(Year), color = Year),
            linewidth = 1.5, alpha = .8) +
  
  # Now, points for true data
  geom_point(data = WSER_data_2,
             # We'll fill them by year to see if it fits the model
             aes(x = Temp_high_C, y = Finish_percent,
                 fill = Year), alpha = .75,
             stroke = .05, shape = 21, size = 3) +

  
  # color scales
  scale_color_cmocean(name = 'haline', guide = 'none') +
  scale_fill_cmocean(name = 'haline',
                     breaks = c(1980, 2000, 2020),
                     labels = c('1980', '2000', '2020')) +
  
  # Labels
  labs(title = 'Model: % finishers at WSER',
       subtitle = 'depending on temperature, across years',
       fill = 'Modelled year: ', color = 'Modelled year: ',
       x = 'Temperature (°C)', y = NULL,) +

  # x- and y-axis limits
  scale_y_continuous(limits = c(40,100)) +
  scale_x_continuous(limits = c(18,42)) +

  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                    linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# Excellent!

# Now let's look at the effect of the year

ggplot(subset(WSER_model, Temp_high_C %in% c(20, 25, 30, 35, 40))) +
  
  # a shaded area for the confidence interval
  geom_ribbon(aes(x = Year,
                  # (We multiply by 100 to transform frequences 
                  # into percentages.)
                  ymax = Upr*100,
                  ymin = Lwr*100, group = as_factor(Temp_high_C),
                  fill = Temp_high_C),
              alpha=0.45) +
  # a line for the model fit
  geom_line(aes(x = Year, y = response*100,
                group = as_factor(Temp_high_C), color = Temp_high_C),
            linewidth = 1.5, alpha = .8) +
  
  # Now, points for true data
  geom_point(data = WSER_data_2,
             # We'll fill them by year to see if it fits the model
             aes(x = Year, y = Finish_percent,
                 fill = Temp_high_C), alpha = .75,
             stroke = .05, shape = 21, size = 3) +
  
  
  # color scales
  scale_color_distiller(palette = 'RdBu', direction = -1, guide = 'none') +
  scale_fill_distiller(palette = 'RdBu', direction = -1) +
  
  # Labels
  labs(title = 'Model: % finishers at WSER',
       subtitle = 'depending on the year, across temperatures',
       fill = 'Modelled temperature: ', color = 'Modelled temperature: ',
       x = 'Year', y = NULL,) +
  
  # x- and y-axis limits
  scale_y_continuous(limits = c(40,100)) +
  scale_x_continuous(limits = c(1979, 2026)) +
  
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                    linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# I'm not a huge fan of the looks of this one, because the color palette is not
# completely adapted for that. Anyway, I think it's alright.
####------------------------------------------------------------------------####
### Bonus plot ####

# How about a little bonus plot with the finish times of the 1st man and woman
# for each edition?

WSER_data <- WSER_data %>%
  mutate(Distance_miles = as_factor(Distance_miles)) %>%
  mutate(Distance_miles = fct_relevel(Distance_miles,
                                      c('89', '93.5', '?', '100.2')))

# A little color palette
sierra_3 <- c('#759AD9', '#225E6C', '#6E7005')

ggplot(WSER_data) +
  ## Finish times
  # Points
  geom_point(aes(x = Year, y = First_man_time),
             size = 3.5, color = '#2B4561', fill = '#76A7E2',
             stroke = .15, shape = 21) +
  geom_point(aes(x = Year, y = First_woman_time),
             size = 3.5, alpha = .8, color = '#711412', fill = '#FC4D6B',
             stroke = .15, shape = 21) +
  # Lines
  geom_line(aes(x = Year, y = First_man_time),
            linewidth = .7, alpha = .8, color = '#2B4561', linetype = 2) +
  geom_line(aes(x = Year, y = First_woman_time),
            linewidth = .7, alpha = .8, color = '#711412', linetype = 2) +
  
  ## Let's add the distance of the course
  geom_line(data = subset(WSER_data, Distance_miles %in% c('89','93.5','100.2')),
            aes(x = Year, y = hms('13H 00M 00S'), 
                color = as_factor(Distance_miles)), linewidth = 3) +
  
  # color scale for distance
  scale_color_discrete(palette = sierra_3) +
  
  # y-axis scale
  scale_y_time(limits = c(hms('13H 00M 00S'), hms('31H 00M 00S')),
               breaks = c(hms('14H 00M 00S'), hms('16H 00M 00S'),
                          hms('20H 00M 00S'), hms('24H 00M 00S'),
                          hms('30H 00M 00S'))
               ) +
  
  # Labels
  labs(title = 'Time of first man and woman at WSER',
       subtitle = '(1974-2025)',
       x = 'Year', y = 'Time in hours, minutes, seconds',
       color = 'Course distance
     (miles): ') +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = 'white', color = 'grey10',
                                         linewidth = .25),
        legend.frame = element_rect(fill = 'transparent', color = 'grey10',
                                    linewidth = .25),
        legend.ticks = element_line(color = 'grey10',
                                    linewidth = .25))

# Pretty good!

####----------------------------End of script-------------------------------####
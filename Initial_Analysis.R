setwd("C:\\Users\\Karthik\\Desktop\\infosec")
data.file <- file.path('data3.csv')
powertable <- read.table(data.file, header = TRUE, sep = ',',fill = TRUE)

# Create a numeric vector containing just real power data
Global_active_power <- (as.numeric(as.character(powertable$realpkwh)))
summary(Global_active_power)


min(Global_active_power)
max(Global_active_power)


c(min(Global_active_power), max(Global_active_power))


range(Global_active_power)

quantile(Global_active_power,na.rm=TRUE)

quantile(Global_active_power, probs = seq(0, 1, by = 0.20))

my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / length(x))
}

my.var(Global_active_power) - var(Global_active_power)

# Update the variance function to make it unbiased.
my.var <- function(x)
{
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x) - 1))
}


my.var(Global_active_power) - var(Global_active_power)

# Check the range predicted by the variance function.
c(mean(Global_active_power) - var(Global_active_power), mean(Global_active_power) + var(Global_active_power))


c(mean(Global_active_power) - var(Global_active_power), mean(Global_active_power) + var(Global_active_power))

range(Global_active_power)


my.sd <- function(x)
{
  return(sqrt(my.var(x)))
}

# Test our standard deviation function for correctness.
my.sd(Global_active_power) - sd(Global_active_power)


c(mean(Global_active_power) - sd(Global_active_power), mean(Global_active_power) + sd(Global_active_power))


range(Global_active_power)

c(mean(Global_active_power) - sd(Global_active_power), mean(Global_active_power) + sd(Global_active_power))


c(quantile(Global_active_power, probs = 0.25), quantile(Global_active_power, probs = 0.75))

#Visualize
library('ggplot2')

# Load the data from scratch for purity.
data.file <- file.path('data', '3.csv')
powertable <- read.csv(data.file, header = TRUE, sep = ',')

# Experiment with histograms.
ggplot(powertable, aes(x = realpkwh)) +
  geom_histogram(binwidth = 1)


ggplot(powertable, aes(x = realpkwh)) +
  geom_histogram(binwidth = 0.01)


ggplot(powertable, aes(x = realpkwh)) +
  geom_density()

ggplot(powertable, aes(x = realpkwh, fill = date)) +
  geom_density()


ggplot(powertable, aes(x = reactivep, fill = date)) +
  geom_density()

# Produce two facets in a single plot to make it easier to see the hidden structure.
ggplot(powertable, aes(x = reactivep, fill = date)) +
  geom_density() +
  facet_grid(date ~ .)

# Experiment with random numbers from the normal distribution.
m <- 0
s <- 1
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) +
  geom_density()

ggplot(powertable, aes(x = realpkwh, y = reactivep)) +
  geom_point()

# Add a smooth shape that relates the two explicitly.
ggplot(powertable, aes(x = realpkwh, y = reactivep)) +
  geom_point() +
  geom_smooth()

ggplot(powertable[1:20, ], aes(x = realpkwh, y = time)) +
  geom_point() +
  geom_smooth()


ggplot(powertable[1:2000, ], aes(x = realpkwh, y = time)) +
  geom_point() +
  geom_smooth()


# Visualize how realpkwhower depends on time and date
ggplot(powertable, aes(x = realpkwh, y = time)) +
  geom_point(aes(color = date, alpha = 0.25)) +
  scale_alpha(guide = "none") + 
  scale_color_manual(values = c("16/12/2006" = "black", "17/12/2006" = "gray")) +
  theme_bw()

# using bright colors.
ggplot(powertable, aes(x = realpkwh, y = time, color = date)) +
  geom_point()

powertable <- transform(powertable,
                             date1 = ifelse(date == '16/12/2006', 1, 0))

logit.model <- glm(date1 ~ realpkwh + reactivep,
                   data = powertable,
                   family = binomial(link = 'logit'))

ggplot(powertable, aes(x = realpkwh, y = time)) +
  geom_point(aes(color = date, alpha = 0.25)) +
  scale_alpha(guide = "none") + 
  scale_color_manual(values = c("16/12/2006" = "blue", "17/12/2006" = "red")) +
  theme_bw() +
  stat_abline(intercept = -coef(logit.model)[1] / coef(logit.model)[2],
              slope = - coef(logit.model)[3] / coef(logit.model)[2],
              geom = 'abline',
              color = 'black')

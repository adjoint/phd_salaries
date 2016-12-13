library(ggplot2)
library(dplyr)
library(GGally)

data = read.csv("uni_data.csv")

m1 <- lm(times_rank ~ usnews_rank, data = data)
summary(m1)

ggplot(data = data, aes(x = times_rank, y = usnews_rank)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

m2 <- lm(median_early ~ median_mid, data = data)
summary(m2)

ggplot(data = data, aes(x = median_early, y = median_mid)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggpairs(data, columns = 5:10)

ggpairs(data, columns = 2:7)

m3 <- lm(median_early ~ times_rank + usnews_rank + all_docs, data = data)
summary(m3)

m2 <- lm(median_early ~ times_rank + usnews_rank, data = data)
summary(m2)

m2 <- lm(median_early ~ times_rank + all_docs, data = data)
summary(m2)

m2 <- lm(median_early ~ usnews_rank + all_docs, data = data)
summary(m2)

m1 <- lm(median_early ~ times_rank, data = data)
summary(m1)

m1 <- lm(median_early ~ usnews_rank, data = data)
summary(m1)

m1 <- lm(median_early ~ all_docs, data = data)
summary(m1)

ggplot(data = data, aes(x = times_rank, y = median_early)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

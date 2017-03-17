library(lattice)

echo = TRUE

activite <- NULL
activite <- read.csv("activity.csv", header = T, sep = ",")

echo = TRUE
df_summary <- NULL
su2 <- NULL
su <- NULL
mn_int <- NULL
activite2 <- NULL
mean_su2 <- NULL
median_su2 <- NULL
activite2_weekend <- NULL
activite2_weekday <- NULL
mean_activite2_weekday <- NULL
mean_activite2_weekend <- NULL

echo = TRUE
su <- tapply(activite$steps, activite$date, sum, na.rm=T)

echo = TRUE
hist(su, xlab = "Relevé des étapes par jour", ylab = "Fréquence", main = "Histogramme des étapes par jour")

echo = TRUE
mean_su <- round(mean(su))
median_su <- round(median(su))

print(c("Cette moyenne est",mean_su))

echo = TRUE
mn_int <- tapply(activite$steps, activite$interval, mean, na.rm=T)
plot(mn_int ~ unique(activite$interval), type="l", xlab = "Intervalle de 5-min")

echo = TRUE
mn_int[which.max(mn_int)]

echo = TRUE
table(is.na(activite) == TRUE)

summary(activite)

echo = TRUE
activite2 <- activite  # creation of the dataset that will have no more NAs
for (i in 1:nrow(activite)){
  if(is.na(activite$steps[i])){
    activite2$steps[i]<- mn_int[[as.character(activite[i, "interval"])]]
  }
}

echo = TRUE
su2 <- tapply(activite2$steps, activite2$date, sum, na.rm=T)
hist(su2, xlab = "Relevé des étapes par jour", ylab = "Fréquence",main = "Histogramme des étapes par jour")

mean_su2 <- round(mean(su2))
median_su2 <- round(median(su2))

echo = TRUE
print(c("La moyenne est",mean_su2))

print(c("La médiane est",median_su2))

echo = TRUE
df_summary <- rbind(df_summary, data.frame(mean = c(mean_su, mean_su2), median = c(median_su, median_su2)))
rownames(df_summary) <- c("avec NA's", "sans NA's")
print(df_summary)

echo = TRUE
summary(activite2)

echo = TRUE
activite2$weekday <- c("weekday")
activite2[weekdays(as.Date(activite2[, 2])) %in% c("Saturday", "Sunday", "samedi", "dimanche", "saturday", "sunday", "Samedi", "Dimanche"), ][4] <- c("weekend")
table(activite2$weekday == "weekend")

activite2$weekday <- factor(activite2$weekday)

echo = TRUE
activite2_weekend <- subset(activite2, activite2$weekday == "weekend")
activite2_weekday <- subset(activite2, activite2$weekday == "weekday")

mean_activite2_weekday <- tapply(activite2_weekday$steps, activite2_weekday$interval, mean)
mean_activite2_weekend <- tapply(activite2_weekend$steps, activite2_weekend$interval, mean)

echo = TRUE
library(lattice)
df_weekday <- NULL
df_weekend <- NULL
df_final <- NULL
df_weekday <- data.frame(interval = unique(activite2_weekday$interval), avg = as.numeric(mean_activite2_weekday), day = rep("weekday", length(mean_activite2_weekday)))
df_weekend <- data.frame(interval = unique(activite2_weekend$interval), avg = as.numeric(mean_activite2_weekend), day = rep("weekend", length(mean_activite2_weekend)))
df_final <- rbind(df_weekday, df_weekend)

xyplot(avg ~ interval | day, data = df_final, layout = c(1, 2), type = "l", ylab = "Nombre d'étapes", xlab = "Intervalle")


library(readxl)
SieveData <- read_excel("~/Documents/DT3/SieveData.xlsx")
View(SieveData)

library(ggplot2)
five_min <- mean((SieveData$`B5`-SieveData$`A5`)/SieveData$`B5`)
ten_min <- mean((SieveData$`B10`-SieveData$`A10`)/SieveData$`B10`)
fifteen_min <- mean((SieveData$`B15`-SieveData$`A15`)/SieveData$`B15`)
se <- 100*c(sd((SieveData$`B5`-SieveData$`A5`)/SieveData$`B5`)/(3^(1/2)), sd((SieveData$`B10`-SieveData$`A10`)/SieveData$`B10`)/(3^(1/2)), sd((SieveData$`B15`-SieveData$`A15`)/SieveData$`B15`)/(3^(1/2)))
df <- data.frame(times = c(5,10,15),
                 percentage = 100*c(five_min,ten_min,fifteen_min))
head(df)

#Standard curve linear regression

equation = function(x) {
  lm_coef <- list(a = round(summary(x)$coef[1], digits = 3),
                  b = round(summary(x)$coef[2], digits = 3),
                  r2 = round(summary(x)$r.squared, digits = 3));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, lm_coef)
  as.character(as.expression(lm_eq));                 
}  

fit <- lm(formula = percentage ~ times, data = df)

p <- ggplot(df,aes(x = times, y = percentage)) +
  #ggtitle("Consistency of Diced Cartilage, 1.3g") +
  scale_y_continuous(limits = c(0,100), name = "Percentage of pieces <1mm (%)") +
  scale_x_continuous(name = "Dicing time (mins)", breaks = seq(0,15,5)) +
  geom_smooth(method=lm, linetype = "dashed", size = 0.75, se=FALSE, color = "steelblue") +
  geom_point(shape = 21, size = 2.5, color = "steelblue", fill = "steelblue") +
  geom_errorbar(aes(ymin=percentage-se, ymax=percentage+se), width=0,
                position=position_dodge(.9), color = "steelblue", size = 0.75) +
  annotate("text", x = 10, y = 0.05, label = equation(fit), size = 6, parse = TRUE) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text=element_text(size=12),
      axis.title=element_text(size=12),
      plot.title=element_text(hjust=0.5))
p

#Cartilage loss bar plot

five_minloss <- mean((1.3-SieveData$`B5`)/1.3)
ten_minloss <- mean((1.3-SieveData$`B10`)/1.3)
fifteen_minloss <- mean((1.3-SieveData$`B15`)/1.3)
se <- 100*c(sd((1.3-SieveData$`B5`)/1.3)/(3^(1/2)), sd((1.3-SieveData$`B10`)/1.3)/(3^(1/2)), sd((1.3-SieveData$`B15`)/1.3)/(3^(1/2)))
df_loss <- data.frame(times_lost = c(5,10,15),
                 percentage_lost = 100*c(five_min,ten_min,fifteen_min))

plt <- ggplot(data=df_loss, aes(x=times_lost, y=percentage_lost)) +
  geom_errorbar(aes(ymin=percentage_lost-0.5, ymax=percentage_lost+se), width=1,
                position=position_dodge(.9)) +
  geom_bar(position=position_dodge(.9), stat="identity", fill = "steelblue") +
  labs(x = "Dicing time (mins)", y = "Percentage of cartilage lost during dicing (%)") +
  scale_y_continuous(limits = c(0,100)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(hjust=0.5))
plt

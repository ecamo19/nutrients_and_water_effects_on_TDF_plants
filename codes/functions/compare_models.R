# Code developer https://gist.github.com/dsparks/4332698
library(ggplot2)
model1Frame <- data.frame(Variable = rownames(summary(models$rmf)$tTable), 
                          Coefficient = summary(models$rmf)$tTable[, 1],
                          SE = summary(models$rmf)$tTable[, 2], modelName = "South Indicator")


model2Frame <- data.frame(Variable = rownames(summary(models$smf)$tTable),
                          Coefficient = summary(models$smf)$tTable[, 1], 
                          SE = summary(models$smf)$tTable[, 2],
                          modelName= "Age Interaction")

model3Frame <- data.frame(Variable = rownames(summary(model3)$tTable),
                          Coefficient = summary(model3)$tTable[, 1],
                          SE = summary(model3)$tTable[, 2],
                          modelName = "Univariate")

allModelFrame <- data.frame(rbind(model1Frame, model2Frame)) 



# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

zp1 <- ggplot(allModelFrame, aes(colour = modelName))

zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle("Comparing several models")
print(zp1)  # The trick to these is position_dodge().
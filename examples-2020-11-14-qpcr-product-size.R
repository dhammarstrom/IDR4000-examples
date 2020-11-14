
### Checking product sizes from qPCR reaction ############

# This example uses data from https://trainome.github.io/sw_pcr_size_determination.html
# 




# Create a data frame of known distances and molecular weights
ladder <- data.frame(dist = c(29, 43.5, 60.5,
                              80.5, 106.5,  141.5,  
                              181.5,    243.5,  281.5,
                              328.5,    390.5,  465.5,  580.5), 
                     mw = c(1000, 900, 800, 
                            700, 600, 500,
                            400, 300, 250, 
                            200, 150, 100, 50))


# Create a new data frame of unknowns
unknown <- data.frame(dist = c(470.5,   391.5,  528.5,
                               421.5,   507.5))


# Fit the model
cal <- lm(log(mw) ~ dist, data = ladder)

# Check model performance, R^2 should be ~ 1.
summary(cal)

ladder %>%
  ggplot(aes(dist, log(mw))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)



# Predict sizes
preds <- exp(predict(cal, newdata = unknown) )

# Expected sizes 
expected <- c(96, 148, 65, 122, 78)

library(tidyverse)

data.frame(preds, expected, 
           primer = c("MyHC1", "MyHC2A", "MyHC2X", "B2m", "CHMP2A")) %>%
  ggplot(aes(expected, preds, label = primer)) + 
  geom_text(position = position_nudge(y = 5, x = 5)) +
  geom_point()

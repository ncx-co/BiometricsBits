####
#Companion code for April 2019 Biometrics Bits article on Bayes' theorem
#Brian Clough
#brian@silviaterra.com
####

library(brms)
library(ggplot2)
library(dplyr)
library(tidybayes)

#Simulate a simple dataset for one stand. 
standMean <- 75
standSD <- 5
nPlots <- 100

bapaDraws <- rnorm(nPlots, mean = standMean, sd = standSD)

trainingData <- data.frame(plot_id = seq(1:nPlots), bapa = bapaDraws)

#Drop some no tally plots for training remote sensing models
tdNoZero <- trainingData %>%
            filter(bapa > 0) 

#Informed prior to simulate domain knowledge; sample_prior = 'only' uses only draws from the prior
#distribution. Note that in this example, we are setting the prior as the known mean and variance of
#the population we simulated above. Try setting it to some "wrong" priors and see what happens!

baPriors <- c(set_prior('normal(75, 5)', class = 'Intercept'))

dkOnlyFit <- brm('bapa  ~ 1', data = tdNoZero, family = 'gaussian', prior = baPriors,
               sample_prior = 'only', seed = 1234)

#A wide, flat prior allows only the data to influence predictions.
widePriors <- c(set_prior('normal(0, 100)', class = 'Intercept'))
invOnlyFit <- brm('bapa  ~ 1', data = tdNoZero, family = 'gaussian', prior = widePriors,
                  seed = 1234)

#Fitted model to data using informed prior specifications. 
bothFit <- brm('bapa ~ 1', data = tdNoZero, family = 'gaussian', 
               prior = baPriors, seed = 1234)


##Generate predictions and summarize. 
dkOut <- data.frame(model = 'Experience only') %>%
         add_predicted_draws(dkOnlyFit)

invOnlyOut <- data.frame(model = 'Data only') %>%
              add_predicted_draws(invOnlyFit)

bothOut <- data.frame(model = 'Data & experience') %>%
  add_predicted_draws(invOnlyFit)


plotData <- bind_rows(dkOut, invOnlyOut, bothOut) %>%
            filter(.prediction > 0 & .prediction < 250)

plotData$model_f <- factor(plotData$model, levels=c('Experience only', 'Data only',
                                                    'Data & experience'))

fig2 <- ggplot(plotData, aes(x = .prediction)) + geom_histogram(col = 'dark green') + facet_wrap(~model_f) +
      theme_bw() + xlab('Simulated basal area per acre') + ylab('Number of draws')

ggsave(fig2, file = '/tmp/figure_2.pdf')

statTab <- plotData %>%
           group_by(model_f) %>%
           summarise(
             postMean = mean(.prediction),
             postMedian = median(.prediction),
             postSD = sd(.prediction),
             postLower = quantile(.prediction, 0.05),
             postUpper = quantile(.prediction, 0.9)
           )






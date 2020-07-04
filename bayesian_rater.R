library(tidyverse)
library(ggplot2)
library(dplyr)
library(BAS)

#Read in dataset
movie_data <- get(load("/Users/Dohyun/Downloads/movies.Rdata"))
movie_data


#The dataset consists of 456 randomly sampled movies released between 1972 to 2014 from IMDB and Rotten Tomatoes. Since the samples were not randomly assigned, we cannot infer causality, making this an observational study rather than an experimental one.

### Exploratory Data Analysis

movieDF = mutate(movie_data,
                 feature_film = ifelse(title_type == 'Feature Film', "yes", "no"),
                 drama = ifelse(genre == 'Drama', "yes", "no"),
                 mpaa_rating_R = ifelse(mpaa_rating == 'R', "yes", "no"),
                 oscar_season = ifelse(thtr_rel_month %in% 10:12 ,"yes","no"),
                 summer_season = ifelse(thtr_rel_month %in% 5:8 ,"yes","no"))

#visualize the relationship between audience scores and different variables of a film
eda <- movieDF %>% select(audience_score, feature_film, drama, mpaa_rating_R, oscar_season, summer_season)

#spreads out the data visuals
gatherDF <- gather(eda,key=varname,value=val,-audience_score)

#plot the boxplots
ggplot(data = gatherDF, aes(x=val,y=audience_score,fill=val)) + 
  geom_boxplot() +
  facet_grid(~varname) +
  xlab("") + ylab("Audience Score") +
  labs(title="Audience Score by Variable",fill="Key")


#Despite feature films being the most present type of film, it has a lower audience score rating of around 60 while non-features have a median score of around 85. Meanwhile, every other category seems to be fairly balanced.

### Bayesian Modeling

#Here we build our Bayesian Regression Model:
  
set.seed(0)
fit1 <- bas.lm(audience_score ~ feature_film + drama +
                 runtime + mpaa_rating_R + thtr_rel_year +
                 oscar_season + summer_season + imdb_rating +
                 imdb_num_votes + critics_score +
                 best_pic_nom + best_pic_win + best_actor_win +
                 best_actress_win + best_dir_win + top200_box,
               data = movieDF,
               prior = "BIC",
               method = "MCMC",
               modelprior = uniform())
fit1

#Checking our assumptions
par(mfrow=c(2,2))
plot(fit1, which=c(1, 2), ask=FALSE)
plot(fit1, which=4, ask=FALSE, cex.lab=0.5)

#The coefficients under each variable represent the likelihood (from 0 to 1) that it is included in the posterior model. 

### Testing our Model

#Test Run 1
grep("Avengers: Endgame", movieDF$title)

#run the movie through the model
testRun1 <- data.frame(feature_film ="yes",
                        drama ="yes",
                        runtime = 181,
                        mpaa_rating_R = "no",
                        thtr_rel_year= 2019,
                        oscar_season ="yes",
                        summer_season = "no",
                        imdb_rating = 8.4,
                        imdb_num_votes= 734914,
                        critics_score= 94,
                        best_pic_nom ="no",
                        best_pic_win ="no",
                        best_actor_win ="no",
                        best_actress_win ="no",
                        best_dir_win ="no",
                        top200_box ="yes")

bma_predictor =  predict(newdata = testRun1, fit1, estimator = "BMA", se.fit = TRUE)
ci_bma = confint(bma_predictor, estimator = "BMA")
ci_bma

#Test Run 2
grep("Venom", movieDF$title)

testRun2 <- data.frame(feature_film ="yes",
                        drama ="yes",
                        runtime = 112,
                        mpaa_rating_R = "no",
                        thtr_rel_year= 2018,
                        oscar_season ="yes",
                        summer_season = "no",
                        imdb_rating = 6.7,
                        imdb_num_votes= 335961,
                        critics_score= 30,
                        best_pic_nom ="no",
                        best_pic_win ="no",
                        best_actor_win ="no",
                        best_actress_win ="no",
                        best_dir_win ="no",
                        top200_box ="yes")

bma_predictor2 =  predict(newdata = testRun2, fit1, estimator = "BMA", se.fit = TRUE)
ci_bma2 = confint(bma_predictor2, estimator = "BMA")
ci_bma2

#Test Run 3
grep("The Last Airbender", movieDF$title)

testRun3 <- data.frame(feature_film ="yes",
                        drama ="no",
                        runtime = 103,
                        mpaa_rating_R = "no",
                        thtr_rel_year= 2010,
                        oscar_season ="no",
                        summer_season = "yes",
                        imdb_rating = 4.1,
                        imdb_num_votes= 147385,
                        critics_score= 5,
                        best_pic_nom ="no",
                        best_pic_win ="no",
                        best_actor_win ="no",
                        best_actress_win ="no",
                        best_dir_win ="no",
                        top200_box ="no")

bma_predictor3 =  predict(newdata = testRun3, fit1, estimator = "BMA", se.fit = TRUE)
ci_bma3 = confint(bma_predictor3, estimator = "BMA")
ci_bma3


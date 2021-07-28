

# ###```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE , warning = FALSE, message = FALSE,
#                       fig.align="center", out.width="60%")
################## Install Basic Package required -- 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

################## Install Additional Package  used in code
#### Used formattable and kableExtra Package to formate Table
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")

# import libraries
library(tidyverse)   # general
library(caret)       # model
library(data.table)
library(kableExtra)
library(formattable)
library(DescTools)   # descriptive statistics
library(ggcorrplot)  # correlation matrix plot
library(ggthemes)
library(lubridate) ## used to deal with timestamp
library(knitr)
library(rmarkdown)
library(dplyr)
set_theme <- theme(text = element_text(size=16), panel.border = element_rect(colour="black", linetype = "solid", fill=NA), plot.title = element_text(hjust = 0.5, size = 18), plot.caption = element_text(hjust = 0.5))

# import data set
data <- read.csv("2021.csv")

#```

#```{r glimpse_data, echo=FALSE}
glimpse(data)
#```

#```{r Figure1 , echo=FALSE, fig.cap="Histogram of Happiness Scores for year2021"}
hist(data$Score, freq=TRUE, col="#41729F", border="white", 
     xlab="Score", ylab="Count")
#```

#```{r summary_score, echo=FALSE}
ss<-summary(data$Score)
#```
# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
# -----------|-----------|------------|---------|-----------|---------
#   `r ss[1]` | `r ss[2]` |`r ss[3]` |`r ss[4]`| `r ss[5]` | `r ss[6]`



#```{r low_high_data, echo=FALSE}
top<-data %>% filter(Overall.rank <=3) %>% select(Overall.rank, Country.or.region, Score)
top_table<-data.frame("Overall rank"=top[1],Country.or.region=top[2],Score=top[3])
top_table %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#41729F", bold = T) %>%
  column_spec(2, color =  "#41729F", bold = T) %>%
  column_spec(3, color =  "#41729F", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))


low<-data %>% filter(Overall.rank >= 147) %>% select(Overall.rank, Country.or.region, Score)
low_table<-data.frame("Overall rank"=low[1],Country.or.region=low[2],Score=low[3])
low_table %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#41729F", bold = T) %>%
  column_spec(2, color =  "#41729F", bold = T) %>%
  column_spec(3, color =  "#41729F", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))
#```


#```{r Figure2 , echo=FALSE, fig.cap=" Correlation among the six parameters"}
temp <- data[, c(3,4,5,6,7,8,9)]
cormat <- signif(cor(temp), 2)
ggcorrplot(cormat)
rm(temp, cormat)+set_theme
#```


#```{r Figure3 , echo=FALSE, fig.cap="Score vs. Generosity"}
ggplot(data = data, aes(x = Score, Generosity)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)+set_theme

#```


#```{r Figure4 , echo=FALSE, fig.cap="Score vs. GDP per capita"}
ggplot(data = data, aes(x = Score, GDP.per.capita)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)+set_theme
#```

#```{r sum_factors_model, echo=FALSE, warning=FALSE}
# find  predicted score by sum method and calculate the corresponding RMSE
sum_model <- data %>% mutate(pred_score = GDP.per.capita +
                               Social.support +
                               Healthy.life.expectancy +
                               Freedom.to.make.life.choices + 
                               Generosity + 
                               Perceptions.of.corruption + 
                               1.972, 
                             RMSE = RMSE(Score, pred_score))

# show top results of the summation model
sum_of_factor<-sum_model %>%
  filter(Overall.rank <= 5) %>%
  select(Overall.rank, Country.or.region, Score, pred_score, RMSE)

sum_of_factor_table<-data.frame("Overall rank"=sum_of_factor[1],Country.or.region=sum_of_factor[2],Score=sum_of_factor[3],Pred_score=sum_of_factor[4], RMSE=sum_of_factor[5])
sum_of_factor_table %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#41729F", bold = T) %>%
  column_spec(2, color =  "#41729F", bold = T) %>%
  column_spec(3, color =  "#41729F", bold = T) %>%
  column_spec(4, color =  "#41729F", bold = T) %>%
  column_spec(5, color =  "#41729F", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))

#```

#```{r mod1_rmse_save, include=FALSE}
# save RMSE for the first model
mod1_rmse <- RMSE(sum_model$Score, sum_model$pred_score)
#```


#```{r echo=FALSE}
# calculate the missing dystopian residuals
sum_model <- sum_model %>% mutate(residual = Score - pred_score)

# show top results of the summation model
residual<-sum_model %>%
  filter(Overall.rank <= 5) %>%
  select(Overall.rank, Country.or.region, Score, pred_score, RMSE, residual)

residual_table<-data.frame("Overall rank"=residual[1],Country.or.region=residual[2],Score=residual[3],Pred_score=residual[4], RMSE=residual[5], Residual=residual[6])

residual_table %>% 
  kable(align = 'c', booktabs = T,
        format = "latex", linesep = "") %>%
  column_spec(1, color =  "#41729F", bold = T) %>%
  column_spec(2, color =  "#41729F", bold = T) %>%
  column_spec(3, color =  "#41729F", bold = T) %>%
  column_spec(4, color =  "#41729F", bold = T) %>%
  column_spec(5, color =  "#41729F", bold = T) %>%
  column_spec(6, color =  "#41729F", bold = T) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))
#```



#```{r best_p_data, echo=FALSE, warning=FALSE}
# --- test for an appropriate ratio in data partitioning
# a sequence of p's we want to test
ps <- seq(from=.30, to=.90, by=.01)

# calculate RMSEs for each p value
rmses <- sapply(ps, function(p){
  train_index <- createDataPartition(data$Score, times=1, p=p, list=FALSE)
  train <- data[train_index,]
  test <- data[-train_index,]
  fit <- glm(Score ~ GDP.per.capita +
               Social.support +
               Healthy.life.expectancy + 
               Freedom.to.make.life.choices + 
               Generosity + 
               Perceptions.of.corruption, 
             data = train)
  test <- test %>% mutate(pred_score = predict.glm(fit, newdata=test))
  RMSE(test$Score, test$pred_score)
})
#```



#```{r Figure5 , echo=FALSE, fig.cap="RMSE vs. Ps."}
# no real clear winner in terms of best accuracy in probabilities
  plot(ps, rmses, xlab='Ps', ylab='RMSE')
  
  
#```


#```{r data_partitioning, echo=FALSE, warning=FALSE}
# set seed to keep partitioning consistent
set.seed(1, sample.kind = "Rounding")

# ----- Data partitioning -----
train_index <- createDataPartition(data$Score, times=1, p=0.70, list=FALSE)
train <- data[train_index,]
test <- data[-train_index,]
#```

#```{r glm_model, echo=FALSE}
# --- fit our glm model, caret::glm
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Generosity + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to a 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))
#```

#```{r glm_results_5_top,  echo=FALSE}
# show top five observations
results %>%
  select(Overall.rank, Country.or.region, Score, pred_score) %>%
  head(results,n=5L)
#```
The lowest five observations, as well as the actual score, are displayed below.
#```{r glm_results_5_low,  echo=FALSE}
# show bottom five observations
results %>%
  select(Overall.rank, Country.or.region, Score, pred_score) %>%
  tail(5)
#```

#```{r mod2_coeff, echo=FALSE}
# save model 2 RMSE
mod2_rmse_gen <- RMSE(results$Score, results$pred_score)

# save coefficients to use in equation
c <- coefficients(fit)
c[] <- lapply(c, round, 3)

# print coefficients of fitted model
fit$coefficients
#```


#```{r Figure6 , echo=FALSE, fig.cap="Actual Scores vs. Predicted Scores for the GLM Model (2021)"}
# plot predicted scores vs actual scores
# also plot y = x line
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')+
  labs(x="Score", y="Pred. Score")+ set_theme
#```


`r RMSE(results$Score, results$pred_score)` is the RMSE for this model. The model coefficients are presented below, along with the model equation, in the same manner as previously.

#```{r mod2_run_nogen, echo=FALSE}
# fit model without generosity
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to a 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

#```


#```{r mod2_save_nogen, echo=FALSE}
# save rmse and coefficients
mod2_rmse_nogen <- RMSE(results$Score, results$pred_score)
c <- coefficients(fit)
c[] <- lapply(c, round, 3)

# print coefficients of fitted model
fit$coefficients
#```



#```{r Figure7 , echo=FALSE, fig.cap="Actual Scores vs. Predicted Scores for the GLM Model without the Generosity parameter for year 2021"}
# fit model without generosity
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to a 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

# plot predicted scores vs actual scores
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')+
  labs(x="Score", y="Pred. Score")+ set_theme 
#```

#```{r mod3_run1_gen, echo=FALSE, warning=FALSE}
# add 2020 data set and keep only used columns
data20<- read.csv("2020.csv")
data20$Overall.rank <- NULL
data20$Country.or.region <- NULL

# remove unused columns in 2021 data frame for merging
data$Overall.rank <- NULL
data$Country.or.region <- NULL

# turn corruption column from factor to numeric, turn NAs to 0
data20$Perceptions.of.corruption <- as.numeric(as.character(data20$Perceptions.of.corruption))
data20[is.na(data20)] <- 0

# build full data set of both 2021, 2020 data
full_data <- rbind(data, data20)

# show full data set
glimpse(full_data)
#```

#```{r mod3_run2_gen, echo=FALSE, warning=FALSE}
# set seed to keep partitioning consistent
set.seed(1, sample.kind = "Rounding")

# partition data
train_index <- createDataPartition(full_data$Score, times=1, p=0.70, list=FALSE)
train <- full_data[train_index,]
test <- full_data[-train_index,]

# fit a model (including generosity)
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Generosity +
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to our 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))
#```

#```{r mod3_save_gen, include=FALSE}
# save rmse and coefficients
mod3_rmse_gen <- RMSE(results$Score, results$pred_score)
c <- coefficients(fit)
c[] <- lapply(c, round, 3)


# print coefficients of fitted model
fit$coefficients
#```


#```{r Figure8 , echo=FALSE, fig.cap="Predicted Scores vs. Actual Scores for GLM Model (2020/2021)"}
# plot predicted scores vs actual scores
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')+
  labs(x="Score", y="Pred. Score")+ set_theme
#```

#```{r mod3_run_nogen, echo=FALSE}
# fit a model (not including generosity)
fit <- glm(Score ~ GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Perceptions.of.corruption, 
           data = train)

# add predicted scores to our 'results' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))
#```

#```{r mod3_save_nogen, echo=FALSE}
# save rmse and coefficients
mod3_rmse_nogen <- RMSE(results$Score, results$pred_score)
c <- coefficients(fit)
c[] <- lapply(c, round, 3)

# print coefficients of fitted model
fit$coefficients
#```


#```{r Figure9 , echo=FALSE, fig.cap="Predicted Scores vs. Actual Scores for GLM Model without the Generosity parameter for years 2020/2021"}
# plot predicted scores vs actual scores
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')+
  labs(x="Score", y="Pred. Score")+ set_theme
#```



#```{r compile_rmses, include=FALSE}
# compiles the list of RMSEs
rmse_list <- list(mod1_rmse,
                  mod2_rmse_gen,
                  mod2_rmse_nogen,
                  mod3_rmse_gen,
                  mod3_rmse_nogen)
rmse_list[] <- lapply(rmse_list, round, 3)
#```

# | Method                              | RMSE              |
#   |-------------------------------------|-------------------|
#   | Sum of Factors Model (2021)         | `r rmse_list[1]`  |
#   | GLM Model (2021)                    | `r rmse_list[2]`  |
#   | GLM Model (2020/2021)               | `r rmse_list[4]`  |
#   | GLM No Generosity Model (2021)      | `r rmse_list[3]`  |
#   | GLM No Generosity Model (2020/2021) | `r rmse_list[5]`  |
  
  
#```{r sum_factors_model_full, echo=FALSE, warning=FALSE}
# find our predicted score and calculate the corresponding RMSE
sum_model <- full_data %>% mutate(pred_score = GDP.per.capita +
                                    Social.support +
                                    Healthy.life.expectancy +
                                    Freedom.to.make.life.choices + 
                                    Generosity + 
                                    Perceptions.of.corruption + 
                                    1.85, 
                                  RMSE = RMSE(Score, pred_score))

# save RMSE for the first model
mod1_rmse_full <- RMSE(sum_model$Score, sum_model$pred_score)
#```

#```{r mod1_full_rmse_save, include=FALSE}
# recompiles the list of RMSEs
rmse_list <- list(mod1_rmse,
                  mod2_rmse_gen,
                  mod2_rmse_nogen,
                  mod3_rmse_gen,
                  mod3_rmse_nogen,
                  mod1_rmse_full)
rmse_list[] <- lapply(rmse_list, round, 3)
#```

# | Method                              | RMSE              |
#   |-------------------------------------|-------------------|
#   | Sum of Factors Model (2021)         | `r rmse_list[1]`  |
#   | Sum of Factors Model (2020/2021)    | `r rmse_list[6]`  |
#   | GLM Model (2021)                    | `r rmse_list[2]`  |
#   | GLM Model (2020/2021)               | `r rmse_list[4]`  |
#   | GLM No Generosity Model (2021)      | `r rmse_list[3]`  |
#   | GLM No Generosity Model (2020/2021) | `r rmse_list[5]`  |
  
  
#### Styling ####
## Figure Styling
fig.width = 4.5, fig.height= 4.5, out.width= "60%", fig.align= 'center', fig.cap="Title", fig.pos="H"
## Table Styling
kable(format = "latex", row.names = NA,
      booktabs = TRUE, digits = 2,
      caption = "Metrics for the Final Fit on Test Data") %>%
kable_styling(position = "center", latex_options = "HOLD_position")
## Boxplot styling
theme_bw() + # Formatting Changes
  scale_x_discrete(labels = c("Female", "Male")) +
  labs(y= "Age", x = "Gender")
## Referencing Stuff
\@ref(fig:cv-ridge)
\@ref(tab:tree-highest-abs-bias)
#### Splines ####
## LM
spline_fit = lm(wage ~ splines::ns(age, df = 5), data = Wage_2007)
## Geom
geom_smooth(method = "lm",
            formula = "y ~ splines::ns(x, df = 5)", # note use y and x instead of wage and age
            se = FALSE)
## CV
# Have to source this from the prof
male_fit <- cross_validate_spline(bmd_train_male$age,
                                  bmd_train_male$spnbmd, 10, 1:15)

#### Log Regressions #####
## Logistic Regression
glm_fit = glm(default ~ student + balance + income,
              family = "binomial",
              data = default_train)
## Prediction
fitted_probabilities = predict(glm_fit,
                               newdata = default_test,
                               type = "response") 
#### Penalty Regressions ####
## Ridge Regression
ridge_fit = cv.glmnet(violentcrimes.perpop ~ ., # formula notation, as usual
                      alpha = 0, # alpha = 0 for ridge
                      nfolds = 10, # number of folds
                      data = crime_data_train) # data to run ridge on
## Lasso Regression
lasso_fit = cv.glmnet(violentcrimes.perpop ~ ., # formula notation, as usual
                      alpha = 1, # alpha = 1 for lasso
                      nfolds = 10, # number of folds
                      data = crime_data_train) # data to run lasso on
## Logistic Ridge
ridge_fit = cv.glmnet(default ~ ., # formula notation, as usual
                      alpha = 0, # alpha = 0 means ridge
                      nfolds = 10, # number of CV folds
                      family = "binomial", # to specify logistic regression
                      type.measure = "class", # use misclassification error in CV
                      data = default_train) # train on default_train data
## Plotting 
# CV Plot
plot(ridge_fit)
# Feature Plotting
plot_glmnet(lasso_fit, crime_data_train) #, features_to_plot = 7
## Prediction
probabilities = predict(ridge_fit, # fit object
                        newdata = default_test, # new data to test on
                        s = "lambda.1se", # which value of lambda to use
                        type = "response")
#### Elastic Regressions ####
## Cross Validation
elnet_fit = cva.glmnet(violentcrimes.perpop ~ ., # formula notation, as usual
                       nfolds = 10, # number of folds
                       data = crime_data_train) # data to run on
## Plotting
plot_cva_glmnet(elnet_fit)
## Getting Best Fit
elnet_fit_best = extract_best_elnet(elnet_fit)

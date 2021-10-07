# install.packages("scales")
library(scales)

# Description: create a trace plot of lasso or ridge regression, highlighting 
#               certain features
# 
# Arguments:
# glmnet_fit:       fit object returned either by glmnet or cv.glmnet
# lambda:           a value of the penalty parameter; if null and cv.glmnet 
#                    was called, then automatically set to the value chosen by CV
# features_to_plot: either number or names of features to highlight in the plot;
#                    if lambda defined, then for lasso it defaults to plotting 
#                    variables with nonzero coefficients
plot_glmnet = function(glmnet_fit, lambda = NULL, features_to_plot = NULL){
  
  # extract coefficients
  if(!is.null(glmnet_fit$beta)){
    beta_hat = glmnet_fit$beta
  } else{
    beta_hat = glmnet_fit$glmnet.fit$beta
  }
  
  # extract feature names
  features = rownames(beta_hat)
  
  # extract alpha
  alpha = glmnet_fit$call$alpha
  
  # set lambda using one-standard-error-rule if cv.glmnet() was called
  if(is.null(lambda) & !is.null(glmnet_fit$lambda.1se)){
    lambda = glmnet_fit$lambda.1se
  }
  
  if(!is.null(lambda)){
    lambda = glmnet_fit$lambda[which.min(abs(glmnet_fit$lambda-lambda))]
  }
  
  # set features_to_plot
  if(is.null(features_to_plot)){
    # if not specified but lambda is specified, plot coefficients with nonzero
    # coefficients at lambda = lambda
    if(!is.null(lambda)){
      coefs = beta_hat[,glmnet_fit$lambda == lambda]
      if(alpha > 0){
        features_to_plot = features[coefs != 0]
      }
    } 
  } else if(is.numeric(features_to_plot)){
    # if given as integer m
    if(length(features_to_plot) > 1){
      print("Error: if numeric, features_to_plot should have length 1")
      return(NULL)
    } else{
      num_features_to_plot = features_to_plot
      if(alpha > 0){
        # lasso and elastic net:
        #  first num_features_to_plot variables to enter lasso path
        features_to_plot = apply(beta_hat, 
                                 1, 
                                 function(row)(min(c(Inf,which(row != 0))))) %>% 
          sort() %>% 
          head(num_features_to_plot) %>% 
          names()
      } else{
        # ridge:
        #  largest num_features_to_plot variables at lambda = lambda
        if(!is.null(lambda)){
          coefs = beta_hat[,glmnet_fit$lambda == lambda]
        } else{
          coefs = beta_hat[,ncol(beta_hat)]
        }
        features_to_plot = abs(coefs) %>% sort(decreasing = TRUE) %>% 
          head(num_features_to_plot) %>% names()
      }
    }
  } else if(is.character(features_to_plot)){
    # features to plot specified explicitly
  } else{
    print("Error: features_to_plot should either be null, numeric, or a character")
    return(NULL)
  }
  
  # data frame to plot
  df_to_plot = t(beta_hat) %>% 
    as.matrix() %>% 
    as_tibble() %>% 
    bind_cols(lambda = glmnet_fit$lambda) %>%
    pivot_longer(-lambda, names_to = "feature", values_to = "beta_hat") %>%
    mutate(feature_label = ifelse(feature %in% features_to_plot, feature, ""))
  
  # split features into those that will be highlighted and those that won't
  df_to_plot_highlight = df_to_plot %>% filter(feature %in% features_to_plot)
  df_to_plot_other = df_to_plot %>% filter(!(feature %in% features_to_plot))
  
  # define reverse-log transformation
  reverselog_trans <- function(base = 10){
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
  }
  
  # produce final plot
  df_to_plot_highlight %>% 
    ggplot(aes(x = lambda, y = beta_hat, group = feature)) + 
    geom_line(data = df_to_plot_other, color = "darkgray") +
    geom_line(aes(color = feature)) + 
    geom_vline(xintercept = lambda, linetype = "dashed") +
    scale_x_continuous(trans = reverselog_trans()) +  
    ylab("Coefficient") + 
    theme_bw()
}

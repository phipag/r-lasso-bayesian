library(glmnet)
library(ggplot2)
theme_set(theme_light() + theme(legend.position = "top")) # ggplot2 theme
set.seed(1337)

y <- mtcars$hp
x <- data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])

fit_plot <-
  ggplot(mtcars, aes(x = seq(hp))) +
  geom_point(aes(y = hp)) +
  xlab("Car") +
  ylab("Car horsepower [HP]")

df_model_coeffs <- data.frame(name = c(),
                              coefficient = c(),
                              lambda = c())

for (lambda in c(1, 30, 50)) {
  model <- glmnet(x, y, alpha = 1, lambda = lambda)
  y_predicted <- predict(model, s = lambda, newx = x)
  
  fit_plot <-
    fit_plot + geom_line(aes_string(y = y_predicted, color = factor(lambda)))
  
  model_coeffs <- coef(model)
  print(model_coeffs)
  
  df_model_coeffs <-
    rbind(
      df_model_coeffs,
      data.frame(
        name = model_coeffs@Dimnames[[1]][2:length(model_coeffs)],
        coefficient = model_coeffs@x[2:length(model_coeffs)],
        lambda = lambda
      )
    )
}

fit_plot <- fit_plot + scale_color_discrete(name = "L1 penalty")
print(fit_plot)

df_model_coeffs[is.na(df_model_coeffs)] <- 0
ggplot(df_model_coeffs,
       aes(
         x = name,
         y = coefficient,
         fill = as.character(lambda)
       )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Coefficient") +
  ylab("Value") +
  scale_fill_discrete(name = "L1 penalty")

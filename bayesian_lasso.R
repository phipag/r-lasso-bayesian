library(rstanarm)
library(bayesplot)
library(ggplot2)
theme_set(theme_light() + theme(legend.position = "top")) # ggplot2 theme
bayesplot_theme_set(theme_get()) # Use ggplot2 current theme for bayesplot as well
set.seed(1337)


fit_plot <-
  ggplot(mtcars, aes(x = seq(hp))) +
  geom_point(aes(y = hp)) +
  xlab("Car") +
  ylab("Car horsepower [HP]")

for (scale in c(0.25, 0.5, 1)) {
  model_bayes <-
    stan_glm(hp ~ mpg + wt + drat + qsec,
             data = mtcars,
             prior = laplace(location = 0, scale = scale))
  y_pred <-
    predict(model_bayes) # Point-estimates (Posterior median)
  fit_plot <-
    fit_plot + geom_line(aes_string(y = y_pred, color = factor(scale)))
  print(
    mcmc_areas(model_bayes, pars = c("mpg", "wt", "drat", "qsec")) +
      labs(
        title = "Posterior distributions",
        subtitle = paste("Laplacian scale = ", scale)
      )
  )
}

fit_plot <-
  fit_plot + scale_color_discrete(name = "Laplacian scale", direction = -1)
print(fit_plot)

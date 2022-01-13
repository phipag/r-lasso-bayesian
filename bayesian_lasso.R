library(rstanarm)
library(bayesplot)
library(ggplot2)
library(reshape2)
theme_set(theme_light() + theme(legend.position = "top")) # ggplot2 theme
bayesplot_theme_set(theme_get()) # Use ggplot2 current theme for bayesplot as well
set.seed(1337)


fit_plot <-
  ggplot(mtcars, aes(x = seq(hp))) +
  geom_point(aes(y = hp)) +
  xlab("Car") +
  ylab("Car horsepower [HP]")

df_posterior_draws = data.frame()

for (scale in c(0.25, 0.5, 1)) {
  model_bayes <-
    stan_glm(hp ~ mpg + wt + drat + qsec,
             data = mtcars,
             prior = laplace(location = 0, scale = scale))
  y_pred <-
    predict(model_bayes) # Point-estimates (Posterior median)
  fit_plot <-
    fit_plot + geom_line(aes_string(y = y_pred, color = factor(scale)))

  df_posterior_draws_new = as.data.frame(model_bayes, pars = c("mpg", "wt", "drat", "qsec"))
  df_posterior_draws_new["scale"] = as.character(scale)
  df_posterior_draws <- rbind(df_posterior_draws, df_posterior_draws_new)
}

fit_plot <-
  fit_plot + scale_color_discrete(name = "Laplacian scale", direction = -1)
print(fit_plot)

ggplot(melt(df_posterior_draws, "scale"), aes(x=variable, y=value, fill=scale)) + 
  geom_violin() +
  xlab("Coefficient") +
  ylab("Marginal posterior density") +
  scale_fill_discrete(name = "Laplacian scale", direction = -1)



rich_class <- cut(data_r$NUMSPECIES, breaks = c(1, 5, 30, 65, 100, 200, Inf),
    labels = c("1-5", "6-30", "31-65", "66-100", "101-200", ">200"),
    include.lowest = TRUE)

summary(data_r)


# QUANTILE REGRESSION
# https://www.statology.org/quantile-regression-in-r/
library(quantreg)

# model <- rq(y ~ x, data = dataset, tau = 0.5)

#fit model
model_09 <- rq(NUMSPECIES ~ veg_area_ha, data = grid_df, tau = 0.9)

model_08 <- rq(NUMSPECIES ~ veg_area_ha, data = grid_df, tau = 0.8)

model_05 <- rq(NUMSPECIES ~ veg_area_ha, data = grid_df, tau = 0.5)

model_005 <- rq(NUMSPECIES ~ veg_area_ha, data = grid_df, tau = 0.05)

#view summary of model
summary(model_09)
summary(model_08)
summary(model_05)
summary(model_005)

library(ggplot2)

#create scatterplot with quantile regression line
ggplot(grid_df, aes(veg_area_ha, NUMSPECIES)) +
  geom_point() + 
  geom_abline(intercept = coef(model_08)[1], slope = coef(model_08)[2])

#create scatterplot with quantile regression line and simple linear regression line
ggplot(grid_df, aes(veg_area_ha, NUMSPECIES)) +
  geom_point() + 
  geom_abline(intercept=coef(model_09)[1], slope=coef(model_09)[2]) +
  geom_abline(intercept=coef(model_08)[1], slope=coef(model_08)[2]) +
  geom_smooth(method="lm", se=F)

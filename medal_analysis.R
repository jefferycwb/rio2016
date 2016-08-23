load("data/medals.Rdata")

library(ggplot2)
library(ggthemes)
library(plotly)
library(DT)

# 01 - create vars --------------------------------------------------------

medals$gdp_per_capita <- round(medals$gdp / medals$pop, 1)
medals$equivalent_medals <- 4 * medals$gold + 2 *medals$silver + 1 * medals$bronze
medals$log_gdp <- round(log(medals$gdp), 2)
medals$log_pop <- round(log(medals$pop), 2)


# 02 - explore ------------------------------------------------------------

summary(medals)

p <- ggplot(medals, aes(x = equivalent_medals)) +
      geom_histogram(bins = max(medals$equivalent_medals)) +
      theme_hc()
ggplotly(p)

p <- ggplot(medals, aes(x = gdp, y = equivalent_medals, size = pop, label = country)) +
      geom_point() +
      theme_hc()
ggplotly(p)

p <- ggplot(medals, aes(x = log_gdp, y = equivalent_medals, size = pop, label = country)) +
      geom_point() +
      theme_hc()
ggplotly(p)

p <- ggplot(medals, aes(x = gdp_per_capita, y = equivalent_medals, size = pop, label = country)) +
      geom_point() +
      theme_hc()
ggplotly(p)


# 03 - fit ----------------------------------------------------------------

form <- equivalent_medals ~ gdp + gdp_per_capita
glm <- glm(form, family = "poisson", data = medals)
medals$expected_medals <- round(predict.glm(glm, medals, type = "response"))
medals$performance <- round(medals$equivalent_medals / medals$expected_medals, 2)


# 04 - visualize ----------------------------------------------------------

p <- ggplot(medals, aes(x = log_gdp, y = equivalent_medals, size = pop, label = country, color = performance)) +
      geom_point() +
      theme_hc() +
      scale_colour_gradient(low = "red", high = "green")
ggplotly(p)

p <- ggplot(medals, aes(x = gdp_per_capita, y = equivalent_medals, size = pop, label = country, color = performance)) +
      geom_point() +
      scale_colour_gradient(low = "red", high = "green") +
      theme_hc()
ggplotly(p)

datatable(medals)
# Global Variables
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# Loading the functions
if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("gridExtra")) install.packages("gridExtra"); library(gridExtra)

# Getting the VIX
db_Volatility <- tq_get("^VIX",
                        from = Sys.Date() - lubridate::years(2),
                        to   = Sys.Date(),
                        get  = "stock.prices",
                        complete_cases = TRUE) %>%
  dplyr::select(date, adjusted) %>%
  left_join(tq_get("SPY",
                   from = Sys.Date() - lubridate::years(2),
                   to   = Sys.Date(),
                   get  = "stock.prices",
                   complete_cases = TRUE) %>%
              dplyr::select(date, adjusted) %>%
              dplyr::mutate(Ret      = TTR::ROC(adjusted, n = 1, type = "discrete"),
                            Real_Vol = roll::roll_sd(Ret, width = 22)*100*sqrt(252)) %>%
              dplyr::select(date, Real_Vol), by = "date") %>%
  na.omit() %>%
  dplyr::mutate(Spread = adjusted - Real_Vol) %>%
  purrr::set_names(c("Date", "Implied Volatility", "Realized Volatility", "Spread"))

p_volatilites <- db_Volatility %>%
  dplyr::select(Date, `Implied Volatility`, `Realized Volatility`) %>%
  pivot_longer(names_to = "Type", values_to = "Volatility", -Date) %>%
  ggplot(aes(x = Date, y = Volatility/100, colour = Type)) +
  geom_line(size = 0.7) +
  theme_bw() +
  labs(title    = "VIX vs Realized SP500 short-term Volatility",
       subtitle = "Volatility behavior of the SP500",
       caption  = "",
       x        = "",
       y        = "") +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")

p_spread <- db_Volatility %>%
  dplyr::select(Date, Spread) %>%
  dplyr::mutate(Mean_Vol = roll::roll_mean(Spread, width = 50)) %>%
  na.omit() %>%
  purrr::set_names(c("Date", "Spread", "Mean")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = Spread/100), size = 0.7) +
  geom_line(aes(x = Date, y = Mean/100), size = 1.1, colour = "red") +
  theme_bw() +
  labs(title    = "Volatility Spread",
       subtitle = "How expensive/cheap is the current volatility?",
       caption  = "",
       x        = "",
       y        = "") +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.title = element_blank())

p_vix <- db_Volatility %>%
  dplyr::select(Date, `Implied Volatility`) %>%
  purrr::set_names(c("Date", "IV")) %>%
  bind_rows(data.frame(Date = Sys.Date(),
                       IV   = getQuote("^VIX") %>% dplyr::select(Last) %>% pull(1))) %>%
  ggplot(aes(x = Date, y = IV/100)) +
  geom_line(size = 0.7) +
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  theme_bw() +
  labs(title    = "CBOE Volatility Index",
       subtitle = "Short-term trend of the Implied Volatility",
       caption  = "Data Source: Yahoo Finance.",
       x        = "",
       y        = "") +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "red", size = 1.2)

grid.arrange(p_volatilites, arrangeGrob(p_spread, p_vix, ncol=2), nrow = 2)

ggsave("VIX_Analysis.png", plot = grid.arrange(p_volatilites, arrangeGrob(p_spread, p_vix, ncol=2), nrow = 2), path = "Plots/", width = 10, height = 7, units = "in") 


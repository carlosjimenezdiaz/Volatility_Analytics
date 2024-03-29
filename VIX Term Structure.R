# Global Variables
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# Getting the functions
source(file = "00_scripts/Libraries.R")

# Deleting all the files from Plots - GEX
do.call(file.remove, list(list.files("Plots", full.names = TRUE)))
do.call(file.remove, list(list.files("cboe_file/From Scraping//", full.names = TRUE)))

# Loading the functions
libraries()

# Starting the Timer
tictoc::tic()

# Local Variables
Ticker_CBOE      <- c("_SPX", "_XSP", "SPY") # The same names you used in the CBOE website
Ticker_YAHOO     <- c("^SPX", "^XSP", "SPY") # The same names you used in Yahoo
Price_Ratio      <- c(1, 10, 10)
Multiplier       <- c(100, 100, 100)
Option_Type      <- c("European", "European", "American")
Chart_label      <- "SPX, XSP and SPY"
Reference_Ticker <- "_SPX"

# Local Dataframes
db_Option_Chain_Combined <- NULL
db_prices_from_CBOE      <- NULL

# Getting the Risk Free Rate (Using the 10 Year Gov Bong Yield as a proxy)
risk_free_rate <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE) %>%
  tail(n = 1) %>%
  as.data.frame() %>%
  pull(1)

# Defining the dataframe with tickers and tickers information
db_Tickers <- data.frame(Ticker_CBOE    = Ticker_CBOE,
                         Ticker_YAHOO   = Ticker_YAHOO,
                         Multiplier     = Multiplier, 
                         Option_Type    = Option_Type,
                         risk_free_rate = risk_free_rate/100, 
                         Price_Ratio    = Price_Ratio) %>%   # By how much you need to multiply the price to put it at the same level as the Reference Ticker
  dplyr::mutate(div_yield = getQuote(Ticker_YAHOO, what = yahooQF("Dividend Yield"))[2] %>% pull(1)) %>% # Dividend Yield of the underlying
  replace(is.na(.), 0)

# Function to download CBOE data
for(i in 1:nrow(db_Tickers)){ # i <- 1
  
  # Sleep for X sec (for multiple symbols to not get block be the CBOE website)
  Sys.sleep(runif(1, min = 5, max = 10) %>% round(digits = 0))
  
  tryCatch(
    expr = {
      # Printing status
      print(str_glue("Getting info from {db_Tickers$Ticker_CBOE[i]}"))
      
      # Downloading data from CBOE website
      str_glue("https://cdn.cboe.com/api/global/delayed_quotes/options/{db_Tickers$Ticker_CBOE[i]}.json") %>%
        read_json(simplifyVector = TRUE) %>%
        pluck("data", "options") %>%
        as.data.frame() %>%  
        dplyr::mutate(Ticker_CBOE = db_Tickers$Ticker_CBOE[i]) %>%
        left_join(db_Tickers, by = "Ticker_CBOE") %>%
        dplyr::mutate(Spot_Price   = getQuote(db_Tickers$Ticker_YAHOO[i])$Last,
                      expiry       = str_sub(option, -15, -10) %>% as.Date(format = "%y%m%d"),
                      opt_type     = str_sub(option, -9, -9),
                      strike       = paste0(str_sub(str_sub(option, -8, -1), -8, -4),".",str_sub(str_sub(option, -8, -1), -3, -1)) %>% as.numeric(),
                      days2Exp     = as.Date(expiry) - Sys.Date(),
                      Mid_price    = round((bid + ask)/2, 2),
                      Time_Process = Sys.time()) %>%
        saveRDS(str_glue("cboe_file/From Scraping/CBOE_Data_{db_Tickers$Ticker_CBOE[i]}.rds"))

    },
    error = function(e){ 
      print(str_glue("ERROR Getting info from {db_Tickers$Ticker_CBOE[i]}"))
    },
    warning = function(w){ 
      print(str_glue("ERROR Getting info from {db_Tickers$Ticker_CBOE[i]}"))
    }
  )
}

# Generating the Option Chain
acum_GEX <- NULL
for(CBOE_File in list.files("cboe_file/From Scraping/")){ # CBOE_File <- "CBOE_Data__SPX.rds"

  # Reading the data from the CBOE File (General Information)
  CBOE_General_Info <- readRDS(str_glue("cboe_file/From Scraping/{CBOE_File}"))

  # Saving the info of the spot price from the CBOE File
  db_prices_from_CBOE <- db_prices_from_CBOE %>%
    rbind(data.frame(Reference_Price = CBOE_General_Info$Spot_Price[1],
                     Ticker          = CBOE_General_Info$Ticker_CBOE[1]))
  
  # Extracting data from the Calls and calculating GEX
    # GEX - Calls
    db_Option_Chain_Calls <- CBOE_General_Info %>%
      dplyr::filter(opt_type == "C") %>%
      dplyr::select(expiry, iv, open_interest, strike, Spot_Price, days2Exp, Time_Process, div_yield, risk_free_rate, Multiplier, Price_Ratio) %>%
      dplyr::mutate(type      = "Calls",
                    days2Exp  = days2Exp %>% as.numeric(),
                    days2Exp  = case_when(days2Exp == 0 ~ 1/365, TRUE ~ days2Exp/365),
                    Scenario1 = case_when(lubridate::week(expiry)  == lubridate::week(Time_Process) ~ "This Week",
                                          TRUE ~ "Not This Week"),
                    Scenario2 = case_when(lubridate::month(expiry)  == lubridate::month(Time_Process) ~ "This Month",
                                          TRUE ~ "Not This Month"),
                    Scenario3 = "All Dates")
    
    # GEX Calculations
    GEX_Calls <- greeks(bscall(s  = db_Option_Chain_Calls$Spot_Price,
                               k  = db_Option_Chain_Calls$strike,
                               v  = db_Option_Chain_Calls$iv,
                               r  = db_Option_Chain_Calls$risk_free_rate,
                               tt = db_Option_Chain_Calls$days2Exp,
                               d  = db_Option_Chain_Calls$div_yield), complete = TRUE) %>%
      dplyr::select(k, s, Gamma) %>%
      dplyr::mutate(Date       = db_Option_Chain_Calls$expiry,
                    OI         = db_Option_Chain_Calls$open_interest %>% as.numeric(),
                    type       = "Calls",
                    Mul        = db_Option_Chain_Calls$Multiplier,
                    PriceRatio = db_Option_Chain_Calls$Price_Ratio,
                    GEX        = +1 * Gamma * Mul * OI * s^2 * 0.01) %>%
      dplyr::select(Date, k, s, GEX, PriceRatio, type)
    
    # GEX - Puts
    db_Option_Chain_Puts <- CBOE_General_Info %>%
      dplyr::filter(opt_type == "P") %>%
      dplyr::select(expiry, iv, open_interest, strike, Spot_Price, days2Exp, Time_Process, div_yield, risk_free_rate, Multiplier, Price_Ratio) %>%
      dplyr::mutate(type      = "Puts",
                    days2Exp  = days2Exp %>% as.numeric(),
                    days2Exp  = case_when(days2Exp == 0 ~ 1/365, TRUE ~ days2Exp/365),
                    Scenario1 = case_when(lubridate::week(expiry)  == lubridate::week(Time_Process) ~ "This Week",
                                          TRUE ~ "Not This Week"),
                    Scenario2 = case_when(lubridate::month(expiry)  == lubridate::month(Time_Process) ~ "This Month",
                                          TRUE ~ "Not This Month"),
                    Scenario3 = "All Dates")
    
    # GEX Calculations
    GEX_Puts <- greeks(bsput(s  = db_Option_Chain_Puts$Spot_Price,
                             k  = db_Option_Chain_Puts$strike,
                             v  = db_Option_Chain_Puts$iv,
                             r  = db_Option_Chain_Puts$risk_free_rate,
                             tt = db_Option_Chain_Puts$days2Exp,
                             d  = db_Option_Chain_Puts$div_yield), complete = TRUE) %>%
      dplyr::select(k, s, Gamma) %>%
      dplyr::mutate(Date       = db_Option_Chain_Puts$expiry,
                    OI         = db_Option_Chain_Puts$open_interest %>% as.numeric(),
                    type       = "Puts",
                    Mul        = db_Option_Chain_Puts$Multiplier,
                    PriceRatio = db_Option_Chain_Puts$Price_Ratio,
                    GEX        = -1 * Gamma * Mul * OI * s^2 * 0.01) %>%
      dplyr::select(Date, k, s, GEX, PriceRatio, type)
  
  # Combining all the DB of Options
  db_Option_Chain_Combined <- db_Option_Chain_Combined %>%
    rbind(db_Option_Chain_Calls) %>%
    rbind(db_Option_Chain_Puts)
  
  # Combining all the GEX DB
  acum_GEX <- acum_GEX %>%
    rbind(GEX_Calls) %>%
    rbind(GEX_Puts)
}

# Selecting the Reference Price
Reference_Price <- db_prices_from_CBOE %>% 
  dplyr::filter(Ticker == Reference_Ticker) %>%
  dplyr::select(Reference_Price) %>% 
  pull(1)

# Extracting the VIX Structure from the Option Chain
MMS_First_Criteria  <- 0.95    # Moneyness criteria for the Implied Volatility Structure
MMS_Second_Criteria <- 1.05    # Moneyness criteria for the Implied Volatility Structure

ATM_Price         <- Reference_Price
MoneynessFC_Price <- ATM_Price*MMS_First_Criteria
MoneynessSC_Price <- ATM_Price*MMS_Second_Criteria

p <- db_Option_Chain_Combined %>% 
  dplyr::select(expiry, type, iv, strike, Price_Ratio) %>%
  dplyr::mutate(strike = (strike %>% as.numeric()) * Price_Ratio) %>%
  dplyr::filter(type == "Puts" & expiry >= Sys.Date()) %>%
  dplyr::mutate(final_date = expiry %>% as.Date()) %>%
  dplyr::mutate(VST = ifelse(dplyr::lead(strike, n = 1) > ATM_Price & dplyr::lag(strike, n = 1) < ATM_Price, "ATM", 
                             ifelse(dplyr::lead(strike, n = 1) > MoneynessFC_Price & dplyr::lag(strike, n = 1) < MoneynessFC_Price, str_glue("MM{MMS_First_Criteria*100}"), 
                                    ifelse(dplyr::lead(strike, n = 1) > MoneynessSC_Price & dplyr::lag(strike, n = 1) < MoneynessSC_Price, str_glue("MM{MMS_Second_Criteria*100}"), "")))) %>% 
  dplyr::filter(VST != "") %>%
  dplyr::select(final_date, iv, VST) %>%
  dplyr::group_by(final_date, VST) %>%
  dplyr::summarise(Mean_IV = mean(iv %>% as.numeric()), .groups = "drop") %>%
  dplyr::mutate(VST = case_when(VST == str_glue("MM{MMS_First_Criteria*100}") ~ str_glue("Moneyness {MMS_First_Criteria*100}%"),
                                VST == str_glue("MM{MMS_Second_Criteria*100}") ~ str_glue("Moneyness {MMS_Second_Criteria*100}%"),
                                TRUE ~ VST)) %>%
  dplyr::group_by(VST) %>%
  dplyr::mutate(Mean_IV = (lowess(final_date, Mean_IV, f = 1/2) %>% pluck(2))) %>%
  ggplot(aes(x = final_date, y = Mean_IV, colour = VST)) +
  geom_line(linewidth = 0.9) +
  labs(title    = "Implied Volatility Structure",
       subtitle = "Data extracted from the Option Chain (Combining SPX, XSP and SPY)",
       caption  = "Data Source: CBOE / Own calculations.",
       x        = "",
       y        = "Implied Volatility") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

p

ggsave("vix_structure.png", plot = p, path = "Plots/", width = 10, height = 7, units = "in") 

# Stoping the Timer
tictoc::toc()

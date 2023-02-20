rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(quantmod)
library(xts)
library(ggplot2)


##1)CARICAMENTO E MANIPOLAZIONE DATI

##GOLDENSPOON

BacktestRaw <- read_excel("Backtest.xlsx", col_types = c("date", 
                                                         "numeric"))
BacktestRaw$Date <- as.Date(BacktestRaw$Date, format = "%Y-%m-%d")

Backtest_last_obs <- BacktestRaw %>%
  group_by(Date) %>%
  slice_tail(n = 1)

Backtest_filled <- Backtest_last_obs %>%
  group_by(Date) %>%
  summarise(Balance = tail(Balance, n = 1)) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  na.locf()

Goldenspoon_portfolio <- xts(Backtest_filled$Balance, order.by = Backtest_filled$Date)
Goldenspoon_returns <- dailyReturn(Goldenspoon_portfolio, type = "arithmetic")
Goldenspoon_total_return <- (tail(BacktestRaw$Balance, n=1)-head(BacktestRaw$Balance,n=1))/head(BacktestRaw$Balance,n=1)


##GOLD
GoldRaw <- read_excel("Gold_Bullion_LBM $-t_oz_DELAY.xlsx",
                      col_types = c("date","numeric"))
GoldRaw$Date <- as.Date(GoldRaw$Date, format = "%Y-%m-%d")

GoldRaw <- GoldRaw %>%
  group_by(Date) %>%
  summarise(Valore = tail(Valore, n = 1)) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  na.locf()

Gold_ts <- xts(GoldRaw$Valore, order.by = GoldRaw$Date)
Gold_returns <- dailyReturn(Gold_ts, type = "arithmetic")
Gold_total_return <- (tail(GoldRaw$Valore, n=1)-head(GoldRaw$Valore,n=1))/head(GoldRaw$Valore,n=1)
Gold_portfolio <- cumprod(1 + Gold_returns) * 10000

##MSCI

MSCIRaw <- read_excel("MSCI_WORLD_U$_PRICE_INDEX.xlsx", col_types = c("date", 
                                                                      "numeric"))
MSCIRaw$Date <- as.Date(MSCIRaw$Date, format = "%Y-%m-%d")

MSCIRaw <- MSCIRaw %>%
  group_by(Date) %>%
  summarise(Valore = tail(Valore, n = 1)) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  na.locf()

MSCI_ts <- xts(MSCIRaw$Valore, order.by = MSCIRaw$Date)
MSCI_returns <- dailyReturn(MSCI_ts, type = "arithmetic")
MSCI_total_return <- (tail(MSCIRaw$Valore, n=1)-head(MSCIRaw$Valore,n=1))/head(MSCIRaw$Valore,n=1)
MSCI_portfolio <- cumprod(1 + MSCI_returns) * 10000

##NASDAQ
NasdaqRaw <- read_excel("NASDAQ_COMPOSITE_PRICE_INDEX.xlsx", col_types = c("date", 
                                                                           "numeric"))
NasdaqRaw$Date <- as.Date(NasdaqRaw$Date, format = "%Y-%m-%d")

NasdaqRaw <- NasdaqRaw %>%
  group_by(Date) %>%
  summarise(Valore = tail(Valore, n = 1)) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  na.locf()

Nasdaq_ts <- xts(NasdaqRaw$Valore, order.by = NasdaqRaw$Date)
Nasdaq_returns <- dailyReturn(Nasdaq_ts, type = "arithmetic")
Nasdaq_total_return <- (tail(NasdaqRaw$Valore, n=1)-head(NasdaqRaw$Valore,n=1))/head(NasdaqRaw$Valore,n=1)
Nasdaq_portfolio <- cumprod(1 + Nasdaq_returns) * 10000

##SP500
SP500Raw <- read_excel("S&P_500_COMPOSITE_PRICE_INDEX.xlsx", col_types = c("date", 
                                                                           "numeric"))
SP500Raw$Date <- as.Date(SP500Raw$Date, format = "%Y-%m-%d")

SP500Raw <- SP500Raw %>%
  group_by(Date) %>%
  summarise(Valore = tail(Valore, n = 1)) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  na.locf()

SP500_ts <- xts(SP500Raw$Valore, order.by = SP500Raw$Date)
SP500_returns <- dailyReturn(SP500_ts, type = "arithmetic")
SP500_total_return <- (tail(SP500Raw$Valore, n=1)-head(SP500Raw$Valore,n=1))/head(SP500Raw$Valore,n=1)
SP500_portfolio <- cumprod(1 + SP500_returns) * 10000

##US10YEAR
US10YearsRaw <- read_excel("US_BENCHMARK_10_YEAR_DS_GOVT_INDEX.xlsx", col_types = c("date", 
                                                                                    "numeric"))
US10YearsRaw$Date <- as.Date(US10YearsRaw$Date, format = "%Y-%m-%d")

US10YearsRaw <- US10YearsRaw %>%
  group_by(Date) %>%
  summarise(Valore = tail(Valore, n = 1)) %>%
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  na.locf()

US10Years_ts <- xts(US10YearsRaw$Valore, order.by = US10YearsRaw$Date)
US10Years_returns <- dailyReturn(US10Years_ts, type = "arithmetic")
US10Years_total_return <- (tail(US10YearsRaw$Valore, n=1)-head(US10YearsRaw$Valore,n=1))/head(US10YearsRaw$Valore,n=1)
US10Years_portfolio <- cumprod(1 + US10Years_returns) * 10000



##2)ANALISI CORRELAZIONI


Correlazione_Goldenspoon_Gold <- cor(Goldenspoon_returns,Gold_returns)
Correlazione_Goldenspoon_MSCI <- cor(Goldenspoon_returns,MSCI_returns)
Correlazione_Goldenspoon_Nasdaq <-cor(Goldenspoon_returns,Nasdaq_returns)
Correlazione_Goldenspoon_SP500 <- cor(Goldenspoon_returns,SP500_returns)
Correlazione_Goldenspoon_US10Years <- cor(Goldenspoon_returns,US10Years_returns)


##3)Grafico portafogli da 10k


Goldenspoon_portfolio_df <- data.frame(date=index(Goldenspoon_portfolio), value=coredata(Goldenspoon_portfolio))
Gold_portfolio_df <- data.frame(date=index(Gold_portfolio), value=coredata(Gold_portfolio))
MSCI_portfolio_df <- data.frame(date=index(MSCI_portfolio), value=coredata(MSCI_portfolio))
Nasdaq_portfolio_df <- data.frame(date=index(Nasdaq_portfolio), value=coredata(Nasdaq_portfolio))
SP500_portfolio_df <- data.frame(date=index(SP500_portfolio), value=coredata(SP500_portfolio))
US10Years_portfolio_df <- data.frame(date=index(US10Years_portfolio), value=coredata(US10Years_portfolio))

ggplot() +
  geom_line(data=Goldenspoon_portfolio_df, aes(x=date, y=value, color="Goldenspoon 274%"), size=1) +
  geom_line(data=Gold_portfolio_df, aes(x=date, y=daily.returns, color="Gold 41,6%"), size=1) +
  geom_line(data=MSCI_portfolio_df, aes(x=date, y=daily.returns, color="MSCI 35,7%"), size=1) +
  geom_line(data=Nasdaq_portfolio_df, aes(x=date, y=daily.returns, color="Nasdaq 70,5%"), size=1) +
  geom_line(data=SP500_portfolio_df , aes(x=date, y=daily.returns, color="S&P500 56,1%"), size=1) +
  geom_line(data=US10Years_portfolio_df, aes(x=date, y=daily.returns, color="US10Years -5,3%"), size=1) +
  labs(title="Confronto rendimenti in 5 anni 2018-2023", x="Data", y="Valore")+
  theme_light()





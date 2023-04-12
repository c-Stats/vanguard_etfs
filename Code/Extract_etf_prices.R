library("data.table")
library("dplyr")
library("quantmod")
library("zoo")

etf_file <- as.data.table(read.csv("./Data/etfs.csv"))
tickers <- unique(etf_file[["Symbol"]])

current_date <- Sys.Date()
nyears <- 10
from <- current_date - nyears * 365

quantmod::getSymbols(tickers,
                        from = from,
                        to = current_date,
                        warnings = FALSE, auto.assign = TRUE)

tickers <- tickers[which(sapply(tickers, exists))]
etfs <- lapply(tickers, get)
etfs <- lapply(etfs, as.data.table)

df <- lapply(etfs, function(x){x[, "index"]}) # nolint
names(df) <- c(1:length(df)) # nolint
df <- unique(dplyr::bind_rows(df))[order(index), ]

#Weird bug
etfs <- append(etfs, c(1), 0)
etfs[[1]] <- df
names(etfs) <- c("Date", tickers)

for(i in 1:length(etfs)){ # nolint
    data.table::setkey(etfs[[i]], "index")
}

#Merge dataframes, keep close prices
merge_reduce <- function(df_1, df_2){ # nolint

    names_2 <- names(df_2)
    keep <- names_2[c(1, grep("Close", names_2))]
    df_2 <- df_2[, keep, with = FALSE]
    names(df_2)[ncol(df_2)] <- strsplit(keep[length(keep)], ".Close")[[1]]

    return(df_2[df_1, ])
}

df <- Reduce(merge_reduce, etfs)
tickers <- names(df)[-1]

#Remove tickers with < nyears history
has_na <- unlist(df[, lapply(.SD, function(x){is.na(x[1])}), .SDcols = tickers]) # nolint
keep <- which(!has_na)
tickers <- tickers[keep]
df <- df[, c("index", tickers), with = FALSE]

#Interpolate missing values
x_ref <- df$index
interpol <- function(y){ # nolint
    if(any(is.na(y))){approx(x=x_ref[which(!is.na(y))], y=y[which(!is.na(y))], xout=x_ref)$y} else {y} # nolint
} # nolint
df_interpol <- df[, lapply(.SD, interpol), .SDcols = tickers]
df <- data.table(index = df$index, df_interpol)

#Remove etf with < 0.8 * VOO yearly returns
returns <- unlist(df[, lapply(.SD, function(x){(x[length(x)]/x[1])^(1/nyears) - 1}), .SDcols = tickers]) # nolint
VOO_returns <- returns[which(tickers == "VOO")] # nolint
keep <- which(returns >= 0.8 * VOO_returns)
tickers <- tickers[keep]
df <- df[, c("index", tickers), with = FALSE]

names(df)[1] <- "Date"

#Compute log-returns
etf_names <- names(df)[-1]
time_diff <- as.integer(diff(df$Date))
log_returns <- df[, lapply(.SD, function(x){log(x[-1] / x[-length(x)]) / time_diff}), .SDcols = etf_names] # nolint
log_returns <- data.table(Date = df$Date[-1], log_returns)

write.csv(df, "./Data/closing_prices.csv", row.names = FALSE)
write.csv(log_returns, "./Data/log_returns.csv", row.names = FALSE)
library("data.table")
library("dplyr")
library("quantmod")
library("quadprog")
library("zoo")
library("caret")

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

#Remove etf with negative returns
has_pos_returns <- unlist(df[, lapply(.SD, function(x){x[1] < x[length(x)]}), .SDcols = tickers]) # nolint
keep <- which(has_pos_returns)
tickers <- tickers[keep]
df <- df[, c("index", tickers), with = FALSE]

#Remove etf with < 99% VOO yearly returns
returns <- unlist(df[, lapply(.SD, function(x){(x[length(x)]/x[1])^(1/nyears) - 1}), .SDcols = tickers]) # nolint
VOO_returns <- returns[which(tickers == "VOO")] # nolint
keep <- which(returns >= 0.99 * VOO_returns)
tickers <- tickers[keep]
df <- df[, c("index", tickers), with = FALSE]

#Use n-weekly returns, mid-week
#Use n as large as can be without making the covariance matrix singular
wednesdays <- which(wday(df$index) == 4)
n_weeks <- min(floor(length(wednesdays) / (ncol(df) - 1)), 4)
print(paste("Numer of weeks:", n_weeks), quote = FALSE)

wednesdays <- wednesdays[seq(from = 1, to = length(wednesdays), by = n_weeks)]
df <- df[wednesdays]
names(df)[1] <- "Date"

#Compute log-returns
etf_names <- names(df)[-1]
time_diff <- as.integer(diff(df$Date))
log_returns <- df[, lapply(.SD, function(x){log(x[-1] / x[-length(x)]) / time_diff}), .SDcols = etf_names] # nolint
log_returns <- data.table(Date = df$Date[-1], log_returns)

#Compute best sortino ratio portfolio
r <- as.matrix(log_returns[, names(log_returns)[-1], with = FALSE])
r_positive_index <- which(r > 0)
r_negative <- r
r_negative[r_positive_index] <- 0

mu <- apply(r, 2, mean)

vcov <- var(r_negative)
var_non_zero <- function(x){var(x[which(x != 0)])} # nolint
nz_var <- log_returns[, lapply(.SD, var_non_zero), .SDcols = etf_names]
diag(vcov) <- unlist(nz_var)

mu <- apply(r, 2, mean)

D.mat <- 2 * vcov # nolint
d.vec <- rep(0, ncol(D.mat)) # nolint
A.mat <- cbind(mu, diag(length(mu))) # nolint
b.vec <- c(1, rep(0, length(mu))) # nolint

qp.out <- quadprog::solve.QP(Dmat = D.mat, dvec = d.vec, # nolint
                    Amat = A.mat, bvec = b.vec, meq = 1) # nolint

weights <- qp.out$solution / sum(qp.out$solution) # nolint
weights[which(weights < 0.01)] <- 0
weights <- weights / sum(weights)

weights_df <- as.data.table(weights / sum(weights))
names(weights_df) <- "Weight" # nolint
weights_df <- data.table(Ticker = names(log_returns)[-1], weights_df)
weights_df <- weights_df[Weight > 0]

pf_sd <- as.numeric(sqrt(weights %*% vcov %*% weights))
pf_mu <- as.numeric(weights %*% mu)
pf_sharpe <- pf_mu / pf_sd
pf_r <- r %*% weights
pf_r_cumsum <- cumsum(pf_r)

pf_initial_prices <- unlist(df[1, weights_df$Ticker, with = FALSE])
pf_weights <- weights_df$Weight / pf_initial_prices
pf_weights <- pf_weights / sum(pf_weights)

pf_value <- as.matrix(df[, weights_df$Ticker, with = FALSE]) %*% pf_weights

#Save the weights for today
pf_current_prices <- unlist(df[nrow(df), weights_df$Ticker, with = FALSE])
today_weights <- weights_df$Weight / pf_current_prices
today_weights <- round(today_weights / sum(today_weights), 2)
today_weights_df <- data.table(Ticker = weights_df$Ticker, as.data.table(today_weights)) # nolint
today_weights_df[, Price := pf_current_prices]
names(today_weights_df)[2] <- "Weight"

print(today_weights_df)

cormat_vars <- c("VOO", today_weights_df$Ticker)
cor(log_returns[, cormat_vars, with = FALSE])

write.csv(today_weights_df, "./Data/weights.csv", row.names = FALSE)

pf_prog <- as.data.table(pf_value / pf_value[1])
names(pf_prog) <- "Value"
pf_prog  <- data.table(Date = df$Date, pf_prog)
write.csv(pf_prog, "./Data/portfolio_unit_value.csv", row.names = FALSE)

plot(pf_prog, type = "l")

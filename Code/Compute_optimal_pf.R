library("data.table")
library("magrittr")
library("dplyr")
library("mlrMBO")

prices <- data.table::fread("./Data/closing_prices.csv")

ndays <- as.integer(prices$Date[nrow(prices)] - prices$Date[1])
nyears <- ndays / 365

date_diff <- diff(prices$Date)

etfs <- names(prices)[-1]
s <- as.matrix(prices[, etfs, with = FALSE])
s <- apply(s, 2, function(x){x/x[1]})  # nolint

#Remove highly correlated etfs
log_r <- prices[, lapply(.SD, function(x){log(x[-1] / x[-length(x)])}), .SDcols = etfs]  # nolint
log_r <- as.matrix(log_r)

mu <- apply(log_r, 2, mean)  # nolint
cormat <- cor(log_r)
diag(cormat) <- 0

treshold <- 0.95
too_high <- unique(which(cormat > treshold, arr.ind = TRUE)[, 1])
while (length(too_high) > 0) {

    rmv <- too_high[which.min(mu[too_high])]
    mu <- mu[-rmv]
    cormat <- cormat[-rmv, -rmv]
    s <- s[, -rmv]
    etfs <- etfs[-rmv]

    too_high <- unique(which(cormat > treshold, arr.ind = TRUE)[, 1])

}

sortino <- function(weights, T = 0.04) {  # nolint

    daily_T <- T / 365 # nolint
    stock <- s %*% weights

    log_r <- log(stock[-1] / stock[-length(stock)])
    downward_log_r <- log_r[which(log_r < daily_T)]  # nolint

    excess_r <- mean(log_r) - daily_T  # nolint
    dlr_density <- density(downward_log_r)
    dlr_pdf_unscalled <- splinefun(x = dlr_density$x, y = dlr_density$y)

    n_sub <- 10 * length(log_r)
    I <- integrate(dlr_pdf_unscalled, min(downward_log_r), max(downward_log_r), subdivisions = n_sub)$value  # nolint
    dlr_pdf <- function(x){dlr_pdf_unscalled (x) / I}  # nolint

    dlr_var_f <- function(x){dlr_pdf(x) * (daily_T - x)^2}  # nolint
    dlr_var <- integrate(dlr_var_f, lower = min(downward_log_r), upper = max(downward_log_r), subdivisions = 10*length(log_r))$value  # nolint
    dlr_sd <- sqrt(dlr_var)

    return(excess_r / dlr_sd)

}

obj.fun = makeSingleObjectiveFunction(  # nolint
    fn = function(x){-sortino(x)},
    par.set = makeNumericParamSet(id = "x", lower = 0, upper = 1, len = length(mu))  # nolint
)

# create base control object
ctrl <- makeMBOControl()
# do three MBO iterations
ctrl <- setMBOControlTermination(ctrl, iters = 100L)
# use 500 points in the focussearch
ctrl <- setMBOControlInfill(ctrl, opt.focussearch.points = 500)
# create initial design
des <- generateDesign(n = 5*length(mu), getParamSet(obj.fun), fun = lhs::maximinLHS)  # nolint

# start mbo
res <- mbo(obj.fun, design = des, control = ctrl)

print(res)

current_param <- unlist(res$x)
current_param <- current_param  / sum(current_param )
current_param <- round(current_param, 2)

while (sum(current_param) > 1) {

    nz <- which(current_param > 0)
    nz_min <- nz[which.min(current_param[nz])]
    current_param[nz_min] <- current_param[nz_min] - 0.01

}

while (sum(current_param) < 1) {

    nz <- which(current_param > 0)
    nz_max <- nz[which.max(current_param[nz])]
    current_param[nz_max] <- current_param[nz_max] + 0.01

}

keep <- which(current_param > 0)
s <- s[, keep]
current_param <- current_param[keep]
current_val <- sortino(current_param)
etfs <- etfs[keep]

weights <- data.table(Ticker = etfs, Weight = current_param, Price = unlist(prices[nrow(prices), etfs, with = FALSE]))  # nolint
weights[, Adjusted_Weights := Weight / Price] %>%
        .[, Adjusted_Weights := round(Adjusted_Weights / sum(Adjusted_Weights), 2)]  # nolint

print(weights)

pf <- data.table(Date = prices$Date, Value = s %*% current_param)
plot(pf, type = "l", ylab = "Unit price", xlab = "date", main = "Optimal Portfolio")  # nolint

index <- match(etfs, colnames(cormat))
diag(cormat) <- 1
cormat[index, index]

write.csv(weights, "./Data/weights.csv", row.names = FALSE)
write.csv(pf, "./Data/portfolio_unit_value.csv", row.names = FALSE)
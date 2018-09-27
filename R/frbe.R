.computeLength <- function(d) {
  res <- length(d)
  return(res)
}


.computeTrendStrength <- function(d) {
  # computing as a p-value of the zero slope in linear regression model
  # TODO: nebylo by lepsi vracet nejak logaritmus pval?)
  # TODO: prokladame primkou (linearni regrese) - co kdybychom zkusili prokladat taky necim jinym
  #       a definovat tak vice ruznych trendStrength-u?
  t <- 1:length(d)
  pval <- summary(lm(d ~ t))$coefficients[2, 4]
  return(1 - pval)
}


.computeSeasonStrength <- function(d) {
  # TODO: nebylo by lepsi vracet nejak logaritmus pval?)
  if (frequency(d) == 1) {
    # perioda = 12 mesicu, rocni casovka automaticky neni sezonni
    return(0)
  } else {
    time <- 1:length(d)
    num <- frequency(d) - 1
    vec <- rep(c(rep(0, num), 1), length(d))
    flags <- matrix(vec[1:(length(d) * num)], byrow=TRUE, ncol=num, nrow=length(d))
    form <- paste("d ~ time +", paste("flags[,", 1:num, "]", sep="", collapse=" + "))
    pval <- min(summary(lm(as.formula(form)))$coefficients[c(-1, -2), 4])
    return(1 - pval)
  }
}


.computeSkewness <- function(d) {
  # TODO: mozna by stalo za to  to transformovat nejak nelinearne
  s <- abs(skewness(d, type=1))
  return(s)
}


.computeKurtosis <- function(d) {
  # TODO: asi urcite by stalo za to  to transformovat nejak nelinearne
  k <- 3 + kurtosis(d, type=1)
  return(k)
}


.computeVarcoef <- function(d) {
  # TODO: uprava i pro zaporna data (tj. data s nulovym prumerem)
  return(sd(d) / mean(d))
}


.computeStationarity <- function(d) {
  # TODO: nebylo by lepsi vracet nejak logaritmus pval?)

  # on very short time series, this sometimes causes error
  # (see https://stackoverflow.com/questions/17282788/r-error-with-adf-test-in-time-series-lapply)
  # to recover that, we return 0.5
  pval <- try(suppressWarnings(adf.test(d)$p.value))
  if (inherits(pval, 'try-error')) {
      return(0.5)
  }
  return(1 - pval)
}


.computeFrequency <- function(d) {
  return(1 / frequency(d))
}







#' Fuzzy Rule-Based Ensemble (FRBE) of time-series forecasts
#'
#' This function computes the fuzzy rule-based ensemble of time-series
#' forecasts.  Several forecasting methods are used to predict future values of
#' given time-series and a weighted sum is computed from them with weights
#' being determined from a fuzzy rule base.
#'
#' This function computes the fuzzy rule-based ensemble of time-series
#' forecasts.  The evaluation comprises of the following steps:
#' 1. Several features are extracted from the given time-series `d`:
#'    * length of the time-series;
#'    * strength of trend;
#'    * strength of seasonality;
#'    * skewness;
#'    * kurtosis;
#'    * variation coefficient;
#'    * stationarity;
#'    * frequency.
#'    These features are used later to infer weights of the forecasting methods.
#' 1. Several forecasting methods are applied on the given time-series `d` to
#'    obtain forecasts. Actually, the following methods are used:
#'    * ARIMA - by calling [forecast::auto.arima()];
#'    * Exponential Smoothing - by calling [forecast::ets()];
#'    * Random Walk with Drift - by calling [forecast::rwf()];
#'    * Theta - by calling [forecast::thetaf().
#' 1. Computed features are input to the fuzzy rule-based inference mechanism
#'    which yields into weights of the forecasting methods. The fuzzy rule base is
#'    hardwired in this package and it was obtained by performing data mining with
#'    the use of the [farules()] function.
#' 1. A weighted sum of forecasts is computed and returned as a result.
#'
#' @param d A source time-series in the ts time-series format.  Note that the
#' frequency of the time-series must to be set properly.
#' @param h A forecasting horizon, i.e. the number of values to forecast.
#' @return Result is a list of class `frbe` with the following elements:
#' * `features` - a data frame with computed features of the given time-series;
#' * `forecasts` - a data frame with forecasts to be ensembled;
#' * `weights` - weights of the forecasting methods as inferred from the features
#'   and the hard-wired fuzzy rule base;
#' * `mean` - the resulting ensembled forecast (computed as a weighted sum
#' of forecasts).
#'
#' @author Michal Burda
#' @seealso [evalfrbe()]
#' @references Štěpnička, M., Burda, M., Štěpničková, L. Fuzzy Rule Base
#' Ensemble Generated from Data by Linguistic Associations Mining. FUZZY SET
#' SYST. 2015.
#' @keywords models robust
#' @examples
#'   # prepare data (from the forecast package)
#'   library(forecast)
#'   horizon <- 10
#'   train <- wineind[-1 * (length(wineind)-horizon+1):length(wineind)]
#'   test <- wineind[(length(wineind)-horizon+1):length(wineind)]
#'
#'   # perform FRBE
#'   f <- frbe(ts(train, frequency=frequency(wineind)), h=horizon)
#'
#'   # evaluate FRBE forecasts
#'   evalfrbe(f, test)
#'
#'   # display forecast results
#'   f$mean
#'
#' @export
#' @importFrom forecast auto.arima
#' @importFrom forecast ets
#' @importFrom forecast rwf
#' @importFrom forecast thetaf
#' @importFrom forecast forecast
#' @importFrom e1071 skewness
#' @importFrom e1071 kurtosis
#' @importFrom stats lm
#' @importFrom stats sd
#' @importFrom stats frequency
#' @importFrom stats as.formula
#' @importFrom tseries adf.test
frbe <- function(d, h=10) {
    .mustBeTs(d);
    .mustBeNumericScalar(h)
    .mustBe(h >= 1, "'h' must be positive")

    result <- list()
    result$data <- d

    result$forecasts <- data.frame(
                arima=as.numeric(forecast(auto.arima(d, stepwise=FALSE), h=h)$mean),
                expSmooth=as.numeric(forecast(ets(d), h=h)$mean),
                randomWalk=as.numeric(rwf(d, drift=FALSE, h=h)$mean),
                theta=as.numeric(thetaf(d, h=h)$mean))


    result$features <- data.frame(length=.computeLength(d),
                                  trendStrength=.computeTrendStrength(d),
                                  seasonStrength=.computeSeasonStrength(d),
                                  skewness=.computeSkewness(d),
                                  kurtosis=.computeKurtosis(d),
                                  varcoef=.computeVarcoef(d),
                                  stationarity=.computeStationarity(d),
                                  frequency=.computeFrequency(d))

    #f <- lcut3(result$features, context=.frbemodel$featuresContext)
    ctx <- lapply(.frbemodel$featuresContext, function(x) {
        do.call('ctx3', as.list(x))
    })
    atomic <- c("sm", "me", "bi")
    hedges <- c("ex", "si", "ve", "-", "ml", "ro", "qr", "vr")
    f <- lcut(result$features, context=ctx, atomic=atomic, hedges=hedges)

    result$weights <- sapply(names(.frbemodel$model),
                             function(n) {
                                 ctx <- do.call('ctx3', as.list(.frbemodel$weightContext[[n]]))
                                 vals <- seq(from=ctx[1], to=ctx[3], length.out=1000)
                                 parts <- lcut(vals, name='weight', context=ctx, atomic=atomic, hedges=hedges)
                                 pbld(f, .frbemodel$model[[n]], parts, vals, type='global')
                             })
    result$weights <- result$weights[colnames(result$forecasts)]

    if (sum(result$weights) == 0) {
        result$weights <- rep(1, ncol(result$forecasts))
        names(result$weights) <- colnames(result$forecasts)
    }

    result$mean <- apply(result$forecasts, 1,
                         function(row) {
                             sum(row * result$weights) / sum(result$weights)
                         })

    class(result) <- c('frbe', class(result))
    return(result)
}

function (data, now, units, onset_date, report_date, moving_window = NULL, 
          max_D = NULL, cutoff_D = NULL, proportion_reported = 1, quiet = TRUE, 
          specs = list(dist = c("Poisson", "NB"), alpha1.mean.prior = 0, 
                       alpha1.prec.prior = 0.001, alphat.shape.prior = 0.001, 
                       alphat.rate.prior = 0.001, beta.priors = NULL, param_names = NULL, 
                       conf = 0.95, dispersion.prior = NULL, nAdapt = 1000, 
                       nChains = 1, nBurnin = 1000, nThin = 1, nSamp = 10000)) 
{
  if (inherits(now, "Date") == FALSE) {
    stop("'Now' argument must be of datatype Date (as.Date)")
  }
  if (dplyr::last(seq(unique(data[, onset_date])[1], now, by = units)) != 
      now) {
    stop("The date `now` is not possible to estimate: the possible nowcast dates are seq(unique(data[,onset_date])[1],now,by=units).")
  }
  message(paste("Computing a nowcast for ", now))
  now.T <- ifelse(is.null(moving_window), length(seq(min(data[, 
                                                              onset_date]), as.Date(now), by = units)), moving_window)
  if (is.null(moving_window)) {
    moving_window <- now.T
  }
  if (is.null(max_D)) {
    max_D <- now.T - 1
  }
  if (is.null(cutoff_D)) {
    cutoff_D <- TRUE
  }
  if (quiet == TRUE) {
    progress.bar <- "none"
  }
  if (quiet == FALSE) {
    progress.bar <- "text"
  }
  if (proportion_reported > 1 | proportion_reported <= 0) {
    stop("The proportion_reported must be a number between (0,1].")
  }
  if ("Poisson" %in% (specs[["dist", exact = TRUE]])) {
    specs$dist <- "Poisson"
  }
  if (is.null(specs[["dist", exact = TRUE]])) {
    specs$dist <- "Poisson"
  }
  if (is.null(specs[["alpha1.mean.prior", exact = TRUE]])) {
    specs$alpha1.mean.prior <- 0
  }
  if (is.null(specs[["alpha1.prec.prior", exact = TRUE]])) {
    specs$alpha1.prec.prior <- 0.001
  }
  if (is.null(specs[["alphat.shape.prior", exact = TRUE]])) {
    specs$alphat.shape.prior <- 0.001
  }
  if (is.null(specs[["alphat.rate.prior", exact = TRUE]])) {
    specs$alphat.rate.prior <- 0.001
  }
  if (is.null(specs[["beta.priors", exact = TRUE]])) {
    specs$beta.priors <- rep(0.1, times = (max_D) + 1)
  }
  if (is.null(specs[["param_names", exact = TRUE]]) & (specs[["dist"]] == 
                                                       "Poisson")) {
    specs$param_names <- c("lambda", "alpha", "beta.logged", 
                           "tau2.alpha", "sum.n")
  }
  if (is.null(specs[["param_names", exact = TRUE]]) & (specs[["dist"]] == 
                                                       "NB")) {
    specs$param_names <- c("lambda", "alpha", "beta.logged", 
                           "tau2.alpha", "sum.n", "r")
  }
  if (is.null(specs[["conf", exact = TRUE]])) {
    specs$conf <- 0.95
  }
  if (is.null(specs[["dispersion.prior", exact = TRUE]]) & 
      (specs[["dist"]] == "NB")) {
    specs$dispersion.prior <- c(0.001, 0.001)
  }
  if (is.null(specs[["nAdapt", exact = TRUE]])) {
    specs$nAdapt <- 1000
  }
  if (is.null(specs[["nChains", exact = TRUE]])) {
    specs$nChains <- 1
  }
  if (is.null(specs[["nBurnin", exact = TRUE]])) {
    specs$nBurnin <- 1000
  }
  if (is.null(specs[["nThin", exact = TRUE]])) {
    specs$nThin <- 1
  }
  if (is.null(specs[["nSamp", exact = TRUE]])) {
    specs$nSamp <- 10000
  }
  if (max_D > (moving_window - 1)) {
    stop("Maximum delay cannot be greater than the length of the moving window minus 1 time unit")
  }
  unit.num <- switch(units, `1 day` = 1, `1 week` = 7)
  w.days <- max((moving_window - 1) * unit.num, (now.T - 1) * 
                  unit.num)
  realtime.data <- subset(data, (data[, onset_date] <= now) & 
                            (data[, onset_date] >= now - w.days) & (data[, report_date] <= 
                                                                      now) & (data[, report_date] >= now - w.days))
  realtime.data$week.t <- (as.numeric(realtime.data[, onset_date] - 
                                        min(realtime.data[, onset_date]))/unit.num) + 1
  realtime.data$delay <- as.numeric(realtime.data[, report_date] - 
                                      realtime.data[, onset_date])/unit.num
  if (cutoff_D == FALSE) {
    realtime.data$delay <- ifelse(realtime.data$delay >= 
                                    max_D, max_D, realtime.data$delay)
  }
  if (length(unique(realtime.data$week.t)) != now.T) {
    warning("Warning! The line list has zero case reports for one or more possible onset dates at one or more delays. Proceeding under the assumption that the true number of cases at the associated delay(s) and week(s) is zero.")
  }
  reporting.triangle <- matrix(NA, nrow = now.T, ncol = (max_D + 
                                                           1))
  for (t in 1:now.T) {
    for (d in 0:max_D) {
      reporting.triangle[t, (d + 1)] <- nrow(realtime.data[which(realtime.data$week.t == 
                                                                   t & realtime.data$delay == d), ])
      if (now.T < (t + d)) {
        reporting.triangle[t, (d + 1)] <- NA
      }
    }
  }
  if (specs[["dist"]] == "Poisson") {
    params = c("lambda", "alpha", "beta.logged", "tau2.alpha", 
               "n", "sum.n", "sum.lambda")
  }
  if (specs[["dist"]] == "NB") {
    params = c("lambda", "alpha", "beta.logged", "tau2.alpha", 
               "n", "sum.n", "sum.lambda", "r")
  }
  nAdapt = specs[["nAdapt"]]
  nChains = specs[["nChains"]]
  nBurnin = specs[["nBurnin"]]
  nThin = specs[["nThin"]]
  nKeep = specs[["nSamp"]]
  nIter = nKeep * nThin
  if (specs[["dist"]] == "Poisson") {
    dataList = list(Today = now.T, D = max_D, n = reporting.triangle, 
                    alpha1.mean.prior = specs$alpha1.mean.prior, alpha1.prec.prior = specs$alpha1.prec.prior, 
                    alphat.rate.prior = specs$alphat.rate.prior, alphat.shape.prior = specs$alphat.shape.prior, 
                    beta.priors = specs$beta.priors)
  }
  if (specs[["dist"]] == "NB") {
    dataList = list(Today = now.T, D = max_D, n = reporting.triangle, 
                    alpha1.mean.prior = specs$alpha1.mean.prior, alpha1.prec.prior = specs$alpha1.prec.prior, 
                    alphat.rate.prior = specs$alphat.rate.prior, alphat.shape.prior = specs$alphat.shape.prior, 
                    beta.priors = specs$beta.priors, dispersion.prior.shape = specs$dispersion.prior[1], 
                    dispersion.prior.rate = specs$dispersion.prior[2])
  }
  JAGSmodPois <- (system.file("JAGS", "nowcastPois.txt", package = "NobBS"))
  JAGSmodNB <- (system.file("JAGS", "nowcastNB.txt", package = "NobBS"))
  nowcastmodel = jags.model(file = ifelse(specs[["dist"]] == 
                                            "Poisson", JAGSmodPois, JAGSmodNB), data = dataList, 
                            n.chains = nChains, n.adapt = nAdapt, inits = list(.RNG.seed = 1, 
                                                                               .RNG.name = "base::Super-Duper"), quiet = quiet)
  update(object = nowcastmodel, n.iter = nBurnin, progress.bar = progress.bar)
  lambda.output = coda.samples(model = nowcastmodel, variable.names = if ("sum.n" %in% 
                                                                          specs$param_names) 
    c(specs$param_names)
    else c(specs$param_names, "sum.n"), n.iter = nIter, thin = nThin, 
    quiet = quiet, progress.bar = progress.bar)
  mymod.mcmc <- as.mcmc(lambda.output)
  mymod.dat <- as.data.frame(as.matrix(mymod.mcmc))
  t.extract <- (now.T - (now.T - 1)):(now.T)
  estimates <- matrix(NA, ncol = 3, nrow = now.T, dimnames = list(NULL, 
                                                                  c("estimate", "lower", "upper")))
  for (v in t.extract) {
    estimates[v, 1] <- median(mymod.dat[, grep(paste("sum.n[", 
                                                     v, "]", sep = ""), colnames(mymod.dat), fixed = TRUE)])
    estimates[v, 2] <- quantile((mymod.dat[, grep(paste("sum.n[", 
                                                        v, "]", sep = ""), colnames(mymod.dat), fixed = TRUE)]), 
                                probs = c((1 - specs$conf)/2, 1 - ((1 - specs$conf)/2)))[1]
    estimates[v, 3] <- quantile((mymod.dat[, grep(paste("sum.n[", 
                                                        v, "]", sep = ""), colnames(mymod.dat), fixed = TRUE)]), 
                                probs = c((1 - specs$conf)/2, 1 - ((1 - specs$conf)/2)))[2]
  }
  estimates.inflated <- matrix(NA, ncol = 3, nrow = now.T, 
                               dimnames = list(NULL, c("estimate_inflated", "lower", 
                                                       "upper")))
  for (v in t.extract) {
    estimates.inflated[v, 1] <- median(mymod.dat[, grep(paste("sum.n[", 
                                                              v, "]", sep = ""), colnames(mymod.dat), fixed = TRUE)])/proportion_reported
    estimates.inflated[v, 2] <- quantile((mymod.dat[, grep(paste("sum.n[", 
                                                                 v, "]", sep = ""), colnames(mymod.dat), fixed = TRUE)]), 
                                         probs = c((1 - specs$conf)/2, 1 - ((1 - specs$conf)/2)))[1]/proportion_reported
    estimates.inflated[v, 3] <- quantile((mymod.dat[, grep(paste("sum.n[", 
                                                                 v, "]", sep = ""), colnames(mymod.dat), fixed = TRUE)]), 
                                         probs = c((1 - specs$conf)/2, 1 - ((1 - specs$conf)/2)))[2]/proportion_reported
  }
  reported <- data.frame(realtime.data %>% dplyr::group_by(!!sym(onset_date)) %>% 
                           dplyr::summarise(n.reported = dplyr::n()))
  names(reported)[1] <- "onset_date"
  estimates <- data.frame(estimates, onset_date = (seq(as.Date(now) - 
                                                         w.days, as.Date(now), by = units))) %>% dplyr::left_join(reported, 
                                                                                                                  by = "onset_date")
  estimates.inflated <- data.frame(estimates.inflated, onset_date = (seq(as.Date(now) - 
                                                                           w.days, as.Date(now), by = units))) %>% dplyr::left_join(reported, 
                                                                                                                                    by = "onset_date")
  t <- now.T
  parameter_extract <- matrix(NA, nrow = 10000)
  if ("lambda" %in% specs$param_names) {
    parameter_extract <- cbind(parameter_extract, mymod.dat %>% 
                                 dplyr::select(starts_with(paste("lambda[", t, ",", 
                                                                 sep = ""))))
  }
  if ("beta.logged" %in% specs$param_names) {
    betas.logged <- matrix(NA, nrow = 10000, ncol = (max_D + 
                                                       1))
    dimnames(betas.logged) = list(NULL, c(paste("Beta", c(0:max_D))))
    for (d in 0:max_D) {
      betas.logged[, (d + 1)] <- (mymod.dat %>% dplyr::select(starts_with(paste("beta.logged[", 
                                                                                (d + 1), "]", sep = ""))))[, 1]
    }
    parameter_extract <- cbind(parameter_extract, betas.logged)
  }
  if ("alpha" %in% specs$param_names) {
    parameter_extract <- cbind(parameter_extract, mymod.dat %>% 
                                 dplyr::select(starts_with(paste("alpha[", t, sep = ""))))
  }
  if ("tau2.alpha" %in% specs$param_names) {
    parameter_extract <- cbind(parameter_extract, mymod.dat %>% 
                                 dplyr::select(starts_with("tau2.alpha")))
  }
  nowcast.post.samps <- (mymod.dat %>% dplyr::select(starts_with(paste("sum.n[", 
                                                                       t, sep = ""))))[, 1]
  nowcast.post.samps.tminus1 <- (mymod.dat %>% dplyr::select(starts_with(paste("sum.n[",(t-1),sep=""))))[,1]
  nowcast.post.samps.tminus2 <- (mymod.dat %>% dplyr::select(starts_with(paste("sum.n[",(t-2),sep=""))))[,1]
  nowcast.post.samps.tminus3 <- (mymod.dat %>% dplyr::select(starts_with(paste("sum.n[",(t-3),sep=""))))[,1]
  nowcast.post.samps.tminus4 <- (mymod.dat %>% dplyr::select(starts_with(paste("sum.n[",(t-4),sep=""))))[,1]
  nowcast.post.samps.tminus5 <- (mymod.dat %>% dplyr::select(starts_with(paste("sum.n[",(t-5),sep=""))))[,1]
  nowcast.post.samps.tminus6 <- (mymod.dat %>% dplyr::select(starts_with(paste("sum.n[",(t-6),sep=""))))[,1]
  
  list(estimates = estimates, estimates.inflated = estimates.inflated, 
       nowcast.post.samps = nowcast.post.samps, nowcast.post.samps.tminus1 = nowcast.post.samps.tminus1, 
       nowcast.post.samps.tminus2 = nowcast.post.samps.tminus2, nowcast.post.samps.tminus3 = nowcast.post.samps.tminus3,
       nowcast.post.samps.tminus4 = nowcast.post.samps.tminus4, nowcast.post.samps.tminus5 = nowcast.post.samps.tminus5,
       nowcast.post.samps.tminus6 = nowcast.post.samps.tminus6, 
       params.post = parameter_extract[, 
                                                                                2:ncol(parameter_extract)])
}

#
# # Check that the sample size is at least equal to the number of brackets
# if (is.null(samplesize) | is.na(samplesize)) {
#     stop("'samplesize' must not be NULL or NA if citype == 2")
# } else if (samplesize < n){
#     stop("'samplesize' too low")
# }
#
# # Estimate sampling error via resampling
# cl <- parallel::makeCluster(parallel::detectCores())
# parallel::clusterExport(cl, envir=environment(), varlist=c(
#     "fitted_quantile", "phi", "deriv_phi", "gpd_top_phi", "gpd_bottom_phi",
#     "gpd_top_deriv_phi", "gpd_bottom_deriv_phi", "gpd_top_mean",
#     "gpd_bottom_mean", "gpd_top_quantile", "gpd_bottom_quantile",
#     "quintic_spline", "deriv_quintic_spline", "h00", "h01", "h10",
#     "h11", "h20", "h21", "h00d1", "h01d1", "h10d1", "h11d1",
#     "h20d1", "h21d1", "p", "n", "samplesize", "xk", "right_derivative",
#     "central_derivative", "left_derivative", "gpd_top_parameters",
#     "gpd_bottom_parameters", "result"
# ))
# resamp_param <- parallel::parLapply(cl, 1:nrep, function(i) {
#     sample <- sort(result$fitted_quantile(runif(samplesize)))
#     truncmean <- rev(cumsum(rev(sample)))/samplesize
#     threshold <- sample[floor(p*samplesize)]
#     m <- truncmean[floor(p*samplesize)]
#
#     yk <- -log(m)
#     sk <- (1 - p)*threshold/m
#     ak <- c(
#         right_derivative(
#             xk[1], xk[2], xk[3],
#             sk[1], sk[2], sk[3]
#         ),
#         central_derivative(
#             xk[1:(n - 2)], xk[2:(n - 1)], xk[3:n],
#             sk[1:(n - 2)], sk[2:(n - 1)], sk[3:n]
#         ),
#         left_derivative(
#             xk[n - 2], xk[n - 1], xk[n],
#             sk[n - 2], sk[n - 1], sk[n]
#         )
#     )
#
#     average <- mean(sample)
#
#     param_top <- gpd_top_parameters(xk[n], yk[n], sk[n], ak[n])
#     if (min(p) > 0) {
#         param_bottom <- gpd_bottom_parameters(xk[1], yk[1], sk[1], ak[1], average)
#     } else {
#         param_bottom <- list(mu=NA, sigma=NA, xi=NA)
#     }
#
#     return(list(yk=yk, sk=sk, ak=ak, average=average,
#         mu_top=param_top$mu, sigma_top=param_top$sigma, xi_top=param_top$xi,
#         mu_bottom=param_bottom$mu, sigma_bottom=param_bottom$sigma, xi_bottom=param_bottom$xi))
# })
# parallel::stopCluster(cl)
#
# # Confidence interval functions
# result$ci_phi <- function(x) {
#     # Estimate sampling error
#     replication <- sapply(resamp_param, function(sample) {
#         return(phi(x,
#             xk, sample$yk, sample$sk, sample$ak,
#             sample$mu_top, sample$sigma_top, sample$xi_top,
#             sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
#         ))
#     })
#     if (nrow(replication) > 1) {
#         lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
#         upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
#     } else {
#         lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
#         upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
#     }
#     # Estimate non-sampling error
#     e <- spline_error_degree3(x, xk, deriv4max)
#     e[is.na(e)] <- 0
#
#     return(list(lower=(lower_resamp - e), upper=(upper_resamp + e)))
# }
# result$ci_deriv_phi <- function(x) {
#     # Estimate sampling error
#     replication <- sapply(resamp_param, function(sample) {
#         return(deriv_phi(x,
#             xk, sample$yk, sample$sk, sample$ak,
#             sample$mu_top, sample$sigma_top, sample$xi_top,
#             sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
#         ))
#     })
#     if (nrow(replication) > 1) {
#         lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
#         upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
#     } else {
#         lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
#         upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
#     }
#     # Estimate non-sampling error
#     e <- deriv_spline_error_degree3(x, xk, deriv4max)
#     e[is.na(e)] <- 0
#
#     return(list(lower=(lower_resamp - e), upper=(upper_resamp + e)))
# }
# result$ci_quantile <- function(p) {
#     x <- -log(1 - p)
#     # Estimate sampling error
#     replication <- sapply(resamp_param, function(sample) {
#         return(fitted_quantile(p,
#             xk, sample$yk, sample$sk, sample$ak,
#             sample$mu_top, sample$sigma_top, sample$xi_top,
#             sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
#         ))
#     })
#     if (nrow(replication) > 1) {
#         lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
#         upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
#     } else {
#         lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
#         upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
#     }
#     y <- result$phi(x)
#     dydx <- result$deriv_phi(x)
#     q <- dydx*exp(x - y)
#     u1 <- q - lower_resamp
#     u2 <- upper_resamp - q
#     # Estimate non-sampling error
#     e1 <- spline_error_degree3(x, xk, deriv4max)
#     e2 <- deriv_spline_error_degree3(x, xk, deriv4max)
#     e1[is.na(e1)] <- 0
#     e2[is.na(e2)] <- 0
#
#     y_lower <- y - e1
#     y_upper <- y + e1
#     dydx_lower <- dydx - e2
#     dydx_upper <- dydx + e2
#
#     lower <- dydx_lower*exp(x - y_upper)
#     upper <- dydx_upper*exp(x - y_lower)
#
#     return(list(lower=(lower - u1), upper=(upper + u2)))
# }
# result$ci_top_share <- function(p) {
#     x <- -log(1 - p)
#     # Estimate sampling error
#     replication <- sapply(resamp_param, function(sample) {
#         return(fitted_top_share(p,
#             xk, sample$yk, sample$sk, sample$ak, sample$average,
#             sample$mu_top, sample$sigma_top, sample$xi_top,
#             sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
#         ))
#     })
#     if (nrow(replication) > 1) {
#         lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
#         upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
#     } else {
#         lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
#         upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
#     }
#     y <- result$phi(x)
#     ts <- exp(-y)/average
#     u1 <- ts - lower_resamp
#     u2 <- upper_resamp - ts
#     # Estimate non-sampling error
#     e <- spline_error_degree3(x, xk, deriv4max)
#     e[is.na(e)] <- 0
#     y_lower <- y - e
#     y_upper <- y + e
#
#     lower <- exp(-y_upper)/average
#     upper <- exp(-y_lower)/average
#
#     return(list(lower=(lower - u1), upper=(upper + u2)))
# }
# result$ci_invpareto <- function(p) {
#     x <- -log(1 - p)
#     # Estimate sampling error
#     replication <- sapply(resamp_param, function(sample) {
#         return(fitted_invpareto(p,
#             xk, sample$yk, sample$sk, sample$ak,
#             sample$mu_top, sample$sigma_top, sample$xi_top,
#             sample$mu_bottom, sample$sigma_bottom, sample$xi_bottom
#         ))
#     })
#     if (nrow(replication) > 1) {
#         lower_resamp <- apply(replication, 1, function(x) quantile(x, cilevel/2, na.rm=TRUE))
#         upper_resamp <- apply(replication, 1, function(x) quantile(x, 1 - cilevel/2, na.rm=TRUE))
#     } else {
#         lower_resamp <- quantile(replication, cilevel/2, na.rm=TRUE)
#         upper_resamp <- quantile(replication, 1 - cilevel/2, na.rm=TRUE)
#     }
#     dydx <- result$deriv_phi(x)
#     invpareto <- 1/dydx
#     u1 <- invpareto - lower_resamp
#     u2 <- upper_resamp - invpareto
#     # Estimate non-sampling error
#     e <- spline_error_degree3(x, xk, deriv4max)
#     e[is.na(e)] <- 0
#
#     dydx_lower <- dydx - e
#     dydx_upper <- dydx + e
#
#     lower <- 1/dydx_upper
#     upper <- 1/dydx_lower
#
#     return(list(lower=(lower - u1), upper=(upper + u2)))
# }

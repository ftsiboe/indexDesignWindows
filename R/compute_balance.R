#' Distributional balance: one basis vs multiple comparison variables (long format)
#'
#' @description
#' For a given **basis** numeric column (`col_x`) and one or more **comparison**
#' numeric columns (`col_y`), this function computes a rich set of diagnostics
#' describing bias, scale, agreement, and distributional differences between
#' the basis and each comparison variable.
#'
#' Results are returned in **long format** with one row per group (defined by
#' `by`) and per comparison variable (`y_level`).
#'
#' Metrics include:
#'
#' - **Sample size & moments**
#'   - `n`: number of non-missing pairs
#'   - `mean_x`, `mean_y`
#'   - `var_x`,  `var_y`
#'   - `sd_x`,   `sd_y`
#'   - `cv_x`,   `cv_y` (coefficients of variation)
#'
#' - **Bias & scale**
#'   - `mean_bias`   = mean_x - mean_y
#'   - `mae`         = mean(|x - y|)
#'   - `rmse`        = sqrt(mean((x - y)^2))
#'   - `smape`       = 2 * mean(|x - y| / (|x| + |y|)), with safe handling when denominator is zero
#'   - `ratio_means` = mean_x / mean_y
#'   - `cv_ratio`    = cv_x / cv_y
#'   - `mpd`         = mean percentage difference: \deqn{(((\bar{x} - \bar{y}) / \bar{y}) - 1) * 100}
#'
#' - **Balance / distribution comparison**
#'   - `msd`       = mean standardized difference: (mean_x - mean_y) / sqrt((sd_x^2 + sd_y^2) / 2)
#'   - `var_ratio` = var_x / var_y
#'   - `ks_stat`   = Kolmogorov-Smirnov D-statistic (two-sample)
#'   - `emd`       = 1D Wasserstein / Earth Mover's Distance, approximated as mean absolute difference between quantiles of x and y
#'   - `hellinger` = Hellinger distance computed from common histograms
#'
#'   - `pvalue_mean`           = p-value for equality of means (paired t-test)
#'   - `pvalue_ks`             = p-value from Kolmogorov-Smirnov test
#'   - `pvalue_var`            = p-value from F-test of equal variances
#'   - `pvalue_levene`         = p-value from Levene test (mean-based)
#'   - `pvalue_brown_forsythe` = p-value from Brown-Forsythe test
#'   - `pvalue_kruskal_wallis` = p-value from Kruskal-Wallis test
#'
#' - **Zero-mass / structural zeros**
#'   - `zero_share_x`   = proportion of observations with x == 0
#'   - `zero_share_y`   = proportion of observations with y == 0
#'   - `zero_share_diff` = zero_share_x - zero_share_y
#'   - `pvalue_zero_mcnemar` = p-value from McNemar test on the paired zero/non-zero indicators
#'
#' - **Bounded-scale / logit-space (for (0, 1) variables)**
#'   - `mean_logit_x`, `mean_logit_y`   = means of logit-transformed x and y
#'   - `mean_logit_diff`                = mean(logit_x - logit_y)
#'   - `rmse_logit`                     = sqrt(mean((logit_x - logit_y)^2))
#'   - `pvalue_mean_logit`              = p-value from paired t-test on logit_x vs logit_y
#'
#'   These logit-based metrics are computed only if all non-missing x and y are
#'   in the interval (0, 1). Otherwise, they are returned as NA.
#'
#' - **Agreement / association**
#'   - `pearson_cor`      = Pearson correlation
#'   - `spearman_cor`     = Spearman rank correlation
#'   - `kendall_cor`      = Kendall's tau
#'   - `ccc`              = Lin's Concordance Correlation Coefficient
#'   - `pvalue_pearson_cor`
#'   - `pvalue_spearman_cor`
#'   - `pvalue_kendall_cor`
#'
#' - **Quantile / tail behavior**
#'   - Quantiles at p = 0.10, 0.25, 0.50, 0.75, 0.90 for x and y:
#'       `q10_x`, `q25_x`, `q50_x`, `q75_x`, `q90_x`
#'       `q10_y`, `q25_y`, `q50_y`, `q75_y`, `q90_y`
#'   - Differences: `q10_diff`, `q25_diff`, `q50_diff`, `q75_diff`, `q90_diff`
#'       computed as (q_x - q_y)
#'
#' @param dt A `data.frame` or `data.table`.
#' @param col_x Character scalar. Name of the **basis** numeric column.
#' @param col_y Character vector. Names of one or more **comparison** numeric
#'   columns. The basis variable `col_x` is always compared against each
#'   variable in `col_y`.
#' @param by Optional character vector of grouping variables. If `NULL`
#'   (default), all rows are treated as one group.
#'
#' @return A `data.table` in **long format** with columns:
#'   \code{c(by,
#'           "y_level",
#'           "n", "mean_x", "mean_y", "var_x", "var_y",
#'           "sd_x", "sd_y", "cv_x", "cv_y", "cv_ratio",
#'           "mean_bias", "mae", "rmse", "smape",
#'           "ratio_means", "mpd",
#'           "msd", "var_ratio", "ks_stat",
#'           "emd", "hellinger",
#'           "pvalue_mean", "pvalue_ks", "pvalue_var",
#'           "pvalue_levene", "pvalue_brown_forsythe", "pvalue_kruskal_wallis",
#'           "zero_share_x", "zero_share_y", "zero_share_diff",
#'           "pvalue_zero_mcnemar",
#'           "mean_logit_x", "mean_logit_y",
#'           "mean_logit_diff", "rmse_logit", "pvalue_mean_logit",
#'           "pearson_cor", "spearman_cor", "kendall_cor", "ccc",
#'           "pvalue_pearson_cor", "pvalue_spearman_cor", "pvalue_kendall_cor",
#'           "q10_x", "q25_x", "q50_x", "q75_x", "q90_x",
#'           "q10_y", "q25_y", "q50_y", "q75_y", "q90_y",
#'           "q10_diff", "q25_diff", "q50_diff", "q75_diff", "q90_diff")}.
#'
#' @export
compute_balance <- function(dt, col_x, col_y, by = NULL) {

  dt <- data.table::as.data.table(dt)
  col_y <- setdiff(as.character(col_y), col_x)  # avoid comparing basis to itself

  # Basic validation -----------------------------------------------------------
  if (!col_x %in% names(dt)) {
    stop("col_x '", col_x, "' not found in dt.")
  }
  if (!all(col_y %in% names(dt))) {
    missing_y <- col_y[!col_y %in% names(dt)]
    stop("The following col_y are not found in dt: ",
         paste(missing_y, collapse = ", "))
  }
  if (length(col_y) == 0L) {
    stop("No valid comparison columns in col_y (after removing col_x).")
  }

  # Helper: safe correlation (returns NA if degenerate) -----------------------
  safe_cor <- function(x, y, method) {
    if (length(x) < 2L || length(y) < 2L) return(NA_real_)
    if (all(is.na(x)) || all(is.na(y))) return(NA_real_)
    v_x <- stats::var(x, na.rm = TRUE)
    v_y <- stats::var(y, na.rm = TRUE)
    if (!is.finite(v_x) || !is.finite(v_y) || v_x == 0 || v_y == 0) {
      return(NA_real_)
    }
    stats::cor(x, y, method = method, use = "complete.obs")
  }

  # Helper: concordance correlation coefficient (Lin's CCC) -------------------
  ccc_fun <- function(x, y) {
    if (length(x) < 2L || length(y) < 2L) return(NA_real_)
    mx <- mean(x)
    my <- mean(y)
    vx <- stats::var(x)
    vy <- stats::var(y)
    if (!is.finite(vx) || !is.finite(vy)) return(NA_real_)
    sxy <- stats::cov(x, y)
    denom <- vx + vy + (mx - my)^2
    if (!is.finite(denom) || denom == 0) return(NA_real_)
    2 * sxy / denom
  }

  # Helper: Earth Mover's Distance via quantiles ------------------------------
  emd_fun <- function(x, y, n_grid = 101L) {
    if (length(x) < 1L || length(y) < 1L) return(NA_real_)
    probs <- seq(0, 1, length.out = n_grid)
    qx <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
    qy <- stats::quantile(y, probs = probs, na.rm = TRUE, names = FALSE)
    mean(abs(qx - qy))
  }

  # Helper: Hellinger distance using a shared histogram -----------------------
  hellinger_fun <- function(x, y, n_breaks = 20L) {
    if (length(x) < 1L || length(y) < 1L) return(NA_real_)
    rng <- range(c(x, y), na.rm = TRUE)
    if (!is.finite(rng[1]) || rng[1] == rng[2]) return(NA_real_)
    brks <- pretty(rng, n = n_breaks)
    hx <- graphics::hist(x, breaks = brks, plot = FALSE)
    hy <- graphics::hist(y, breaks = brks, plot = FALSE)
    px <- hx$density
    py <- hy$density
    sx <- sum(px)
    sy <- sum(py)
    if (sx <= 0 || sy <= 0) return(NA_real_)
    px <- px / sx
    py <- py / sy
    sqrt(0.5 * sum((sqrt(px) - sqrt(py))^2))
  }

  # Main loop across comparison columns ---------------------------------------
  out_list <- vector("list", length(col_y))

  for (i in seq_along(col_y)) {

    cmp <- col_y[i]

    # Drop rows with missing pairs
    dt_sub <- dt[!is.na(get(col_x)) & !is.na(get(cmp))]

    if (nrow(dt_sub) == 0L) {
      out_list[[i]] <- NULL
      next
    }

    res <- dt_sub[
      ,
      {
        x <- get(col_x)
        y <- get(cmp)

        n <- length(x)

        # Means, variances, SDs ----------------------------------------------
        mean_x <- mean(x)
        mean_y <- mean(y)
        var_x  <- stats::var(x)
        var_y  <- stats::var(y)
        sd_x   <- sqrt(var_x)
        sd_y   <- sqrt(var_y)

        # Coefficients of variation ------------------------------------------
        cv_x <- if (is.finite(mean_x) && mean_x != 0) sd_x / mean_x else NA_real_
        cv_y <- if (is.finite(mean_y) && mean_y != 0) sd_y / mean_y else NA_real_
        cv_ratio <- if (is.finite(cv_y) && cv_y != 0) cv_x / cv_y else NA_real_

        # Bias & scale metrics -----------------------------------------------
        diff_xy <- x - y
        abs_diff <- abs(diff_xy)
        mean_bias <- mean(diff_xy)
        mae <- mean(abs_diff)
        rmse <- sqrt(mean(diff_xy^2))

        denom_smape <- abs(x) + abs(y)
        smape <- if (all(denom_smape == 0)) {
          NA_real_
        } else {
          mean(2 * abs_diff[denom_smape > 0] / denom_smape[denom_smape > 0])
        }

        ratio_means <- if (is.finite(mean_y) && mean_y != 0) {
          mean_x / mean_y
        } else NA_real_

        # Mean percentage difference -----------------------------------------
        mpd <- if (is.finite(mean_y) && mean_y != 0) {
          (((mean_x - mean_y) / mean_y) - 1) * 100
        } else NA_real_

        # MSD and variance ratio ---------------------------------------------
        pooled_sd <- sqrt((sd_x^2 + sd_y^2) / 2)
        msd <- if (is.finite(pooled_sd) && pooled_sd > 0) {
          (mean_x - mean_y) / pooled_sd
        } else NA_real_

        var_ratio <- if (is.finite(var_y) && var_y > 0) var_x / var_y else NA_real_

        # KS statistic + p-value ---------------------------------------------
        ks_obj <- tryCatch(
          stats::ks.test(x, y),
          error = function(e) NULL
        )
        ks_stat <- if (is.null(ks_obj)) NA_real_ else ks_obj$statistic[[1]]
        pvalue_ks <- if (is.null(ks_obj)) NA_real_ else ks_obj$p.value

        # Correlations and concordance ---------------------------------------
        pearson_cor  <- safe_cor(x, y, method = "pearson")
        spearman_cor <- safe_cor(x, y, method = "spearman")
        kendall_cor  <- safe_cor(x, y, method = "kendall")
        ccc          <- ccc_fun(x, y)

        # Distribution distances ---------------------------------------------
        emd       <- emd_fun(x, y)
        hellinger <- hellinger_fun(x, y)

        # Quantiles and differences ------------------------------------------
        probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
        qx <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
        qy <- stats::quantile(y, probs = probs, na.rm = TRUE, names = FALSE)

        q10_x <- qx[1]; q25_x <- qx[2]; q50_x <- qx[3]; q75_x <- qx[4]; q90_x <- qx[5]
        q10_y <- qy[1]; q25_y <- qy[2]; q50_y <- qy[3]; q75_y <- qy[4]; q90_y <- qy[5]

        q10_diff <- q10_x - q10_y
        q25_diff <- q25_x - q25_y
        q50_diff <- q50_x - q50_y
        q75_diff <- q75_x - q75_y
        q90_diff <- q90_x - q90_y

        # Zero-mass / structural zeros ---------------------------------------
        zero_share_x <- mean(x == 0)
        zero_share_y <- mean(y == 0)
        zero_share_diff <- zero_share_x - zero_share_y

        pvalue_zero_mcnemar <- NA_real_
        if (n >= 2L) {
          tab <- table(
            factor(x == 0, levels = c(FALSE, TRUE)),
            factor(y == 0, levels = c(FALSE, TRUE))
          )
          # Need at least some discordant pairs for McNemar
          if (sum(tab) == n && any(tab[c(2, 3)] > 0)) {
            mcn_obj <- tryCatch(
              stats::mcnemar.test(tab),
              error = function(e) NULL
            )
            if (!is.null(mcn_obj)) {
              pvalue_zero_mcnemar <- mcn_obj$p.value
            }
          }
        }

        # Bounded-scale / logit-space metrics --------------------------------
        mean_logit_x     <- NA_real_
        mean_logit_y     <- NA_real_
        mean_logit_diff  <- NA_real_
        rmse_logit       <- NA_real_
        pvalue_mean_logit <- NA_real_

        # Compute only if all x,y are in [0, 1]
        if (n >= 2L) {
          rng_x <- range(x, na.rm = TRUE)
          rng_y <- range(y, na.rm = TRUE)
          bounded <- is.finite(rng_x[1]) && is.finite(rng_x[2]) &&
            is.finite(rng_y[1]) && is.finite(rng_y[2]) &&
            rng_x[1] >= 0 && rng_x[2] <= 1 &&
            rng_y[1] >= 0 && rng_y[2] <= 1

          if (bounded) {
            eps <- 1e-6
            px <- pmin(pmax(x, eps), 1 - eps)
            py <- pmin(pmax(y, eps), 1 - eps)
            lx <- stats::qlogis(px)
            ly <- stats::qlogis(py)
            diff_logit <- lx - ly

            mean_logit_x    <- mean(lx)
            mean_logit_y    <- mean(ly)
            mean_logit_diff <- mean(diff_logit)
            rmse_logit      <- sqrt(mean(diff_logit^2))

            logit_test <- tryCatch(
              stats::t.test(lx, ly, paired = TRUE),
              error = function(e) NULL
            )
            if (!is.null(logit_test)) {
              pvalue_mean_logit <- logit_test$p.value
            }
          }
        }

        # Hypothesis tests & p-values (means/variances) ----------------------
        # Means: paired t-test H0: mean(x - y) = 0
        if (n >= 2L) {
          mean_test <- tryCatch(
            stats::t.test(x, y, paired = TRUE),
            error = function(e) NULL
          )
          pvalue_mean <- if (is.null(mean_test)) NA_real_ else mean_test$p.value
        } else {
          pvalue_mean <- NA_real_
        }

        # Variance F-test: var.test(x, y)
        if (n >= 2L) {
          var_test <- tryCatch(
            stats::var.test(x, y),
            error = function(e) NULL
          )
          pvalue_var <- if (is.null(var_test)) NA_real_ else var_test$p.value
        } else {
          pvalue_var <- NA_real_
        }

        # Levene / Brown-Forsythe / Kruskal-Wallis (two groups: x vs y) -----
        pvalue_levene <- NA_real_
        pvalue_brown_forsythe <- NA_real_
        pvalue_kruskal_wallis <- NA_real_

        if (n >= 2L) {
          z <- c(x, y)
          g <- factor(rep(c("x", "y"), times = c(length(x), length(y))))

          # Levene: deviations from group means
          mu <- tapply(z, g, mean)
          w_mean <- abs(z - mu[g])
          lev_fit <- tryCatch(
            stats::lm(w_mean ~ g),
            error = function(e) NULL
          )
          if (!is.null(lev_fit)) {
            lev_aov <- stats::anova(lev_fit)
            pvalue_levene <- lev_aov[["Pr(>F)"]][1L]
          }

          # Brown-Forsythe: deviations from group medians
          med <- tapply(z, g, stats::median)
          w_med <- abs(z - med[g])
          bf_fit <- tryCatch(
            stats::lm(w_med ~ g),
            error = function(e) NULL
          )
          if (!is.null(bf_fit)) {
            bf_aov <- stats::anova(bf_fit)
            pvalue_brown_forsythe <- bf_aov[["Pr(>F)"]][1L]
          }

          # Kruskal-Wallis
          kw_obj <- tryCatch(
            stats::kruskal.test(z, g),
            error = function(e) NULL
          )
          if (!is.null(kw_obj)) {
            pvalue_kruskal_wallis <- kw_obj$p.value
          }
        }

        # Correlation p-values -----------------------------------------------
        pvalue_pearson_cor  <- NA_real_
        pvalue_spearman_cor <- NA_real_
        pvalue_kendall_cor  <- NA_real_

        if (n >= 3L) {
          # Pearson
          pearson_test <- tryCatch(
            stats::cor.test(x, y, method = "pearson"),
            error = function(e) NULL
          )
          if (!is.null(pearson_test)) {
            pvalue_pearson_cor <- pearson_test$p.value
          }

          # Spearman
          spearman_test <- tryCatch(
            stats::cor.test(x, y, method = "spearman", exact = FALSE),
            error = function(e) NULL
          )
          if (!is.null(spearman_test)) {
            pvalue_spearman_cor <- spearman_test$p.value
          }

          # Kendall
          kendall_test <- tryCatch(
            stats::cor.test(x, y, method = "kendall", exact = FALSE),
            error = function(e) NULL
          )
          if (!is.null(kendall_test)) {
            pvalue_kendall_cor <- kendall_test$p.value
          }
        }

        list(
          y_level      = cmp,
          n            = n,
          mean_x       = mean_x,
          mean_y       = mean_y,
          var_x        = var_x,
          var_y        = var_y,
          sd_x         = sd_x,
          sd_y         = sd_y,
          cv_x         = cv_x,
          cv_y         = cv_y,
          cv_ratio     = cv_ratio,
          mean_bias    = mean_bias,
          mae          = mae,
          rmse         = rmse,
          smape        = smape,
          ratio_means  = ratio_means,
          mpd          = mpd,
          msd          = msd,
          var_ratio    = var_ratio,
          ks_stat      = ks_stat,
          emd          = emd,
          hellinger    = hellinger,
          pvalue_mean           = pvalue_mean,
          pvalue_ks             = pvalue_ks,
          pvalue_var            = pvalue_var,
          pvalue_levene         = pvalue_levene,
          pvalue_brown_forsythe = pvalue_brown_forsythe,
          pvalue_kruskal_wallis = pvalue_kruskal_wallis,
          zero_share_x          = zero_share_x,
          zero_share_y          = zero_share_y,
          zero_share_diff       = zero_share_diff,
          pvalue_zero_mcnemar   = pvalue_zero_mcnemar,
          mean_logit_x          = mean_logit_x,
          mean_logit_y          = mean_logit_y,
          mean_logit_diff       = mean_logit_diff,
          rmse_logit            = rmse_logit,
          pvalue_mean_logit     = pvalue_mean_logit,
          pearson_cor           = pearson_cor,
          spearman_cor          = spearman_cor,
          kendall_cor           = kendall_cor,
          ccc                   = ccc,
          pvalue_pearson_cor    = pvalue_pearson_cor,
          pvalue_spearman_cor   = pvalue_spearman_cor,
          pvalue_kendall_cor    = pvalue_kendall_cor,
          q10_x        = q10_x,
          q25_x        = q25_x,
          q50_x        = q50_x,
          q75_x        = q75_x,
          q90_x        = q90_x,
          q10_y        = q10_y,
          q25_y        = q25_y,
          q50_y        = q50_y,
          q75_y        = q75_y,
          q90_y        = q90_y,
          q10_diff     = q10_diff,
          q25_diff     = q25_diff,
          q50_diff     = q50_diff,
          q75_diff     = q75_diff,
          q90_diff     = q90_diff
        )
      },
      by = by
    ]

    out_list[[i]] <- res
  }

  data.table::rbindlist(out_list, use.names = TRUE, fill = TRUE)
}

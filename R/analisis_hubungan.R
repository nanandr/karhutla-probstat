# R/analisis_hubungan.R

analisis_hubungan_lengkap <- function(df, lag_dipilih = 2, max_lag = 7) {
  # 1. Hitung korelasi Spearman untuk lag 0..max_lag
  daftar_lag <- lapply(0:max_lag, function(l) {
    x_lag <- dplyr::lag(df$api_nilai, n = l)
    valid <- complete.cases(x_lag, df$pm25)
    x_v <- x_lag[valid]
    y_v <- df$pm25[valid]
    n <- length(x_v)

    if (n < 5 || sd(x_v) == 0 || sd(y_v) == 0) {
      return(tibble::tibble(
        lag = l, n = n, rho = NA_real_, p = NA_real_,
        ci_bawah = NA_real_, ci_atas = NA_real_, signifikan = FALSE
      ))
    }

    uji <- suppressWarnings(stats::cor.test(x_v, y_v, method = "spearman", exact = FALSE))
    rho <- as.numeric(uji$estimate)
    se <- 1 / sqrt(n - 3)
    z <- atanh(pmin(pmax(rho, -0.999), 0.999))

    tibble::tibble(
      lag = l,
      n = n,
      rho = rho,
      p = uji$p.value,
      ci_bawah = tanh(z - 1.96 * se),
      ci_atas = tanh(z + 1.96 * se),
      signifikan = uji$p.value < 0.05
    )
  })
  df_lag <- dplyr::bind_rows(daftar_lag)

  # 2. Hitung korelasi bivariat & parsial pada lag_dipilih
  df_sub <- dplyr::mutate(df, x_lag = dplyr::lag(api_nilai, n = lag_dipilih))
  df_sub <- dplyr::select(df_sub, pm25, x_lag, hujan_mm, angin_maks)
  df_sub <- stats::na.omit(df_sub)

  if (nrow(df_sub) < 6 || sd(df_sub$x_lag) == 0 || sd(df_sub$pm25) == 0) {
    parsial <- list(
      lag = lag_dipilih, n = nrow(df_sub),
      rho_bivariat = NA_real_, p_bivariat = NA_real_,
      rho_parsial = NA_real_, p_parsial = NA_real_
    )
  } else {
    uji_biv <- suppressWarnings(stats::cor.test(df_sub$x_lag, df_sub$pm25, method = "spearman", exact = FALSE))
    res_y <- stats::residuals(stats::lm(rank(df_sub$pm25) ~ rank(df_sub$hujan_mm) + rank(df_sub$angin_maks)))
    res_x <- stats::residuals(stats::lm(rank(df_sub$x_lag) ~ rank(df_sub$hujan_mm) + rank(df_sub$angin_maks)))
    uji_par <- stats::cor.test(res_y, res_x, method = "pearson")

    rho_p <- as.numeric(uji_par$estimate)
    df_res <- nrow(df_sub) - 4
    t_stat <- rho_p * sqrt(df_res / (1 - rho_p^2))
    p_val <- 2 * stats::pt(-abs(t_stat), df = df_res)

    parsial <- list(
      lag = lag_dipilih,
      n = nrow(df_sub),
      rho_bivariat = as.numeric(uji_biv$estimate),
      p_bivariat = uji_biv$p.value,
      rho_parsial = rho_p,
      p_parsial = p_val
    )
  }

  list(df_lag = df_lag, parsial = parsial)
}
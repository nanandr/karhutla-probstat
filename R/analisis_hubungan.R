# R/analisis_hubungan.R

hitung_korelasi_lag <- function(df, max_lag = 7) {
  lapply(0:max_lag, function(l) {
    x_lag <- dplyr::lag(df$api_nilai, n = l)
    valid <- complete.cases(x_lag, df$pm25)
    x_v <- x_lag[valid]
    y_v <- df$pm25[valid]
    n <- length(x_v)
    
    if (n < 5 || sd(x_v) == 0 || sd(y_v) == 0) {
      return(tibble(lag = l, rho = NA_real_, p = NA_real_, ci_bawah = NA_real_, ci_atas = NA_real_))
    }
    
    uji <- suppressWarnings(cor.test(x_v, y_v, method = "spearman", exact = FALSE))
    rho <- as.numeric(uji$estimate)
    
    # Aproksimasi 95% CI untuk Spearman rho (transformasi Fisher z)
    se <- 1 / sqrt(n - 3)
    z <- atanh(pmin(pmax(rho, -0.999), 0.999))
    ci_bawah <- tanh(z - 1.96 * se)
    ci_atas  <- tanh(z + 1.96 * se)
    
    tibble(
      lag = l,
      rho = rho,
      p = uji$p.value,
      ci_bawah = ci_bawah,
      ci_atas = ci_atas
    )
  }) |> bind_rows()
}

hitung_korelasi_parsial <- function(df, lag_n) {
  df_sub <- df |>
    mutate(x_lag = dplyr::lag(api_nilai, n = lag_n)) |>
    select(pm25, x_lag, hujan_mm, angin_maks) |>
    na.omit()
  
  if (nrow(df_sub) < 6) {
    return(list(lag = lag_n, rho_parsial = NA_real_, p_parsial = NA_real_))
  }
  
  # Korelasi parsial Spearman = korelasi Pearson dari residual regresi berbasis rank
  r_y <- rank(df_sub$pm25)
  r_x <- rank(df_sub$x_lag)
  r_z1 <- rank(df_sub$hujan_mm)
  r_z2 <- rank(df_sub$angin_maks)
  
  res_y <- residuals(lm(r_y ~ r_z1 + r_z2))
  res_x <- residuals(lm(r_x ~ r_z1 + r_z2))
  
  uji <- cor.test(res_y, res_x, method = "pearson")
  
  # Koreksi derajat bebas (df = n - 2 - jumlah_kovariat)
  df_res <- nrow(df_sub) - 4
  t_stat <- as.numeric(uji$estimate) * sqrt(df_res / (1 - as.numeric(uji$estimate)^2))
  p_val <- 2 * pt(-abs(t_stat), df = df_res)
  
  list(
    lag = lag_n,
    rho_parsial = as.numeric(uji$estimate),
    p_parsial = p_val
  )
}
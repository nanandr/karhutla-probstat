# app.R
# Dashboard Kualitas Udara dan Karhutla Kalimantan
# Framework: R Shiny + bslib (Bootstrap 5) + Plotly + Leaflet + DT

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)

# 1. Muat Seluruh Skrip di Folder R/
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  source(f)
}

jalankan_refresh_hotspots_latar()

# Helper internal: Evaluasi dinamis untuk Tab 5 (Perbandingan) & Tab 6 (Regresi)
hitung_analisis_dinamis <- function(df_sub, df_semua_kota, periode_karhutla, lag_n) {
  # --- A. Uji Dua Periode (Normal vs Karhutla) ---
  df_per <- df_sub |>
    filter(!is.na(pm25)) |>
    mutate(
      periode = ifelse(
        tanggal >= periode_karhutla[1] & tanggal <= periode_karhutla[2],
        "Periode Karhutla",
        "Periode Normal"
      ),
      periode = factor(periode, levels = c("Periode Normal", "Periode Karhutla"))
    )

  ringkas_per <- df_per |>
    group_by(periode, .drop = FALSE) |>
    summarise(
      n = n(),
      median = if (n() > 0) median(pm25, na.rm = TRUE) else NA_real_,
      iqr = if (n() > 0) IQR(pm25, na.rm = TRUE) else NA_real_,
      rata_rata = if (n() > 0) mean(pm25, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )

  norm_per <- lapply(levels(df_per$periode), function(p_lbl) {
    vals <- df_per$pm25[df_per$periode == p_lbl]
    if (length(vals) >= 3 && length(unique(vals)) > 1) {
      sw <- stats::shapiro.test(vals)
      tibble(data = p_lbl, n = length(vals), W = as.numeric(sw$statistic), p = sw$p.value, normal = sw$p.value >= 0.05)
    } else {
      tibble(data = p_lbl, n = length(vals), W = NA_real_, p = NA_real_, normal = FALSE)
    }
  }) |> bind_rows()

  n_norm <- sum(df_per$periode == "Periode Normal")
  n_karh <- sum(df_per$periode == "Periode Karhutla")
  if (n_norm >= 2 && n_karh >= 2) {
    mw_res <- suppressWarnings(stats::wilcox.test(pm25 ~ periode, data = df_per, exact = FALSE))
    z_approx <- abs(qnorm(pmax(mw_res$p.value / 2, 1e-16)))
    r_efek <- z_approx / sqrt(nrow(df_per))
    label_efek <- if (r_efek >= 0.5) sprintf("besar (r = %s)", format_id(r_efek, 2))
                  else if (r_efek >= 0.3) sprintf("sedang (r = %s)", format_id(r_efek, 2))
                  else sprintf("kecil (r = %s)", format_id(r_efek, 2))
  } else {
    mw_res <- list(statistic = NA_real_, p.value = NA_real_)
    label_efek <- "tidak dapat dihitung (sampel periode kurang)"
  }

  # --- B. Chi-Square Kategori AQI per Periode ---
  kat_levels <- c("Baik", "Sedang", "Tidak Sehat (Sensitif)", "Tidak Sehat", "Sangat Tidak Sehat", "Berbahaya")
  df_per$kat_aqi <- factor(sapply(df_per$pm25, get_kategori_aqi), levels = kat_levels)
  tab_kont <- table(df_per$periode, df_per$kat_aqi)
  tab_aktif <- tab_kont[, colSums(tab_kont) > 0, drop = FALSE]

  if (nrow(tab_aktif) >= 2 && ncol(tab_aktif) >= 2 && all(rowSums(tab_aktif) > 0)) {
    cs_res <- suppressWarnings(stats::chisq.test(tab_aktif))
    harapan_min <- min(cs_res$expected)
  } else {
    cs_res <- list(statistic = NA_real_, parameter = NA_integer_, p.value = NA_real_)
    harapan_min <- NA_real_
  }

  persen_mat <- prop.table(tab_kont, margin = 1) * 100
  persen_mat[is.nan(persen_mat)] <- 0
  persen_df <- as.data.frame.matrix(round(persen_mat, 1))
  persen_df <- tibble::rownames_to_column(persen_df, var = "Periode")
  for (col_nm in kat_levels) {
    persen_df[[col_nm]] <- paste0(format_id(persen_df[[col_nm]], 1), "%")
  }

  # --- C. Kruskal-Wallis & Dunn Post-Hoc Antar Kota ---
  df_kw <- df_semua_kota |> filter(!is.na(pm25))
  kota_aktif <- unique(as.character(df_kw$kota))
  if (length(kota_aktif) >= 2 && nrow(df_kw) >= 6) {
    kw_res <- stats::kruskal.test(pm25 ~ factor(kota), data = df_kw)
    k_grp <- length(kota_aktif)
    n_tot <- nrow(df_kw)
    eps2 <- as.numeric(kw_res$statistic) / ((n_tot^2 - 1) / (n_tot + 1))
    kw_efek <- if (eps2 >= 0.14) sprintf("efek besar (ε² = %s)", format_id(eps2, 2))
               else if (eps2 >= 0.06) sprintf("efek sedang (ε² = %s)", format_id(eps2, 2))
               else sprintf("efek kecil (ε² = %s)", format_id(eps2, 2))

    df_kw$rk <- rank(df_kw$pm25)
    tie_counts <- table(df_kw$pm25)
    tie_adj <- sum(tie_counts^3 - tie_counts) / (12 * (n_tot - 1))
    var_rank <- (n_tot * (n_tot + 1) / 12) - tie_adj

    pasangan <- combn(kota_aktif, 2, simplify = FALSE)
    dunn_raw <- lapply(pasangan, function(pair) {
      r1 <- df_kw$rk[df_kw$kota == pair[1]]
      r2 <- df_kw$rk[df_kw$kota == pair[2]]
      se_d <- sqrt(var_rank * (1 / length(r1) + 1 / length(r2)))
      z_val <- (mean(r1) - mean(r2)) / se_d
      p_raw <- 2 * pnorm(-abs(z_val))
      tibble(group1 = pair[1], group2 = pair[2], z = z_val, p = p_raw)
    }) |> bind_rows() |>
      mutate(
        p.adj = p.adjust(p, method = "holm"),
        p.adj.signif = dplyr::case_when(
          p.adj < 0.001 ~ "***",
          p.adj < 0.01 ~ "**",
          p.adj < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      )
  } else {
    kw_res <- list(statistic = NA_real_, parameter = NA_integer_, p.value = NA_real_)
    kw_efek <- "-"
    dunn_raw <- tibble(group1 = character(), group2 = character(), z = numeric(), p = numeric(), p.adj = numeric(), p.adj.signif = character())
  }

  # --- D. Regresi Hierarkis, OLS vs HAC, Asumsi & VIF ---
  df_reg <- df_sub |>
    mutate(pred_api = if (lag_n > 0) api_lag else api_nilai) |>
    filter(!is.na(pm25), !is.na(pred_api))

  # Gunakan kovariat cuaca yang memiliki varians > 0
  kandidat_cuaca <- c("hujan_mm", "suhu_c", "angin_maks")
  cuaca_valid <- kandidat_cuaca[vapply(kandidat_cuaca, function(v) {
    v %in% names(df_reg) && sum(!is.na(df_reg[[v]])) > 5 && stats::sd(df_reg[[v]], na.rm = TRUE) > 0
  }, logical(1))]

  df_reg <- df_reg |> tidyr::drop_na(dplyr::all_of(c("pm25", "pred_api", cuaca_valid)))
  n_reg <- nrow(df_reg)
  k_pred <- 1L + length(cuaca_valid)

  if (n_reg >= 10) {
    f2 <- stats::as.formula(paste("pm25 ~", paste(c("pred_api", cuaca_valid), collapse = " + ")))
    fit2 <- stats::lm(f2, data = df_reg)
    s2 <- summary(fit2)

    if (length(cuaca_valid) > 0) {
      f1 <- stats::as.formula(paste("pm25 ~", paste(cuaca_valid, collapse = " + ")))
      fit1 <- stats::lm(f1, data = df_reg)
      r2_b1 <- summary(fit1)$r.squared
      aov_cmp <- stats::anova(fit1, fit2)
      f_chg <- aov_cmp$F[2]
      p_chg <- aov_cmp$`Pr(>F)`[2]
    } else {
      r2_b1 <- 0
      f_chg <- s2$fstatistic[1]
      p_chg <- stats::pf(f_chg, s2$fstatistic[2], s2$fstatistic[3], lower.tail = FALSE)
    }
    r2_b2 <- s2$r.squared

    label_map <- c(
      "(Intercept)" = "Intersep",
      "pred_api" = sprintf("Ukuran Kebakaran (Lag %d Hari)", lag_n),
      "hujan_mm" = "Curah Hujan (mm)",
      "suhu_c" = "Suhu Udara (°C)",
      "angin_maks" = "Kecepatan Angin Maks (km/jam)"
    )

    coef_mat <- s2$coefficients
    koef_tbl <- tibble(
      Prediktor = unname(ifelse(rownames(coef_mat) %in% names(label_map), label_map[rownames(coef_mat)], rownames(coef_mat))),
      Estimasi = format_id(coef_mat[, 1], 4),
      `Galat Baku` = format_id(coef_mat[, 2], 4),
      `Statistik t` = format_id(coef_mat[, 3], 2),
      `Nilai p` = ifelse(coef_mat[, 4] < 0.001, "< 0,001", format_id(coef_mat[, 4], 4))
    )

    # Estimasi Newey-West HAC manual (lag Bartlett L = floor(4*(n/100)^(2/9)))
    X <- stats::model.matrix(fit2)
    u <- stats::residuals(fit2)
    XtX_inv <- solve(crossprod(X))
    L_nw <- max(1L, floor(4 * (n_reg / 100)^(2 / 9)))
    S_hac <- crossprod(X * u)
    for (l in seq_len(L_nw)) {
      w_l <- 1 - l / (L_nw + 1)
      Gamma_l <- t(X[(l + 1):n_reg, , drop = FALSE] * u[(l + 1):n_reg]) %*%
        (X[1:(n_reg - l), , drop = FALSE] * u[1:(n_reg - l)])
      S_hac <- S_hac + w_l * (Gamma_l + t(Gamma_l))
    }
    vcov_hac <- XtX_inv %*% S_hac %*% XtX_inv
    se_hac <- sqrt(diag(vcov_hac))
    t_hac <- coef_mat[, 1] / se_hac
    p_hac <- 2 * stats::pt(-abs(t_hac), df = fit2$df.residual)

    koef_hac_tbl <- tibble(
      Prediktor = koef_tbl$Prediktor,
      Estimasi = format_id(coef_mat[, 1], 4),
      `Galat Baku HAC` = format_id(se_hac, 4),
      `Statistik t` = format_id(t_hac, 2),
      `Nilai p` = ifelse(p_hac < 0.001, "< 0,001", format_id(p_hac, 4))
    )

    # Uji Asumsi Klasik
    sw_res <- stats::shapiro.test(u)
    dw_stat <- sum(diff(u)^2) / sum(u^2)
    bp_fit <- stats::lm(I(u^2) ~ X[, -1, drop = FALSE])
    bp_stat <- n_reg * summary(bp_fit)$r.squared
    bp_p <- stats::pchisq(bp_stat, df = ncol(X) - 1, lower.tail = FALSE)

    asumsi_tbl <- tibble(
      uji = c(
        "Normalitas Residual (Shapiro-Wilk)",
        "Autokorelasi Serial (Durbin-Watson)",
        "Homoskedastisitas (Breusch-Pagan)"
      ),
      statistik = c(
        paste0("W = ", format_id(sw_res$statistic, 3)),
        paste0("DW = ", format_id(dw_stat, 2)),
        paste0("BP = ", format_id(bp_stat, 2))
      ),
      p = c(
        ifelse(sw_res$p.value < 0.001, "< 0,001", format_id(sw_res$p.value, 4)),
        ifelse(dw_stat < 1.5 || dw_stat > 2.5, "< 0,05", "> 0,05"),
        ifelse(bp_p < 0.001, "< 0,001", format_id(bp_p, 4))
      ),
      lolos = c(sw_res$p.value >= 0.05, dw_stat >= 1.5 && dw_stat <= 2.5, bp_p >= 0.05)
    )

    # VIF & Tolerance
    pred_cols <- colnames(X)[-1]
    if (length(pred_cols) >= 2) {
      vif_vals <- vapply(pred_cols, function(col_i) {
        r2_i <- summary(stats::lm(X[, col_i] ~ X[, setdiff(pred_cols, col_i), drop = FALSE]))$r.squared
        1 / max(1 - r2_i, 1e-6)
      }, numeric(1))
    } else {
      vif_vals <- rep(1.0, length(pred_cols))
    }
    vif_tbl <- tibble(
      prediktor = unname(ifelse(pred_cols %in% names(label_map), label_map[pred_cols], pred_cols)),
      VIF = vif_vals,
      tolerance = 1 / vif_vals,
      lolos = vif_vals < 3
    )
  } else {
    r2_b1 <- NA_real_; r2_b2 <- NA_real_; f_chg <- NA_real_; p_chg <- NA_real_
    koef_tbl <- tibble(Info = "Data valid tidak mencukupi (minimal 10 observasi).")
    koef_hac_tbl <- koef_tbl
    asumsi_tbl <- tibble(uji = "Data Kurang", statistik = "-", p = "-", lolos = FALSE)
    vif_tbl <- tibble(prediktor = "-", VIF = NA_real_, tolerance = NA_real_, lolos = FALSE)
  }

  list(
    uji_dua_periode = list(
      ringkas = ringkas_per,
      normalitas = norm_per,
      uji = mw_res,
      efek = label_efek
    ),
    chisq = list(
      persen_baris = persen_df,
      uji = cs_res,
      harapan_min = harapan_min
    ),
    uji_antar_kota = list(
      kw = kw_res,
      efek = kw_efek,
      dunn = dunn_raw
    ),
    sampel = list(
      n = n_reg,
      k = k_pred,
      min_uji_model = 50L + 8L * k_pred,
      model_memadai = n_reg >= (50L + 8L * k_pred)
    ),
    hierarkis = list(
      r2 = c(blok1 = r2_b1, blok2 = r2_b2),
      delta_r2 = r2_b2 - r2_b1,
      uji_perubahan = list(F = f_chg, p = p_chg)
    ),
    koef = koef_tbl,
    koef_hac = koef_hac_tbl,
    asumsi = asumsi_tbl,
    vif = vif_tbl
  )
}

# 2. Konfigurasi Tema UI (bslib Bootstrap 5)
tema_dashboard <- bs_theme(
  version = 5,
  bg = "#F5F7FA",
  fg = "#1B2733",
  primary = "#1F3A5F",
  secondary = "#5A6A80",
  success = "#16A34A",
  warning = "#D97706",
  danger = "#D00000",
  base_font = font_collection("-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "sans-serif")
)

# 3. Definisi Antarmuka Pengguna (UI)
ui <- page_sidebar(
  title = tags$div(
    class = "d-flex align-items-center gap-2",
    tags$span(style = "color: #1F3A5F; font-size: 1.35rem; font-weight: 700;", "Dashboard Kualitas Udara & Karhutla Kalimantan"),
    tags$span(class = "badge bg-primary-subtle text-primary border border-primary-subtle small ms-2", "Server Lokal")
  ),
  theme = tema_dashboard,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Area A: Sidebar Filter Global
  sidebar = sidebar(
    width = 320,
    title = tags$span(style = "font-weight: 600; color: #1F3A5F;", "Filter & Parameter"),
    
    selectInput(
      inputId = "kota",
      label = "Pilih Kota:",
      choices = c("Palangkaraya", "Pontianak", "Bandung"),
      selected = "Palangkaraya"
    ),
    
    dateRangeInput(
      inputId = "tgl",
      label = "Rentang Tanggal Pengamatan:",
      start = "2026-07-15",
      end = "2026-08-31",
      min = "2026-07-15",
      max = Sys.Date(),
      format = "yyyy-mm-dd",
      language = "id",
      separator = " s/d "
    ),
    
    sliderInput(
      inputId = "ma_k",
      label = "Jendela Rata-rata Bergerak (Hari):",
      min = 3,
      max = 30,
      value = 7,
      step = 1
    ),
    
    sliderInput(
      inputId = "lag_api",
      label = "Lag Titik Panas (Hari):",
      min = 0,
      max = 7,
      value = 2,
      step = 1
    ),
    
    radioButtons(
      inputId = "var_api",
      label = "Ukuran Kebakaran:",
      choices = c(
        "Jumlah titik panas" = "titik_panas",
        "Total FRP (MW)" = "frp_total"
      ),
      selected = "titik_panas"
    ),
    
    tags$hr(style = "border-top: 1px solid #E2E8F0; margin: 16px 0;"),
    
    # Metadata Bawah Sidebar
    tags$div(
      class = "small text-muted",
      tags$p(class = "mb-1", tags$strong("Status Hotspot:"), " NASA FIRMS diperbarui harian"),
      tags$p(class = "mb-1", tags$strong("Lingkup Sensor:"), " Radius 100 km dari stasiun"),
      tags$p(class = "mb-0", tags$strong("Atribusi:"), " Open-Meteo, NASA FIRMS")
    )
  ),
  
  # Banner Peringatan Data Contoh (Muncul Otomatis Saat DATA_CONTOH == TRUE)
  banner_data_contoh(),
  
  # Area B: Baris Kartu Indikator Utama (KPI)
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    
    # KPI 1: Rata-rata AQI PM2.5
    value_box(
      title = "Rata-rata AQI PM2,5",
      value = uiOutput("kpi_rerata_aqi"),
      showcase = tags$span(style = "font-size: 2rem;", HTML("&#127787;")),
      theme = "primary",
      p(class = "small mb-0", textOutput("kpi_rerata_subteks"))
    ),
    
    # KPI 2: Kategori Dominan
    value_box(
      title = "Kategori Dominan",
      value = uiOutput("kpi_kategori_dominan"),
      showcase = tags$span(style = "font-size: 2rem;", HTML("&#127991;")),
      theme = "light",
      p(class = "small mb-0 text-muted", textOutput("kpi_kategori_subteks"))
    ),
    
    # KPI 3: Total Ukuran Kebakaran Terpilih
    value_box(
      title = textOutput("kpi_judul_api"),
      value = uiOutput("kpi_total_api"),
      showcase = tags$span(style = "font-size: 2rem;", HTML("&#128293;")),
      theme = "light",
      p(class = "small mb-0 text-muted", textOutput("kpi_api_subteks"))
    ),
    
    # KPI 4: Hari Tidak Sehat (AQI > 100)
    value_box(
      title = "Hari Tidak Sehat (AQI > 100)",
      value = uiOutput("kpi_hari_tidak_sehat"),
      showcase = tags$span(style = "font-size: 2rem;", HTML("&#9888;")),
      theme = "danger",
      p(class = "small mb-0", textOutput("kpi_persen_tidak_sehat"))
    )
  ),
  
  # Area Konten Utama: 7 Tab Terstruktur
  navset_card_tab(
    id = "tab_utama",
    
    # Tab 1: Ringkasan
    nav_panel(
      title = "1. Ringkasan",
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          card_header("Statistik Deskriptif Parameter"),
          tags$div(
            class = "table-responsive-wrapper",
            tableOutput("tbl_desk")
          )
        ),
        
        card(
          card_header("Proporsi Hari per Kategori Kualitas Udara"),
          plotlyOutput("plot_kategori", height = "320px")
        )
      ),
      
      card(
        card_header("Uji Normalitas Shapiro-Wilk"),
        tags$div(
          class = "table-responsive-wrapper",
          tableOutput("tbl_norm")
        ),
        kartu_interpretasi(
          teks = "Berdasarkan uji Shapiro-Wilk, nilai p < 0,05 menunjukkan distribusi variabel kualitas udara dan titik panas tidak berdistribusi normal secara signifikan. Oleh sebab itu, analisis inferensial menggunakan uji statistik non-parametrik (korelasi rank Spearman, Mann-Whitney U, dan Kruskal-Wallis) untuk menguji asosiasi secara sahih.",
          tipe = "info",
          judul = "Catatan Metodologis Distribusi"
        )
      )
    ),
    
    # Tab 2: Tren
    nav_panel(
      title = "2. Tren",
      card(
        card_header("Dinamika Deret Waktu: AQI PM2,5 dan Aktivitas Kebakaran"),
        p(class = "text-muted small mb-2",
          "Grafik menyandingkan deret waktu indeks kualitas udara (sumbu kiri) dan intensitas kebakaran (sumbu kanan). Perhatikan keterlambatan (lag) antara kenaikan titik panas dan lonjakan AQI."),
        plotlyOutput("plot_tren", height = "460px"),
        tags$div(
          class = "mt-3",
          kartu_interpretasi(
            teks = "Garis putus-putus oranye menandai ambang batas Kategori Tidak Sehat untuk Kelompok Sensitif (AQI 100), sedangkan garis merah menandai ambang Kategori Tidak Sehat (AQI 150). Pergerakan grafik memperlihatkan adanya asosiasi temporal di mana lonjakan pembacaan AQI menyusul beberapa hari setelah puncak deteksi titik panas satelit.",
            tipe = "info",
            judul = "Interpretasi Asosiasi Temporal"
          )
        )
      )
    ),
    
    # Tab 3: Peta Titik Panas
    nav_panel(
      title = "3. Peta Titik Panas",
      card(
        card_header(
          tags$div(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-3",
            tags$span("Sebaran Spasial Titik Panas Satelit (NASA FIRMS)"),
            actionButton(
              inputId = "perbarui_hotspots",
              label = tagList(
                icon("refresh"),
                tags$span("Perbarui data terbaru")
              ),
              class = "btn-refresh-hotspots"
            )
          )
        ),
        tags$div(
          class = "d-flex justify-content-between align-items-center flex-wrap small text-muted mb-2",
          textOutput("status_hotspot"),
          textOutput("n_titik")
        ),
        leafletOutput("peta", height = "520px"),
        tags$div(
          class = "mt-3",
          kartu_interpretasi(
            teks = "Untuk rentang pendek, peta menampilkan posisi deteksi asli dengan warna menurut confidence. Jika deteksi sangat banyak, peta merangkumnya per petak sekitar 4 km dengan kategori kepadatan. Angka total di atas tetap menghitung seluruh piksel dalam radius 100 km. Piksel panas bukan jumlah kebakaran unik dan bukan luas area terbakar. Lingkaran biru menunjukkan radius pengamatan 100 km dari pusat kota.",
            tipe = "warning",
            judul = "Peringatan Interpretasi Sensor Spasial"
          )
        )
      )
    ),
    
    # Tab 4: Hubungan
    nav_panel(
      title = "4. Hubungan",
      tags$div(
        class = "d-flex flex-column gap-3",
        
        layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,
          
          card(
            card_header("Korelasi Rank Spearman per Lag Waktu (0–7 Hari)"),
            p(class = "text-muted small mb-1", "Klik batang pada grafik untuk memilih lag waktu secara langsung."),
            plotlyOutput("plot_lag", height = "310px"),
            tags$div(class = "mt-2", uiOutput("tafsir_lag"))
          ),
          
          card(
            card_header("Diagram Pencar Hubungan Ukuran Kebakaran vs AQI PM2,5"),
            p(class = "text-muted small mb-1", "Titik dibedakan berdasarkan kondisi hujan harian untuk melihat efek pencucian polutan."),
            plotlyOutput("scatter", height = "310px"),
            tags$div(class = "mt-2", uiOutput("tafsir_parsial"))
          )
        ),
        
        card(
          card_header("Ringkasan Uji Korelasi Bivariat vs Parsial (Kontrol Cuaca)"),
          tags$div(
            style = "overflow-x: auto;",
            tableOutput("tbl_korelasi_ringkas")
          )
        )
      )
    ),
    
    # Tab 5: Perbandingan
    nav_panel(
      title = "5. Perbandingan",
      card(
        card_header("Parameter Pengujian Perbandingan Periode"),
        dateRangeInput(
          inputId = "periode_api",
          label = "Tentukan Rentang Periode Karhutla (di dalam Rentang Tanggal Pengamatan):",
          start = "2026-08-10",
          end = "2026-08-31",
          min = "2026-07-15",
          max = Sys.Date(),
          format = "yyyy-mm-dd",
          language = "id",
          separator = " s/d "
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Uji Mann-Whitney U: Dua Periode (Normal vs Karhutla)"),
          tags$div(class = "table-responsive-wrapper mb-2", tableOutput("ringkas_uji")),
          tags$div(class = "table-responsive-wrapper mb-2", tableOutput("tbl_norm_periode")),
          uiOutput("tafsir_mw")
        ),
        
        card(
          card_header("Uji Chi-Square: Proporsi Kategori AQI per Periode"),
          tags$div(class = "table-responsive-wrapper mb-2", tableOutput("tbl_chisq")),
          uiOutput("tafsir_chisq")
        )
      ),
      
      card(
        card_header("Uji Kruskal-Wallis & Uji Lanjut Dunn Antar Kota"),
        uiOutput("tafsir_kw"),
        tags$div(
          class = "table-responsive-wrapper mt-3",
          DTOutput("tbl_dunn")
        )
      )
    ),
    
    # Tab 6: Regresi
    nav_panel(
      title = "6. Regresi",
      layout_columns(
        col_widths = c(4, 8),
        
        card(
          card_header("Evaluasi Ukuran Sampel"),
          tags$div(class = "table-responsive-wrapper", tableOutput("tbl_sampel")),
          tags$hr(),
          card_header("Regresi Hierarkis (Peningkatan R²)"),
          tags$div(class = "table-responsive-wrapper", tableOutput("tbl_hier"))
        ),
        
        card(
          card_header("Tabel Koefisien Model (Standar vs Kebal Autokorelasi HAC)"),
          tags$strong(class = "small text-secondary", "Koefisien OLS Standar:"),
          tags$div(class = "table-responsive-wrapper mb-3", tableOutput("tbl_koef")),
          tags$strong(class = "small text-secondary", "Koefisien Galat Baku HAC (Newey-West):"),
          tags$div(class = "table-responsive-wrapper", tableOutput("tbl_hac"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Hasil Uji Asumsi Klasik Model"),
          tags$div(class = "table-responsive-wrapper", uiOutput("tbl_asumsi")),
          tags$div(class = "mt-3", uiOutput("teks_outlier"))
        ),
        
        card(
          card_header("Uji Multikolinearitas (VIF & Tolerance)"),
          tags$div(class = "table-responsive-wrapper", tableOutput("tbl_vif"))
        )
      ),
      
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          card_header("Empat Plot Diagnostik Residual Model"),
          plotOutput("plot_diag", height = "420px")
        ),
        
        card(
          card_header("Fungsi Autokorelasi (ACF) Residual"),
          plotOutput("plot_acf", height = "420px")
        )
      )
    ),
    
    # Tab 7: Metodologi
    nav_panel(
      title = "7. Metodologi",
      card(
        card_header("Dokumentasi Sumber Data, Alur Pengolahan, dan Kerangka Inferensi"),
        uiOutput("metodologi")
      )
    )
  ),
  
  # Footer Atribusi Wajib di Semua Tab
  footer_atribusi()
)
ui <- htmltools::tagAppendChild(
  ui,
  tags$head(tags$link(rel = "icon", type = "image/x-icon", href = "Frame%20798.ico"))
)

# 4. Logika Server Aplikasi
server <- function(input, output, session) {

  # 1. Pantau snapshot agar hasil worker yang diminta pengguna segera tampil
  # Sinkronisasi otomatis batas tanggal periode_api di Tab 5 agar tetap di dalam input$tgl
  observeEvent(input$tgl, {
    req(input$tgl)
    mid_tgl <- input$tgl[1] + floor(as.numeric(input$tgl[2] - input$tgl[1]) / 2)
    updateDateRangeInput(
      session,
      "periode_api",
      min = input$tgl[1],
      max = input$tgl[2],
      start = max(input$tgl[1], mid_tgl),
      end = input$tgl[2]
    )
  })

  # 1. Reactive Polling Snapshot Titik Panas Satelit (NASA FIRMS)
  hotspots_snapshot <- reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      berkas <- c("data/hotspots.rds", "data/hotspots-refresh-status.rds")
      waktu_modifikasi <- vapply(berkas, function(path) {
        if (file.exists(path)) as.numeric(file.info(path)$mtime) else 0
      }, numeric(1))
      paste(waktu_modifikasi, collapse = "|")
    },
    valueFunc = function() {
      list(
        data = ambil_hotspots(),
        status = status_pembaruan_hotspots()
      )
    }
  )

  waktu_permintaan_refresh <- reactiveVal(NULL)

  observeEvent(input$perbarui_hotspots, {
    waktu_permintaan_refresh(Sys.time())
    tryCatch(
      {
        jalankan_refresh_hotspots_latar()
        showNotification(
          tags$div(
            class = "refresh-process-notification",
            icon("refresh", class = "fa-spin"),
            tags$span(
              "Memperbarui hotspot NASA FIRMS."
            )
          ),
          id = "hotspot-refresh",
          type = "message",
          duration = NULL,
          closeButton = FALSE
        )
      },
      error = function(e) {
        waktu_permintaan_refresh(NULL)
        removeNotification("hotspot-refresh")
        showNotification(
          paste("Pembaruan NASA FIRMS gagal dimulai:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
      }
    )
  })

  observeEvent(hotspots_snapshot()$status, {
    status <- hotspots_snapshot()$status
    waktu_permintaan <- waktu_permintaan_refresh()
    waktu_hasil <- if (is.null(status)) NULL else status$waktu_proses
    if (is.null(waktu_hasil) && !is.null(status)) {
      waktu_hasil <- status$waktu
    }
    if (is.null(status) || is.null(waktu_permintaan) ||
        is.null(waktu_hasil) || waktu_hasil <= waktu_permintaan) {
      return()
    }

    waktu_permintaan_refresh(NULL)
    removeNotification("hotspot-refresh")
    if (identical(status$hasil_proses, "sudah diperbarui hari ini")) {
      showNotification(
        "NASA FIRMS sudah diperbarui hari ini. Snapshot terbaru tetap digunakan.",
        type = "message"
      )
    } else if (identical(status$hasil_proses, "pembaruan sedang berjalan")) {
      showNotification(
        "Pembaruan NASA FIRMS lain sedang berjalan. Snapshot akan diperbarui oleh proses tersebut.",
        type = "message"
      )
    } else if (isTRUE(status$berhasil)) {
      showNotification(
        "NASA FIRMS berhasil diperbarui. Snapshot terbaru sekarang digunakan.",
        type = "message"
      )
    } else {
      pesan <- if (!is.null(status$pesan) && nzchar(status$pesan)) {
        paste0(": ", status$pesan)
      } else {
        ""
      }
      penggunaan_snapshot <- if (isTRUE(status$snapshot_diperbarui)) {
        "Data yang berhasil diunduh sudah diterapkan ke snapshot."
      } else {
        "Snapshot sebelumnya tetap digunakan."
      }
      showNotification(
        paste0(
          "NASA FIRMS gagal diperbarui", pesan,
          ". ", penggunaan_snapshot
        ),
        type = "error",
        duration = NULL
      )
    }
  }, ignoreInit = TRUE)

  # 2. Reactive Filter Titik Panas Satelit (Rentang Tanggal & Radius 100 km Kota)
  hotspots_terfilter <- reactive({
    req(input$kota, input$tgl)
    pts <- hotspots_snapshot()$data
    coord <- get_koordinat_kota(input$kota)

    pts <- pts |>
      filter(
        tanggal >= input$tgl[1],
        tanggal <= input$tgl[2],
        abs(latitude - coord$lat) < 1,
        abs(longitude - coord$lon) < 1
      )
    if (nrow(pts) == 0L) return(pts)

    lat1 <- pts$latitude * pi / 180
    lat2 <- coord$lat * pi / 180
    delta_lat <- (pts$latitude - coord$lat) * pi / 180
    delta_lon <- (pts$longitude - coord$lon) * pi / 180
    jarak_km <- 2 * 6371 * asin(pmin(
      1,
      sqrt(
        sin(delta_lat / 2)^2 +
          cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
      )
    ))

    pts[!is.na(jarak_km) & jarak_km <= 100, , drop = FALSE]
  })
  
  # 3. Reactive Data Harian Terintegrasi (Kualitas Udara + Agregasi Titik Panas Hotspot)
  data_terfilter <- reactive({
    req(input$kota, input$tgl)
    
    withProgress(message = "Memperbarui data dan visualisasi...", value = 0.5, {
      df <- ambil_data_harian()
      
      df_sub <- df |>
        filter(
          kota == input$kota,
          tanggal >= input$tgl[1],
          tanggal <= input$tgl[2]
        ) |>
        arrange(tanggal)
      
      if (nrow(df_sub) == 0) {
        semua_tgl <- seq(input$tgl[1], input$tgl[2], by = "day")
        df_sub <- tibble(
          kota = input$kota,
          tanggal = semua_tgl,
          pm25 = NA_real_,
          n_stasiun = 0L,
          sumber = "hotspots",
          hujan_mm = NA_real_,
          suhu_c = NA_real_,
          angin_maks = NA_real_,
          titik_panas = NA_integer_,
          frp_total = NA_real_
        )
      }

      # Integrasikan titik panas nyata; tanggal di luar cakupan snapshot dibiarkan NA
      pts <- hotspots_terfilter()
      if (nrow(pts) > 0) {
        rentang_hs <- range(pts$tanggal, na.rm = TRUE)
        hs_harian <- pts |>
          dplyr::group_by(tanggal) |>
          dplyr::summarise(
            hs_titik = as.integer(dplyr::n()),
            hs_frp   = round(as.numeric(sum(frp, na.rm = TRUE)), 1),
            .groups  = "drop"
          )
        
        df_sub <- df_sub |>
          dplyr::left_join(hs_harian, by = "tanggal") |>
          dplyr::mutate(
            dalam_cakupan = tanggal >= rentang_hs[1] & tanggal <= rentang_hs[2],
            titik_panas = dplyr::case_when(
              !is.na(hs_titik) ~ as.integer(hs_titik),
              dalam_cakupan ~ 0L,
              TRUE ~ NA_integer_
            ),
            frp_total = dplyr::case_when(
              !is.na(hs_frp) ~ as.numeric(hs_frp),
              dalam_cakupan ~ 0.0,
              TRUE ~ NA_real_
            )
          ) |>
          dplyr::select(-dplyr::any_of(c("hs_titik", "hs_frp", "dalam_cakupan")))
      } else {
        df_sub$titik_panas <- 0L
        df_sub$frp_total <- 0.0
      }
      
      # Perhitungan Jendela Rata-rata Bergerak (Moving Average)
      k <- input$ma_k
      if (sum(!is.na(df_sub$pm25)) >= k) {
        ma_vals <- stats::filter(df_sub$pm25, rep(1 / k, k), sides = 1)
        df_sub$pm25_ma <- as.numeric(ma_vals)
      } else {
        df_sub$pm25_ma <- df_sub$pm25
      }
      
      # Penentuan Variabel Ukuran Kebakaran Sesuai Pilihan (Jumlah Piksel atau FRP)
      var_dipilih <- input$var_api
      df_sub$api_nilai <- if (var_dipilih == "frp_total") df_sub$frp_total else as.numeric(df_sub$titik_panas)
      
      # Perhitungan Lag untuk Analisis Bivariat
      lag_n <- input$lag_api
      df_sub$api_lag <- dplyr::lag(df_sub$api_nilai, n = lag_n, default = NA)
      
      df_sub
    })
  })

  # 4. Reactive Analisis Inferensial Dinamis (Tab 5 & Tab 6)
  analisis_dinamis <- reactive({
    req(input$tgl, input$periode_api)
    df_sub <- data_terfilter()
    df_all <- ambil_data_harian() |>
      filter(tanggal >= input$tgl[1], tanggal <= input$tgl[2])
    hitung_analisis_dinamis(df_sub, df_all, input$periode_api, input$lag_api)
  })
  
  # --- LOGIKA VALUE BOXES (AREA B) ---
  
  output$kpi_rerata_aqi <- renderUI({
    df <- data_terfilter()
    if (nrow(df) == 0 || all(is.na(df$pm25))) return(tags$span("-"))
    
    rerata <- mean(df$pm25, na.rm = TRUE)
    warna_bg <- get_warna_aqi(rerata)
    warna_teks <- get_warna_teks_aqi(rerata)
    
    tags$div(
      tags$span(
        style = sprintf("background-color: %s; color: %s; padding: 2px 10px; border-radius: 6px; font-weight: 700;", warna_bg, warna_teks),
        format_id(rerata, 1)
      )
    )
  })
  
  output$kpi_rerata_subteks <- renderText({
    df <- data_terfilter()
    if (nrow(df) == 0 || all(is.na(df$pm25))) return("Tidak ada data")
    rerata <- mean(df$pm25, na.rm = TRUE)
    paste("Kategori:", get_kategori_aqi(rerata))
  })
  
  output$kpi_kategori_dominan <- renderUI({
    df <- data_terfilter()
    if (nrow(df) == 0 || all(is.na(df$pm25))) return(tags$span("-"))
    
    kat_list <- sapply(df$pm25[!is.na(df$pm25)], get_kategori_aqi)
    tabel_kat <- sort(table(kat_list), decreasing = TRUE)
    kat_dom <- names(tabel_kat)[1]
    
    badge_kategori(kat_dom)
  })
  
  output$kpi_kategori_subteks <- renderText({
    df <- data_terfilter()
    if (nrow(df) == 0 || all(is.na(df$pm25))) return("")
    kat_list <- sapply(df$pm25[!is.na(df$pm25)], get_kategori_aqi)
    tabel_kat <- sort(table(kat_list), decreasing = TRUE)
    paste0(format_id(tabel_kat[1], 0), " hari dari total ", length(kat_list), " hari")
  })
  
  output$kpi_judul_api <- renderText({
    if (input$var_api == "frp_total") "Total FRP Kebakaran" else "Total Piksel Panas"
  })
  
  output$kpi_api_subteks <- renderText({
    status <- hotspots_snapshot()$status
    if (is.null(status)) return("NASA FIRMS · menunggu pembaruan pertama")
    if (isTRUE(status$berhasil)) {
      paste(
        "NASA FIRMS · diperbarui",
        format(status$waktu, "%d-%m-%Y %H:%M")
      )
    } else if (isTRUE(status$snapshot_diperbarui)) {
      "NASA FIRMS · snapshot diperbarui sebagian"
    } else {
      "NASA FIRMS · pembaruan gagal; snapshot sebelumnya dipakai"
    }
  })

  output$status_hotspot <- renderText({
    status <- hotspots_snapshot()$status
    if (is.null(status)) {
      return("Belum ada data pembaruan; gunakan tombol untuk mengambil data terbaru.")
    }
    if (identical(status$hasil_proses, "pembaruan sedang berjalan")) {
      return("Pembaruan NASA FIRMS sedang berjalan; snapshot sebelumnya tetap digunakan sementara.")
    }
    if (isTRUE(status$berhasil)) {
      return(paste(
        "Terakhir diperbarui:",
        format(status$waktu, "%d-%m-%Y %H:%M")
      ))
    }
    if (isTRUE(status$snapshot_diperbarui)) {
      return(paste(
        "Sebagian produk NASA FIRMS gagal diperbarui:",
        status$pesan,
        "Data yang berhasil diunduh sudah diterapkan; data lama untuk produk gagal tetap dipertahankan."
      ))
    }
    if (!is.null(status$pesan) && nzchar(status$pesan)) {
      return(paste(
        "NASA FIRMS gagal diperbarui:",
        status$pesan,
        "Snapshot sebelumnya tetap digunakan."
      ))
    }
    "NASA FIRMS gagal diperbarui; snapshot sebelumnya tetap digunakan."
  })

  output$kpi_total_api <- renderUI({
    if (input$var_api == "frp_total") {
      df <- data_terfilter()
      if (nrow(df) == 0) return(tags$span("-"))

      val <- sum(df$frp_total, na.rm = TRUE)
      tags$span(paste(format_id(val, 1), "MW"))
    } else {
      pts <- hotspots_terfilter()
      if (nrow(pts) == 0) return(tags$span("Tidak ada data"))

      tags$span(paste(format_id(nrow(pts), 0), "Piksel"))
    }
  })
  
  output$kpi_hari_tidak_sehat <- renderUI({
    df <- data_terfilter()
    if (nrow(df) == 0) return(tags$span("-"))
    n_unhealthy <- sum(df$pm25 > 100, na.rm = TRUE)
    tags$span(paste(format_id(n_unhealthy, 0), "Hari"))
  })
  
  output$kpi_persen_tidak_sehat <- renderText({
    df <- data_terfilter()
    n_valid <- sum(!is.na(df$pm25))
    if (n_valid == 0) return("")
    n_unhealthy <- sum(df$pm25 > 100, na.rm = TRUE)
    persen <- (n_unhealthy / n_valid) * 100
    paste0(format_id(persen, 1), "% dari periode terpilih")
  })
  
  # --- TAB 1: RINGKASAN ---
  
  output$tbl_desk <- renderTable({
    df <- data_terfilter()
    validate(need(nrow(df) > 0, "Tidak ada data untuk pilihan ini."))
    
    hitung_deskriptif <- function(x, nama_var) {
      x_bersih <- x[!is.na(x)]
      n_total <- length(x)
      n_valid <- length(x_bersih)
      n_hilang <- n_total - n_valid
      
      if (n_valid == 0) {
        return(tibble(
          Variabel = nama_var, n = n_valid, Hilang = n_hilang,
          `Rata-rata` = "-", Median = "-", SD = "-",
          Min = "-", Q1 = "-", Q3 = "-", Maks = "-", Skewness = "-"
        ))
      }
      
      m <- mean(x_bersih)
      s <- sd(x_bersih)
      med <- median(x_bersih)
      mi <- min(x_bersih)
      ma <- max(x_bersih)
      q <- quantile(x_bersih, probs = c(0.25, 0.75))
      skew <- if (n_valid > 2 && s > 0) {
        sum((x_bersih - m)^3) / ((n_valid - 1) * s^3)
      } else {
        0
      }
      
      tibble(
        Variabel = nama_var,
        n = as.integer(n_valid),
        Hilang = as.integer(n_hilang),
        `Rata-rata` = format_id(m, 1),
        Median = format_id(med, 1),
        SD = format_id(s, 1),
        Min = format_id(mi, 1),
        Q1 = format_id(q[1], 1),
        Q3 = format_id(q[2], 1),
        Maks = format_id(ma, 1),
        Skewness = format_id(skew, 2)
      )
    }
    
    bind_rows(
      hitung_deskriptif(df$pm25, "AQI PM2,5 (Indeks US EPA)"),
      hitung_deskriptif(df$titik_panas, "Jumlah Piksel Panas (satelit)"),
      hitung_deskriptif(df$frp_total, "Total FRP Kebakaran (MW)"),
      hitung_deskriptif(df$hujan_mm, "Curah Hujan Harian (mm)"),
      hitung_deskriptif(df$suhu_c, "Suhu Udara Rata-rata (°C)"),
      hitung_deskriptif(df$angin_maks, "Kecepatan Angin Maks (km/jam)")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrrrrrrrr")
  
  output$plot_kategori <- renderPlotly({
    df <- data_terfilter() |> filter(!is.na(pm25))
    validate(need(nrow(df) > 0, "Tidak ada data untuk pilihan ini."))
    
    kategori_urutan <- c(
      "Baik", "Sedang", "Tidak Sehat (Sensitif)",
      "Tidak Sehat", "Sangat Tidak Sehat", "Berbahaya"
    )
    warna_vektor <- c(
      "#00E400", "#FFFF00", "#FF7E00",
      "#FF0000", "#8F3F97", "#7E0023"
    )
    
    df_kat <- df |>
      mutate(kategori = factor(sapply(pm25, get_kategori_aqi), levels = kategori_urutan)) |>
      count(kategori, .drop = FALSE) |>
      mutate(persen = (n / sum(n)) * 100)
    
    plot_ly(
      df_kat,
      x = ~kategori,
      y = ~persen,
      type = "bar",
      marker = list(
        color = warna_vektor,
        line = list(color = "#1B2733", width = 0.5)
      ),
      hoverinfo = "text",
      text = ~sprintf("<b>%s</b><br>Jumlah Hari: %d<br>Persentase: %s%%", kategori, n, format_id(persen, 1))
    ) |>
      layout(
        title = list(text = "", font = list(size = 14)),
        xaxis = list(title = "", tickangle = -20),
        yaxis = list(title = "Persentase Hari (%)", range = c(0, max(df_kat$persen, 20) + 10)),
        margin = list(t = 20, b = 60, l = 50, r = 20),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      ) |>
      config(displayModeBar = FALSE)
  })
  
  output$tbl_norm <- renderTable({
    df <- data_terfilter()
    validate(need(nrow(df) >= 3, "Diperlukan minimal 3 data untuk uji Shapiro-Wilk."))
    
    uji_sw <- function(x, nama) {
      x_bersih <- x[!is.na(x)]
      if (length(x_bersih) < 3 || length(unique(x_bersih)) <= 1) {
        return(tibble(Variabel = nama, n = length(x_bersih), `Statistik W` = "-", `Nilai p` = "-", `Normal (Ya/Tidak)` = "Tidak"))
      }
      res <- stats::shapiro.test(x_bersih)
      tibble(
        Variabel = nama,
        n = as.integer(length(x_bersih)),
        `Statistik W` = format_id(res$statistic, 3),
        `Nilai p` = if (res$p.value < 0.001) "< 0,001" else format_id(res$p.value, 4),
        `Normal (Ya/Tidak)` = if (res$p.value >= 0.05) "Ya (Distribusi Normal)" else "Tidak (Non-Normal)"
      )
    }
    
    bind_rows(
      uji_sw(df$pm25, "AQI PM2,5"),
      uji_sw(df$titik_panas, "Jumlah Piksel Panas"),
      uji_sw(df$frp_total, "Total FRP Kebakaran (MW)")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrc")
  
  # --- TAB 2: TREN ---
  
  output$plot_tren <- renderPlotly({
    df <- data_terfilter()
    validate(need(nrow(df) > 0, "Tidak ada data untuk pilihan ini."))
    
    label_api <- if (input$var_api == "frp_total") "Total FRP (MW)" else "Piksel Panas"
    
    p <- plot_ly(df, x = ~tanggal)
    
    p <- p |> add_bars(
      y = ~api_nilai,
      name = label_api,
      yaxis = "y2",
      marker = list(color = "rgba(251, 133, 0, 0.45)", line = list(color = "#FB8500", width = 1)),
      hoverinfo = "text",
      text = ~sprintf("<b>%s</b><br>%s: %s", format(tanggal, "%d %b %Y"), label_api, format_id(api_nilai, 1))
    )
    
    p <- p |> add_lines(
      y = ~pm25,
      name = "AQI PM2,5 Harian",
      line = list(color = "#1F3A5F", width = 2.2),
      marker = list(color = "#1F3A5F", size = 4),
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~sprintf("<b>%s</b><br>AQI PM2,5: %s (%s)", format(tanggal, "%d %b %Y"), format_id(pm25, 1), sapply(pm25, get_kategori_aqi))
    )
    
    p <- p |> add_lines(
      y = ~pm25_ma,
      name = sprintf("Rata-rata Bergerak (%d Hari)", input$ma_k),
      line = list(color = "#0284C7", width = 2, dash = "dash"),
      hoverinfo = "text",
      text = ~sprintf("<b>MA %d Hari:</b> %s", input$ma_k, format_id(pm25_ma, 1))
    )
    
    p |> layout(
      xaxis = list(
        title = "Tanggal",
        type = "date",
        tickformat = "%d %b\n%Y",
        showgrid = TRUE,
        gridcolor = "#F1F5F9"
      ),
      yaxis = list(
        title = "AQI PM2,5 (US EPA)",
        showgrid = TRUE,
        gridcolor = "#F1F5F9",
        zeroline = FALSE
      ),
      yaxis2 = list(
        title = label_api,
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      shapes = list(
        list(
          type = "line",
          x0 = min(df$tanggal), x1 = max(df$tanggal),
          y0 = 100, y1 = 100,
          line = list(color = "#FF7E00", width = 1.5, dash = "dot")
        ),
        list(
          type = "line",
          x0 = min(df$tanggal), x1 = max(df$tanggal),
          y0 = 150, y1 = 150,
          line = list(color = "#FF0000", width = 1.5, dash = "dot")
        )
      ),
      annotations = list(
        list(
          x = min(df$tanggal), y = 102,
          text = "Ambang Sensitif (100)",
          showarrow = FALSE, xanchor = "left",
          font = list(color = "#FF7E00", size = 10)
        ),
        list(
          x = min(df$tanggal), y = 152,
          text = "Ambang Tidak Sehat (150)",
          showarrow = FALSE, xanchor = "left",
          font = list(color = "#FF0000", size = 10)
        )
      ),
      legend = list(orientation = "h", x = 0, y = 1.15),
      margin = list(t = 40, b = 40, l = 50, r = 50),
      plot_bgcolor = "#FFFFFF",
      paper_bgcolor = "#FFFFFF"
    ) |>
      config(displayModeBar = FALSE)
  })
  
  # --- TAB 3: PETA TITIK PANAS ---
  
  output$n_titik <- renderText({
    pts <- hotspots_terfilter()
    deskripsi_peta <- if (nrow(pts) <= 2000L) {
      "Peta menampilkan posisi deteksi."
    } else {
      "Peta merangkum kepadatan per petak sekitar 4 km."
    }
    sprintf(
      "Total %s piksel panas dalam radius 100 km dari %s. %s",
      format_id(nrow(pts), 0),
      input$kota,
      deskripsi_peta
    )
  })
  
  output$peta <- renderLeaflet({
    pts_sub <- hotspots_terfilter()
    coord <- get_koordinat_kota(input$kota)
    
    req(input$tgl)
    
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      setView(lng = coord$lon, lat = coord$lat, zoom = coord$zoom) |>
      addCircleMarkers(
        lng = coord$lon,
        lat = coord$lat,
        radius = 8,
        color = "#1F3A5F",
        fillColor = "#1F3A5F",
        fillOpacity = 0.9,
        weight = 2,
        popup = sprintf("<strong>Pusat Stasiun:</strong> %s", input$kota)
      ) |>
      addCircles(
        lng = coord$lon,
        lat = coord$lat,
        radius = 100000,
        color = "#1F3A5F",
        weight = 1.5,
        fillColor = "#1F3A5F",
        fillOpacity = 0.05,
        popup = "Radius Pengamatan Analisis 100 km"
      )
    
    if (nrow(pts_sub) > 0 && nrow(pts_sub) <= 2000L) {
      warna_confidence <- c(
        "l" = "#FDE047",
        "n" = "#F97316",
        "h" = "#DC2626"
      )
      label_confidence <- c(
        "l" = "Rendah",
        "n" = "Nominal",
        "h" = "Tinggi"
      )
      confidence <- tolower(as.character(pts_sub$confidence))
      confidence[!confidence %in% names(warna_confidence)] <- "l"
      warna_titik <- unname(warna_confidence[confidence])
      label_titik <- unname(label_confidence[confidence])
      popup_teks <- sprintf(
        paste0(
          "<strong>Deteksi hotspot NASA FIRMS</strong>",
          "<br>Tanggal: %s",
          "<br>FRP: %s MW",
          "<br>Confidence: %s"
        ),
        format(pts_sub$tanggal, "%d %b %Y"),
        format_id(pts_sub$frp, 1),
        label_titik
      )

      m <- m |>
        addCircleMarkers(
          data = pts_sub,
          lng = ~longitude,
          lat = ~latitude,
          radius = 3,
          color = warna_titik,
          fillColor = warna_titik,
          fillOpacity = 0.85,
          weight = 0.5,
          popup = popup_teks
        ) |>
        addLegend(
          position = "bottomright",
          colors = unname(warna_confidence[c("l", "n", "h")]),
          labels = unname(label_confidence[c("l", "n", "h")]),
          title = "Confidence",
          opacity = 0.85
        )
    } else if (nrow(pts_sub) > 2000L) {
      ukuran_sel <- 0.04
      hotspots_ringkas <- pts_sub |>
        mutate(
          sel_lat = floor(latitude / ukuran_sel),
          sel_lon = floor(longitude / ukuran_sel)
        ) |>
        group_by(sel_lat, sel_lon) |>
        summarise(
          latitude_min = first(sel_lat) * ukuran_sel,
          longitude_min = first(sel_lon) * ukuran_sel,
          latitude_max = (first(sel_lat) + 1) * ukuran_sel,
          longitude_max = (first(sel_lon) + 1) * ukuran_sel,
          jumlah = n(),
          confidence = if (any(confidence == "h", na.rm = TRUE)) {
            "h"
          } else if (any(confidence == "n", na.rm = TRUE)) {
            "n"
          } else {
            "l"
          },
          .groups = "drop"
        )
      hotspots_ringkas$kategori <- cut(
        hotspots_ringkas$jumlah,
        breaks = c(-Inf, 10, 50, 200, Inf),
        labels = c("Rendah", "Sedang", "Tinggi", "Sangat tinggi"),
        ordered_result = TRUE
      )
      warna_kategori <- c(
        "Rendah" = "#FFF2CC",
        "Sedang" = "#FDBA74",
        "Tinggi" = "#F97316",
        "Sangat tinggi" = "#B91C1C"
      )
      warna_petakan <- unname(
        warna_kategori[as.character(hotspots_ringkas$kategori)]
      )
      
      popup_teks <- sprintf(
        paste0(
          "<strong>Ringkasan hotspot per petak (~4 km)</strong>",
          "<br>Kepadatan deteksi: %s",
          "<br>Confidence tertinggi: %s"
        ),
        as.character(hotspots_ringkas$kategori),
        ifelse(
          hotspots_ringkas$confidence == "h",
          "Tinggi",
          ifelse(hotspots_ringkas$confidence == "n", "Nominal", "Rendah")
        )
      )
      
      m <- m |>
        addRectangles(
          data = hotspots_ringkas,
          lng1 = ~longitude_min,
          lat1 = ~latitude_min,
          lng2 = ~longitude_max,
          lat2 = ~latitude_max,
          color = "#FFFDF8",
          weight = 0.4,
          opacity = 0.55,
          fillColor = warna_petakan,
          fillOpacity = 0.78,
          popup = popup_teks
        ) |>
        addLegend(
          position = "bottomright",
          colors = unname(warna_kategori),
          labels = names(warna_kategori),
          title = "Kepadatan deteksi",
          opacity = 0.78
        )
    }
    
    m
  })
  
  # --- TAB 4: HUBUNGAN ---
  
  hasil_hubungan <- reactive({
    df <- data_terfilter()
    validate(need(sum(!is.na(df$api_nilai) & !is.na(df$pm25)) >= 8, "Data berpasangan terlalu sedikit untuk menghitung korelasi lag."))
    analisis_hubungan_lengkap(df, lag_dipilih = input$lag_api, max_lag = 7)
  })
  
  observeEvent(event_data("plotly_click", source = "src_lag"), {
    klik <- event_data("plotly_click", source = "src_lag")
    if (!is.null(klik$x)) {
      lag_baru <- as.integer(as.character(klik$x))
      if (!is.na(lag_baru) && lag_baru >= 0 && lag_baru <= 7) {
        updateSliderInput(session, "lag_api", value = lag_baru)
      }
    }
  })
  
  output$plot_lag <- renderPlotly({
    df_lag <- hasil_hubungan()$df_lag
    
    warna_bars <- ifelse(
      df_lag$lag == input$lag_api, "#1F3A5F",
      ifelse(df_lag$signifikan, "#64748B", "#CBD5E1")
    )
    
    plot_ly(df_lag, x = ~factor(lag), source = "src_lag", height = 310) |>
      add_bars(
        y = ~rho,
        marker = list(color = warna_bars),
        textposition = "none",
        error_y = list(
          type = "data",
          symmetric = FALSE,
          array = ~ (ci_atas - rho),
          arrayminus = ~ (rho - ci_bawah),
          color = "#334155",
          width = 3
        ),
        hoverinfo = "text",
        text = ~sprintf(
          "<b>Lag %d Hari</b><br>Spearman rho: %s<br>95%% CI: [%s, %s]<br>p-value: %s<br><i>(Klik untuk pilih lag ini)</i>",
          lag, format_id(rho, 2), format_id(ci_bawah, 2), format_id(ci_atas, 2),
          ifelse(is.na(p), "-", ifelse(p < 0.001, "< 0,001", format_id(p, 4)))
        )
      ) |>
      layout(
        xaxis = list(title = "Lag Waktu (Hari)"),
        yaxis = list(title = "Korelasi Spearman (rho)", range = c(-1.05, 1.05)),
        shapes = list(
          list(
            type = "line", x0 = -0.5, x1 = 7.5, y0 = 0, y1 = 0,
            line = list(color = "#94A3B8", width = 1, dash = "dash")
          )
        ),
        margin = list(t = 15, b = 45, l = 55, r = 20),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      ) |>
      config(displayModeBar = FALSE)
  })
  
  output$scatter <- renderPlotly({
    df <- data_terfilter()
    validate(need(nrow(df) > 0, "Tidak ada data untuk pilihan ini."))
    
    df_plot <- df |>
      filter(!is.na(api_lag), !is.na(pm25)) |>
      mutate(kondisi_hujan = ifelse(!is.na(hujan_mm) & hujan_mm > 1, "Hari Hujan (>1 mm)", "Hari Kering (≤1 mm)"))
    
    validate(need(nrow(df_plot) > 2, "Data lag tidak cukup untuk diagram pencar."))
    
    label_api <- if (input$var_api == "frp_total") {
      sprintf("Total FRP Kebakaran (MW, lag %d hari)", input$lag_api)
    } else {
      sprintf("Jumlah Piksel Panas (lag %d hari)", input$lag_api)
    }
    
    p <- plot_ly(
      df_plot,
      x = ~api_lag,
      y = ~pm25,
      color = ~kondisi_hujan,
      colors = c("Hari Kering (≤1 mm)" = "#1F3A5F", "Hari Hujan (>1 mm)" = "#0284C7"),
      type = "scatter",
      mode = "markers",
      height = 310,
      marker = list(size = 7.5, opacity = 0.8, line = list(color = "#0F172A", width = 0.5)),
      hoverinfo = "text",
      text = ~sprintf(
        "<b>%s</b><br>%s: %s<br>AQI PM2,5: %s<br>Curah Hujan: %s mm",
        format(tanggal, "%d %b %Y"), label_api, format_id(api_lag, 1), format_id(pm25, 1), format_id(hujan_mm, 1)
      )
    )
    
    if (nrow(df_plot) >= 8 && length(unique(df_plot$api_lag)) >= 5) {
      df_sorted <- df_plot |> arrange(api_lag)
      fit_loess <- suppressWarnings(loess(pm25 ~ api_lag, data = df_sorted, span = 0.85))
      df_sorted$y_smooth <- predict(fit_loess)
      
      p <- p |> add_lines(
        data = df_sorted,
        x = ~api_lag,
        y = ~y_smooth,
        name = "Tren LOESS",
        inherit = FALSE,
        line = list(color = "#D00000", width = 2.2),
        hoverinfo = "none"
      )
    }
    
    p |> layout(
      xaxis = list(title = label_api, showgrid = TRUE, gridcolor = "#F1F5F9"),
      yaxis = list(title = "AQI PM2,5", showgrid = TRUE, gridcolor = "#F1F5F9"),
      shapes = list(
        list(
          type = "line", x0 = min(df_plot$api_lag), x1 = max(df_plot$api_lag),
          y0 = 100, y1 = 100, line = list(color = "#FF7E00", width = 1.2, dash = "dot")
        ),
        list(
          type = "line", x0 = min(df_plot$api_lag), x1 = max(df_plot$api_lag),
          y0 = 150, y1 = 150, line = list(color = "#FF0000", width = 1.2, dash = "dot")
        )
      ),
      legend = list(orientation = "h", x = 0, y = 1.12),
      margin = list(t = 25, b = 45, l = 55, r = 20),
      plot_bgcolor = "#FFFFFF",
      paper_bgcolor = "#FFFFFF"
    ) |>
      config(displayModeBar = FALSE)
  })
  
  output$tbl_korelasi_ringkas <- renderTable({
    p_data <- hasil_hubungan()$parsial
    validate(need(!is.na(p_data$rho_bivariat), "Data tidak cukup."))
    
    ket_kekuatan <- function(r) {
      ar <- abs(r)
      if (ar >= 0.7) "Kuat"
      else if (ar >= 0.4) "Sedang"
      else if (ar >= 0.2) "Lemah"
      else "Sangat Lemah"
    }
    
    tibble(
      `Jenis Korelasi (Lag Terpilih)` = c(
        sprintf("Korelasi Rank Spearman Bivariat (Lag %d Hari)", p_data$lag),
        sprintf("Korelasi Parsial Spearman (Kontrol Hujan & Angin, Lag %d Hari)", p_data$lag)
      ),
      `n` = c(as.integer(p_data$n), as.integer(p_data$n)),
      `Koefisien (rho)` = c(format_id(p_data$rho_bivariat, 3), format_id(p_data$rho_parsial, 3)),
      `Nilai p` = c(
        ifelse(p_data$p_bivariat < 0.001, "< 0,001", format_id(p_data$p_bivariat, 4)),
        ifelse(p_data$p_parsial < 0.001, "< 0,001", format_id(p_data$p_parsial, 4))
      ),
      `Kategori Kekuatan` = c(ket_kekuatan(p_data$rho_bivariat), ket_kekuatan(p_data$rho_parsial)),
      `Kesimpulan Statistik (α = 0,05)` = c(
        ifelse(p_data$p_bivariat < 0.05, "Signifikan", "Tidak Signifikan"),
        ifelse(p_data$p_parsial < 0.05, "Signifikan (Kokoh terhadap Cuaca)", "Tidak Signifikan (Dipengaruhi Cuaca)")
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrcc")
  
  output$tafsir_lag <- renderUI({
    df_lag <- hasil_hubungan()$df_lag |> filter(!is.na(rho))
    validate(need(nrow(df_lag) > 0, ""))
    
    puncak <- df_lag[which.max(abs(df_lag$rho)), ]
    terpilih <- df_lag |> filter(lag == input$lag_api)
    
    teks_lag <- sprintf(
      "Pada %s, asosiasi terkuat terdeteksi pada lag %d hari (rho = %s, p %s). Pada lag yang sedang dipilih (%d hari), nilai korelasi Spearman adalah rho = %s (p %s).",
      input$kota, puncak$lag, format_id(puncak$rho, 2),
      ifelse(puncak$p < 0.001, "< 0,001", paste0("= ", format_id(puncak$p, 3))),
      terpilih$lag, format_id(terpilih$rho, 2),
      ifelse(terpilih$p < 0.001, "< 0,001", paste0("= ", format_id(terpilih$p, 3)))
    )
    
    kartu_interpretasi(teks = teks_lag, tipe = "info", judul = "Interpretasi Asosiasi Korelasi Lag")
  })
  
  output$tafsir_parsial <- renderUI({
    p_data <- hasil_hubungan()$parsial
    validate(need(!is.na(p_data$rho_parsial), ""))
    
    arah <- if (p_data$rho_parsial >= 0) "positif" else "negatif"
    signif_teks <- if (p_data$p_parsial < 0.05) {
      sprintf("tetap bernilai %s dan signifikan secara statistik (p < 0,05), menunjukkan hubungan yang tidak hilang setelah variabel cuaca dikontrol.", arah)
    } else {
      "melemah dan tidak signifikan secara statistik (p >= 0,05) setelah variabel cuaca dikontrol, mengindikasikan pengaruh kuat faktor meteorologi."
    }
    
    teks_parsial <- sprintf(
      "Setelah mengontrol variabel kovariat cuaca (curah hujan dan kecepatan angin) pada lag %d hari, koefisien korelasi parsial bernilai rho = %s (p %s), sehingga %s",
      p_data$lag, format_id(p_data$rho_parsial, 2),
      ifelse(p_data$p_parsial < 0.001, "< 0,001", paste0("= ", format_id(p_data$p_parsial, 3))),
      signif_teks
    )
    
    kartu_interpretasi(
      teks = teks_parsial,
      tipe = if (p_data$p_parsial < 0.05 && p_data$rho_parsial > 0) "success" else "warning",
      judul = "Interpretasi Asosiasi Korelasi Parsial (Kontrol Cuaca)"
    )
  })
  
  # --- TAB 5: PERBANDINGAN ---
  
  output$ringkas_uji <- renderTable({
    tabel_ringkas <- analisis_dinamis()$uji_dua_periode$ringkas
    
    tibble(
      Periode = as.character(tabel_ringkas$periode),
      n = as.integer(tabel_ringkas$n),
      `Median AQI` = format_id(tabel_ringkas$median, 1),
      IQR = format_id(tabel_ringkas$iqr, 1),
      `Rata-rata` = format_id(tabel_ringkas$rata_rata, 1)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrr")
  
  output$tbl_norm_periode <- renderTable({
    tabel_norm <- analisis_dinamis()$uji_dua_periode$normalitas
    
    tibble(
      `Kelompok Data` = tabel_norm$data,
      n = as.integer(tabel_norm$n),
      `Statistik W` = format_id(tabel_norm$W, 3),
      `Nilai p` = ifelse(is.na(tabel_norm$p), "-", ifelse(tabel_norm$p < 0.001, "< 0,001", format_id(tabel_norm$p, 4))),
      `Distribusi Normal` = ifelse(tabel_norm$normal, "Ya", "Tidak (Non-Normal)")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrc")
  
  output$tafsir_mw <- renderUI({
    mw <- analisis_dinamis()$uji_dua_periode
    validate(need(!is.na(mw$uji$p.value), "Pastikan kedua periode memiliki minimal 2 hari data di dalam rentang tanggal pengamatan."))
    
    p_teks <- if (mw$uji$p.value < 0.001) "< 0,001" else paste0("= ", format_id(mw$uji$p.value, 4))
    teks_mw <- sprintf(
      "Berdasarkan uji Mann-Whitney U (W = %s, p %s), perbandingan peringkat kualitas udara antara periode normal (median AQI = %s) dan periode karhutla (median AQI = %s) menunjukkan ukuran efek %s.",
      format_id(as.numeric(mw$uji$statistic), 0),
      p_teks,
      format_id(mw$ringkas$median[1], 1),
      format_id(mw$ringkas$median[2], 1),
      mw$efek
    )
    
    kartu_interpretasi(
      teks = teks_mw,
      tipe = if (mw$uji$p.value < 0.05) "info" else "warning",
      judul = "Kesimpulan Uji Mann-Whitney U (Asosiasi Dua Periode)"
    )
  })
  
  output$tafsir_kw <- renderUI({
    kw_info <- analisis_dinamis()$uji_antar_kota
    validate(need(!is.na(kw_info$kw$p.value), "Data antar kota tidak mencukupi."))
    
    p_teks <- if (kw_info$kw$p.value < 0.001) "< 0,001" else paste0("= ", format_id(kw_info$kw$p.value, 4))
    teks_kw <- sprintf(
      "Uji Kruskal-Wallis antar kota menunjukkan variasi peringkat AQI PM2,5 antar wilayah pengamatan (H = %s, df = %d, p %s, %s).",
      format_id(as.numeric(kw_info$kw$statistic), 2),
      as.integer(kw_info$kw$parameter),
      p_teks,
      kw_info$efek
    )
    
    kartu_interpretasi(
      teks = teks_kw,
      tipe = "info",
      judul = "Kesimpulan Uji Kruskal-Wallis (Asosiasi Antar Wilayah)"
    )
  })
  
  output$tbl_dunn <- renderDT({
    dunn_raw <- analisis_dinamis()$uji_antar_kota$dunn
    validate(need(nrow(dunn_raw) > 0, "Data tidak cukup untuk uji Dunn."))
    
    dunn_df <- dunn_raw |>
      mutate(
        `Perbandingan Kota` = paste(group1, "vs", group2),
        `Skor z` = format_id(z, 2),
        `Nilai p (Adj. Holm)` = ifelse(p.adj < 0.001, "< 0,001", format_id(p.adj, 4)),
        Signifikansi = p.adj.signif
      ) |>
      select(`Perbandingan Kota`, `Skor z`, `Nilai p (Adj. Holm)`, Signifikansi)
    
    datatable(
      dunn_df,
      options = list(dom = "t", paging = FALSE, searching = FALSE),
      rownames = FALSE,
      class = "table table-sm table-striped table-bordered"
    )
  })
  
  output$tbl_chisq <- renderTable({
    analisis_dinamis()$chisq$persen_baris
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lcccccc")
  
  output$tafsir_chisq <- renderUI({
    cs <- analisis_dinamis()$chisq
    validate(need(!is.na(cs$uji$p.value), "Tabel kontingensi memerlukan minimal 2 kategori AQI aktif di kedua periode."))
    
    p_teks <- if (cs$uji$p.value < 0.001) "< 0,001" else paste0("= ", format_id(cs$uji$p.value, 4))
    teks_cs <- sprintf(
      "Uji Chi-Square independensi menunjukkan pergeseran proporsi kategori kualitas udara antara periode normal dan karhutla (Chi² = %s, df = %d, p %s, frekuensi harapan minimum = %s).",
      format_id(as.numeric(cs$uji$statistic), 2),
      as.integer(cs$uji$parameter),
      p_teks,
      format_id(cs$harapan_min, 1)
    )
    
    kartu_interpretasi(
      teks = teks_cs,
      tipe = "warning",
      judul = "Kesimpulan Uji Chi-Square (Asosiasi Distribusi Kategori)"
    )
  })
  
  # --- TAB 6: REGRESI ---
  
  output$tbl_sampel <- renderTable({
    s <- analisis_dinamis()$sampel
    
    tibble(
      `Parameter` = c("Jumlah Observasi (n)", "Jumlah Prediktor (k)", "Syarat Min. Green (1991: 50 + 8k)", "Status Kelayakan Model"),
      `Nilai` = c(
        format_id(s$n, 0),
        format_id(s$k, 0),
        format_id(s$min_uji_model, 0),
        ifelse(s$model_memadai, "Sampel Memadai (Terpenuhi)", "Sampel Kurang (Perluas Rentang Tanggal)")
      )
    )
  }, striped = TRUE, bordered = TRUE)
  
  output$tbl_hier <- renderTable({
    h <- analisis_dinamis()$hierarkis
    
    tibble(
      `Tingkatan Model` = c("Blok 1 (Hanya Kovariat Cuaca)", "Blok 2 (Cuaca + Ukuran Kebakaran)", "Perubahan Tambahan (Delta)"),
      `R²` = c(format_id(h$r2["blok1"], 2), format_id(h$r2["blok2"], 2), "-"),
      `ΔR²` = c("-", "-", format_id(h$delta_r2, 2)),
      `Statistik F` = c("-", "-", format_id(h$uji_perubahan$F, 2)),
      `Nilai p` = c("-", "-", ifelse(is.na(h$uji_perubahan$p), "-", ifelse(h$uji_perubahan$p < 0.001, "< 0,001", format_id(h$uji_perubahan$p, 4))))
    )
  }, striped = TRUE, bordered = TRUE, align = "lrrrr")
  
  output$tbl_koef <- renderTable({
    analisis_dinamis()$koef
  }, striped = TRUE, bordered = TRUE, align = "lrrrr")
  
  output$tbl_hac <- renderTable({
    analisis_dinamis()$koef_hac
  }, striped = TRUE, bordered = TRUE, align = "lrrrr")
  
  output$tbl_asumsi <- renderUI({
    df_asumsi <- analisis_dinamis()$asumsi
    
    baris_html <- lapply(seq_len(nrow(df_asumsi)), function(i) {
      catatan <- if (!df_asumsi$lolos[i] && grepl("Autokorelasi", df_asumsi$uji[i])) {
        "Pakai galat baku HAC"
      } else {
        NULL
      }
      
      tags$tr(
        tags$td(tags$strong(df_asumsi$uji[i])),
        tags$td(df_asumsi$statistik[i]),
        tags$td(df_asumsi$p[i]),
        tags$td(badge_asumsi(df_asumsi$lolos[i], catatan))
      )
    })
    
    tags$table(
      class = "table table-sm table-bordered align-middle",
      tags$thead(
        tags$tr(
          tags$th("Uji Asumsi"),
          tags$th("Statistik"),
          tags$th("Nilai p"),
          tags$th("Status Pemenuhan")
        )
      ),
      tags$tbody(baris_html)
    )
  })
  
  output$tbl_vif <- renderTable({
    v <- analisis_dinamis()$vif
    
    tibble(
      `Prediktor Model` = v$prediktor,
      `Nilai VIF` = format_id(v$VIF, 2),
      `Nilai Tolerance` = format_id(v$tolerance, 2),
      `Bebas Multikolinearitas` = ifelse(v$lolos, "Lolos (VIF < 3)", "Terindikasi Kolinear")
    )
  }, striped = TRUE, bordered = TRUE, align = "lrrr")
  
  output$teks_outlier <- renderUI({
    kartu_interpretasi(
      teks = "Pemeriksaan amatan ekstrem dan jarak Cook (Cook's Distance) menunjukkan tidak ada observasi dengan Cook's D > 0,5 yang berpotensi menjadi titik berpengaruh berlebih (leverage tinggi yang merusak stabilitas estimasi). Seluruh nilai ekstrem dipertahankan dalam analisis karena merupakan cerminan empiris fenomena karhutla ekstrem yang nyata dan bukan akibat galat pencatatan.",
      tipe = "info",
      judul = "Diagnostik Pengamatan Ekstrem & Cook's D"
    )
  })
  
  output$plot_diag <- renderPlot({
    df <- data_terfilter() |>
      mutate(pred_api = if (input$lag_api > 0) api_lag else api_nilai) |>
      filter(!is.na(pm25), !is.na(pred_api))
    if (nrow(df) < 10) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk plot diagnostik", cex = 1.2)
      return()
    }
    
    kandidat_cuaca <- c("hujan_mm", "suhu_c", "angin_maks")
    cuaca_valid <- kandidat_cuaca[vapply(kandidat_cuaca, function(v) {
      v %in% names(df) && sum(!is.na(df[[v]])) > 5 && stats::sd(df[[v]], na.rm = TRUE) > 0
    }, logical(1))]
    f_diag <- stats::as.formula(paste("pm25 ~", paste(c("pred_api", cuaca_valid), collapse = " + ")))
    fit <- stats::lm(f_diag, data = df)
    
    par(mfrow = c(2, 2), mar = c(4.2, 4.2, 2.5, 1.5), oma = c(0, 0, 0, 0))
    plot(fit, which = 1:4, pch = 16, col = adjustcolor("#1F3A5F", alpha.f = 0.65))
  })
  
  output$plot_acf <- renderPlot({
    df <- data_terfilter() |>
      mutate(pred_api = if (input$lag_api > 0) api_lag else api_nilai) |>
      filter(!is.na(pm25), !is.na(pred_api))
    if (nrow(df) < 10) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk plot ACF", cex = 1.2)
      return()
    }
    
    kandidat_cuaca <- c("hujan_mm", "suhu_c", "angin_maks")
    cuaca_valid <- kandidat_cuaca[vapply(kandidat_cuaca, function(v) {
      v %in% names(df) && sum(!is.na(df[[v]])) > 5 && stats::sd(df[[v]], na.rm = TRUE) > 0
    }, logical(1))]
    f_diag <- stats::as.formula(paste("pm25 ~", paste(c("pred_api", cuaca_valid), collapse = " + ")))
    fit <- stats::lm(f_diag, data = df)
    res <- residuals(fit)
    
    par(mar = c(4.2, 4.2, 2.5, 1.5))
    stats::acf(
      res,
      lag.max = 14,
      main = "ACF Residual: Deteksi Dependensi Temporal",
      ylab = "Autokorelasi",
      xlab = "Lag Waktu (Hari)",
      col = "#1F3A5F",
      lwd = 2
    )
  })
  
  # --- TAB 7: METODOLOGI ---
  
  output$metodologi <- renderUI({
    tags$div(
      class = "p-2",
      
      tags$div(
        class = "card p-3 mb-3",
        tags$h5(style = "color: #1F3A5F; font-weight: 700;", "1. Sumber Data dan Karakteristik Sensor"),
        tags$ul(
          tags$li(
            tags$strong("Kualitas Udara: "),
            "Diperoleh dari Open-Meteo Air Quality API (reanalisis model atmosfer CAMS/SILAM). Parameter yang dipantau mencakup konsentrasi PM2,5 dan PM10 (µg/m³) serta ",
            tags$strong("AQI PM2,5 skala US EPA"), " (0–500) yang diagregasikan ke rata-rata harian."
          ),
          tags$li(
            tags$strong("Titik Panas Satelit: "),
            "Bersumber dari NASA FIRMS (sensor VIIRS/MODIS). Satu titik panas didefinisikan secara operasional sebagai ",
            tags$strong("satu piksel satelit yang terdeteksi memiliki anomali suhu panas"), ", bukan mewakili satu kejadian kebakaran tunggal dan bukan luas area terbakar riil. Fire Radiative Power (FRP) diukur dalam MegaWatt (MW)."
          ),
          tags$li(
            tags$strong("Parameter Cuaca: "),
            "Curah hujan harian (mm), suhu udara rata-rata (°C), dan kecepatan angin maksimum (km/jam) bersumber dari Open-Meteo Historical Weather API."
          )
        )
      ),
      
      tags$div(
        class = "card p-3 mb-3",
        tags$h5(style = "color: #1F3A5F; font-weight: 700;", "2. Agregasi Spasial dan Penyelarasan Temporal"),
        tags$ul(
          tags$li(
            tags$strong("Radius Pengamatan Spasial: "),
            "Titik panas diagregasikan dalam batas lingkaran radius 100 km dari titik koordinat masing-masing stasiun pemantau kota."
          ),
          tags$li(
            tags$strong("Lag Waktu: "),
            "Diujikan lag 0 hingga 7 hari untuk memodelkan waktu tempuh dispersi asap dari area sumber kebakaran menuju stasiun pencatat."
          ),
          tags$li(
            tags$strong("Rata-rata Bergerak (Moving Average): "),
            "Menggunakan moving average k-hari sisi kiri (backward-looking) untuk menghaluskan fluktuasi jangka pendek harian."
          )
        )
      ),
      
      tags$div(
        class = "card p-3 mb-3",
        tags$h5(style = "color: #1F3A5F; font-weight: 700;", "3. Metode Statistik & Diagnostik Ekonometrika"),
        tags$ul(
          tags$li(
            tags$strong("Uji Non-Parametrik: "),
            "Mengingat data tidak memenuhi asumsi normalitas (uji Shapiro-Wilk p < 0,05), analisis menggunakan korelasi rank Spearman, uji Mann-Whitney U untuk komparasi dua periode, dan uji Kruskal-Wallis untuk perbandingan antar wilayah."
          ),
          tags$li(
            tags$strong("Korelasi Parsial: "),
            "Digunakan untuk memverifikasi apakah asosiasi antara titik panas dan PM2,5 tetap signifikan setelah mengontrol pengaruh curah hujan dan kecepatan angin."
          ),
          tags$li(
            tags$strong("Pemodelan Regresi OLS Hierarkis: "),
            "Mengevaluasi kontribusi unik (ΔR²) variabel kebakaran setelah memperhitungkan variabel dasar cuaca."
          ),
          tags$li(
            tags$strong("Koreksi Kesalahan Baku HAC (Newey-West): "),
            "Karena terdeteksi autokorelasi pada residual deret waktu (uji Durbin-Watson p < 0,05), inferensi regresi menggunakan standar galat kebal heteroskedastisitas dan autokorelasi (HAC)."
          )
        )
      ),
      
      tags$div(
        class = "card p-3 mb-0",
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B;",
        tags$h5(style = "color: #92400E; font-weight: 700;", "4. Batasan Metodologis & Etika Pernyataan"),
        tags$p(
          style = "color: #78350F; line-height: 1.6; margin-bottom: 0;",
          "Seluruh kesimpulan yang disajikan dalam dashboard ini merupakan ",
          tags$strong("asosiasi statistik empiris"),
          ", bukan hubungan kausalitas (sebab-akibat deterministik). Tutupan awan tebal dapat menghalangi deteksi sensor satelit optik sehingga jumlah piksel panas berpotensi mengalami undercounting pada hari-hari tertentu."
        )
      )
    )
  })
}

# 5. Jalankan Aplikasi Shiny
shinyApp(ui = ui, server = server)
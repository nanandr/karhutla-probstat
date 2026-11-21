# R/dummy_data.R
# Seluruh data contoh (dummy) dan analisis simulasi diisolasi di berkas ini.
# Kolom dan struktur data dibuat identik dengan skema data riil.

library(dplyr)
library(tibble)

# Indikator data contoh: ubah ke FALSE saat beralih ke data nyata
DATA_CONTOH <- FALSE
HOTSPOTS_CONTOH <- FALSE 

#' Generator Data Harian Contoh per Kota
#' Menghasilkan deret waktu kualitas udara dan cuaca dengan pola puncak karhutla
buat_kota_contoh <- function(kota, skala = 1, pengaruh = 0.55, seed = 1) {
  set.seed(seed)
  tanggal <- seq(as.Date("2026-07-15"), as.Date("2026-10-15"), by = "day")
  n <- length(tanggal)
  hari <- seq_len(n)
  
  # Pola titik panas (puncak sekitar hari ke-60, yaitu pertengahan September)
  titik <- pmax(0, round(skala * (20 + 280 * exp(-((hari - 60)^2) / (2 * 14^2))) +
                           rnorm(n, 0, 15)))
  frp   <- round(titik * runif(n, 8, 14), 1)
  
  # Pola curah hujan (mulai meningkat setelah hari ke-85)
  hujan <- round(rexp(n, rate = 1 / 4) * ifelse(hari > 85, 2.5, 0.6), 1)
  
  # PM2.5 AQI (skala US EPA) dengan asosiasi terhadap titik panas ber-lag 2 hari
  lag_titik <- dplyr::lag(titik, 2, default = 20)
  pm25  <- pmax(5, round(25 + pengaruh * lag_titik - 0.8 * hujan + rnorm(n, 0, 12)))
  
  tibble(
    kota = kota,
    tanggal = tanggal,
    pm25 = as.numeric(pm25),
    n_stasiun = 1L,
    sumber = "contoh",
    hujan_mm = as.numeric(hujan),
    suhu_c = round(rnorm(n, 28, 1.2), 1),
    angin_maks = round(runif(n, 6, 22), 1),
    titik_panas = as.integer(titik),
    frp_total = as.numeric(frp)
  )
}

# 1. Tabel Data Harian Contoh
DUMMY_HARIAN <- bind_rows(
  buat_kota_contoh("Palangkaraya", skala = 1,    pengaruh = 0.55, seed = 1),
  buat_kota_contoh("Pontianak",    skala = 0.5,  pengaruh = 0.35, seed = 2),
  buat_kota_contoh("Bandung",      skala = 0.02, pengaruh = 0.02, seed = 3)
) |>
  group_by(kota) |>
  mutate(
    batas_ekstrem = quantile(pm25, 0.75, na.rm = TRUE) + 3 * IQR(pm25, na.rm = TRUE),
    outlier = pm25 > batas_ekstrem
  ) |>
  ungroup()

# 2. Tabel Titik Panas Contoh (NASA FIRMS Dummy)
# Sebaran koordinat di sekitar masing-masing kota (radius ~100 km)
buat_hotspot_kota <- function(df_kota, lat_pusat, lon_pusat, seed = 100) {
  set.seed(seed)
  df_sample <- df_kota |>
    filter(titik_panas > 0)
  
  if (nrow(df_sample) == 0) return(tibble())
  
  df_sample |>
    slice_sample(n = min(70, nrow(df_sample))) |>
    rowwise() |>
    reframe(
      tanggal = tanggal,
      latitude  = rnorm(min(titik_panas, 25), lat_pusat, 0.45),
      longitude = rnorm(min(titik_panas, 25), lon_pusat, 0.45),
      confidence = sample(c("l", "n", "h"), min(titik_panas, 25), replace = TRUE, prob = c(0.1, 0.4, 0.5)),
      frp = round(rexp(min(titik_panas, 25), 1 / 11) + 2, 1)
    )
}

DUMMY_HOTSPOTS <- bind_rows(
  buat_hotspot_kota(filter(DUMMY_HARIAN, kota == "Palangkaraya"), -2.2096, 113.9213, seed = 101),
  buat_hotspot_kota(filter(DUMMY_HARIAN, kota == "Pontianak"),    -0.0263, 109.3425, seed = 102),
  buat_hotspot_kota(filter(DUMMY_HARIAN, kota == "Bandung"),      -6.9175, 107.6191, seed = 103)
)

# 3. Hasil Analisis Contoh (DUMMY_ANALISIS)
# Struktur keluaran meniru fungsi statistik tahap lanjut
DUMMY_ANALISIS <- list(
  # Korelasi Spearman ber-lag (0 s.d. 7 hari)
  korelasi_lag = tibble(
    lag = 0:7,
    n = rep(92L, 8),
    rho = c(0.38, 0.49, 0.61, 0.55, 0.44, 0.35, 0.27, 0.20),
    p = c(0.0002, 1.2e-6, 1.1e-10, 8.4e-9, 1.5e-5, 0.0006, 0.009, 0.055),
    p_adj = c(0.0014, 9.6e-6, 8.8e-10, 5.9e-8, 9.0e-5, 0.003, 0.036, 0.055),
    ci_bawah = c(0.19, 0.32, 0.46, 0.39, 0.26, 0.16, 0.07, -0.01),
    ci_atas = c(0.54, 0.63, 0.72, 0.68, 0.59, 0.52, 0.45, 0.39)
  ),
  
  tafsir_korelasi = "Asosiasi terkuat terdeteksi pada lag 2 hari (rho = 0,61; hubungan kuat positif; signifikan secara statistik setelah koreksi Holm, p < 0,001). Ini mencerminkan asosiasi empiris pergerakan asap terhadap pembacaan stasiun pemantau, bukan klaim sebab-akibat langsung.",
  
  # Korelasi parsial (mengontrol variabel hujan dan angin)
  parsial = tibble(
    n = 92L,
    lag = 2L,
    rho_biasa = 0.61,
    rho_parsial = 0.48,
    p_parsial = 0.000018
  ),
  
  # Uji Mann-Whitney U (dua periode: Normal vs Karhutla)
  uji_dua_periode = list(
    ringkas = tibble(
      periode = c("Normal (sebelum 15 Agt)", "Karhutla (15 Agt - 30 Sep)"),
      n = c(31L, 47L),
      median = c(48.0, 154.0),
      iqr = c(22.0, 58.0),
      rata_rata = c(51.2, 162.8)
    ),
    normalitas = tibble(
      data = c("Periode Normal", "Periode Karhutla"),
      n = c(31L, 47L),
      W = c(0.894, 0.821),
      p = c(0.0004, 0.000015),
      normal = c(FALSE, FALSE)
    ),
    uji = list(
      statistic = 112,
      p.value = 1.4e-12,
      method = "Wilcoxon rank sum test with continuity correction"
    ),
    efek = "r = 0,64 (asosiasi efek besar)"
  ),
  
  # Uji Kruskal-Wallis antar kota
  uji_antar_kota = list(
    kw = list(
      statistic = 84.32,
      parameter = 2,
      p.value = 4.8e-19,
      method = "Kruskal-Wallis rank sum test"
    ),
    efek = "eta² = 0,46 (perbedaan peringkat sangat nyata)",
    dunn = tibble(
      group1 = c("Palangkaraya", "Palangkaraya", "Pontianak"),
      group2 = c("Pontianak", "Bandung", "Bandung"),
      z = c(3.42, 9.15, 5.73),
      p.adj = c(0.0018, 1.2e-19, 3.1e-8),
      p.adj.signif = c("**", "****", "****")
    )
  ),
  
  # Uji Chi-Square Kategori AQI antar Periode
  chisq = list(
    persen_baris = tibble(
      Periode = c("Normal", "Karhutla"),
      `Baik (0-50)` = c("45,2%", "0,0%"),
      `Sedang (51-100)` = c("48,4%", "8,5%"),
      `Sensitif (101-150)` = c("6,4%", "29,8%"),
      `Tidak Sehat (151-200)` = c("0,0%", "36,2%"),
      `Sangat Tdk Sehat (201-300)` = c("0,0%", "19,1%"),
      `Berbahaya (>300)` = c("0,0%", "6,4%")
    ),
    uji = list(
      statistic = 68.74,
      parameter = 5,
      p.value = 1.8e-13,
      method = "Pearson's Chi-squared test"
    ),
    harapan_min = 12.4
  ),
  
  # Regresi Hierarkis
  hierarkis = list(
    r2 = c(blok1 = 0.22, blok2 = 0.37),
    delta_r2 = 0.15,
    uji_perubahan = list(F = 21.84, p = 0.000011)
  ),
  
  # Koefisien Model Standar
  koef = tibble(
    Variabel = c("(Intercept)", "Ukuran Kebakaran (lag 2)", "Curah Hujan (mm)", "Kecepatan Angin Maks (km/jam)"),
    Estimasi = c(45.12, 0.42, -1.15, 0.65),
    SE = c(6.24, 0.06, 0.38, 0.29),
    t = c(7.23, 7.00, -3.03, 2.24),
    p = c("< 0,001", "< 0,001", "0,003", "0,027")
  ),
  
  # Koefisien Model dengan Kesalahan Baku Kebal Autokorelasi (HAC)
  koef_hac = tibble(
    Variabel = c("(Intercept)", "Ukuran Kebakaran (lag 2)", "Curah Hujan (mm)", "Kecepatan Angin Maks (km/jam)"),
    Estimasi = c(45.12, 0.42, -1.15, 0.65),
    SE = c(8.10, 0.09, 0.49, 0.36),
    t = c(5.57, 4.67, -2.35, 1.81),
    p = c("< 0,001", "< 0,001", "0,021", "0,074")
  ),
  
  # Uji Asumsi Regresi
  asumsi = tibble(
    uji = c("Normalitas Residual (Shapiro-Wilk)", "Linearitas (Harvey-Collier / RESET)",
            "Autokorelasi (Durbin-Watson)", "Heteroskedastisitas (Breusch-Pagan)"),
    statistik = c("W = 0,985", "t = 1,120", "DW = 1,240", "BP = 2,810"),
    p = c("0,381", "0,265", "0,002", "0,422"),
    lolos = c(TRUE, TRUE, FALSE, TRUE)
  ),
  
  # Multikolinearitas (VIF)
  vif = tibble(
    prediktor = c("Ukuran Kebakaran (lag 2)", "Curah Hujan", "Kecepatan Angin"),
    VIF = c(1.45, 1.18, 1.28),
    tolerance = c(0.69, 0.85, 0.78),
    lolos = c(TRUE, TRUE, TRUE)
  ),
  
  # Kecukupan Sampel
  sampel = tibble(
    n = 92L,
    k = 3L,
    min_uji_model = 74L,
    model_memadai = TRUE
  )
)

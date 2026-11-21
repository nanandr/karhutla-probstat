# R/ui_helpers.R
# Komponen pembantu UI berulang: kartu interpretasi, badge, skema warna AQI,
# format angka Indonesia, banner status, dan footer atribusi.

library(shiny)
library(bslib)

# 1. Definisi Palet Warna dan Kategori AQI (US EPA 6 Kategori)
AQI_LEVELS <- list(
  baik = list(
    nama = "Baik",
    min = 0, max = 50,
    warna = "#00E400",
    warna_teks = "#1B2733",
    deskripsi = "Tingkat kualitas udara sangat baik, tidak berisiko bagi kesehatan."
  ),
  sedang = list(
    nama = "Sedang",
    min = 51, max = 100,
    warna = "#FFFF00",
    warna_teks = "#1B2733",
    deskripsi = "Kualitas udara masih dapat diterima, sedikit sensitivitas bagi kelompok rentan."
  ),
  sensitif = list(
    nama = "Tidak Sehat (Sensitif)",
    min = 101, max = 150,
    warna = "#FF7E00",
    warna_teks = "#1B2733",
    deskripsi = "Dapat mempengaruhi kelompok sensitif (asma, lansia, anak-anak)."
  ),
  tidak_sehat = list(
    nama = "Tidak Sehat",
    min = 151, max = 200,
    warna = "#FF0000",
    warna_teks = "#FFFFFF",
    deskripsi = "Setiap orang dapat mulai mengalami dampak kesehatan."
  ),
  sangat_tidak_sehat = list(
    nama = "Sangat Tidak Sehat",
    min = 201, max = 300,
    warna = "#8F3F97",
    warna_teks = "#FFFFFF",
    deskripsi = "Peringatan darurat kesehatan; seluruh populasi terdampak nyata."
  ),
  berbahaya = list(
    nama = "Berbahaya",
    min = 301, max = Inf,
    warna = "#7E0023",
    warna_teks = "#FFFFFF",
    deskripsi = "Kondisi darurat seluruh populasi; dampak kesehatan serius."
  )
)

#' Menentukan label kategori AQI dari nilai numerik AQI PM2.5
get_kategori_aqi <- function(aqi) {
  if (is.na(aqi)) return("Tidak Ada Data")
  if (aqi <= 50) return(AQI_LEVELS$baik$nama)
  if (aqi <= 100) return(AQI_LEVELS$sedang$nama)
  if (aqi <= 150) return(AQI_LEVELS$sensitif$nama)
  if (aqi <= 200) return(AQI_LEVELS$tidak_sehat$nama)
  if (aqi <= 300) return(AQI_LEVELS$sangat_tidak_sehat$nama)
  return(AQI_LEVELS$berbahaya$nama)
}

#' Mengambil kode warna latar kategori AQI
get_warna_aqi <- function(aqi) {
  if (is.na(aqi)) return("#E2E8F0")
  if (aqi <= 50) return(AQI_LEVELS$baik$warna)
  if (aqi <= 100) return(AQI_LEVELS$sedang$warna)
  if (aqi <= 150) return(AQI_LEVELS$sensitif$warna)
  if (aqi <= 200) return(AQI_LEVELS$tidak_sehat$warna)
  if (aqi <= 300) return(AQI_LEVELS$sangat_tidak_sehat$warna)
  return(AQI_LEVELS$berbahaya$warna)
}

#' Mengambil warna teks kontras untuk kategori AQI
get_warna_teks_aqi <- function(aqi) {
  if (is.na(aqi)) return("#1B2733")
  if (aqi <= 150) return("#1B2733")
  return("#FFFFFF")
}

#' Komponen Badge Kategori AQI
badge_kategori <- function(kategori) {
  warna_bg <- switch(
    kategori,
    "Baik" = AQI_LEVELS$baik$warna,
    "Sedang" = AQI_LEVELS$sedang$warna,
    "Tidak Sehat (Sensitif)" = AQI_LEVELS$sensitif$warna,
    "Tidak Sehat" = AQI_LEVELS$tidak_sehat$warna,
    "Sangat Tidak Sehat" = AQI_LEVELS$sangat_tidak_sehat$warna,
    "Berbahaya" = AQI_LEVELS$berbahaya$warna,
    "#CBD5E1"
  )
  warna_teks <- if (kategori %in% c("Tidak Sehat", "Sangat Tidak Sehat", "Berbahaya")) "#FFFFFF" else "#1B2733"
  
  tags$span(
    class = "badge aqi-badge",
    style = sprintf("background-color: %s; color: %s; font-weight: 600; padding: 6px 12px; border-radius: 6px;", warna_bg, warna_teks),
    kategori
  )
}

#' Badge Status Uji Asumsi (Terpenuhi / Tidak Terpenuhi)
badge_asumsi <- function(lolos, catatan = NULL) {
  if (isTRUE(lolos)) {
    badge <- tags$span(
      class = "badge bg-success-subtle text-success-emphasis border border-success-subtle",
      style = "font-weight: 600; padding: 5px 10px; font-size: 0.85rem;",
      HTML("&#10003; Terpenuhi")
    )
  } else {
    badge <- tags$span(
      class = "badge bg-danger-subtle text-danger-emphasis border border-danger-subtle",
      style = "font-weight: 600; padding: 5px 10px; font-size: 0.85rem;",
      HTML("&#10007; Tidak terpenuhi")
    )
  }
  
  if (!is.null(catatan) && nchar(catatan) > 0) {
    tagList(
      badge,
      tags$div(class = "text-muted small mt-1", catatan)
    )
  } else {
    badge
  }
}

#' Format Angka Standar Indonesia (koma untuk desimal, titik untuk ribuan)
format_id <- function(x, digits = 1) {
  if (length(x) == 0) return("-")
  res <- sapply(x, function(val) {
    if (is.na(val)) return("-")
    formatC(as.numeric(val), format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  }, USE.NAMES = FALSE)
  if (length(res) == 1) res[[1]] else res
}

#' Format Persentase Indonesia
format_persen_id <- function(x, digits = 1) {
  if (length(x) == 0) return("-%")
  res <- sapply(x, function(val) {
    if (is.na(val)) return("-%")
    paste0(formatC(as.numeric(val) * 100, format = "f", digits = digits, big.mark = ".", decimal.mark = ","), "%")
  }, USE.NAMES = FALSE)
  if (length(res) == 1) res[[1]] else res
}

#' Komponen Kartu Interpretasi Statistik
#' Memiliki aksen visual khusus dan menekankan istilah 'asosiasi'
kartu_interpretasi <- function(teks, tipe = c("info", "warning", "success"), judul = "Interpretasi Asosiasi Statistik") {
  tipe <- match.arg(tipe)
  
  ikon <- switch(
    tipe,
    "info" = tags$span(style = "color: #1F3A5F; font-size: 1.25rem; margin-right: 8px;", HTML("&#9432;")),
    "warning" = tags$span(style = "color: #D97706; font-size: 1.25rem; margin-right: 8px;", HTML("&#9888;")),
    "success" = tags$span(style = "color: #16A34A; font-size: 1.25rem; margin-right: 8px;", HTML("&#10003;"))
  )
  
  tags$div(
    class = paste("kartu-interpretasi", paste0("kartu-interpretasi-", tipe)),
    tags$div(
      class = "kartu-interpretasi-header d-flex align-items-center mb-1",
      ikon,
      tags$strong(style = "color: #1F3A5F; font-size: 0.95rem;", judul)
    ),
    tags$div(
      class = "kartu-interpretasi-body",
      style = "color: #1B2733; font-size: 0.92rem; line-height: 1.55;",
      teks
    )
  )
}

#' Banner Status Data Contoh (Wajib Tampil jika DATA_CONTOH == TRUE)
banner_data_contoh <- function() {
  if (exists("DATA_CONTOH") && isTRUE(DATA_CONTOH)) {
    keterangan_hotspot <- if (exists("HOTSPOTS_CONTOH") && isTRUE(HOTSPOTS_CONTOH)) {
      "Titik panas juga menggunakan data contoh."
    } else {
      "Titik panas bersumber dari NASA FIRMS."
    }

    tags$div(
      class = "banner-data-contoh",
      tags$div(
        class = "d-flex align-items-center justify-content-between flex-wrap gap-2",
        tags$div(
          tags$span(class = "badge bg-warning text-dark me-2", "MODE SIMULASI"),
          tags$strong("DATA CONTOH (bukan data sebenarnya)"),
          tags$span(" — Data harian dan analisis memakai data sintetis. ", keterangan_hotspot)
        ),
        tags$span(class = "badge bg-secondary-subtle text-secondary small", "Tangkapan Layar: Data Contoh")
      )
    )
  } else {
    NULL
  }
}

#' Footer Wajib untuk Semua Tab dan Halaman
footer_atribusi <- function() {
  tags$footer(
    class = "footer-atribusi text-muted mt-4 pt-3 pb-3 border-top",
    tags$div(
      class = "container-fluid px-0",
      tags$p(
        class = "mb-1 small",
        HTML("<strong>Sumber Data:</strong> Kualitas udara dari stasiun BMKG melalui WAQI (aqicn.org), <em>belum terverifikasi</em>. Titik panas dari NASA FIRMS (sensor MODIS/VIIRS). Cuaca harian dari Open-Meteo.")
      ),
      tags$p(
        class = "mb-0 small text-secondary",
        HTML("<strong>Pernyataan Etika Analisis:</strong> Seluruh hasil analisis menyajikan <em>asosiasi statistik</em>, bukan kesimpulan sebab-akibat langsung. Titik panas mencerminkan <em>piksel satelit yang terdeteksi anomali panas</em>, bukan jumlah kebakaran dan bukan luas area terbakar riil.")
      )
    )
  )
}

#' Koordinat Pusat Kota untuk Peta Leaflet
get_koordinat_kota <- function(kota) {
  coords <- list(
    "Palangkaraya" = list(lat = -2.2096, lon = 113.9213, zoom = 9),
    "Pontianak"    = list(lat = -0.0263, lon = 109.3425, zoom = 9),
    "Bandung"      = list(lat = -6.9175, lon = 107.6191, zoom = 9)
  )
  if (kota %in% names(coords)) coords[[kota]] else coords[["Palangkaraya"]]
}

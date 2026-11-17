# R/data_source.R
# Fungsi adaptor perantara antara UI dan sumber data.
# Seluruh komponen antarmuka HANYA mengakses data lewat fungsi-fungsi ini.

#' Mengambil tabel data harian kualitas udara, cuaca, dan indikator kebakaran
ambil_data_harian <- function() {
  if (exists("DATA_CONTOH") && isTRUE(DATA_CONTOH)) {
    DUMMY_HARIAN
  } else {
    readRDS("data/data_harian.rds")
  }
}

#' Mengambil titik-titik panas satelit (NASA FIRMS)
ambil_hotspots <- function() {
  if (exists("DATA_CONTOH") && isTRUE(DATA_CONTOH)) {
    DUMMY_HOTSPOTS
  } else {
    readRDS("data/hotspots.rds")
  }
}

#' Mengambil ringkasan hasil analisis statistik
ambil_analisis <- function() {
  # Hasil statistik statis untuk mockup. Pada versi penuh, bagian ini dihapus
  # dan server memanggil fungsi stats_*.R dengan data terfilter.
  DUMMY_ANALISIS
}

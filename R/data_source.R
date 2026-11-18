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
  if (exists("HOTSPOTS_CONTOH") && isTRUE(HOTSPOTS_CONTOH)) {
    DUMMY_HOTSPOTS
  } else {
    readRDS("data/hotspots.rds")
  }
}

#' Mengunduh data titik panas FIRMS untuk kota dan rentang tanggal terpilih
unduh_hotspots_firms <- function(
  tanggal_mulai,
  tanggal_selesai,
  kota = c("Palangkaraya", "Pontianak", "Bandung"),
  file = "data/hotspots.rds"
) {
  map_key <- Sys.getenv("FIRMS_MAP_KEY")
  if (!nzchar(map_key)) {
    stop("FIRMS_MAP_KEY belum tersedia. Isi di .Renviron lalu restart sesi R.", call. = FALSE)
  }

  tanggal_mulai <- as.Date(tanggal_mulai)
  tanggal_selesai <- as.Date(tanggal_selesai)
  if (length(tanggal_mulai) != 1L || is.na(tanggal_mulai) ||
      length(tanggal_selesai) != 1L || is.na(tanggal_selesai) ||
      tanggal_mulai > tanggal_selesai) {
    stop("Rentang tanggal tidak valid.", call. = FALSE)
  }
  if (tanggal_selesai > Sys.Date()) {
    stop("Tanggal akhir tidak boleh melewati hari ini.", call. = FALSE)
  }

  koordinat <- list(
    Palangkaraya = c(lat = -2.2096, lon = 113.9213),
    Pontianak = c(lat = -0.0263, lon = 109.3425),
    Bandung = c(lat = -6.9175, lon = 107.6191)
  )
  kota <- unique(kota)
  if (length(kota) == 0L || any(!kota %in% names(koordinat))) {
    stop("Pilih setidaknya satu kota yang didukung.", call. = FALSE)
  }

  tanggal_chunk <- seq(tanggal_mulai, tanggal_selesai, by = 5)
  hasil <- list()
  indeks <- 1L

  for (nama_kota in kota) {
    pusat <- koordinat[[nama_kota]]
    area <- paste(
      pusat[["lon"]] - 1.3,
      pusat[["lat"]] - 1.3,
      pusat[["lon"]] + 1.3,
      pusat[["lat"]] + 1.3,
      sep = ","
    )

    for (tanggal in tanggal_chunk) {
      tanggal <- as.Date(tanggal, origin = "1970-01-01")
      hari <- min(5L, as.integer(tanggal_selesai - tanggal) + 1L)
      url <- paste0(
        "https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
        utils::URLencode(map_key, reserved = TRUE),
        "/VIIRS_SNPP_SP/",
        area,
        "/",
        hari,
        "/",
        format(tanggal, "%Y-%m-%d")
      )

      respons <- tryCatch(
        httr::GET(url, httr::timeout(30)),
        error = function(e) {
          stop("Tidak dapat menghubungi NASA FIRMS API.", call. = FALSE)
        }
      )
      if (httr::status_code(respons) != 200L) {
        stop(
          sprintf("NASA FIRMS API mengembalikan HTTP %s.", httr::status_code(respons)),
          call. = FALSE
        )
      }

      teks <- httr::content(respons, as = "text", encoding = "UTF-8")
      if (!nzchar(trimws(teks))) next

      data <- utils::read.csv(text = teks, stringsAsFactors = FALSE)
      kolom_wajib <- c("latitude", "longitude", "acq_date", "confidence", "frp")
      if (!all(kolom_wajib %in% names(data))) {
        stop("Format respons NASA FIRMS tidak sesuai dengan kolom yang diharapkan.", call. = FALSE)
      }

      hasil[[indeks]] <- data.frame(
        tanggal = as.Date(data$acq_date),
        latitude = as.numeric(data$latitude),
        longitude = as.numeric(data$longitude),
        confidence = tolower(as.character(data$confidence)),
        frp = as.numeric(data$frp)
      )
      indeks <- indeks + 1L
    }
  }

  hotspot <- if (length(hasil) > 0L) {
    do.call(rbind, hasil)
  } else {
    data.frame(
      tanggal = as.Date(character()),
      latitude = numeric(),
      longitude = numeric(),
      confidence = character(),
      frp = numeric()
    )
  }

  direktori <- dirname(file)
  if (!dir.exists(direktori)) {
    dir.create(direktori, recursive = TRUE)
  }
  saveRDS(hotspot, file)
  message(sprintf("%s titik panas tersimpan di %s.", nrow(hotspot), file))
  invisible(hotspot)
}

#' Mengambil ringkasan hasil analisis statistik
ambil_analisis <- function() {
  # Hasil statistik statis untuk mockup. Pada versi penuh, bagian ini dihapus
  # dan server memanggil fungsi stats_*.R dengan data terfilter.
  DUMMY_ANALISIS
}

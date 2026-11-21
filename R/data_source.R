# R/data_source.R
# Fungsi adaptor perantara antara UI dan sumber data.
# Seluruh komponen antarmuka HANYA mengakses data lewat fungsi-fungsi ini.

#' Koordinat acuan pusat kota pemantauan kualitas udara dan karhutla
KOORDINAT_KOTA <- list(
  Palangkaraya = c(lat = -2.2083, lon = 113.9167),
  Pontianak    = c(lat = -0.0319, lon = 109.3250),
  Bandung      = c(lat = -6.9222, lon = 107.6069)
)

#' Normalisasi nama kota agar fleksibel terhadap spasi / variasi nama
normalisasi_nama_kota <- function(kota) {
  sapply(kota, function(k) {
    k_clean <- tolower(gsub("[^a-z]", "", k))
    if (grepl("palang", k_clean)) "Palangkaraya"
    else if (grepl("ponti", k_clean)) "Pontianak"
    else if (grepl("band", k_clean)) "Bandung"
    else k
  }, USE.NAMES = FALSE)
}

#' Mengambil tabel data harian kualitas udara, cuaca, dan indikator kebakaran
ambil_data_harian <- function() {
  df <- if (exists("DATA_CONTOH") && isTRUE(DATA_CONTOH)) {
    DUMMY_HARIAN
  } else if (file.exists("data/data_harian.rds")) {
    readRDS("data/data_harian.rds")
  } else if (file.exists("data/kualitas_udara.rds")) {
    readRDS("data/kualitas_udara.rds")
  } else {
    readRDS("data/data_harian.rds")
  }

  # Pastikan kolom cuaca & kebakaran terisi nilai default jika belum digabungkan
  if (!"titik_panas" %in% names(df)) df$titik_panas <- 0L
  if (!"frp_total" %in% names(df)) df$frp_total <- 0.0
  if (!"hujan_mm" %in% names(df)) df$hujan_mm <- 0.0
  if (!"suhu_c" %in% names(df)) df$suhu_c <- 28.0
  if (!"angin_maks" %in% names(df)) df$angin_maks <- 10.0

  df
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

  kota_norm <- unique(normalisasi_nama_kota(kota))
  if (length(kota_norm) == 0L || any(!kota_norm %in% names(KOORDINAT_KOTA))) {
    stop("Pilih setidaknya satu kota yang didukung: Palangkaraya (Palangka Raya), Pontianak, Bandung.", call. = FALSE)
  }

  tanggal_chunk <- seq(tanggal_mulai, tanggal_selesai, by = 5)
  hasil <- list()
  indeks <- 1L

  for (nama_kota in kota_norm) {
    pusat <- KOORDINAT_KOTA[[nama_kota]]
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

#' Tabel Breakpoint Resmi Standar US EPA (EPA-454/B-18-007)
BREAKPOINTS_PM25_EPA <- data.frame(
  bp_lo = c(0.0,   12.1,  35.5,  55.5,  150.5, 250.5, 350.5),
  bp_hi = c(12.0,  35.4,  55.4,  150.4, 250.4, 350.4, 500.4),
  i_lo  = c(0,     51,    101,   151,   201,   301,   401),
  i_hi  = c(50,    100,   150,   200,   300,   400,   500),
  kategori = c("Baik", "Sedang", "Tidak Sehat (Sensitif)", "Tidak Sehat", "Sangat Tidak Sehat", "Berbahaya", "Berbahaya"),
  stringsAsFactors = FALSE
)

BREAKPOINTS_PM25_EPA_2024 <- data.frame(
  bp_lo = c(0.0,   9.1,   35.5,  55.5,  125.5, 225.5, 325.5),
  bp_hi = c(9.0,   35.4,  55.4,  125.4, 225.4, 325.4, 500.4),
  i_lo  = c(0,     51,    101,   151,   201,   301,   401),
  i_hi  = c(50,    100,   150,   200,   300,   400,   500),
  kategori = c("Baik", "Sedang", "Tidak Sehat (Sensitif)", "Tidak Sehat", "Sangat Tidak Sehat", "Berbahaya", "Berbahaya"),
  stringsAsFactors = FALSE
)

BREAKPOINTS_PM10_EPA <- data.frame(
  bp_lo = c(0,     55,    155,   255,   355,   425,   505),
  bp_hi = c(54,    154,   254,   354,   424,   504,   604),
  i_lo  = c(0,     51,    101,   151,   201,   301,   401),
  i_hi  = c(50,    100,   150,   200,   300,   400,   500),
  kategori = c("Baik", "Sedang", "Tidak Sehat (Sensitif)", "Tidak Sehat", "Sangat Tidak Sehat", "Berbahaya", "Berbahaya"),
  stringsAsFactors = FALSE
)

#' Menghitung AQI Standar dari Konsentrasi Partikulat
#' Mengikuti rumus interpolasi linier baku US EPA (EPA-454/B-18-007):
#' I = ((I_hi - I_lo) / (BP_hi - BP_lo)) * (C - BP_lo) + I_lo
#'
#' @param konsentrasi Vektor numerik konsentrasi polutan (µg/m³)
#' @param polutan Jenis polutan: "pm25" atau "pm10" (default: "pm25")
#' @param standar Standar breakpoint PM2.5: "us_epa" (klasik/AirNow) atau "us_epa_2024" (revisi NAAQS 2024)
#' @param ekstrapolasi_ekstrem Apakah melakukan ekstrapolasi linear jika nilai melampaui batas tabel (default: TRUE)
#' @return Vektor integer AQI (0–500+)
hitung_aqi_standar <- function(
  konsentrasi,
  polutan = c("pm25", "pm10"),
  standar = c("us_epa", "us_epa_2024"),
  ekstrapolasi_ekstrem = TRUE
) {
  polutan <- match.arg(polutan)
  standar <- match.arg(standar)

  bp_table <- if (polutan == "pm10") {
    BREAKPOINTS_PM10_EPA
  } else if (standar == "us_epa_2024") {
    BREAKPOINTS_PM25_EPA_2024
  } else {
    BREAKPOINTS_PM25_EPA
  }

  sapply(konsentrasi, function(c_raw) {
    if (is.na(c_raw) || c_raw < 0) return(NA_integer_)

    # Sesuai pedoman US EPA: PM2.5 dipotong (truncated) ke 1 desimal, PM10 ke integer
    c_val <- if (polutan == "pm25") {
      trunc(c_raw * 10) / 10
    } else {
      trunc(c_raw)
    }

    n_row <- nrow(bp_table)

    # Penanganan konsentrasi ekstrem di atas batas tertinggi tabel (Hazardous ekstrapolasi)
    if (c_val > bp_table$bp_hi[n_row]) {
      if (isTRUE(ekstrapolasi_ekstrem)) {
        slope <- (bp_table$i_hi[n_row] - bp_table$i_lo[n_row]) / (bp_table$bp_hi[n_row] - bp_table$bp_lo[n_row])
        aqi_extrapolated <- round(bp_table$i_hi[n_row] + slope * (c_val - bp_table$bp_hi[n_row]))
        return(as.integer(aqi_extrapolated))
      } else {
        return(as.integer(bp_table$i_hi[n_row]))
      }
    }

    # Cari segmen breakpoint yang memuat konsentrasi c_val
    idx <- which(c_val <= bp_table$bp_hi)
    idx <- if (length(idx) == 0L) n_row else idx[1]

    bp_lo <- bp_table$bp_lo[idx]
    bp_hi <- bp_table$bp_hi[idx]
    i_lo  <- bp_table$i_lo[idx]
    i_hi  <- bp_table$i_hi[idx]

    # Rumus interpolasi linear baku US EPA
    aqi_num <- ((i_hi - i_lo) / (bp_hi - bp_lo)) * (c_val - bp_lo) + i_lo
    as.integer(round(aqi_num))
  }, USE.NAMES = FALSE)
}

#' Menghitung AQI PM2.5 menurut standar US EPA (breakpoint 24 jam)
hitung_aqi_pm25 <- function(konsentrasi, standar = "us_epa") {
  hitung_aqi_standar(konsentrasi, polutan = "pm25", standar = standar)
}

#' Menghitung AQI PM10 menurut standar US EPA (breakpoint 24 jam)
hitung_aqi_pm10 <- function(konsentrasi) {
  hitung_aqi_standar(konsentrasi, polutan = "pm10")
}

#' Mengunduh data kualitas udara (PM2.5 dan PM10) dari Open-Meteo Air Quality API
#'
#' Contoh request URL:
#' https://air-quality-api.open-meteo.com/v1/air-quality?latitude=-0.0319&longitude=109.325&hourly=pm10,pm2_5&start_date=2026-10-01&end_date=2026-10-31
#'
#' @param tanggal_mulai Tanggal awal ("YYYY-MM-DD" atau Date)
#' @param tanggal_selesai Tanggal akhir ("YYYY-MM-DD" atau Date)
#' @param kota Vektor nama kota yang didukung (default: Palangkaraya, Pontianak, Bandung)
#' @param file Lokasi penyimpanan file .rds keluaran (default: "data/kualitas_udara.rds")
#' @param timezone Zona waktu API (default: "Asia/Jakarta")
#' @param agregasi_harian Apakah agregasi data per jam menjadi rata-rata harian (default: TRUE)
#' @param standar Standar breakpoint PM2.5: "us_epa" (default) atau "us_epa_2024"
#' @return Data frame kualitas udara (invisibly)
unduh_kualitas_udara_open_meteo <- function(
  tanggal_mulai,
  tanggal_selesai,
  kota = c("Palangkaraya", "Pontianak", "Bandung"),
  file = "data/kualitas_udara.rds",
  timezone = "Asia/Jakarta",
  agregasi_harian = TRUE,
  standar = c("us_epa", "us_epa_2024")
) {
  standar <- match.arg(standar)
  tanggal_mulai <- as.Date(tanggal_mulai)
  tanggal_selesai <- as.Date(tanggal_selesai)
  if (length(tanggal_mulai) != 1L || is.na(tanggal_mulai) ||
      length(tanggal_selesai) != 1L || is.na(tanggal_selesai) ||
      tanggal_mulai > tanggal_selesai) {
    stop("Rentang tanggal tidak valid (pastikan tanggal_mulai <= tanggal_selesai).", call. = FALSE)
  }

  kota_norm <- unique(normalisasi_nama_kota(kota))
  if (length(kota_norm) == 0L || any(!kota_norm %in% names(KOORDINAT_KOTA))) {
    stop("Pilih setidaknya satu kota yang didukung: Palangkaraya (Palangka Raya), Pontianak, Bandung.", call. = FALSE)
  }

  hasil_list <- list()

  for (nama_kota in kota_norm) {
    pusat <- KOORDINAT_KOTA[[nama_kota]]
    url <- sprintf(
      "https://air-quality-api.open-meteo.com/v1/air-quality?latitude=%.4f&longitude=%.4f&hourly=pm10,pm2_5&start_date=%s&end_date=%s&timezone=%s",
      pusat[["lat"]],
      pusat[["lon"]],
      format(tanggal_mulai, "%Y-%m-%d"),
      format(tanggal_selesai, "%Y-%m-%d"),
      utils::URLencode(timezone, reserved = TRUE)
    )

    message(sprintf("Mengunduh data kualitas udara Open-Meteo untuk %s...", nama_kota))

    respons <- tryCatch(
      httr::GET(url, httr::timeout(30)),
      error = function(e) {
        stop(sprintf("Tidak dapat menghubungi Open-Meteo Air Quality API untuk %s: %s", nama_kota, e$message), call. = FALSE)
      }
    )

    if (httr::status_code(respons) != 200L) {
      stop(
        sprintf("Open-Meteo API mengembalikan HTTP %s untuk kota %s.", httr::status_code(respons), nama_kota),
        call. = FALSE
      )
    }

    teks <- httr::content(respons, as = "text", encoding = "UTF-8")
    data_json <- jsonlite::fromJSON(teks)

    if (!"hourly" %in% names(data_json) ||
        !"pm2_5" %in% names(data_json$hourly) ||
        !"pm10" %in% names(data_json$hourly)) {
      stop("Respons Open-Meteo tidak memiliki kolom hourly$pm2_5 atau hourly$pm10 yang diharapkan.", call. = FALSE)
    }

    df_kota_jam <- data.frame(
      kota = nama_kota,
      waktu = as.character(data_json$hourly$time),
      tanggal = as.Date(substr(data_json$hourly$time, 1, 10)),
      pm10 = as.numeric(data_json$hourly$pm10),
      pm2_5 = as.numeric(data_json$hourly$pm2_5),
      stringsAsFactors = FALSE
    )

    if (isTRUE(agregasi_harian)) {
      df_kota_harian <- df_kota_jam |>
        dplyr::group_by(kota, tanggal) |>
        dplyr::summarise(
          pm25_ugm3    = round(mean(pm2_5, na.rm = TRUE), 2),
          pm10_ugm3    = round(mean(pm10, na.rm = TRUE), 2),
          pm25         = as.numeric(hitung_aqi_pm25(mean(pm2_5, na.rm = TRUE), standar = standar)),
          pm10_aqi     = as.numeric(hitung_aqi_pm10(mean(pm10, na.rm = TRUE))),
          aqi_gabungan = pmax(pm25, pm10_aqi, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          pm10 = pm10_ugm3,
          n_stasiun = 1L,
          sumber = "open-meteo"
        )
      hasil_list[[nama_kota]] <- df_kota_harian
    } else {
      hasil_list[[nama_kota]] <- df_kota_jam
    }
  }

  hasil <- if (length(hasil_list) > 0L) {
    dplyr::bind_rows(hasil_list)
  } else {
    data.frame()
  }

  if (isTRUE(agregasi_harian) && nrow(hasil) > 0L) {
    hasil <- hasil |>
      dplyr::group_by(kota) |>
      dplyr::mutate(
        batas_ekstrem = quantile(pm25, 0.75, na.rm = TRUE) + 3 * IQR(pm25, na.rm = TRUE),
        outlier = pm25 > batas_ekstrem
      ) |>
      dplyr::ungroup()
  }

  direktori <- dirname(file)
  if (!dir.exists(direktori)) {
    dir.create(direktori, recursive = TRUE)
  }
  saveRDS(hasil, file)
  message(sprintf("%s baris data kualitas udara tersimpan di %s.", nrow(hasil), file))
  invisible(hasil)
}

#' Alias singkat untuk unduh_kualitas_udara_open_meteo
unduh_kualitas_udara <- unduh_kualitas_udara_open_meteo

#' Mengambil ringkasan hasil analisis statistik
ambil_analisis <- function() {
  # Hasil statistik statis untuk mockup. Pada versi penuh, bagian ini dihapus
  # dan server memanggil fungsi stats_*.R dengan data terfilter.
  DUMMY_ANALISIS
}

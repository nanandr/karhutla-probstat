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

#' Koordinat pusat kota untuk permintaan data NASA FIRMS
KOORDINAT_HOTSPOTS_FIRMS <- list(
  Palangkaraya = c(lat = -2.2096, lon = 113.9213),
  Pontianak = c(lat = -0.0263, lon = 109.3425),
  Bandung = c(lat = -6.9175, lon = 107.6191)
)

#' Mengunduh data titik panas FIRMS untuk kota dan rentang tanggal terpilih
unduh_hotspots_firms <- function(
  tanggal_mulai,
  tanggal_selesai,
  kota = c("Palangkaraya", "Pontianak", "Bandung"),
  file = "data/hotspots.rds",
  produk = "VIIRS_NOAA20_SP"
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

  kota <- unique(kota)
  if (length(kota) == 0L || any(!kota %in% names(KOORDINAT_HOTSPOTS_FIRMS))) {
    stop("Pilih setidaknya satu kota yang didukung.", call. = FALSE)
  }
  produk_didukung <- c(
    "VIIRS_SNPP_SP",
    "VIIRS_SNPP_NRT",
    "VIIRS_NOAA20_SP",
    "VIIRS_NOAA21_SP",
    "VIIRS_NOAA20_NRT",
    "VIIRS_NOAA21_NRT"
  )
  if (length(produk) != 1L || !produk %in% produk_didukung) {
    stop("Produk NASA FIRMS tidak didukung.", call. = FALSE)
  }

  tanggal_chunk <- seq(tanggal_mulai, tanggal_selesai, by = 5)
  hasil <- list()
  indeks <- 1L

  for (nama_kota in kota) {
    pusat <- KOORDINAT_HOTSPOTS_FIRMS[[nama_kota]]
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
        "/",
        produk,
        "/",
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
        frp = as.numeric(data$frp),
        produk = rep(produk, nrow(data))
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
      frp = numeric(),
      produk = character()
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

#' Mengambil rentang tanggal yang tersedia untuk produk FIRMS
ketersediaan_hotspots_firms <- function() {
  map_key <- Sys.getenv("FIRMS_MAP_KEY")
  if (!nzchar(map_key)) {
    stop("FIRMS_MAP_KEY belum tersedia. Isi di .Renviron lalu restart sesi R.", call. = FALSE)
  }

  url <- paste0(
    "https://firms.modaps.eosdis.nasa.gov/api/data_availability/csv/",
    utils::URLencode(map_key, reserved = TRUE),
    "/ALL"
  )
  respons <- tryCatch(
    httr::GET(url, httr::timeout(30)),
    error = function(e) {
      stop("Tidak dapat memeriksa ketersediaan data NASA FIRMS.", call. = FALSE)
    }
  )
  if (httr::status_code(respons) != 200L) {
    stop(
      sprintf("NASA FIRMS availability API mengembalikan HTTP %s.", httr::status_code(respons)),
      call. = FALSE
    )
  }

  teks <- httr::content(respons, as = "text", encoding = "UTF-8")
  tersedia <- utils::read.csv(text = teks, stringsAsFactors = FALSE)
  kolom_wajib <- c("data_id", "min_date", "max_date")
  if (!all(kolom_wajib %in% names(tersedia))) {
    stop("Format availability NASA FIRMS tidak sesuai.", call. = FALSE)
  }
  if (nrow(tersedia) == 0L || anyNA(tersedia$min_date) || anyNA(tersedia$max_date)) {
    stop("NASA FIRMS tidak mengembalikan rentang tanggal yang valid.", call. = FALSE)
  }

  hasil <- data.frame(
    produk = as.character(tersedia$data_id),
    tanggal_mulai = as.Date(tersedia$min_date),
    tanggal_selesai = as.Date(tersedia$max_date),
    stringsAsFactors = FALSE
  )
  if (anyNA(hasil$tanggal_mulai) || anyNA(hasil$tanggal_selesai)) {
    stop("NASA FIRMS mengembalikan tanggal availability yang tidak valid.", call. = FALSE)
  }
  hasil
}

#' Menggabungkan hasil FIRMS baru dengan snapshot, mengganti area/tanggal yang diunduh
gabungkan_snapshot_hotspots <- function(
  snapshot,
  terbaru,
  tanggal_mulai,
  tanggal_selesai,
  kota,
  produk,
  hapus_legacy = FALSE
) {
  tanggal_mulai <- as.Date(tanggal_mulai)
  tanggal_selesai <- as.Date(tanggal_selesai)
  kolom_wajib <- c("tanggal", "latitude", "longitude", "confidence", "frp", "produk")
  if (!"produk" %in% names(snapshot)) snapshot$produk <- "legacy"
  if (!all(kolom_wajib %in% names(snapshot)) ||
      !all(kolom_wajib %in% names(terbaru))) {
    stop("Skema snapshot hotspot tidak sesuai.", call. = FALSE)
  }
  snapshot$produk[is.na(snapshot$produk) | !nzchar(snapshot$produk)] <- "legacy"
  if (length(kota) == 0L || any(!kota %in% names(KOORDINAT_HOTSPOTS_FIRMS))) {
    stop("Pilih setidaknya satu kota yang didukung.", call. = FALSE)
  }
  if (length(produk) != 1L || anyNA(terbaru$produk) ||
      any(terbaru$produk != produk)) {
    stop("Produk pada hasil unduhan tidak sesuai dengan permintaan.", call. = FALSE)
  }

  dalam_wilayah <- rep(FALSE, nrow(snapshot))
  for (nama_kota in kota) {
    pusat <- KOORDINAT_HOTSPOTS_FIRMS[[nama_kota]]
    dalam_wilayah <- dalam_wilayah |
      (abs(snapshot$latitude - pusat[["lat"]]) < 1.3 &
         abs(snapshot$longitude - pusat[["lon"]]) < 1.3)
  }
  dalam_rentang <- !is.na(snapshot$tanggal) &
    snapshot$tanggal >= tanggal_mulai &
    snapshot$tanggal <= tanggal_selesai
  produk_diperbarui <- snapshot$produk == produk
  if (hapus_legacy) produk_diperbarui <- produk_diperbarui | snapshot$produk == "legacy"

  dplyr::bind_rows(
    snapshot[!(dalam_wilayah & dalam_rentang & produk_diperbarui), , drop = FALSE],
    terbaru
  ) |>
    dplyr::arrange(tanggal)
}

#' Membaca status pembaruan terakhir, jika tersedia
status_pembaruan_hotspots <- function(file = "data/hotspots.rds") {
  file_status <- file.path(dirname(file), "hotspots-refresh-status.rds")
  if (!file.exists(file_status)) return(NULL)
  readRDS(file_status)
}

#' Menyimpan status pembaruan tanpa menampilkan credential
simpan_status_pembaruan_hotspots <- function(
  status,
  file = "data/hotspots.rds"
) {
  file_status <- file.path(dirname(file), "hotspots-refresh-status.rds")
  saveRDS(status, file_status)
  invisible(status)
}

#' Memperbarui produk FIRMS yang tersedia dan meninjau ulang tujuh hari terakhir
perbarui_hotspots_harian <- function(
  file = "data/hotspots.rds",
  hari_tinjau = 7L,
  batas_backfill_hari = 180L,
  kota = names(KOORDINAT_HOTSPOTS_FIRMS)
) {
  if (length(hari_tinjau) != 1L || is.na(hari_tinjau) ||
      hari_tinjau < 1L || hari_tinjau != as.integer(hari_tinjau)) {
    stop("Jumlah hari tinjau harus bilangan bulat positif.", call. = FALSE)
  }
  if (length(batas_backfill_hari) != 1L || is.na(batas_backfill_hari) ||
      batas_backfill_hari < hari_tinjau ||
      batas_backfill_hari != as.integer(batas_backfill_hari)) {
    stop("Batas backfill harus bilangan bulat dan minimal sama dengan hari tinjau.", call. = FALSE)
  }
  if (length(kota) == 0L || any(!kota %in% names(KOORDINAT_HOTSPOTS_FIRMS))) {
    stop("Pilih setidaknya satu kota yang didukung.", call. = FALSE)
  }

  direktori <- dirname(file)
  if (!dir.exists(direktori) && !dir.create(direktori, recursive = TRUE)) {
    stop("Direktori snapshot hotspot tidak dapat dibuat.", call. = FALSE)
  }

  status <- status_pembaruan_hotspots(file)
  tanggal_status <- if (is.null(status)) {
    as.Date(NA)
  } else if (!is.null(status$tanggal)) {
    as.Date(status$tanggal)
  } else if (!is.null(status$waktu)) {
    as.Date(status$waktu, tz = "")
  } else {
    as.Date(NA)
  }
  if (file.exists(file) && !is.null(status$produk) &&
      isTRUE(status$berhasil) && !is.na(tanggal_status) &&
      tanggal_status >= Sys.Date()) {
    return(invisible(list(diperbarui = FALSE, alasan = "sudah diperbarui hari ini")))
  }

  lock_dir <- paste0(file, ".refresh-lock")
  lock_didapat <- dir.create(lock_dir, showWarnings = FALSE)
  if (!lock_didapat && dir.exists(lock_dir)) {
    umur_lock <- as.numeric(difftime(
      Sys.time(),
      file.info(lock_dir)$mtime,
      units = "hours"
    ))
    if (!is.na(umur_lock) && umur_lock > 3) {
      unlink(lock_dir, recursive = TRUE)
      lock_didapat <- dir.create(lock_dir, showWarnings = FALSE)
    }
  }
  if (!lock_didapat) {
    message("Pembaruan hotspot lain sedang berjalan; permintaan ini dilewati.")
    return(invisible(list(diperbarui = FALSE, alasan = "pembaruan sedang berjalan")))
  }
  on.exit(unlink(lock_dir, recursive = TRUE), add = TRUE)

  status <- status_pembaruan_hotspots(file)
  tanggal_status <- if (is.null(status)) {
    as.Date(NA)
  } else if (!is.null(status$tanggal)) {
    as.Date(status$tanggal)
  } else if (!is.null(status$waktu)) {
    as.Date(status$waktu, tz = "")
  } else {
    as.Date(NA)
  }
  if (file.exists(file) && !is.null(status$produk) &&
      isTRUE(status$berhasil) && !is.na(tanggal_status) &&
      tanggal_status >= Sys.Date()) {
    return(invisible(list(diperbarui = FALSE, alasan = "sudah diperbarui hari ini")))
  }

  if (file.exists(file)) {
    snapshot <- readRDS(file)
    kolom_wajib <- c("tanggal", "latitude", "longitude", "confidence", "frp")
    if (!all(kolom_wajib %in% names(snapshot)) ||
        !inherits(snapshot$tanggal, "Date")) {
      stop("Skema snapshot hotspot tidak sesuai.", call. = FALSE)
    }
    migrasi <- !"produk" %in% names(snapshot)
    if (migrasi) snapshot$produk <- "legacy"
    snapshot$produk[is.na(snapshot$produk) | !nzchar(snapshot$produk)] <- "legacy"
  } else {
    snapshot <- data.frame(
      tanggal = as.Date(character()),
      latitude = numeric(),
      longitude = numeric(),
      confidence = character(),
      frp = numeric(),
      produk = character()
    )
    migrasi <- FALSE
  }

  tanggal_snapshot <- snapshot$tanggal[!is.na(snapshot$tanggal)]
  tanggal_awal_snapshot <- if (length(tanggal_snapshot) > 0L) {
    min(tanggal_snapshot)
  } else {
    as.Date(NA)
  }
  ketersediaan <- ketersediaan_hotspots_firms()
  tanggal_akhir_lengkap <- Sys.Date() - 1
  produk_refresh <- c(
    "VIIRS_SNPP_SP",
    "VIIRS_NOAA20_SP",
    "VIIRS_NOAA21_SP",
    "VIIRS_SNPP_NRT",
    "VIIRS_NOAA20_NRT",
    "VIIRS_NOAA21_NRT"
  )

  file_snapshot_baru <- tempfile(
    pattern = "hotspots-snapshot-",
    tmpdir = direktori,
    fileext = ".rds"
  )
  file_cadangan <- NULL
  on.exit(unlink(file_snapshot_baru, force = TRUE), add = TRUE)
  on.exit({
    if (!is.null(file_cadangan) && file.exists(file_cadangan)) {
      if (file.exists(file)) {
        unlink(file_cadangan)
      } else {
        file.rename(file_cadangan, file)
      }
    }
  }, add = TRUE)
  gabungan <- snapshot
  progres_produk <- if (!is.null(status$produk)) status$produk else list()
  rentang_diperbarui <- list()

  for (produk in produk_refresh) {
    metadata <- ketersediaan[ketersediaan$produk == produk, , drop = FALSE]
    if (nrow(metadata) != 1L) next

    tanggal_min_produk <- as.Date(metadata$tanggal_mulai[1])
    tanggal_selesai_tersedia <- as.Date(metadata$tanggal_selesai[1])
    if (is.na(tanggal_min_produk) || is.na(tanggal_selesai_tersedia)) {
      stop(
        sprintf("Tanggal availability NASA FIRMS tidak valid untuk produk %s.", produk),
        call. = FALSE
      )
    }
    tanggal_akhir_produk <- min(tanggal_selesai_tersedia, tanggal_akhir_lengkap)
    if (grepl("_NRT$", produk)) {
      produk_sp <- sub("_NRT$", "_SP", produk)
      metadata_sp <- ketersediaan[ketersediaan$produk == produk_sp, , drop = FALSE]
      if (nrow(metadata_sp) == 1L) {
        tanggal_selesai_sp <- as.Date(metadata_sp$tanggal_selesai[1])
        if (is.na(tanggal_selesai_sp)) {
          stop(
            sprintf("Tanggal availability NASA FIRMS tidak valid untuk produk %s.", produk_sp),
            call. = FALSE
          )
        }
        tanggal_min_produk <- max(
          tanggal_min_produk,
          tanggal_selesai_sp + 1L
        )
      }
    }

    progres_sebelumnya <- progres_produk[[produk]]
    if (!is.null(progres_sebelumnya$tanggal_selesai)) {
      tanggal_mulai_produk <- max(
        tanggal_min_produk,
        min(
          as.Date(progres_sebelumnya$tanggal_selesai) - hari_tinjau + 1L,
          tanggal_akhir_produk - hari_tinjau + 1L
        )
      )
    } else {
      tanggal_awal_backfill <- tanggal_akhir_produk - batas_backfill_hari + 1L
      if (!is.na(tanggal_awal_snapshot)) {
        tanggal_awal_backfill <- max(tanggal_awal_backfill, tanggal_awal_snapshot)
      }
      tanggal_mulai_produk <- max(tanggal_min_produk, tanggal_awal_backfill)
    }
    if (tanggal_mulai_produk > tanggal_akhir_produk) next

    file_unduhan <- tempfile(
      pattern = "hotspots-download-",
      tmpdir = direktori,
      fileext = ".rds"
    )
    on.exit(unlink(file_unduhan, force = TRUE), add = TRUE)
    terbaru <- unduh_hotspots_firms(
      tanggal_mulai_produk,
      tanggal_akhir_produk,
      kota = kota,
      file = file_unduhan,
      produk = produk
    )
    gabungan <- gabungkan_snapshot_hotspots(
      gabungan,
      terbaru,
      tanggal_mulai_produk,
      tanggal_akhir_produk,
      kota,
      produk = produk,
      hapus_legacy = migrasi
    )
    progres_produk[[produk]] <- list(
      tanggal_selesai = tanggal_akhir_produk,
      tanggal_mulai_tersedia = tanggal_min_produk,
      tanggal_selesai_tersedia = tanggal_selesai_tersedia
    )
    rentang_diperbarui[[produk]] <- c(tanggal_mulai_produk, tanggal_akhir_produk)
  }
  if (length(rentang_diperbarui) == 0L) {
    stop("Tidak ada rentang data NASA FIRMS yang tersedia untuk diperbarui.", call. = FALSE)
  }

  saveRDS(gabungan, file_snapshot_baru)

  if (file.exists(file)) {
    file_cadangan <- tempfile(
      pattern = "hotspots-backup-",
      tmpdir = direktori,
      fileext = ".rds"
    )
    if (!file.rename(file, file_cadangan)) {
      stop("Snapshot lama tidak dapat diamankan sebelum penggantian.", call. = FALSE)
    }
  }
  if (!file.rename(file_snapshot_baru, file)) {
    if (is.null(file_cadangan) || !file.rename(file_cadangan, file)) {
      stop("Snapshot baru gagal dipasang dan pemulihan cadangan gagal.", call. = FALSE)
    }
    file_cadangan <- NULL
    stop("Snapshot baru gagal dipasang; snapshot sebelumnya dipulihkan.", call. = FALSE)
  }
  if (!is.null(file_cadangan)) {
    unlink(file_cadangan)
    file_cadangan <- NULL
  }

  hasil <- list(
    berhasil = TRUE,
    tanggal = Sys.Date(),
    waktu = Sys.time(),
    tanggal_mulai = min(vapply(rentang_diperbarui, function(x) as.numeric(x[1]), numeric(1))),
    tanggal_selesai = max(vapply(rentang_diperbarui, function(x) as.numeric(x[2]), numeric(1))),
    produk = progres_produk,
    jumlah = nrow(gabungan)
  )
  hasil$tanggal_mulai <- as.Date(hasil$tanggal_mulai, origin = "1970-01-01")
  hasil$tanggal_selesai <- as.Date(hasil$tanggal_selesai, origin = "1970-01-01")
  simpan_status_pembaruan_hotspots(hasil, file)
  message(sprintf(
    "Snapshot NASA FIRMS diperbarui hingga %s (%s piksel).",
    format(hasil$tanggal_selesai, "%Y-%m-%d"),
    format(nrow(gabungan), big.mark = ".", decimal.mark = ",")
  ))
  invisible(c(list(diperbarui = TRUE), hasil))
}

#' Menjalankan worker pembaruan tanpa memblokir aplikasi Shiny
jalankan_refresh_hotspots_latar <- function(
  script = "scripts/refresh_hotspots.R"
) {
  if (isTRUE(getOption("karhutla.disable_hotspot_autorefresh"))) {
    return(invisible(FALSE))
  }
  if (!file.exists(script)) {
    stop("Worker pembaruan hotspot tidak ditemukan.", call. = FALSE)
  }

  executable <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  if (!file.exists(executable)) {
    stop("Executable Rscript tidak ditemukan.", call. = FALSE)
  }

  system2(
    executable,
    args = shQuote(normalizePath(script, winslash = "/", mustWork = TRUE)),
    wait = FALSE,
    stdout = FALSE,
    stderr = FALSE
  )
  invisible(TRUE)
}

#' Mengambil ringkasan hasil analisis statistik
ambil_analisis <- function() {
  # Hasil statistik statis untuk mockup. Pada versi penuh, bagian ini dihapus
  # dan server memanggil fungsi stats_*.R dengan data terfilter.
  DUMMY_ANALISIS
}

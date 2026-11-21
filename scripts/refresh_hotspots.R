argumen <- commandArgs(trailingOnly = FALSE)
file_script <- sub("^--file=", "", argumen[grepl("^--file=", argumen)][1])
if (length(file_script) == 0L || is.na(file_script) || !nzchar(file_script)) {
  stop("Lokasi worker pembaruan hotspot tidak dapat ditentukan.", call. = FALSE)
}

direktori_proyek <- normalizePath(
  file.path(dirname(file_script), ".."),
  winslash = "/",
  mustWork = TRUE
)
setwd(direktori_proyek)

jalankan_worker <- function() {
  if (!dir.exists("data") && !dir.create("data", recursive = TRUE)) {
    stop("Direktori data tidak dapat dibuat.", call. = FALSE)
  }

  log_file <- file.path("data", "hotspots-refresh.log")
  log_connection <- file(log_file, open = "at")
  on.exit({
    sink(type = "message")
    while (sink.number() > 0L) sink()
    if (isOpen(log_connection)) close(log_connection)
  }, add = TRUE)
  sink(log_connection, split = TRUE)
  sink(log_connection, type = "message")

  cat(sprintf("[%s] Memulai pembaruan NASA FIRMS.\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  tryCatch(
    {
      source("R/data_source.R")
      hasil <- perbarui_hotspots_harian()
      if (!isTRUE(hasil$diperbarui)) {
        message("Pembaruan hotspot dilewati: ", hasil$alasan)
      }
      0L
    },
    error = function(e) {
      status_sebelumnya <- status_pembaruan_hotspots()
      simpan_status_pembaruan_hotspots(
        list(
          berhasil = FALSE,
          tanggal = Sys.Date(),
          waktu = Sys.time(),
          produk = status_sebelumnya$produk,
          pesan = conditionMessage(e)
        )
      )
      message("Pembaruan NASA FIRMS gagal: ", conditionMessage(e))
      1L
    }
  )
}

if (jalankan_worker() != 0L) {
  quit(save = "no", status = 1L)
}

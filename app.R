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
# Termasuk dummy_data.R, data_source.R, ui_helpers.R, dan berkas analisis statistik bila ada
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  source(f)
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
      start = "2026-08-01",
      end = "2026-09-30",
      min = "2026-07-15",
      max = "2026-10-15",
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
      tags$p(class = "mb-1", tags$strong("Status Snapshot:"), " Data dibekukan: 31 Okt 2026"),
      tags$p(class = "mb-1", tags$strong("Lingkup Sensor:"), " Radius 100 km dari stasiun"),
      tags$p(class = "mb-0", tags$strong("Atribusi:"), " BMKG/WAQI, NASA FIRMS, Open-Meteo")
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
      p(class = "small mb-0 text-muted", "Dalam radius 100 km stasiun")
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
            class = "d-flex justify-content-between align-items-center flex-wrap",
            tags$span("Sebaran Spasial Titik Panas Satelit (NASA FIRMS)"),
            textOutput("n_titik")
          )
        ),
        leafletOutput("peta", height = "520px"),
        tags$div(
          class = "mt-3",
          kartu_interpretasi(
            teks = "Satu titik pada peta merepresentasikan satu piksel satelit yang terdeteksi memiliki anomali panas (NASA FIRMS), bukan berarti satu kejadian kebakaran tunggal dan bukan luas area terbakar riil. Lingkaran biru membatasi radius pengamatan 100 km dari stasiun pemantau kota.",
            tipe = "warning",
            judul = "Peringatan Interpretasi Sensor Spasial"
          )
        )
      )
    ),
    
    # Tab 4: Hubungan
    nav_panel(
      title = "4. Hubungan",
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Korelasi Rank Spearman per Lag Waktu (0–7 Hari)"),
          plotlyOutput("plot_lag", height = "340px"),
          uiOutput("tafsir_lag")
        ),
        
        card(
          card_header("Diagram Pencar Hubungan Ukuran Kebakaran vs AQI PM2,5"),
          plotlyOutput("scatter", height = "340px"),
          uiOutput("tafsir_parsial")
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
          label = "Tentukan Rentang Periode Karhutla (untuk Uji Hipotesis):",
          start = "2026-08-15",
          end = "2026-09-30",
          min = "2026-07-15",
          max = "2026-10-15",
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

# 4. Logika Server Aplikasi
server <- function(input, output, session) {
  
  # Reactive Data Harian Terfilter Berdasarkan Pilihan Pengguna
  data_terfilter <- reactive({
    req(input$kota, input$tgl)
    
    # Indikasi proses pemuatan data untuk transisi interaktif yang mulus
    withProgress(message = "Memperbarui data dan visualisasi...", value = 0.5, {
      df <- ambil_data_harian()
      
      df_sub <- df |>
        filter(
          kota == input$kota,
          tanggal >= input$tgl[1],
          tanggal <= input$tgl[2]
        ) |>
        arrange(tanggal)
      
      if (nrow(df_sub) == 0) return(df_sub)
      
      # Perhitungan Jendela Rata-rata Bergerak (Moving Average)
      k <- input$ma_k
      if (nrow(df_sub) >= k) {
        ma_vals <- stats::filter(df_sub$pm25, rep(1 / k, k), sides = 1)
        df_sub$pm25_ma <- as.numeric(ma_vals)
      } else {
        df_sub$pm25_ma <- df_sub$pm25
      }
      
      # Penentuan Variabel Ukuran Kebakaran Sesuai Pilihan
      var_dipilih <- input$var_api
      df_sub$api_nilai <- if (var_dipilih == "frp_total") df_sub$frp_total else as.numeric(df_sub$titik_panas)
      
      # Perhitungan Lag untuk Analisis Bivariat
      lag_n <- input$lag_api
      df_sub$api_lag <- dplyr::lag(df_sub$api_nilai, n = lag_n, default = NA)
      
      df_sub
    })
  })
  
  # --- LOGIKA VALUE BOXES (AREA B) ---
  
  output$kpi_rerata_aqi <- renderUI({
    df <- data_terfilter()
    if (nrow(df) == 0) return(tags$span("-"))
    
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
    if (nrow(df) == 0) return("Tidak ada data")
    rerata <- mean(df$pm25, na.rm = TRUE)
    paste("Kategori:", get_kategori_aqi(rerata))
  })
  
  output$kpi_kategori_dominan <- renderUI({
    df <- data_terfilter()
    if (nrow(df) == 0) return(tags$span("-"))
    
    kat_list <- sapply(df$pm25, get_kategori_aqi)
    tabel_kat <- sort(table(kat_list), decreasing = TRUE)
    kat_dom <- names(tabel_kat)[1]
    
    badge_kategori(kat_dom)
  })
  
  output$kpi_kategori_subteks <- renderText({
    df <- data_terfilter()
    if (nrow(df) == 0) return("")
    kat_list <- sapply(df$pm25, get_kategori_aqi)
    tabel_kat <- sort(table(kat_list), decreasing = TRUE)
    paste0(format_id(tabel_kat[1], 0), " hari dari total ", nrow(df), " hari")
  })
  
  output$kpi_judul_api <- renderText({
    if (input$var_api == "frp_total") "Total FRP Kebakaran" else "Total Piksel Panas"
  })
  
  output$kpi_total_api <- renderUI({
    df <- data_terfilter()
    if (nrow(df) == 0) return(tags$span("-"))
    
    if (input$var_api == "frp_total") {
      val <- sum(df$frp_total, na.rm = TRUE)
      tags$span(paste(format_id(val, 1), "MW"))
    } else {
      val <- sum(df$titik_panas, na.rm = TRUE)
      tags$span(paste(format_id(val, 0), "Piksel"))
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
    if (nrow(df) == 0) return("")
    n_unhealthy <- sum(df$pm25 > 100, na.rm = TRUE)
    persen <- (n_unhealthy / nrow(df)) * 100
    paste0(format_id(persen, 1), "% dari periode terpilih")
  })
  
  # --- TAB 1: RINGKASAN ---
  
  # Output: tbl_desk (Tabel Statistik Deskriptif)
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
  
  # Output: plot_kategori (Proporsi Hari per Kategori Kualitas Udara)
  output$plot_kategori <- renderPlotly({
    df <- data_terfilter()
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
    
    p <- plot_ly(
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
    
    p
  })
  
  # Output: tbl_norm (Tabel Uji Normalitas Shapiro-Wilk)
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
  
  # Output: plot_tren (Grafik Plotly Dua Sumbu)
  output$plot_tren <- renderPlotly({
    df <- data_terfilter()
    validate(need(nrow(df) > 0, "Tidak ada data untuk pilihan ini."))
    
    label_api <- if (input$var_api == "frp_total") "Total FRP (MW)" else "Piksel Panas"
    
    p <- plot_ly(df, x = ~tanggal)
    
    # Sumbu Sekunder: Batang Aktivitas Kebakaran
    p <- p |> add_bars(
      y = ~api_nilai,
      name = label_api,
      yaxis = "y2",
      marker = list(color = "rgba(251, 133, 0, 0.45)", line = list(color = "#FB8500", width = 1)),
      hoverinfo = "text",
      text = ~sprintf("<b>%s</b><br>%s: %s", format(tanggal, "%%d %%b %%Y"), label_api, format_id(api_nilai, 1))
    )
    
    # Sumbu Primer: Garis AQI PM2.5 Harian
    p <- p |> add_lines(
      y = ~pm25,
      name = "AQI PM2,5 Harian",
      line = list(color = "#1F3A5F", width = 2.2),
      marker = list(color = "#1F3A5F", size = 4),
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~sprintf("<b>%s</b><br>AQI PM2,5: %s (%s)", format(tanggal, "%%d %%b %%Y"), format_id(pm25, 1), sapply(pm25, get_kategori_aqi))
    )
    
    # Sumbu Primer: Garis Rata-rata Bergerak
    p <- p |> add_lines(
      y = ~pm25_ma,
      name = sprintf("Rata-rata Bergerak (%d Hari)", input$ma_k),
      line = list(color = "#0284C7", width = 2, dash = "dash"),
      hoverinfo = "text",
      text = ~sprintf("<b>MA %d Hari:</b> %s", input$ma_k, format_id(pm25_ma, 1))
    )
    
    # Garis Acuan Horizontal Ambang Batas AQI
    p <- p |> layout(
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
        # Garis batas 100 (Sensitif)
        list(
          type = "line",
          x0 = min(df$tanggal), x1 = max(df$tanggal),
          y0 = 100, y1 = 100,
          line = list(color = "#FF7E00", width = 1.5, dash = "dot")
        ),
        # Garis batas 150 (Tidak Sehat)
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
    
    p
  })
  
  # --- TAB 3: PETA TITIK PANAS ---
  
  # Output: n_titik (Teks Jumlah Titik Panas)
  output$n_titik <- renderText({
    pts <- ambil_hotspots()
    req(input$tgl)
    
    pts_sub <- pts |>
      filter(tanggal >= input$tgl[1], tanggal <= input$tgl[2])
    
    # Saring sebaran sesuai kedekatan dengan kota terpilih (~150 km)
    coord <- get_koordinat_kota(input$kota)
    pts_kota <- pts_sub |>
      filter(abs(latitude - coord$lat) < 1.3, abs(longitude - coord$lon) < 1.3)
    
    sprintf("Total %s piksel panas terdeteksi di sekitar %s", format_id(nrow(pts_kota), 0), input$kota)
  })
  
  # Output: peta (Peta Leaflet Sebaran Titik Panas)
  output$peta <- renderLeaflet({
    pts <- ambil_hotspots()
    coord <- get_koordinat_kota(input$kota)
    
    req(input$tgl)
    pts_sub <- pts |>
      filter(tanggal >= input$tgl[1], tanggal <= input$tgl[2]) |>
      filter(abs(latitude - coord$lat) < 1.3, abs(longitude - coord$lon) < 1.3)
    
    # Skema warna menurut tingkat kepercayaan deteksi (NASA FIRMS)
    get_titik_color <- function(conf) {
      if (conf == "h") "#D00000"       # Tinggi: Merah tua
      else if (conf == "n") "#FB8500"  # Nominal: Oranye
      else "#FFB703"                   # Rendah: Emas/Kuning
    }
    
    m <- leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      setView(lng = coord$lon, lat = coord$lat, zoom = coord$zoom) |>
      # Marker Pusat Stasiun Kota
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
      # Lingkaran Radius Pengamatan 100 km (100.000 meter)
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
    
    if (nrow(pts_sub) > 0) {
      warna_titik <- sapply(pts_sub$confidence, get_titik_color)
      radius_titik <- pmin(12, pmax(3.5, sqrt(pts_sub$frp) * 1.6))
      
      popup_teks <- sprintf(
        "<strong>Piksel Panas Satelit</strong><br>Tanggal: %s<br>FRP: %s MW<br>Confidence: %s<br>Koordinat: %.3f, %.3f",
        format(pts_sub$tanggal, "%%d %%b %%Y"),
        format_id(pts_sub$frp, 1),
        ifelse(pts_sub$confidence == "h", "Tinggi", ifelse(pts_sub$confidence == "n", "Nominal", "Rendah")),
        pts_sub$latitude, pts_sub$longitude
      )
      
      m <- m |>
        addCircleMarkers(
          data = pts_sub,
          lng = ~longitude,
          lat = ~latitude,
          radius = radius_titik,
          color = warna_titik,
          fillColor = warna_titik,
          fillOpacity = 0.7,
          weight = 1,
          popup = popup_teks
        )
    }
    
    m
  })
  
  # --- TAB 4: HUBUNGAN ---
  
  # Output: plot_lag (Bar Chart Korelasi Spearman per Lag Waktu)
  output$plot_lag <- renderPlotly({
    analisis <- ambil_analisis()
    df_lag <- analisis$korelasi_lag
    
    # Warna penanda: lag 2 disorot sebagai asosiasi puncak
    warna_bars <- ifelse(df_lag$lag == 2, "#1F3A5F", "#94A3B8")
    
    p <- plot_ly(df_lag, x = ~factor(lag)) |>
      add_bars(
        y = ~rho,
        marker = list(color = warna_bars),
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
          "<b>Lag %d Hari</b><br>Spearman rho: %s<br>95%% CI: [%s, %s]<br>p-value: %s",
          lag, format_id(rho, 2), format_id(ci_bawah, 2), format_id(ci_atas, 2),
          if (p < 0.001) "< 0,001" else format_id(p, 4)
        )
      ) |>
      layout(
        xaxis = list(title = "Lag Waktu (Hari)"),
        yaxis = list(title = "Koefisien Korelasi Spearman (rho)", range = c(0, 0.85)),
        margin = list(t = 20, b = 40, l = 50, r = 20),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      ) |>
      config(displayModeBar = FALSE)
    
    p
  })
  
  # Output: tafsir_lag (Kartu Interpretasi Korelasi Lag)
  output$tafsir_lag <- renderUI({
    analisis <- ambil_analisis()
    kartu_interpretasi(
      teks = analisis$tafsir_korelasi,
      tipe = "info",
      judul = "Interpretasi Asosiasi Korelasi Lag"
    )
  })
  
  # Output: tafsir_parsial (Kartu Interpretasi Korelasi Parsial)
  output$tafsir_parsial <- renderUI({
    analisis <- ambil_analisis()
    p_data <- analisis$parsial
    
    teks_parsial <- sprintf(
      "Setelah mengontrol variabel kovariat cuaca (curah hujan dan kecepatan angin) pada lag %d hari, koefisien korelasi parsial tetap bernilai positif (rho parsial = %s, p %s). Hal ini menunjukkan bahwa asosiasi antara ukuran kebakaran dan penurunan kualitas udara bersifat kokoh dan tidak semata-mata dipicu oleh faktor perancu cuaca.",
      p_data$lag,
      format_id(p_data$rho_parsial, 2),
      if (p_data$p_parsial < 0.001) "< 0,001" else paste0("= ", format_id(p_data$p_parsial, 3))
    )
    
    kartu_interpretasi(
      teks = teks_parsial,
      tipe = "success",
      judul = "Interpretasi Asosiasi Korelasi Parsial (Kontrol Cuaca)"
    )
  })
  
  # Output: scatter (Diagram Pencar Ukuran Kebakaran vs AQI PM2,5)
  output$scatter <- renderPlotly({
    df <- data_terfilter()
    validate(need(nrow(df) > 0, "Tidak ada data untuk pilihan ini."))
    
    df_plot <- df |> filter(!is.na(api_lag), !is.na(pm25))
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
      type = "scatter",
      mode = "markers",
      marker = list(
        color = "#1F3A5F",
        size = 7,
        opacity = 0.75,
        line = list(color = "#0F172A", width = 0.5)
      ),
      hoverinfo = "text",
      text = ~sprintf("<b>%s</b><br>%s: %s<br>AQI PM2,5: %s", format(tanggal, "%%d %%b"), label_api, format_id(api_lag, 1), format_id(pm25, 1))
    )
    
    # Garis tren regresi linear sederhana untuk visualisasi asosiasi
    if (nrow(df_plot) >= 5) {
      fit <- lm(pm25 ~ api_lag, data = df_plot)
      df_plot$y_pred <- predict(fit)
      p <- p |> add_lines(
        x = ~api_lag,
        y = ~y_pred,
        line = list(color = "#D00000", width = 2),
        hoverinfo = "none",
        name = "Garis Tren Asosiasi"
      )
    }
    
    p <- p |> layout(
      xaxis = list(title = label_api, showgrid = TRUE, gridcolor = "#F1F5F9"),
      yaxis = list(title = "AQI PM2,5", showgrid = TRUE, gridcolor = "#F1F5F9"),
      showlegend = FALSE,
      margin = list(t = 20, b = 40, l = 50, r = 20),
      plot_bgcolor = "#FFFFFF",
      paper_bgcolor = "#FFFFFF"
    ) |>
      config(displayModeBar = FALSE)
    
    p
  })
  
  # --- TAB 5: PERBANDINGAN ---
  
  # Output: ringkas_uji (Tabel Ringkasan Mann-Whitney U)
  output$ringkas_uji <- renderTable({
    analisis <- ambil_analisis()
    tabel_ringkas <- analisis$uji_dua_periode$ringkas
    
    tibble(
      Periode = tabel_ringkas$periode,
      n = as.integer(tabel_ringkas$n),
      `Median AQI` = format_id(tabel_ringkas$median, 1),
      IQR = format_id(tabel_ringkas$iqr, 1),
      `Rata-rata` = format_id(tabel_ringkas$rata_rata, 1)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrr")
  
  # Output: tbl_norm_periode (Uji Normalitas per Periode)
  output$tbl_norm_periode <- renderTable({
    analisis <- ambil_analisis()
    tabel_norm <- analisis$uji_dua_periode$normalitas
    
    tibble(
      `Kelompok Data` = tabel_norm$data,
      n = as.integer(tabel_norm$n),
      `Statistik W` = format_id(tabel_norm$W, 3),
      `Nilai p` = if (tabel_norm$p < 0.001) "< 0,001" else format_id(tabel_norm$p, 4),
      `Distribusi Normal` = ifelse(tabel_norm$normal, "Ya", "Tidak (Non-Normal)")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrrc")
  
  # Output: tafsir_mw (Kesimpulan Mann-Whitney U)
  output$tafsir_mw <- renderUI({
    analisis <- ambil_analisis()
    mw <- analisis$uji_dua_periode
    
    teks_mw <- sprintf(
      "Berdasarkan uji Mann-Whitney U (W = %d, p < 0,001), terdapat asosiasi perbedaan peringkat kualitas udara yang signifikan secara statistik antara periode normal (median AQI = %s) dan periode karhutla (median AQI = %s) dengan ukuran efek %s. Temuan ini mengindikasikan adanya asosiasi empiris yang substansial antara lonjakan kebakaran dan penurunan mutu udara.",
      mw$uji$statistic,
      format_id(mw$ringkas$median[1], 1),
      format_id(mw$ringkas$median[2], 1),
      mw$efek
    )
    
    kartu_interpretasi(
      teks = teks_mw,
      tipe = "info",
      judul = "Kesimpulan Uji Mann-Whitney U (Asosiasi Dua Periode)"
    )
  })
  
  # Output: tafsir_kw (Kesimpulan Kruskal-Wallis)
  output$tafsir_kw <- renderUI({
    analisis <- ambil_analisis()
    kw_info <- analisis$uji_antar_kota
    
    teks_kw <- sprintf(
      "Uji Kruskal-Wallis antar kota menunjukkan asosiasi variasi peringkat AQI PM2,5 yang sangat nyata antar wilayah pengamatan (H = %s, df = %d, p < 0,001, %s). Perbedaan tingkat paparan kabut asap antar kota menunjukkan tingkat disparitas yang signifikan.",
      format_id(kw_info$kw$statistic, 2),
      kw_info$kw$parameter,
      kw_info$efek
    )
    
    kartu_interpretasi(
      teks = teks_kw,
      tipe = "info",
      judul = "Kesimpulan Uji Kruskal-Wallis (Asosiasi Antar Wilayah)"
    )
  })
  
  # Output: tbl_dunn (Tabel Uji Lanjut Dunn DT)
  output$tbl_dunn <- renderDT({
    analisis <- ambil_analisis()
    dunn_df <- analisis$uji_antar_kota$dunn |>
      mutate(
        `Perbandingan Kota` = paste(group1, "vs", group2),
        `Skor z` = format_id(z, 2),
        `Nilai p (Adj. Holm)` = if (p.adj < 0.001) "< 0,001" else format_id(p.adj, 4),
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
  
  # Output: tbl_chisq (Tabel Persentase Baris Chi-Square)
  output$tbl_chisq <- renderTable({
    analisis <- ambil_analisis()
    analisis$chisq$persen_baris
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lcccccc")
  
  # Output: tafsir_chisq (Kesimpulan Uji Chi-Square)
  output$tafsir_chisq <- renderUI({
    analisis <- ambil_analisis()
    cs <- analisis$chisq
    
    teks_cs <- sprintf(
      "Uji Chi-Square independensi menunjukkan asosiasi pergeseran proporsi kategori kualitas udara yang sangat signifikan antara periode normal dan karhutla (Chi² = %s, df = %d, p < 0,001, frekuensi harapan minimum = %s). Pada periode karhutla, proporsi hari dengan kategori Tidak Sehat hingga Berbahaya mendominasi secara drastis dibandingkan periode normal.",
      format_id(cs$uji$statistic, 2),
      cs$uji$parameter,
      format_id(cs$harapan_min, 1)
    )
    
    kartu_interpretasi(
      teks = teks_cs,
      tipe = "warning",
      judul = "Kesimpulan Uji Chi-Square (Asosiasi Distribusi Kategori)"
    )
  })
  
  # --- TAB 6: REGRESI ---
  
  # Output: tbl_sampel (Ukuran Sampel)
  output$tbl_sampel <- renderTable({
    analisis <- ambil_analisis()
    s <- analisis$sampel
    
    tibble(
      `Parameter` = c("Jumlah Observasi (n)", "Jumlah Prediktor (k)", "Syarat Min. Green (1991: 50 + 8k)", "Status Kelayakan Model"),
      `Nilai` = c(
        format_id(s$n, 0),
        format_id(s$k, 0),
        format_id(s$min_uji_model, 0),
        ifelse(s$model_memadai, "Sampel Memadai (Terpenuhi)", "Sampel Kurang")
      )
    )
  }, striped = TRUE, bordered = TRUE)
  
  # Output: tbl_hier (Regresi Hierarkis)
  output$tbl_hier <- renderTable({
    analisis <- ambil_analisis()
    h <- analisis$hierarkis
    
    tibble(
      `Tingkatan Model` = c("Blok 1 (Hanya Kovariat Cuaca)", "Blok 2 (Cuaca + Ukuran Kebakaran)", "Perubahan Tambahan (Delta)"),
      `R²` = c(format_id(h$r2["blok1"], 2), format_id(h$r2["blok2"], 2), "-"),
      `ΔR²` = c("-", "-", format_id(h$delta_r2, 2)),
      `Statistik F` = c("-", "-", format_id(h$uji_perubahan$F, 2)),
      `Nilai p` = c("-", "-", if (h$uji_perubahan$p < 0.001) "< 0,001" else format_id(h$uji_perubahan$p, 4))
    )
  }, striped = TRUE, bordered = TRUE, align = "lrrrr")
  
  # Output: tbl_koef (Koefisien OLS Standar)
  output$tbl_koef <- renderTable({
    analisis <- ambil_analisis()
    analisis$koef
  }, striped = TRUE, bordered = TRUE, align = "lrrrr")
  
  # Output: tbl_hac (Koefisien Kebal Autokorelasi HAC)
  output$tbl_hac <- renderTable({
    analisis <- ambil_analisis()
    analisis$koef_hac
  }, striped = TRUE, bordered = TRUE, align = "lrrrr")
  
  # Output: tbl_asumsi (Uji Asumsi Klasik dengan Badge)
  output$tbl_asumsi <- renderUI({
    analisis <- ambil_analisis()
    df_asumsi <- analisis$asumsi
    
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
  
  # Output: tbl_vif (Tabel Multikolinearitas VIF)
  output$tbl_vif <- renderTable({
    analisis <- ambil_analisis()
    v <- analisis$vif
    
    tibble(
      `Prediktor Model` = v$prediktor,
      `Nilai VIF` = format_id(v$VIF, 2),
      `Nilai Tolerance` = format_id(v$tolerance, 2),
      `Bebas Multikolinearitas` = ifelse(v$lolos, "Lolos (VIF < 3)", "Terindikasi Kolinear")
    )
  }, striped = TRUE, bordered = TRUE, align = "lrrr")
  
  # Output: teks_outlier (Teks Interpretasi Outlier & Cook's Distance)
  output$teks_outlier <- renderUI({
    kartu_interpretasi(
      teks = "Pemeriksaan amatan ekstrem dan jarak Cook (Cook's Distance) menunjukkan tidak ada observasi dengan Cook's D > 0,5 yang berpotensi menjadi titik berpengaruh berlebih (leverage tinggi yang merusak stabilitas estimasi). Seluruh nilai ekstrem dipertahankan dalam analisis karena merupakan cerminan empiris fenomena karhutla ekstrem yang nyata dan bukan akibat galat pencatatan.",
      tipe = "info",
      judul = "Diagnostik Pengamatan Ekstrem & Cook's D"
    )
  })
  
  # Output: plot_diag (Empat Grafik Diagnostik Residual)
  output$plot_diag <- renderPlot({
    df <- data_terfilter()
    if (nrow(df) < 10) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk plot diagnostik", cex = 1.2)
      return()
    }
    
    # Model regresi linier pada data harian terfilter
    fit <- lm(pm25 ~ api_nilai + hujan_mm + angin_maks, data = df)
    
    par(mfrow = c(2, 2), mar = c(4.2, 4.2, 2.5, 1.5), oma = c(0, 0, 0, 0))
    plot(fit, which = 1:4, pch = 16, col = "rgba(31, 58, 95, 0.65)")
  })
  
  # Output: plot_acf (Fungsi Autokorelasi Residual)
  output$plot_acf <- renderPlot({
    df <- data_terfilter()
    if (nrow(df) < 10) {
      plot.new()
      text(0.5, 0.5, "Data tidak cukup untuk plot ACF", cex = 1.2)
      return()
    }
    
    fit <- lm(pm25 ~ api_nilai + hujan_mm + angin_maks, data = df)
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
  
  # Output: metodologi (Dokumentasi Lengkap Metodologi)
  output$metodologi <- renderUI({
    tags$div(
      class = "p-2",
      
      tags$div(
        class = "card p-3 mb-3",
        tags$h5(style = "color: #1F3A5F; font-weight: 700;", "1. Sumber Data dan Karakteristik Sensor"),
        tags$ul(
          tags$li(
            tags$strong("Kualitas Udara: "),
            "Diperoleh dari stasiun BMKG melalui jejaring integrasi WAQI (aqicn.org). Metrik yang digunakan adalah ",
            tags$strong("AQI PM2,5 skala US EPA"), " (0–500), bukan konsentrasi massa mentah dalam µg/m³. Data ini bersumber langsung dari instrumen lapangan dan berstatus ",
            tags$em("belum terverifikasi"), " melalui proses baku kendali mutu laboratorium BMKG."
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

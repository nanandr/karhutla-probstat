# Blueprint UI untuk Web Design Agent: Dashboard Kualitas Udara dan Karhutla Kalimantan (R Shiny, Server Lokal)

*Versi 2. Ganti seluruh instruksi sebelumnya yang memakai React dan Tailwind dengan dokumen ini.*

## 1. Tugas untuk Agent

Buat **antarmuka (UI) dashboard** untuk aplikasi analisis hubungan kebakaran hutan dan lahan (karhutla) dengan kualitas udara di Kalimantan. Studi kasus utama: Palangka Raya. Kota lain dan kota pembanding (Bandung) hanya muncul jika datanya ada.

- **Bahasa dan kerangka:** R dengan Shiny. Bukan React, bukan HTML/JS mentah, bukan Tailwind.
- **Bentuk aplikasi:** server web lokal. R menyalakan server di komputer pengguna (`http://127.0.0.1:3838`) dan dibuka lewat browser. Tidak ada hosting publik.
- **Fokus pekerjaanmu:** tata letak, tata warna, tabel, grafik, peta, dan teks interpretasi. **Jangan** membuat integrasi API atau logika statistik sungguhan. Semua angka berasal dari data contoh (dummy) yang diisolasi rapi dan mudah diganti data nyata.

## 2. Perubahan dari Versi Sebelumnya

| Sebelumnya | Sekarang |
| --- | --- |
| React.js atau HTML/JS + Tailwind CSS | R Shiny + `bslib` (Bootstrap 5) + sedikit CSS di `www/styles.css` |
| Recharts / Chart.js | `plotly` (dan `ggplot2` lewat `ggplotly`) |
| Leaflet JS | paket R `leaflet` |
| Tabel HTML | `DT` dan `tableOutput` |
| `const DUMMY_DATA = {...}` (JSON) | `R/dummy_data.R` dengan tabel R yang **bernama kolom sama persis** dengan data nyata |
| Satu halaman dengan 4 area | Sidebar filter + baris KPI + 7 tab (lihat bagian 7) |
| Tanpa tanda bahwa data palsu | **Banner "DATA CONTOH" wajib** selama memakai dummy |
| Kategori AQI 4 jenis | 6 kategori AQI US EPA |
| Hosting bebas | Hanya server lokal (data aqicn tidak boleh dipublikasikan) |

## 3. Aturan Keras

**Wajib**

1. Seluruh kode dalam **R**. Satu-satunya berkas non-R yang boleh ada adalah `www/styles.css`.
2. Semua data UI berasal dari `R/dummy_data.R` lewat **fungsi adaptor** (bagian 6). Komponen UI tidak boleh membaca objek dummy langsung dan tidak boleh berisi angka atau teks data yang ditulis manual.
3. Selama `DATA_CONTOH <- TRUE`, tampilkan banner di bagian atas: **"DATA CONTOH (bukan data sebenarnya)"**, dan beri keterangan yang sama pada setiap tangkapan layar.
4. Semua teks interpretasi memakai kata **"asosiasi"**, bukan "penyebab" atau "akibat".
5. Titik panas dijelaskan sebagai **piksel satelit yang terdeteksi panas**, bukan jumlah kebakaran dan bukan luas area terbakar.
6. Nama input dan output Shiny **harus sama** dengan tabel di bagian 7, supaya UI ini bisa digabung dengan logika statistik tanpa mengubah nama.
7. Footer atribusi tampil di semua tab (bagian 9).

**Dilarang**

- React, Vue, npm, Node, Tailwind, CDN framework JS, atau font dari server luar.
- Memakai `localStorage` atau menulis JavaScript kustom kecuali benar-benar perlu.
- Mengklaim sebab-akibat, atau menampilkan data dummy seolah-olah data nyata.
- Tombol unduh untuk data mentah kualitas udara (hanya hasil analisis yang boleh diunduh).

## 4. Tech Stack

Paket R yang boleh dipakai: `shiny`, `bslib`, `dplyr`, `tidyr`, `ggplot2`, `plotly`, `leaflet`, `DT`. Paket lain hanya jika benar-benar perlu dan harus disebutkan.

- **Tema:** `bslib::bs_theme(version = 5, ...)` dengan palet di bagian 8.
- **Tata letak:** `page_sidebar()`, `layout_columns()`, `card()`, `value_box()`, `navset_card_tab()`.
- **Font:** tumpukan font sistem (tanpa unduhan font), agar aplikasi jalan offline.

## 5. Struktur Berkas

```
karhutla-udara/
├── app.R              # UI + server
├── R/
│   ├── dummy_data.R   # SEMUA data contoh (satu-satunya tempat)
│   ├── data_source.R  # fungsi adaptor: dummy sekarang, data nyata nanti
│   └── ui_helpers.R   # komponen UI berulang (kartu interpretasi, badge, dst.)
└── www/
    └── styles.css     # CSS tambahan yang kecil
```

Catatan: pada proyek penuh, folder `R/` juga berisi fungsi statistik (`stats_*.R`) dan `config.R`. `app.R` memuat semua berkas di `R/` dengan `for (f in list.files('R', pattern = '[.]R$', full.names = TRUE)) source(f)`. Jangan menimpa nama fungsi `stats_*` jika berkas itu ada.

## 6. Kontrak Data

### 6.1 Fungsi adaptor (`R/data_source.R`)

UI hanya boleh memanggil fungsi berikut. Saat memakai data nyata, **hanya isi fungsi ini yang diganti**.

```r
# R/data_source.R
ambil_data_harian <- function() {
  if (DATA_CONTOH) DUMMY_HARIAN else readRDS('data/data_harian.rds')
}
ambil_hotspots <- function() {
  if (DATA_CONTOH) DUMMY_HOTSPOTS else readRDS('data/hotspots.rds')
}
ambil_analisis <- function() {
  # Hasil statistik statis untuk mockup. Pada versi penuh, bagian ini dihapus
  # dan server memanggil fungsi stats_*.R dengan data terfilter.
  DUMMY_ANALISIS
}
```

### 6.2 Skema tabel (nama kolom TIDAK boleh diubah)

**`data_harian`** (satu baris per kota per hari)

| Kolom | Tipe | Arti |
| --- | --- | --- |
| `kota` | teks | Palangkaraya, Pontianak, Bandung |
| `tanggal` | Date | Tanggal harian |
| `pm25` | angka | **AQI PM2,5** (skala US EPA), bukan µg/m³ |
| `n_stasiun` | integer | Jumlah stasiun |
| `sumber` | teks | `aqicn`, `CAMS`, atau `contoh` |
| `hujan_mm`, `suhu_c`, `angin_maks` | angka | Cuaca harian |
| `titik_panas` | integer | Jumlah piksel panas dalam radius 100 km dari pusat kota |
| `frp_total` | angka | Total FRP (MW) dalam radius yang sama |
| `batas_ekstrem`, `outlier` | angka, logika | Penanda nilai ekstrem (tidak dibuang) |

**`hotspots`**: `tanggal`, `latitude`, `longitude`, `confidence` (`l`, `n`, `h`), `frp`.

### 6.3 Generator data contoh (`R/dummy_data.R`)

Sketsa yang boleh kamu perbaiki agar grafik tampak meyakinkan (puncak kebakaran, lalu turun saat hujan datang):

```r
# R/dummy_data.R
library(dplyr)

DATA_CONTOH <- TRUE   # ubah ke FALSE saat memakai data nyata

buat_kota_contoh <- function(kota, skala = 1, pengaruh = 0.55, seed = 1) {
  set.seed(seed)
  tanggal <- seq(as.Date('2026-07-15'), as.Date('2026-10-15'), by = 'day')
  n <- length(tanggal)
  hari <- seq_len(n)
  titik <- pmax(0, round(skala * (20 + 280 * exp(-((hari - 60)^2) / (2 * 14^2))) +
                           rnorm(n, 0, 15)))
  frp   <- round(titik * runif(n, 8, 14), 1)
  hujan <- round(rexp(n, rate = 1 / 4) * ifelse(hari > 85, 2.5, 0.6), 1)
  pm25  <- pmax(5, round(25 + pengaruh * dplyr::lag(titik, 2, default = 20) -
                           0.8 * hujan + rnorm(n, 0, 12)))
  tibble(kota = kota, tanggal = tanggal, pm25 = pm25, n_stasiun = 1L,
         sumber = 'contoh', hujan_mm = hujan,
         suhu_c = round(rnorm(n, 28, 1.2), 1), angin_maks = round(runif(n, 6, 22), 1),
         titik_panas = as.integer(titik), frp_total = frp)
}

DUMMY_HARIAN <- bind_rows(
  buat_kota_contoh('Palangkaraya', skala = 1,    pengaruh = 0.55, seed = 1),
  buat_kota_contoh('Pontianak',    skala = 0.5,  pengaruh = 0.35, seed = 2),
  buat_kota_contoh('Bandung',      skala = 0.02, pengaruh = 0.02, seed = 3)
) |>
  group_by(kota) |>
  mutate(batas_ekstrem = quantile(pm25, 0.75) + 3 * IQR(pm25),
         outlier = pm25 > batas_ekstrem) |>
  ungroup()

# Titik panas contoh: sebar acak di sekitar pusat kota, maksimal ~5.000 baris
DUMMY_HOTSPOTS <- DUMMY_HARIAN |>
  filter(kota == 'Palangkaraya', titik_panas > 0) |>
  slice_sample(n = 120) |>
  rowwise() |>
  reframe(tanggal = tanggal,
          latitude  = rnorm(min(titik_panas, 40), -2.2, 0.5),
          longitude = rnorm(min(titik_panas, 40), 113.9, 0.5),
          confidence = sample(c('n', 'h'), min(titik_panas, 40), TRUE, c(0.4, 0.6)),
          frp = round(rexp(min(titik_panas, 40), 1 / 11), 1))
```

### 6.4 Hasil analisis contoh (`DUMMY_ANALISIS`)

Bentuk setiap objek **harus meniru keluaran fungsi aslinya** (nama kolom dan elemen seperti di tutorial Tahap 7), agar nanti cukup diganti pemanggilan fungsi.

| Elemen | Bentuk | Nilai contoh |
| --- | --- | --- |
| `korelasi_lag` | tibble: `lag`, `n`, `rho`, `p`, `p_adj`, `ci_bawah`, `ci_atas` untuk lag 0–7 | rho: 0,38; 0,49; 0,61; 0,55; 0,44; 0,35; 0,27; 0,20 (puncak di lag 2) |
| `tafsir_korelasi` | teks | "Asosiasi terkuat pada lag 2 hari (rho = 0,61; hubungan kuat positif; signifikan setelah koreksi Holm). Ini asosiasi statistik, bukan sebab-akibat." |
| `parsial` | tibble: `n`, `lag`, `rho_biasa`, `rho_parsial`, `p_parsial` | rho biasa 0,61; rho parsial 0,48 |
| `uji_dua_periode` | list: `ringkas` (periode, n, median), `normalitas` (data, n, W, p, normal), `uji$p.value`, `efek` | median Karhutla 148 vs Normal 52; p < 0,001; normal = FALSE |
| `uji_antar_kota` | list: `kw$p.value`, `efek`, `dunn` (group1, group2, p.adj, p.adj.signif) | p < 0,001 |
| `chisq` | list: `persen_baris`, `uji$p.value`, `harapan_min` | p < 0,001; harapan minimum 12,4 |
| `hierarkis` | list: `r2` (blok1, blok2), `delta_r2`, `uji_perubahan` (F, p) | R² blok 1 = 0,22; blok 2 = 0,37; ΔR² = 0,15; p < 0,001 |
| `koef`, `koef_hac` | tabel koefisien (Variabel, Estimasi, SE, t, p) | `api`, `hujan_mm`, `angin_maks` |
| `asumsi` | tibble: `uji`, `statistik`, `p`, `lolos` | Normalitas residual TRUE; Linearitas TRUE; Autokorelasi (Durbin-Watson) **FALSE**; Heteroskedastisitas TRUE |
| `vif` | tibble: `prediktor`, `VIF`, `tolerance`, `lolos` | semua VIF < 3 |
| `sampel` | tibble: `n`, `k`, `min_uji_model`, `model_memadai` | n = 92, k = 3, memadai |

## 7. Tata Letak dan Komponen

### 7.1 Kerangka halaman

`page_sidebar()` dengan: banner DATA CONTOH di atas, **sidebar** (filter global), **baris KPI**, lalu **`navset_card_tab()`** berisi 7 tab. Footer di bawah.

### 7.2 Sidebar: filter global (Area A)

| Input | ID | Isi |
| --- | --- | --- |
| Dropdown kota | `kota` | Palangkaraya (default), Pontianak, Bandung (pembanding) |
| Rentang tanggal | `tgl` | default 1 Agustus 2026 s.d. 30 September 2026 |
| Rata-rata bergerak | `ma_k` | slider 3–30 hari, default 7 |
| Lag titik panas | `lag_api` | slider 0–7 hari, default 2 |
| Ukuran kebakaran | `var_api` | "Jumlah titik panas" (`titik_panas`) atau "Total FRP (MW)" (`frp_total`) |

Bawah sidebar: teks kecil tanggal pengambilan data (contoh: "Data dibekukan: 31 Okt 2026") dan label atribusi.

### 7.3 Baris KPI (Area B, di atas tab)

Empat `value_box()` yang dihitung dari data terfilter:

1. **Rata-rata AQI PM2,5**, berwarna sesuai kategorinya.
2. **Kategori dominan** (kategori PAK dengan hari terbanyak), badge berwarna.
3. **Total titik panas** (atau total FRP sesuai `var_api`).
4. **Hari tidak sehat** (AQI > 100): jumlah hari dan persentasenya.

### 7.4 Tujuh tab

| Tab | Komponen | ID output |
| --- | --- | --- |
| **1. Ringkasan** | Tabel statistik deskriptif (n, hilang, rata-rata, median, sd, min, Q1, Q3, maks, skewness); bar chart proporsi hari per kategori AQI (6 kategori, warna EPA); tabel Shapiro-Wilk dengan kolom Normal (Ya/Tidak) | `tbl_desk`, `plot_kategori`, `tbl_norm` |
| **2. Tren** | Grafik `plotly` dua sumbu: garis AQI PM2,5 (kiri), **bar** ukuran kebakaran (kanan, sumbu sekunder), garis putus-putus rata-rata bergerak; garis acuan horizontal pada AQI 100 dan 150 | `plot_tren` |
| **3. Peta Titik Panas** | Peta `leaflet`: lingkaran kecil berukuran menurut FRP, warna menurut confidence (tinggi = merah tua, nominal = oranye), marker kota dan lingkaran radius 100 km; teks jumlah titik; catatan "satu titik = satu piksel satelit, bukan satu kebakaran" | `n_titik`, `peta` |
| **4. Hubungan** | Bar chart korelasi Spearman per lag 0–7 dengan selang kepercayaan; kartu interpretasi korelasi; kartu interpretasi korelasi parsial; scatter plot ukuran kebakaran vs AQI | `plot_lag`, `tafsir_lag`, `tafsir_parsial`, `scatter` |
| **5. Perbandingan** | Input `periode_api` (rentang periode karhutla); kartu Mann–Whitney U (tabel ringkas, tabel normalitas, kalimat kesimpulan); kartu Kruskal–Wallis + tabel Dunn (`DT`); kartu Chi-square (tabel persen baris, kalimat kesimpulan) | `ringkas_uji`, `tbl_norm_periode`, `tafsir_mw`, `tafsir_kw`, `tbl_dunn`, `tbl_chisq`, `tafsir_chisq` |
| **6. Regresi** | Cek ukuran sampel; tabel hierarchical regression (R² blok 1, blok 2, ΔR², F, p); tabel koefisien dan koefisien HAC; **badge uji asumsi** (hijau "Terpenuhi", merah "Tidak terpenuhi") untuk normalitas residual, linearitas, autokorelasi, heteroskedastisitas; tabel VIF dan tolerance; teks outlier dan Cook's D; empat grafik diagnostik dan ACF residual | `tbl_sampel`, `tbl_hier`, `tbl_koef`, `tbl_hac`, `tbl_asumsi`, `tbl_vif`, `teks_outlier`, `plot_diag`, `plot_acf` |
| **7. Metodologi** | Sumber data, pengolahan, daftar metode, dan keterbatasan, ditulis sebagai teks rapi (`markdown()` atau kartu bertingkat) | `metodologi` |

Untuk `plot_diag` dan `plot_acf` pada mockup, boleh menjalankan `lm()` sederhana pada data contoh lalu `plot()`. Autokorelasi harus tampil **"Tidak terpenuhi"** dengan catatan kecil: "Pakai galat baku HAC".

### 7.5 Perilaku UI

- Mengubah filter memperbarui semua tab. Untuk mockup, tampilkan keadaan *loading* singkat (`withProgress` atau spinner) lalu data contoh yang sama atau sedikit berbeda per kota.
- **Keadaan kosong:** jika data suatu kota atau rentang kosong, tampilkan kartu pesan "Tidak ada data untuk pilihan ini", bukan error merah (`validate(need(...))`).
- Setiap kalimat interpretasi tampil dalam **kartu interpretasi** (ikon kecil, latar lembut, teks jelas) yang membedakannya dari angka mentah.

## 8. Panduan Visual

**Palet**

| Peran | Warna |
| --- | --- |
| Primer (judul, tombol, tab aktif) | `#1F3A5F` |
| Latar halaman | `#F5F7FA` |
| Kartu | `#FFFFFF`, bayangan halus, sudut membulat |
| Teks utama | `#1B2733` |
| Titik panas (gradasi) | `#FFB703` → `#FB8500` → `#D00000` |

**Kategori AQI (PAK, skala US EPA)**

| Kategori | Rentang AQI | Warna |
| --- | --- | --- |
| Baik | 0–50 | `#00E400` |
| Sedang | 51–100 | `#FFFF00` |
| Tidak sehat (sensitif) | 101–150 | `#FF7E00` |
| Tidak sehat | 151–200 | `#FF0000` |
| Sangat tidak sehat | 201–300 | `#8F3F97` |
| Berbahaya | di atas 300 | `#7E0023` |

Teks di atas warna kuning harus gelap, dan di atas ungu atau marun harus putih.

**Prinsip**

- Tampilan bersih dan informatif, banyak ruang putih, satu aksen warna.
- Warna tidak boleh menjadi satu-satunya pembawa makna: selalu sertakan label teks (nama kategori, "Terpenuhi").
- Kontras teks memenuhi WCAG AA.
- Responsif: kartu KPI turun jadi 2 kolom lalu 1 kolom di layar sempit; tabel lebar bisa digulir horizontal di dalam kartunya sendiri.
- Angka memakai format Indonesia (koma desimal, titik ribuan) pada teks tampilan.

## 9. Label dan Footer Wajib

Footer statis di semua tab:

> Sumber data kualitas udara: stasiun BMKG melalui WAQI (aqicn.org), **belum terverifikasi**. Titik panas: NASA FIRMS. Cuaca: Open-Meteo. Hasil menunjukkan asosiasi statistik, bukan sebab-akibat.

Tambahan wajib:

- Banner "DATA CONTOH" selama `DATA_CONTOH <- TRUE`.
- Catatan di tab Peta: satu titik panas adalah satu piksel satelit, bukan satu kebakaran.
- Tidak ada tombol unduh data mentah kualitas udara.

## 10. Cara Menjalankan (Server Lokal)

Dari folder project:

```r
shiny::runApp('.', host = '127.0.0.1', port = 3838, launch.browser = TRUE)
```

Atau dari Terminal:

```
Rscript -e "shiny::runApp('.', port = 3838, launch.browser = TRUE)"
```

Bila agent hanya bisa menghasilkan kode (tidak bisa menjalankan R), serahkan berkas `app.R` dan `R/*.R`; pengguna akan menjalankannya sendiri dengan perintah di atas. Pratinjau Shiny tidak bisa dirender di Claude Artifacts atau v0.

## 11. Kriteria Penerimaan

1. `app.R` dan seluruh berkas di `R/` berjalan tanpa error dengan `shiny::runApp()`.
2. Hanya R yang dipakai (plus `www/styles.css`); tidak ada React, Tailwind, atau CDN JS.
3. Ketujuh tab tampil lengkap dengan ID input dan output persis seperti bagian 7.
4. Tidak ada angka atau teks data yang ditulis manual di UI; semua berasal dari fungsi adaptor.
5. Mengganti `DATA_CONTOH <- FALSE` dan mengisi `ambil_data_harian()` dengan `readRDS(...)` sudah cukup untuk memakai data nyata tanpa mengubah UI.
6. Banner DATA CONTOH dan footer atribusi tampil.
7. Keadaan kosong dan *loading* tertangani; tampilan nyaman di layar laptop dan tablet.
8. Semua kalimat interpretasi memakai "asosiasi".

## 12. Prompt Siap Pakai

> Bertindaklah sebagai R Shiny developer dan UI designer. Buat aplikasi Shiny lengkap berdasarkan blueprint ini: `app.R`, `R/dummy_data.R`, `R/data_source.R`, `R/ui_helpers.R`, dan `www/styles.css`. Pakai hanya R (`shiny`, `bslib`, `plotly`, `leaflet`, `DT`, `dplyr`, `ggplot2`); jangan memakai React, Tailwind, atau framework JavaScript. Ikuti kontrak data di bagian 6 dan nama input/output di bagian 7 persis. Semua data berasal dari fungsi adaptor. Tampilkan banner DATA CONTOH dan footer atribusi. Aplikasi dijalankan sebagai server lokal dengan `shiny::runApp()`.

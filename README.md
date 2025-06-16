# 📊 Pemodelan Kemiskinan di Sumatera Utara Menggunakan Regresi Nonparametrik Spline

## 📌 Deskripsi Singkat

Penelitian ini bertujuan untuk menganalisis hubungan antara **Indeks Pembangunan Manusia (IPM)** dan **kemiskinan** di Provinsi Sumatera Utara menggunakan pendekatan regresi nonparametrik spline. Metode ini dipilih karena mampu menangkap pola hubungan nonlinier yang tidak terdeteksi oleh model parametrik biasa.

## Dataset
- Sumber data: Badan Pusat Statistik (https://sumut.bps.go.id)
- Unit analisis: 33 kabupaten/kota di Provinsi Sumatera Utara (tahun 2020)
- Variabel:
  - Y: Tingkat Kemiskinan (%)
  - X1: Indeks Pembangunan Manusia (IPM)
  - X2: Tingkat Pengangguran Terbuka

## Tools & Bahasa
- R & RStudio
- Library: `ggplot2`, `readxl`, `stats4`, fungsi custom spline

## 🔍 Hasil Utama
- Model spline terbaik menggunakan **1 titik knot** pada IPM = **66.51**
- Nilai GCV minimum: **0.2249**, MSE: **6.1351**
- Terdapat pengaruh signifikan antara IPM dan kemiskinan (F hitung = 37.48 > F tabel)

## 🧾 Kesimpulan
Model regresi nonparametrik spline mampu menggambarkan hubungan nonlinear antara IPM dan kemiskinan secara lebih fleksibel. Hasil analisis menunjukkan bahwa peningkatan IPM secara umum berkorelasi dengan penurunan tingkat kemiskinan di Sumatera Utara.

## 👩‍💻 Author
**Yuni Sabrina Effendy**  


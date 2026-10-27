# ============================================================
#  Electricity Usage Anomaly Detector  (R Micro-Project)
#  Only base R is used -> no package installation needed
# ============================================================
#  Methods:
#   1. Z-score  (per hour-of-day)
#   2. IQR rule (per hour-of-day)
#   3. Hampel filter (rolling median + MAD)
#  Final decision: voting (anomaly if >= 2 methods agree)
# ============================================================

set.seed(42)

# ------------------------------------------------------------
# STEP 1: DATA
# Option A (default): simulated hourly smart-meter data
# Option B: your own CSV with columns: datetime, kwh
#           -> set USE_REAL_DATA <- TRUE and give file path
# ------------------------------------------------------------
USE_REAL_DATA <- FALSE
CSV_PATH      <- "my_meter_data.csv"

if (USE_REAL_DATA) {
  df <- read.csv(CSV_PATH, stringsAsFactors = FALSE)
  df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
  df$true_anomaly <- NA            # unknown for real data
} else {
  n <- 24 * 90                     # 90 days of hourly data
  datetime <- seq(as.POSIXct("2025-01-01 00:00", tz = "UTC"),
                  by = "hour", length.out = n)
  hr  <- as.integer(format(datetime, "%H"))
  wd  <- format(datetime, "%u")    # 1=Mon ... 7=Sun
  weekend <- wd %in% c("6", "7")

  # daily pattern: morning peak (~8) and evening peak (~20)
  base <- 0.6 +
    0.8 * exp(-((hr - 8)^2)  / 6) +
    1.4 * exp(-((hr - 20)^2) / 8)
  base <- ifelse(weekend, base * 1.15, base)   # weekends use more
  kwh  <- base + rnorm(n, 0, 0.10)

  # ---- inject known anomalies (so we can measure accuracy) ----
  true_anomaly <- rep(FALSE, n)

  spike_idx <- sample(n, 12)                   # sudden spikes
  kwh[spike_idx] <- kwh[spike_idx] + runif(12, 3, 5)
  true_anomaly[spike_idx] <- TRUE

  outage_idx <- sample(n, 6)                   # meter fault / outage
  kwh[outage_idx] <- 0
  true_anomaly[outage_idx] <- TRUE

  leak_idx <- 1200:1215                        # sustained high usage
  kwh[leak_idx] <- kwh[leak_idx] + 2.5         # (faulty appliance)
  true_anomaly[leak_idx] <- TRUE

  kwh <- pmax(kwh, 0)
  df  <- data.frame(datetime, kwh, true_anomaly)
}

df$hour <- as.integer(format(df$datetime, "%H"))
df$date <- as.Date(df$datetime)

# ------------------------------------------------------------
# STEP 2: METHOD 1 - Z-score per hour-of-day
# ------------------------------------------------------------
mu <- ave(df$kwh, df$hour, FUN = mean)
sdv <- ave(df$kwh, df$hour, FUN = sd)
df$z_score  <- (df$kwh - mu) / sdv
df$flag_z   <- abs(df$z_score) > 3

# ------------------------------------------------------------
# STEP 3: METHOD 2 - IQR rule per hour-of-day
# ------------------------------------------------------------
q1  <- ave(df$kwh, df$hour, FUN = function(x) quantile(x, 0.25))
q3  <- ave(df$kwh, df$hour, FUN = function(x) quantile(x, 0.75))
iqr <- q3 - q1
df$flag_iqr <- df$kwh < (q1 - 1.5 * iqr) | df$kwh > (q3 + 1.5 * iqr)

# ------------------------------------------------------------
# STEP 4: METHOD 3 - Hampel filter (rolling median + MAD)
# ------------------------------------------------------------
hampel_flag <- function(x, k = 12, threshold = 3.5) {
  n <- length(x)
  flags <- logical(n)
  for (i in seq_len(n)) {
    lo  <- max(1, i - k)
    hi  <- min(n, i + k)
    win <- x[lo:hi]
    med <- median(win)
    s   <- max(mad(win), 0.05)          # floor avoids divide-by-~0
    flags[i] <- abs(x[i] - med) / s > threshold
  }
  flags
}
df$flag_hampel <- hampel_flag(df$kwh)

# ------------------------------------------------------------
# STEP 5: VOTING - anomaly if at least 2 of 3 methods agree
# ------------------------------------------------------------
df$votes   <- df$flag_z + df$flag_iqr + df$flag_hampel
df$anomaly <- df$votes >= 2

# ------------------------------------------------------------
# STEP 6: CLASSIFY ANOMALY TYPE
# ------------------------------------------------------------
df$type <- NA_character_
df$type[df$anomaly & df$kwh == 0]        <- "Outage / Meter fault"
df$type[df$anomaly & is.na(df$type) &
          df$z_score > 0]                <- "Spike / High usage"
df$type[df$anomaly & is.na(df$type)]     <- "Unusually low usage"

# ------------------------------------------------------------
# STEP 7: REPORT
# ------------------------------------------------------------
cat("\n===== ANOMALY DETECTION SUMMARY =====\n")
cat("Total readings        :", nrow(df), "\n")
cat("Anomalies detected    :", sum(df$anomaly), "\n")
cat("Anomaly rate          :", round(100 * mean(df$anomaly), 2), "%\n\n")
cat("By type:\n"); print(table(df$type))

cat("\nTop 10 worst readings:\n")
top <- df[df$anomaly, ]
top <- head(top[order(-abs(top$z_score)), c("datetime", "kwh", "z_score", "type")], 10)
print(top, row.names = FALSE)

# Accuracy check (only works for simulated data)
if (!USE_REAL_DATA) {
  TP <- sum(df$anomaly & df$true_anomaly)
  FP <- sum(df$anomaly & !df$true_anomaly)
  FN <- sum(!df$anomaly & df$true_anomaly)
  precision <- TP / (TP + FP)
  recall    <- TP / (TP + FN)
  f1        <- 2 * precision * recall / (precision + recall)
  cat("\n===== MODEL EVALUATION =====\n")
  cat("Precision:", round(precision, 3), "\n")
  cat("Recall   :", round(recall, 3), "\n")
  cat("F1-score :", round(f1, 3), "\n")
  cat("\nConfusion matrix:\n")
  print(table(Actual = df$true_anomaly, Predicted = df$anomaly))

  cat("\nIndividual methods (recall / false positives):\n")
  for (m in c("flag_z", "flag_iqr", "flag_hampel")) {
    cat(sprintf("%-12s recall=%.2f  FP=%d\n", m,
                sum(df[[m]] & df$true_anomaly) / sum(df$true_anomaly),
                sum(df[[m]] & !df$true_anomaly)))
  }
}

# ------------------------------------------------------------
# STEP 8: DAILY SUMMARY (useful for a bill-alert system)
# ------------------------------------------------------------
daily <- aggregate(cbind(kwh, anomaly) ~ date, data = df, FUN = sum)
names(daily) <- c("date", "total_kwh", "anomaly_count")
cat("\nDays with the most anomalies:\n")
print(head(daily[order(-daily$anomaly_count), ], 5), row.names = FALSE)

# ------------------------------------------------------------
# STEP 9: VISUALISATION (saved as PNG files)
# ------------------------------------------------------------
# Plot 1: full time series with anomalies in red
png("plot1_timeseries.png", width = 1200, height = 500)
plot(df$datetime, df$kwh, type = "l", col = "grey40",
     xlab = "Date", ylab = "Consumption (kWh)",
     main = "Electricity Usage with Detected Anomalies")
points(df$datetime[df$anomaly], df$kwh[df$anomaly],
       col = "red", pch = 19, cex = 1.1)
legend("topright", legend = c("Normal usage", "Anomaly"),
       col = c("grey40", "red"), lty = c(1, NA), pch = c(NA, 19))
dev.off()

# Plot 2: average daily load profile (shows normal pattern)
png("plot2_hourly_profile.png", width = 900, height = 500)
boxplot(kwh ~ hour, data = df, col = "lightblue", outpch = 19, outcol = "red",
        xlab = "Hour of day", ylab = "kWh",
        main = "Hourly Usage Distribution (red dots = outliers)")
dev.off()

# Plot 3: daily anomaly count
png("plot3_daily_anomalies.png", width = 1000, height = 450)
barplot(daily$anomaly_count, names.arg = format(daily$date, "%d-%b"),
        col = ifelse(daily$anomaly_count > 0, "tomato", "grey80"),
        las = 2, cex.names = 0.5,
        main = "Anomalies per Day", ylab = "Count")
dev.off()

# Plot 4: zoom on the sustained-fault period (simulated data)
if (!USE_REAL_DATA) {
  seg <- 1150:1260
  png("plot4_zoom.png", width = 1000, height = 450)
  plot(df$datetime[seg], df$kwh[seg], type = "b", pch = 16, col = "steelblue",
       xlab = "Time", ylab = "kWh", main = "Zoom: Sustained High Usage Detection")
  points(df$datetime[seg][df$anomaly[seg]], df$kwh[seg][df$anomaly[seg]],
         col = "red", pch = 19, cex = 1.4)
  dev.off()
}

# ------------------------------------------------------------
# STEP 10: EXPORT RESULTS
# ------------------------------------------------------------
write.csv(df[df$anomaly, c("datetime", "kwh", "z_score", "votes", "type")],
          "detected_anomalies.csv", row.names = FALSE)
write.csv(daily, "daily_summary.csv", row.names = FALSE)
cat("\nSaved: 3-4 PNG plots, detected_anomalies.csv, daily_summary.csv\n")
cat("Working directory:", getwd(), "\n")

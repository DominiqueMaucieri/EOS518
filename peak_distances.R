library("dplyr")
library("here")
library("pracma")
library("ggplot2")

theme_DOM <- function () {
  theme_classic(base_size = 12) + 
    theme(
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "plain"),
      legend.text = element_text(size = 12, face = "plain"),
      legend.title = element_text(size = 14, face = "bold")
    )
}

comb_glider <- read.csv(here::here("", "Cleaned_Glider_Data.csv"))

comb_glider$time <- as.POSIXct(comb_glider$time, origin = "1970-01-01T00:00:00+00:00")


CAN_deep <- comb_glider %>% 
  filter(ID == 'CAN_VI_deep')
  
CAN_shallow <- comb_glider %>% 
  filter(ID == 'CAN_QC_shallow')

WA_shallow <- comb_glider %>% 
  filter(ID == 'USA_WA_shallow')

WA_deep <- comb_glider %>% 
  filter(ID == 'USA_WA_deep')

ggplot(CAN_deep, aes(y=-depth, x= distance_km)) +
  geom_line() + 
  theme_DOM()

ggplot(WA_shallow, aes(y=-depth, x= distance_km)) +
  geom_line() + 
  theme_DOM()

ggplot(WA_deep, aes(y=-depth, x= distance_km)) +
  geom_line() + 
  theme_DOM()


# CAN deep peak distance 
CAN_deep_peaks <- as.data.frame(findpeaks(CAN_deep$depth, nups = 3))
colnames(CAN_deep_peaks) <- c("height", "position", "start_peak", "end_peak")

for (i in 1:nrow(CAN_deep_peaks)) {
  xx <- CAN_deep[CAN_deep_peaks[i, 2],]
  CAN_deep_peaks$distance[i] <- xx$distance_km
  CAN_deep_peaks$time[i] <- xx$time
  CAN_deep_peaks$depth[i] <- xx$depth
}


for(i in 1:nrow(CAN_deep_peaks)){
  if(i>1){
    CAN_deep_peaks$resol[i] <- CAN_deep_peaks$distance[i-1] - CAN_deep_peaks$distance[i]
    CAN_deep_peaks$dt[i] <- CAN_deep_peaks$time[i] - CAN_deep_peaks$time[i-1]
    CAN_deep_peaks$dz[i] <- CAN_deep_peaks$depth[i] - CAN_deep_peaks$depth[i-1]
  } else {
    CAN_deep_peaks$resol[i] <- NA
    CAN_deep_peaks$dt[i] <- NA
    CAN_deep_peaks$dz[i] <- NA
  }
}

CAN_deep_peaks$dz_dt <- CAN_deep_peaks$dz / CAN_deep_peaks$dt

Avg_resol_CAN_deep <- mean(CAN_deep_peaks$resol, na.rm = TRUE)
Avg_velocity_CAN_deep <- mean(abs(CAN_deep_peaks$dz_dt), na.rm = TRUE)

ggplot(CAN_deep_peaks, aes(y=dz, x= dt)) +
  geom_line() +
  theme_DOM()



# CAN shallow peak distance 
CAN_shallow_peaks <- as.data.frame(findpeaks(CAN_shallow$depth, nups = 3))
colnames(CAN_shallow_peaks) <- c("height", "position", "start_peak", "end_peak")

for (i in 1:nrow(CAN_shallow_peaks)) {
  xx <- CAN_shallow[CAN_shallow_peaks[i, 2],]
  CAN_shallow_peaks$distance[i] <- xx$distance_km
  CAN_shallow_peaks$time[i] <- xx$time
  CAN_shallow_peaks$depth[i] <- xx$depth
}


for(i in 1:nrow(CAN_shallow_peaks)){
  if(i>1){
    CAN_shallow_peaks$resol[i] <- CAN_shallow_peaks$distance[i-1] - CAN_shallow_peaks$distance[i]
    CAN_shallow_peaks$dt[i] <- CAN_shallow_peaks$time[i] - CAN_shallow_peaks$time[i-1]
    CAN_shallow_peaks$dz[i] <- CAN_shallow_peaks$depth[i] - CAN_shallow_peaks$depth[i-1]
  }
}

CAN_shallow_peaks$dz_dt <- CAN_shallow_peaks$dz / CAN_shallow_peaks$dt

Avg_resol_CAN_shallow <- mean(CAN_shallow_peaks$resol, na.rm = TRUE)
Avg_velocity_CAN_shallow <- mean(abs(CAN_shallow_peaks$dz_dt), na.rm = TRUE)

ggplot(CAN_shallow_peaks, aes(y=dz, x= dt)) +
  geom_line() +
  theme_DOM()


# WA shallow peak distance 

WA_shallow_peaks <- as.data.frame(findpeaks(WA_shallow$depth))

colnames(WA_shallow_peaks) <- c("height", "position", "start_peak", "end_peak")

for (i in 1:nrow(WA_shallow_peaks)) {
  xx <- WA_shallow[WA_shallow_peaks[i, 2],]
  WA_shallow_peaks$distance[i] <- xx$distance_km
  WA_shallow_peaks$time[i] <- xx$time
  WA_shallow_peaks$depth[i] <- xx$depth
}

for(i in 1:nrow(WA_shallow_peaks)){
  if(i>1){
    WA_shallow_peaks$resol[i] <- WA_shallow_peaks$distance[i-1] - WA_shallow_peaks$distance[i]
    WA_shallow_peaks$dt[i] <- WA_shallow_peaks$time[i] - WA_shallow_peaks$time[i-1]
    WA_shallow_peaks$dz[i] <- WA_shallow_peaks$depth[i] - WA_shallow_peaks$depth[i-1]
  }
}

WA_shallow_peaks$dz_dt <- WA_shallow_peaks$dz / WA_shallow_peaks$dt

Avg_resol_WA_shallow <- mean(WA_shallow_peaks$resol, na.rm = TRUE)
Avg_velocity_WA_shallow <- mean(abs(WA_shallow_peaks$dz_dt), na.rm = TRUE)

ggplot(WA_shallow_peaks, aes(y=dz, x= dt)) +
  geom_line() +
  theme_DOM()

# WA deep peak distance 
WA_deep_peaks <- as.data.frame(findpeaks(WA_deep$depth))

colnames(WA_deep_peaks) <- c("height", "position", "start_peak", "end_peak")

for (i in 1:nrow(WA_deep_peaks)) {
  xx <- WA_deep[WA_deep_peaks[i, 2],]
  WA_deep_peaks$distance[i] <- xx$distance_km
  WA_deep_peaks$time[i] <- xx$time
  WA_deep_peaks$depth[i] <- xx$depth
}

for(i in 1:nrow(WA_deep_peaks)){
  if(i>1){
    WA_deep_peaks$resol[i] <- WA_deep_peaks$distance[i-1] - WA_deep_peaks$distance[i]
    WA_deep_peaks$dt[i] <- WA_deep_peaks$time[i] - WA_deep_peaks$time[i-1]
    WA_deep_peaks$dz[i] <- WA_deep_peaks$depth[i] - WA_deep_peaks$depth[i-1]
  }
}

WA_deep_peaks$dz_dt <- WA_deep_peaks$dz / WA_deep_peaks$dt

Avg_resol_WA_deep <- mean(WA_deep_peaks$resol, na.rm = TRUE)
WA_deep_peaks_noinf <- WA_deep_peaks %>% filter(dz_dt != "Inf")
Avg_velocity_WA_deep <- mean(abs(WA_deep_peaks_noinf$dz_dt), na.rm = TRUE)

ggplot(WA_deep_peaks, aes(y=dz, x= dt)) +
  geom_line() +
  theme_DOM()

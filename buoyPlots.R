library(cowplot)
library("openair")
library("clifro")
library("ggplot2")
library(dplyr)

#### PLOTTING ####

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

#Queen Charlotte Island 

QCbuoy_filter <- read.csv(here::here("Buoy_Data", "QCbuoy_filter.csv"))
QCbuoy_filter$Date <- as.Date(QCbuoy_filter$Date, format = "%Y-%m-%d")

### QC Horizontal Wind Speed ###
ggplot(QCbuoy_filter, aes(y=WSPD, x=Date)) +
  geom_vline(xintercept = as.Date("2022-06-25"), color= "chocolate1", size = 2.5, alpha = 0.5) +
  geom_point() +
  theme_DOM() +
  labs(y ="Horizontal wind speed (m/s)", x="Date") +
  geom_smooth(method = "gam", colour = "darkgreen")

### QC Wind speed (m/s) ###
ggplot(QCbuoy_filter, aes(y=GSPD, x=Date)) +
  geom_vline(xintercept = as.Date("2022-06-25"), color= "chocolate1", size = 2.5, alpha = 0.5) +
  geom_point() +
  theme_DOM() +
  labs(y ="Wind speed (m/s)", x="Date") +
  geom_smooth(method = "gam", colour = "darkgreen")


### QC Characteristic significant wave height (m) ###
ggplot(QCbuoy_filter, aes(y=VWH., x=Date)) +
  geom_vline(xintercept = as.Date("2022-06-25"), color= "chocolate1", size = 2.5, alpha = 0.5) +
  geom_point() +
  theme_DOM() +
  labs(y ="Characteristic significant wave height (m)", x="Date") +
  geom_smooth(method = "gam", colour = "darkgreen")

### QC Windrose ###

for (i in 1:nrow(QCbuoy_filter)) {
  if(QCbuoy_filter$Date[i] < as.Date("2022-06-25")){
    QCbuoy_filter$timeblock[i] <- "before"
    } else {
    QCbuoy_filter$timeblock[i] <- "during"
  }
}

QCbuoy_filter$WDIR <- as.numeric(QCbuoy_filter$WDIR)
WC_wind <- windrose(QCbuoy_filter$WSPD, QCbuoy_filter$WDIR, facet = QCbuoy_filter$timeblock,
                    ggtheme = "minimal")
WC_wind

##############################################################################################
#Vancouver Island - Line P 

OSPbuoy_filter <- read.csv(here::here("Buoy_Data", "OSPbuoy_filter.csv"))
OSPbuoy_filter$Date <- as.Date(OSPbuoy_filter$Date, format = "%Y-%m-%d")

### VI Wind speed (m/s) ###
ggplot(OSPbuoy_filter, aes(y=WSPD, x=Date)) +
  geom_vline(xintercept = as.Date("2022-08-04"), color= "orangered3", size = 2.5, alpha = 0.5) +
  geom_point() +
  theme_DOM() +
  labs(y ="Wind speed (m/s)", x="Date") +
  geom_smooth(method = "gam", colour = "darkgreen")

### VI Wind gust speed (m/s) ###
ggplot(OSPbuoy_filter, aes(y=GST, x=Date)) +
  geom_vline(xintercept = as.Date("2022-08-04"), color= "orangered3", size = 2.5, alpha = 0.5) +
  geom_point() +
  theme_DOM() +
  labs(y ="Wind gust speed (m/s)", x="Date") +
  geom_smooth(method = "gam", colour = "darkgreen")

### VI Windrose ###

for (i in 1:nrow(OSPbuoy_filter)) {
  if(OSPbuoy_filter$Date[i] < as.Date("2022-08-04")){
    OSPbuoy_filter$timeblock[i] <- "before"
  } else {
    OSPbuoy_filter$timeblock[i] <- "during"
  }
}

OSPbuoy_filter$WDIR <- as.numeric(OSPbuoy_filter$WDIR)


OSP_wind <- windrose(OSPbuoy_filter$WSPD, OSPbuoy_filter$WDIR, facet = OSPbuoy_filter$timeblock,
                    ggtheme = "linedraw")
OSP_wind


##############################################################################################
#Washington 

WAbuoy_filter <- read.csv(here::here("Buoy_Data", "WA_buoy_filter.csv"))
WAbuoy_filter$Date <- as.Date(WAbuoy_filter$Date, format = "%Y-%m-%d")
WAbuoy_filter$WDIR <- as.numeric(WAbuoy_filter$WDIR)
WAbuoy_filter$WSPD <- as.numeric(WAbuoy_filter$WSPD)


### WA Wind speed (m/s) ###
ggplot(WAbuoy_filter, aes(y=WSPD, x=Date)) +
  geom_vline(xintercept = as.Date("2022-08-03"), color= "skyblue2", size = 2.5, alpha = 0.5) +
  geom_vline(xintercept = as.Date("2022-08-06"), color= "blue2", size = 2.5, alpha = 0.5)+
  geom_point() +
  theme_DOM() +
  labs(y ="Wind speed (m/s)", x="Date") +
  geom_smooth(method = "gam", colour = "darkgreen")


### WA Windrose ###

for (i in 1:nrow(WAbuoy_filter)) {
  if(WAbuoy_filter$Date[i] < as.Date("2022-08-03")){
    WAbuoy_filter$timeblock[i] <- "before"
  } else {
    WAbuoy_filter$timeblock[i] <- "during"
  }
}

WA_wind <- windrose(WAbuoy_filter$WSPD, WAbuoy_filter$WDIR, facet = WAbuoy_filter$timeblock,
                    ggtheme = "minimal")
WA_wind


#COMBINING ALL WIND 

WA_sub <- WAbuoy_filter %>%
  filter(timeblock == "before") %>%
  select(WSPD, WDIR) %>% 
  mutate(ID = "USA_WA")
names(WA_sub)


OSP_sub <- OSPbuoy_filter %>%
  filter(timeblock == "before")%>%
  select(WSPD, WDIR) %>% 
  mutate(ID = "CAN_VI")
names(OSP_sub)


QC_sub <- QCbuoy_filter %>%
  filter(timeblock == "before") %>%
  select(WSPD, WDIR) %>% 
  mutate(ID = "CAN_QC")
names(QC_sub)


before_comb <- rbind(WA_sub, OSP_sub, QC_sub)

windrose(before_comb$WSPD, before_comb$WDIR, facet = before_comb$ID,
         ggtheme = "linedraw", n_col = 3)

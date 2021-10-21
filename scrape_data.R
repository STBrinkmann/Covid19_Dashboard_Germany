library(sf)
library(dplyr)
library(jsonlite)
library(curl)
library(lubridate)
library(ggplot2)


# Shapefile for NUTS3 regions
germany_nuts3 <- readRDS(file.path("data", "germany_nuts3.RDS"))

# 1. Downlaod 7 days incidence of the last 14 days
kzf <- c(germany_nuts3$Kennziffer[1:400], 11001:11012) %>% 
  as.character() %>% 
  lapply(function(x) {
    if (nchar(x) < 5) {
      x <- paste0(0, x)
    }
    return(x)
  }) %>% unlist(use.names = F)

nuts3_14d <- tibble(
  "Kennziffer" = numeric(),
  "Date" = Date(),
  "Cases_Last_Week" = numeric()
)

cat("1. Downlaod historic 7-days incidence data\n")

pb = txtProgressBar(min = 0, max = length(kzf), initial = 0, style = 3)
for (nuts3 in kzf) {
  nuts3.url <- paste0("https://github.com/entorb/COVID-19-Coronavirus-German-Regions/raw/master/data/de-districts/de-district_timeseries-", nuts3, ".tsv")
  nuts3.tbl <- read.table(nuts3.url, header = T, sep = "\t") %>% 
    dplyr::as_tibble() %>%
    mutate(Date = lubridate::as_date(Date)) %>%
    filter(Date >= Sys.Date()-14)
  
  if (nrow(nuts3.tbl) > 0) {
    nuts3_14d <- nuts3.tbl %>%
      mutate(Kennziffer = as.numeric(nuts3),
             Cases_Last_Week_per_100000 = round(Cases_Last_Week_Per_Million/10)) %>% 
      select(Kennziffer, Date, Cases_Last_Week_per_100000) %>% 
      rename(Cases_Last_Week = Cases_Last_Week_per_100000) %>% 
      add_row(nuts3_14d, .)
  }
  setTxtProgressBar(pb, which(kzf == nuts3))
}

# Summarise Berlin
nuts3_14d <- add_row(nuts3_14d[!nuts3_14d$Kennziffer %in% 11001:11012, ],
                     nuts3_14d[nuts3_14d$Kennziffer %in% 11001:11012, ] %>%
                       group_by(Date) %>% 
                       summarize(Kennziffer = 11000, Date = Date, Cases_Last_Week = mean(Cases_Last_Week)) %>% 
                       ungroup() %>% 
                       distinct()) %>% 
  distinct()

cat("/n")

# 2. Download recent 7 days incidence and join with NUTS3 shp
cat("2. Downlaod recent 7-days incidence data\n")

month.abb <- c("Jan", "Feb", "MÃ¤rz", "Apr", "Mai", "Juni", "Juli", "Aug", "Sept", "Okt", "Nov", "Dez")
germany_nuts <- sapply(jsonlite::read_json("https://github.com/entorb/COVID-19-Coronavirus-German-Regions/raw/master/data/de-districts/de-districts-results.json"), 
                       function(x) {
                         if ("DIVI_Intensivstationen_Betten_belegt_Prozent" %in% names(x)) {
                           return(c(x$LK_ID, x$Cases_Last_Week_Per_100000, x$LK_Typ,
                                    paste0(x$DIVI_Intensivstationen_Betten_belegt_Prozent, "%"), 
                                    paste0(x$DIVI_Intensivstationen_Covid_Prozent, "%"),
                                    x$Date_Latest, x$Bundesland))
                         } else {
                           return(c(x$LK_ID, x$Cases_Last_Week_Per_100000, x$LK_Typ, "Nicht verfÃ¼gbar", "Nicht verfÃ¼gbar",
                                    x$Date_Latest, x$Bundesland))
                         }
                       }) %>%
  do.call(rbind, .) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(Kennziffer = V1,
                LastWeek_100k = V2,
                LK_Typ = V3,
                DIVI_ges = V4,
                DIVI_covid = V5,
                Date_Latest = V6,
                Bundesland = V7) %>% 
  dplyr::mutate(Kennziffer = as.numeric(Kennziffer),
                LastWeek_100k = as.numeric(LastWeek_100k),
                Date_Latest = lubridate::ymd(Date_Latest),
                Date_Latest = paste0(lubridate::day(Date_Latest), ". ", 
                                     month.abb[lubridate::month(Date_Latest)], " ",
                                     lubridate::year(Date_Latest)))

# Summarise Berlin
germany_nuts[germany_nuts$Kennziffer %in% 11001:11012, ] <- germany_nuts[germany_nuts$Kennziffer %in% 11001:11012, ] %>%
  summarize(Kennziffer = 11000, LastWeek_100k = mean(LastWeek_100k), 
            LK_Typ, DIVI_ges, DIVI_covid, Date_Latest, Bundesland)

germany_nuts <- germany_nuts %>% 
  mutate(sieben_tage = round(LastWeek_100k)) %>% 
  distinct() %>% 
  dplyr::inner_join(germany_nuts3, .) %>% 
  dplyr::relocate(geom, .after = last_col())

# 3. Add dynamic color code
cat("3. Color coding\n")

if (max(germany_nuts$sieben_tage, na.rm = TRUE) < 300) {
  germany_nuts <- germany_nuts %>% 
    mutate(color = case_when(
      is.na(sieben_tage) ~ as.numeric(NA),
      sieben_tage < 5  ~ 1,
      sieben_tage > 250 ~ 7,
      (sieben_tage >= 5 & sieben_tage <= 25)  ~ 2,
      (sieben_tage > 25 & sieben_tage <= 50)  ~ 3,
      (sieben_tage > 50 & sieben_tage <= 100)  ~ 4,
      (sieben_tage > 100 & sieben_tage <= 150)  ~ 5,
      (sieben_tage > 150 & sieben_tage <= 250 )  ~ 6
    ) %>% factor(levels = 1:7)) %>% 
    dplyr::relocate(geom, .after = last_col())
} else if (max(germany_nuts$sieben_tage, na.rm = TRUE) < 500) {
  germany_nuts <- germany_nuts %>% 
    mutate(color = case_when(
      is.na(sieben_tage) ~ as.numeric(NA),
      sieben_tage < 5  ~ 1,
      sieben_tage > 300 ~ 7,
      (sieben_tage >= 5 & sieben_tage <= 25)  ~ 2,
      (sieben_tage > 25 & sieben_tage <= 50)  ~ 3,
      (sieben_tage > 50 & sieben_tage <= 100)  ~ 4,
      (sieben_tage > 100 & sieben_tage <= 200)  ~ 5,
      (sieben_tage > 200 & sieben_tage <= 300 )  ~ 6
    ) %>% factor(levels = 1:7)) %>% 
    dplyr::relocate(geom, .after = last_col())
} else {
  germany_nuts <- germany_nuts %>% 
    mutate(color = case_when(
      is.na(sieben_tage) ~ as.numeric(NA),
      sieben_tage < 5  ~ 1,
      sieben_tage > 500 ~ 7,
      (sieben_tage >= 5 & sieben_tage <= 25)  ~ 2,
      (sieben_tage > 25 & sieben_tage <= 50)  ~ 3,
      (sieben_tage > 50 & sieben_tage <= 150)  ~ 4,
      (sieben_tage > 150 & sieben_tage <= 300)  ~ 5,
      (sieben_tage > 300 & sieben_tage <= 500 )  ~ 6
    ) %>% factor(levels = 1:7)) %>% 
    dplyr::relocate(geom, .after = last_col())
}


# Safe data
saveRDS(germany_nuts, file.path("data", "germany_nuts3_incidence.RDS"))


# 4. Build and save plots
cat("4. Building plots\n")

suppressWarnings(
  suppressMessages(
    {
      for (i in unique(nuts3_14d$Kennziffer)) {
        nuts3 <- nuts3_14d %>% 
          filter(Kennziffer == i) %>% 
          arrange(desc(Date))
        
        p1 <- nuts3 %>% 
          ggplot(aes(x = Date, y = Cases_Last_Week)) + 
          geom_smooth(se = F, span = 0.2, color = "black", lwd = 4.5) + 
          geom_point(data = nuts3[1,], size = 12) + 
          scale_x_date(breaks = c(Sys.Date()-12, Sys.Date()-4),
                       date_labels = "%d %b") +
          scale_y_continuous(breaks = round(c(min(nuts3$Cases_Last_Week),
                                              min(nuts3$Cases_Last_Week) + diff(range(nuts3$Cases_Last_Week))/2,
                                              max(nuts3$Cases_Last_Week)))) +
          labs(x = "", y = "") +
          ggthemes::theme_few() +
          theme(panel.border=element_blank(),
                axis.line = element_line(size = 2, colour = "black"),
                axis.ticks = element_line(size = 2, colour = "black"),
                axis.text.x = element_text(size = 35, colour = "black", face = "bold"),
                axis.text.y = element_text(size = 42, colour = "black", face = "bold"))
        
        ggplot2::ggsave(file.path("Plots", paste0(i, ".svg")), p1, device = "svg", 
                        width = 10, height = 4)
      }
    }
  )
)

cat("Done!/n")

#1Czy zarobki podczas pandemii korona wirus roznia sie poza !!!!
#3Jakie gry przynaszą najwiekszy zysk !!!!
#4wpływ turniejow na zarobki !!!
#6 Czy istnieje sezonowość w organizowaniu turniejów, a jeśli tak, to jak wpływa to na uczestnictwo graczy i uzyskiwane dochody? !!!!
#7#Czy wzrost ilosci graczy ozancza rozwój turniejów

#
install.packages("pie")

#install.packages("readxl")
library(readxl)
#install.packages("mice")
library(mice)
#install.packages("outliers")
library(outliers)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(europepmc)
library(cowplot)
library(lubridate)



dane <- read_excel("C:\\Users\\margr\\OneDrive\\Dokumenty\\PassMark\\BatteryMon\\HistoricalEsportData.xlsx")

dane
#opis daty
str(dane$Date)

dane$Date <- as.Date(dane$Date, format="%Y-%m-%d")
head(dane)
str(dane)
summary(dane)
dane$Earnings <- as.numeric(dane$Earnings)
dane <- dane %>%
  filter(Earnings >= 1000 & Players >= 10 & Date > as.Date("2009-01-01"))
head(dane)
#dane$Earnings <- dane$Earnings/1000
dane
head(dane)
str(dane)
summary(dane)
md.pattern(dane)
table(dane$Game)
boxplot(dane$Earnings)
grubbs.test(dane$Earnings)
table(dane$Game)

# 111111111111Czy zarobki w eSporcie były niższe podczas pandemii Covid-19? opis

daneCovid <- dane %>%
  filter(Date >= as.Date("2020-03-21") & Date <= as.Date("2021-03-21")) %>%
  group_by(Month = format(Date, "%m")) %>%
  summarise(ZarobkiCovid = mean(Earnings))
danePrzedCovid <- dane %>%
  filter(Date < as.Date("2020-03-21") & Date > as.Date("2019-03-21")) %>%
group_by(Month = format(Date, "%m")) %>%
  summarise(ZarobkiPrzedCovid = mean(Earnings))
danePoCovid <- dane %>%
  filter(Date > as.Date("2021-03-21") & Date < as.Date("2022-03-21")) %>%
group_by(Month = format(Date, "%m")) %>%
  summarise(ZarobkiPoCovid = mean(Earnings))

danePoCovid

wykres <- ggplot() +
  geom_line(data = daneCovid, aes(x = as.numeric(Month), y = ZarobkiCovid / 1000000, color = "Covid"), linewidth = 1) +
  geom_line(data = danePrzedCovid, aes(x = as.numeric(Month), y = ZarobkiPrzedCovid / 1000000, color = "Przed Covid"), linewidth = 1) +
  geom_line(data = danePoCovid, aes(x = as.numeric(Month), y = ZarobkiPoCovid / 1000000, color = "Po Covid"), linewidth = 1) +
  labs(title = "Średnie zarobki w miesiącach",
       x = "Miesiąc",
       y = "Średnie zarobki (Miliony)") +
  scale_color_manual(values = c("Covid" = "blue", "Przed Covid" = "red", "Po Covid" = "yellow")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12)
wykres

DaneSCovid <- subset(dane, Date >= as.Date("2020-03-11") & Date <= as.Date("2021-03-30"))
DanePoCovid <- subset(dane, Date > as.Date("2021-03-11") & Date <= as.Date("2022-03-30"))
DanePrzedCovid <- subset(dane, Date < as.Date("2020-03-11") & Date > as.Date("2019-03-30"))

sredniaPo <- round(mean(DanePoCovid$Earnings), digits = 2)
sredniaPrzed <- round(mean(DanePrzedCovid$Earnings), digits = 2)
sredniaCovid <- round(mean(DaneSCovid$Earnings), digits = 2)

sredniaPo
sredniaCovid
sredniaPrzed

paste('Srednia zarobków w czasie Pandemii :', sredniaCovid, 'Srednia zarobków po Pandemii :', sredniaPo, 'Srednia zarobków przed Pandemia: ', sredniaPrzed)
#22222222222222222222Jakie gry przynosza najwiekszy zysk wykres




PrzychodyZGier <- tapply(dane$Earnings, dane$Game, mean, na.omit = TRUE)
PrzychodyZGier
PrzychodyZGierLiczba <- format(PrzychodyZGier, scientific = F)
PrzychodyZGierLiczba <- as.numeric(PrzychodyZGierLiczba)
PrzychodyZGierLiczba <- PrzychodyZGier / 1000000
PrzychodyZGierLiczba <- round(PrzychodyZGierLiczba, digits = 2)
PrzychodyZGierLiczba
table <- data.frame(Game = names(PrzychodyZGier), Earnings = PrzychodyZGierLiczba)
top <- arrange(table, desc(Earnings))
top
top8 <- head(top, 8)
top8$Game[3] <- "BATTLEGROUNDS Mobile"
top8$Game[8] <- "BATTLEGROUNDS"
top8

wykres <- ggplot(top8, aes(x = reorder(Game, Earnings), y = Earnings / 1000000)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = Earnings), vjust = 2, size = 4) +
  labs(title = "Średnie zarobki z gier(w milionach.)", x = "Gry", y = "Przychód") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6),
                     breaks = seq(1:max(top8$Earnings)))
wykres


#33333333333333333333Zaleznosc zarobków od liczby turniejów wykres
top20 <- dane %>%
  group_by(Game) %>%
  summarise(Zarobki = round(mean(Earnings), digits = 2),
            Gracze = round(mean(Players), digits = 0))
top20

table <- data.frame(Games = top20$Game,
                    Players = top20$Gracze,
                    Earnings = top20$Zarobki)
table

top20 <- head(arrange(table, desc(Players)), 10)
 
top20$Games[5] = "BATTLEGROUNDS Mobile"
top20$Games[6] = "BATTLEGROUNDS"
top20$Games[1] = "CS GO"
top20
ggplot(top20, aes(x = Players, y = Earnings, label = Games, color = Games)) +
  geom_point() +
  geom_text(
    aes(label = Games),
    vjust = -0.7,  # Dostosowanie pionowej pozycji etykiety
    hjust = 0.7,   # Dostosowanie poziomej pozycji etykiety
    nudge_x = 0.5, # Dostosowanie pozycji etykiety wzdłuż osi x
    nudge_y = 0.5  # Dostosowanie pozycji etykiety wzdłuż osi y
  ) +
  labs(
    title = "Zależność między Liczbą Turniejów a Zarobkami",
    x = "Liczba Turniejów",
    y = "Zarobki"
  ) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, sep = ""))

dane

#===========
SrednieZarobkiTurnieje <- dane %>%
  group_by(Tournaments) %>%
  summarise(SrednieZarobkiTurnieje = mean(Earnings))


SrednieZarobkiTurnieje$SrednieZarobkiTurnieje <- SrednieZarobkiTurnieje$SrednieZarobkiTurnieje / 1000000
SrednieZarobkiTurnieje


wykres <- SrednieZarobkiTurnieje %>%
  ggplot(aes(x = Tournaments, y = SrednieZarobkiTurnieje)) +
  geom_line() +
  labs(
    x = "Liczba turniejów",
    y = "Zarobki",
    title = "Zależność zarobków od liczby turniejów",
  ) +
  
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6)) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), labels = seq(0, 200, by = 20))
wykres




#4444444444444444444zarobki od popularnosci wykres

SredniaGraczyZarobkiGry <- dane %>%
group_by(Game) %>%  
summarise(SredniaGraczyGry = round(mean(Players), digits = 0), SredniaZarobkow = mean(Earnings))  
SredniaGraczyZarobkiGry

WykresPunktowy <- ggplot(SredniaGraczyZarobkiGry, aes(x = SredniaGraczyGry, y = SredniaZarobkow))+
  geom_point(size = 1, color = "red") +
              labs(
                x = "Srednia liczba graczy",
                y = "Srednia liczba zarobków",
                title = "Zależność między ilością graczy a średnimi zarobkami"
              ) + 
              theme_minimal()
WykresPunktowy
aov(Earnings ~ Game, data = dane)

# trend
#install.packages("europepmc")
#install.packages("cowplot")
#install.packages("lubridate")
library(europepmc)
library(cowplot)
library(lubridate)

#55555555555555555555555trend wykres

trend<- europepmc::epmc_hits_trend(query = "Earnings", period = 2009:2022)
trend


trend %>%
  ggplot(aes(x = factor(year), y = query_hits / all_hits)) +
  geom_point() +
  geom_line()
  


trend %>%
  ggplot(aes(x = factor(year), y = (query_hits / all_hits)))+
  geom_col(fill = "#56B4E9", width = 0.6, alpha = 0.9) +
  labs(x = "Lata", y = "Procentowy udział wyników zapytania Zarobków") +
  ggtitle("Trend wyników zapytania Zarobki w latach 1998-2022")
  



trend %>%
  ggplot(aes(x = factor(year), y = (query_hits / all_hits)))+
  geom_col(fill = "blue", width = 0.6, alpha = 0.9)+
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal_hgrid(12) +
  labs(x = "Year", y = "costam2") + 
  ggtitle("chybatytl")
dane

#6666666666666666korelacja liczby graczy z turniejami opis
dane %>%
  group_by(Players)
  summarise(Srednia)
  ggplot(dane, aes(x = Players, y = Tournaments)) +
  geom_point() +
  geom_line() +
  theme_minimal()
  
 
  
  # Wyświetl wyniki
  print(paste("Współczynnik korelacji:", korelacja))
  
  # Stwórz wykres punktowy
  wykres_korelacji <- ggplot(dane, aes(x = Players, y = Tournaments)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Korelacja między Liczbą Graczy a Liczbą Turniejów",
         x = "Liczba Graczy",
         y = "Liczba Turniejów")
  
wykres_korelacji




# umiarkowanie silne


#677777777777777777Ilosc turniejow w latach


DaneTurniejowe <- dane %>%
  group_by(Game) %>%
  summarise(SrednieTurnieje = round(mean(Tournaments), digits = 0))
DaneTurniejowe
 table <- arrange(DaneTurniejowe, desc(SrednieTurnieje))
table
top_8 <- head(table, 8)
top_8



#===========================================================

DaneTurniejowe <- dane %>%
  group_by(Year = format(Date, "%Y")) %>%
  summarise(SredniaTurniejow = sum(Tournaments))

ggplot(DaneTurniejowe, aes(x = Year, y = SredniaTurniejow))+
  geom_bar(stat = "identity", fill = "blue", color = "black", width = 0.8) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) + 
  theme_minimal() + 
  labs(title = "Ilość turniejów zorganizowanych w danych latach",
       x = "Lata", y = "Suma Turniejów" )

odch <- sd(DaneTurniejowe$SredniaTurniejow)
srednia <- mean(DaneTurniejowe$SredniaTurniejow)
odch
srednia


#Zmiany w strukturze zarobków:
  
 # Jak zmieniała się struktura zarobków w e-sporcie? Czy można zauważyć zmiany w proporcjach zarobków pomiędzy różnymi grami?
  
analiza <- top8 
suma
SumaZarobkow <- sum(analiza$Earnings)



#Rozkład zarobków w poszczególnych grach
ggplot(dane, aes(x = Game, y = Earnings)) +
  geom_boxplot() +
  labs(title = "Rozkład zarobków w różnych grach",
       x = "Gra",
       y = "Zarobki") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#=============================#
Year <- format(Date, "%Y")
wykres <- ggplot(top8, aes(x = Year), y = )


DaneTurniejowe <- dane %>%
  group_by(Year = format(Date, "%Y")) %>%
  summarise(SredniaTurniejow = sum(Tournaments))

ggplot(DaneTurniejowe, aes(x = Year, y = SredniaTurniejow))+
  geom_bar(stat = "identity", fill = "blue", color = "black", width = 0.8) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) + 
  theme_minimal() + 
  labs(title = "Ilość turniejów zorganizowanych w danych latach",
       x = "Lata", y = "Suma Turniejów" )

odch <- sd(DaneTurniejowe$SredniaTurniejow)
srednia <- mean(DaneTurniejowe$SredniaTurniejow)
odch
srednia



#==========================================

PopularneGryDoTurniejow <- dane %>%
  group_by(Game) %>%
  summarise(GryTurnieje = sum(Tournaments))
  PopularneGryDoTurniejow

table <- data.frame(Games = PopularneGryDoTurniejow$Game,
                    Tournaments = PopularneGryDoTurniejow$GryTurnieje)
table
top10 <- arrange(table, desc(Tournaments))

wykres <- ggplot(top10, aes(x = reorder(Games, Tournaments), y = Tournaments)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Top 10 gier z największą ilością turniejów",
       x = "Gra",
       y = "Liczba turniejów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
wykres

dane


#=================jaka gra jest najczesciej na turniejach SEZ
dane$month <- format(dane$Date, "%m")
dane$Year <- format(dane$Date, "%Y")
lata <- subset(dane, Year %in% c("2018", "2019", "2020", "2021", "2022", "2023"))
sredniatruniejow <- aggregate(Tournaments ~ month, data = dane, FUN = mean)
wykres <- ggplot(lata, aes(x = as.numeric(Month), y = Tournaments, group = interaction(Year, Month), color = Year)) +
  geom_line() +
  geom_point() +
  labs(title = "Liczba turniejów w poszczególnych miesiącach (2018-2023)", x = "Miesiące", y = "Liczba turniejów") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))


sez <- dane %>%
  group_by(Date) %>%


  
  
  # Wyodrębnienie miesiąca i roku z daty
  dane$Month <- format(dane$Date, "%m")
dane$Year <- format(dane$Date, "%Y")

# Przedział lat do uwzględnienia na wykresie
lata_do_wykresu <- c("2019", "2020", "2021", "2022")

# Wybór danych tylko dla wybranych lat
dane_wybrane_lata <- subset(dane, Year %in% lata_do_wykresu)

# Tworzenie wykresu linii z rozmieszczeniem kolorów dla lat
wykres_linii <- ggplot(dane_wybrane_lata, aes(x = Month, y = Tournaments, group = Year, color = Year)) +
  stat_summary(fun = "mean", geom = "line", position = position_dodge(width = 0.8), size = 1) +
  labs(title = "Średnia liczba turniejów w danych miesiącach na przestrzeni lat (2018-2022)",
       x = "Miesiąc",
       y = "Średnia liczba turniejów") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set1")
# Wyświetlenie wykresu punktowego
wykres_linii


wykres <- ggplot(top8, aes(x = Earnings , y = reorder(Game, -Earnings))) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = Earnings), vjust = 2, size = 4) +
  labs(title = "Średnia pula nagród w zależności od gry (w milionach.)", x = "Średnia wartość puli nagród", y = "Gry") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 4))

wykres


daneCovid <- dane %>%
  filter(Date >= as.Date("2020-03-21") & Date <= as.Date("2021-03-21")) %>%
  group_by(Month = format(Date, "%m")) %>%
  summarise(ZarobkiCovid = sum(Earnings))

# Filtruj dane przed okresem pandemii
danePrzedCovid <- dane %>%
  filter(Date < as.Date("2020-03-21") & Date > as.Date("2019-03-21")) %>%
  group_by(Month = format(Date, "%m")) %>%
  summarise(ZarobkiPrzedCovid = sum(Earnings))

# Filtruj dane po okresie pandemii
danePoCovid <- dane %>%
  filter(Date > as.Date("2021-03-21") & Date < as.Date("2022-03-21")) %>%
  group_by(Month = format(Date, "%m")) %>%
  summarise(ZarobkiPoCovid = sum(Earnings))

# Twórz wykres linii
wykres <- ggplot() +
  geom_line(data = daneCovid, aes(x = as.numeric(Month), y = ZarobkiCovid / 1000000, color = "Covid"), linewidth = 1) +
  geom_line(data = danePrzedCovid, aes(x = as.numeric(Month), y = ZarobkiPrzedCovid / 1000000, color = "Przed Covid"), linewidth = 1) +
  geom_line(data = danePoCovid, aes(x = as.numeric(Month), y = ZarobkiPoCovid / 1000000, color = "Po Covid"), linewidth = 1) +
  labs(title = "Suma zarobków w zależności od okresu Pandemicznego",
       x = "Miesiąc",
       y = "Suma zarobków (Miliony)") +
  scale_color_manual(values = c("Covid" = "blue", "Przed Covid" = "red", "Po Covid" = "yellow")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12)

wykres
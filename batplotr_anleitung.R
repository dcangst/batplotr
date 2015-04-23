# Installation (muss nur einmal gemacht werden!)
install.packages(c("devtools"),dependencies=TRUE)
devtools::install_github("dcangst/batplotr",dependencies=TRUE)

# Laden der Package und setzen des Arbeitsverzeichnisses
library(batplotr)
setwd("/Users/daniel/Dropbox/Bioakustik Austausch Manu Daniel")

# Daten laden
  daten <- readBatscopeXLSX(
    species_col_name = "AutoClass1",
    quality_col_name = "AutoClass1Qual",
    quality_threshold  = 0.8
    )

# Daten Zusammenfassen
  daten_sum <- sumBatscopeData(
    daten,
    bin_length=5,
    progress="text"
    )

# Daten inspizieren
  str(daten_sum)

  ddply(daten_sum,.(ProjectName,SurveyDate),summarize,n_events_day=sum(n_events))

# nightPlot mit Standardwerten
  nightPlot(daten_sum)

# Datumsauswahl1
  unique(daten_sum$SurveyDate)
  nightPlot(daten_sum,day="2014-08-23")

# Datumsauswahl 2
  nightPlot(daten_sum,day=unique(daten_sum$SurveyDate)[1])

# Datumsauswahl 3, 
  nightPlot(daten_sum,day=c("2014-08-23","2014-08-24"))

# Speziesauswahl 1
  unique(daten_sum$species)
  nightPlot(daten_sum,"2015-04-15",sel_species="Pipistrellus nathusii")
  nightPlot(daten_sum,"2014-07-24",sel_species=unique(daten_sum$species)[3])

# Speziesauswahl 2
  nightPlot(daten_sum,day="2014-07-24",
    sel_species=c("Pipistrellus nathusii","Pipistrellus kuhlii"))
  nightPlot(daten_sum,day="2014-07-24",
    sel_species=unique(daten_sum$species)[3:4])

# Standortauswahl 1
  unique(daten_sum$ProjectName)
  daten_sum_StO1 <- subset(daten_sum,ProjectName=="PT 2014 Waidberg")
  # oder direkt:
  daten_sum_StO1 <- subset(daten_sum,
    ProjectName==unique(daten_sum$ProjectName)[4])
  nightPlot(daten_sum_StO1, day="2014-07-24")

# Anpassung der Achsen
  nightPlot(daten_sum_StO1, day=c("2014-08-23","2014-08-24"),
    x_limits=as.POSIXct(c("2014-08-23 16:00","2014-08-25 10:00")),
    y_limits=c(0,12))

# alles zusammen!
    daten_sum_StO1 <- subset(daten_sum,
      ProjectName==unique(daten_sum$ProjectName)[4])
    nightPlot(daten_sum_StO1,
      day=c("2014-08-23","2014-08-24"),
      sel_species=unique(daten_sum_StO1$species)[c(1,2,4)],
      x_limits=as.POSIXct(c("2014-08-23 16:00","2014-08-25 10:00")),
      y_limits=c(0,12))

## periodPlot 1 mit Standardwerten
periodPlot(daten_sum)

# periodPlot 2
plot1 <- periodPlot(daten_sum,
  start_date="2014-03-01",
  end_date="2014-11-01",
  sel_species=unique(daten_sum$species)[3:5],
  y_limits=c("1900-01-01 17:30","1900-01-02 08:30"))

# plotdaten 
head(plot1$data)

# hilfe
?batplotr
?nightlot


# batplotR - BatScope Datenvisualisation mit R

## Über `batplotR`



`batplotR` ist eine R Package die im Rahmen eines Zivildiensteinsatzes entwickelt wurde. Sie wurde für die Darstellung von mit [BatScope](http://www.wsl.ch/dienstleistungen/produkte/software/batscope/index_DE) ausgewerteten Bioakustikdaten entwickelt und erlaubt es kurze (d.h. über  eine Nacht) und lange (d.h. mehrere Nächte) Beobachtungsreihen darzustellen. Ausgangspunkt ist in jedem Fall ein `.xlsx`-Exportfile von Batscope ( getestet , mit BatScope v.3.1.6)
    
Die Package ist GPL-3 lizenziert und auf [GitHub gehostet](https://github.com/dcangst/batplotr), kann also beliebig geforkt, weitergegeben und  verändert  werden.

## Installation

`batplotR` kann direkt von GitHub installiert werden, voraussetzung ist die Package [`devtools`](https://github.com/hadley/devtools) die über CRAN erhältlich ist und wie folgt installiert werden kann:


```r
install.packages(c("devtools"),dependencies=TRUE)
```
danach kann batplotR direkt von GitHub installiert werden (es werden auch verschiedene andere Packages installiert, die von batplotR benötigt werden):


```r
devtools::install_github("dcangst/batplotr",dependencies=TRUE)
```

## Benutzung

mit `batplotR` visualisieren Sie ihre BatScope-Projekte in _3 einfachen Schritten!_

1. Daten laden!
2. Daten zusammenfassen!
3. Daten visualizieren!

et voila!

### Laden der Package

Zuerst wird die Package geladen und das Arbeitsverzeichniss[^1] gesetzt. 


```r
library(batplotr)
setwd("/Users/daniel/Dropbox/Bioakustik Austausch Manu Daniel")
```


[^1]: Das Arbeitsverzeichniss in `R` ist das Standardverzeichnis relativ zu dem R alle angegebenen Pfad- und Dateinamen interpretiert. Hier werden z.B. Dateien gespeichert, wenn man nicht einen kompletten Pfad angibt.

### Laden der BatScope Daten

jetzt können die Daten eingelesen werden. Dafür wird der Befel `readBatscopeXLSX` verwendet.


```r
daten <- readBatscopeXLSX(
  species_col_name = "AutoClass1",
  quality_col_name = "AutoClass1Qual",
  quality_threshold  = 0.8
  )
```

```
## 
## /Users/Daniel/Dropbox/Bioakustik Austausch Manu Daniel/Export_2015-04-14_11.50.35_PT.xlsx
## wird eingelesen, kann eine Weile dauern...
```

```
## Summary of AutoClass1Qual
## 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.306   0.591   0.686   0.696   0.798   1.000    9552 
## 
## 'Discarded 27350 of 33199 sequences (82.382%); 5849 remaining
```
Der Befehl ruft ein 'Datei öffnen...' Dialog auf bei dem das `.xlsx`-file vom BatScope export ausgewählt werden muss. Ausserdem werden noch die folgenden Parameter benötigt:

----------------------------------------------------------------
Parameter          Was?                    Standardwert
-----------------  ----------------------  ---------------------
path               Pfad zum `.xlsx` -file   file.choose()[^2]

species_col_name   Spaltenname Spezies     `"AutoClass1"`

quality_col_name   Spaltenname Qualität    `"AutoClass1Qual"`

quality_threshold  minimale Qualität der   `0.8`
                   Spezieszuordnung
----------------------------------------------------------------

Table: Tabelle 1: Parameter der Funktion `readBatscopeXLSX`

[^2]: zeigt einen 'Datei öffnen...'-Dialog an

(*Mit R 3.1.3 wird ein Fehler in der Konsole ausgeworfen. Dieser kann ignoriert werden*)

Die eingelesenen Daten werden unter dem Namen `daten` abgelegt (mit Hilfe der Zuweisung `<-`). Wie der Tabelle zu entnehmen ist, hätten in diesem Fall die Daten auch mit `readBatscopeXLSX()` alleine eingelesen werden können, da alle Standardwerte verwendet wurden.

Es empfiehlt sich den Import mit der funktion `str(daten)` oder `head(daten)`anzuschauen. Als nächstes werden die Daten nach Standorten und Tagen in kurze Zeitabschnitte zusammengefasst (sogenanntes 'binning').

### Zusammenfassung der Daten

Um die Daten zu visualisieren werden die einzelnen Sequenzen in kurzen Zeitabschnitten zusammengefasst. Die Länge dieser Zeitabschnitte kann vom Benutzer eingestellt werden. Ausserdem werden hier die Sonnenaufgangs und -untergangszeiten berechnet (nach Algorithmen der NOAA). Bei grossen Datensätzen kann dieser Befehl eine kleine Weile dauern.

Für die Zusammenfassung wird der Befehl `sumBatscopeData` ausgeführt:


```r
  daten_sum <- sumBatscopeData(
    daten,
    bin_length=5,
    progress="true"
    )
```

```
## Zusammenfassung nach Tag, Project, Species und Bins...
## Zusammenfassung Total aller species...
## GPS Koordinaten bearbeiten...
```

```
## Koordinaten von Batlogger verwendet.
```

```
##         ProjectName      lat     long
## 1 PT 2014 Limmattal 47.38614 8.517271
## 2       PT 2014 See 47.38596 8.517406
## 3 PT 2014 Uetliberg 47.38582 8.517228
## 4  PT 2014 Waidberg 47.38631 8.517434
## Berechne Sonnenauf/untergangszeiten...
```
Der Befehl benötigt die folgenden Parameter:

----------------------------------------------------------------------------
Parameter          Was?                                Standardwert
-----------------  ---------------------------------   ---------------------
data_r             `data.frame` generiert mit          -
                   `readBatscopeXLSX' 

bin_width          Zeitspanne in min über die          `5`
                   zusammengefasst wird
                   (binning)

lat                Breitengrad (N-S in Decimalgrad)    `NULL`
                    der Station. Wenn `NULL`
                   werden die GPS Koordinaten des
                   BatLoggers verwendet

long               Längengrad (O-W in Decimalgrad)     `NULL`
                    der Station. Wenn `NULL`
                   werden die GPS Koordinaten des
                   BatLoggers verwendet

progress           Art der Fortschritssanzeige.        `"text"`
                   `"none"` für keine.
----------------------------------------------------------------------------

Table: Tabelle 2: Parameter der Funktion `sumBatscopeData`

Die eingelesenen Daten werden unter dem Namen `daten_sum` als data.frame abgelegt (mithilfe der Zuweisung `<-`). Wie der Tabelle zu entnehmen ist, hätten in diesem Fall die Daten auch mit `sumBatscopeData(daten)` alleine eingelesen werden können, da alle Standardwerte verwendet wurden. Mit `str(daten_sum)` erhält man einen kurzen Überblick über die Daten


```r
str(daten_sum)
```

```
## 'data.frame':	4329 obs. of  12 variables:
##  $ ProjectName: Factor w/ 4 levels "PT 2014 Limmattal",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ SurveyDate : POSIXct, format: "2014-03-07" "2014-03-07" ...
##  $ species    : Factor w/ 23 levels "all","Eptesicus nilssonii",..: 2 13 15 15 15 15 15 15 14 14 ...
##  $ bins_factor: Factor w/ 16896 levels "2014-03-07 13:00:00",..: 76 100 72 73 77 112 113 132 11766 11801 ...
##  $ n_events   : int  1 1 3 3 1 1 1 1 1 1 ...
##  $ sum_nCalls : num  1 1 161 113 54 1 1 1 5 16 ...
##  $ meanT_BL   : num  9 6 12 12 9 5 4 3 15 14 ...
##  $ bins       : POSIXct, format: "2014-03-07 19:15:00" "2014-03-07 21:15:00" ...
##  $ lat        : num  47.4 47.4 47.4 47.4 47.4 ...
##  $ long       : num  8.52 8.52 8.52 8.52 8.52 ...
##  $ sunset     : POSIXct, format: "2014-03-07 18:19:32" "2014-03-07 18:19:32" ...
##  $ sunrise    : POSIXct, format: "2014-03-08 06:53:13" "2014-03-08 06:53:13" ...
```

Mit den Daten kann natürlich mit den vielen Tools von R weitergearbeitet werden. Im folgenden ein Beispiel das die Package [`plyr`](http://plyr.had.co.nz) benützt um die Summe von Events (d.h. Anzahl Sequenzen) an den verschiedenen Standorten pro Tag auszurechnen (aus Platzgründen werden hier nur die ersten 6 Zeilen angezeigt)


```r
  ddply(daten_sum,.(ProjectName,SurveyDate),summarize,n_events_day=sum(n_events))
```

```
##         ProjectName SurveyDate n_events_day
## 1 PT 2014 Limmattal 2014-03-07           24
## 2 PT 2014 Limmattal 2014-03-11           28
## 3 PT 2014 Limmattal 2014-03-17           18
## 4 PT 2014 Limmattal 2014-03-20           46
## 5 PT 2014 Limmattal 2014-03-28          144
## 6 PT 2014 Limmattal 2014-04-17           24
```

### Plotten der Daten

Das Objekt `daten_sum`, generiert mit `sumBatscopeData`, kann jetzt auch mit den beiden Plotfunktionen `nightPlot` und `periodPlot` verwendet werden.

#### nightPlot

Die Funktion `nightPlot` generiert ein [ggplot](http://ggplot2.org) Objekt und hat folgende Parameter:

----------------------------------------------------------------------------
Parameter          Was?                                Standardwert
-----------------  ---------------------------------   ---------------------
plotData           `data.frame` generiert mit          -
                   `sumBatscopeData' 

day                Tag(e) die geplottet werden         erster Tag im
                                                       Datensatz

sel_species        Name der Spezies oder Namen von     `"every"`
                   mehreren Spezies als Vector oder
                   `"every"` für alle Spezies
                   einzeln oder `"all"` für die
                   Summe aller Spezies

x_limits           höchster und tiefster Wert der      `NULL`
                   x-Achse (ein Vector im POSIXct
                   Format), `NULL` für automatisch 
                   (halbe Stunde vor Sonnenuntergang
                   bis halbe Stunde nach 
                   Sonnenuntergang)

y_limits           höchster und tiefster Wert der      `NULL`
                   y-Achse (ein numerischer Vector),
                   `NULL` für automatisch
----------------------------------------------------------------------------

Table: Tabelle 3: Parameter der Funktion `nightPlot`

Die Standardwerte generieren in der Regel einen guten ersten Eindruck der Daten:


```r
nightPlot(daten_sum)
```

<figure><img src="batplotr_anleitung_files/figure-html/nightPlot1-1.png"><figcaption>Figure 1: Beispiel eines `nightPlots`</figcaption></figure>
Im folgenden einige Beispiele für die verschiedenen Optionen:

* __Datumsauswahl__

    mit dem R-Befehl `unique` können zunächst mal die Daten angezeigt werden, die im Datensatz vorhanden sind. Danach kann eines der Daten als Argument an `nightPlot` übergeben werden:

    
    ```r
    unique(daten_sum$SurveyDate)
    nightPlot(daten_sum,day="2014-07-24")
    ```

    da `unique(daten_sum$Survey)` ein Vector zurückgibt kann ein Element davon auch gleich an `nightPlot` übergeben werden:

    
    ```r
    nightPlot(daten_sum,day=unique(daten_sum$SurveyDate)[24])
    ```

    es können auch mehrere Tage angegeben werden (sollten aufeinanderfolgend sein):

    
    ```r
    nightPlot(daten_sum,day=c("2014-08-23","2014-08-24"))
    ```

* __Auswahl einer/mehrerer Spezies__

    ähnlich wie die Daten können auch die Spezies ausgewählt werden.

    
    ```r
    nightPlot(daten_sum,"2014-07-24",sel_species="Pipistrellus nathusii")
    nightPlot(daten_sum,"2014-07-24",sel_species=unique(daten_sum$species)[3])
    ```
    
    Es können auch mehrere Spezies ausgewählt werden:

    
    ```r
    nightPlot(daten_sum,day="2014-07-24",sel_species=c("Pipistrellus nathusii","Pipistrellus kuhlii"))
    nightPlot(daten_sum,day="2014-07-24",sel_species=unique(daten_sum$species)[3:4])
    ```

* __Darstellung von nur einem Standort__
    
    Um nur einen der Standorte im Datensatz anzuzeigen müssen zuerst die Daten eingschränkt werden:

    
    ```r
    unique(daten_sum$ProjectName)    
    daten_sum_StO1 <- subset(daten_sum,ProjectName=="PT 2014 Waidberg")
    # oder direkt:
    daten_sum_StO1 <- subset(daten_sum,ProjectName==unique(daten_sum$ProjectName)[4])
    nightPlot(daten_sum_StO1, day="2014-07-24")
    ```

* __x_limits & y_limits__
    
    um die Achsen anzupassen werden die Were `x_limits` & `y_limits` gesetzt:

    
    ```r
    nightPlot(daten_sum_StO1, day=c("2014-08-23","2014-08-24"), x_limits=as.POSIXct(c("2014-08-23 16:00","2014-08-25 10:00")), y_limits=c(0,12))
    ```

Ein Beispiel mit vielen Optionen:


```r
    daten_sum_StO1 <- subset(daten_sum,ProjectName==unique(daten_sum$ProjectName)[4])
    nightPlot(daten_sum_StO1, day=c("2014-08-23","2014-08-24"),sel_species=unique(daten_sum_StO1$species)[c(1,2,4)], x_limits=as.POSIXct(c("2014-08-23 16:00","2014-08-25 10:00")), y_limits=c(0,12))
```

<figure><img src="batplotr_anleitung_files/figure-html/nightPlot Beispiel-1.png"><figcaption>Figure 2: Beispiel eines `nightPlots` mit vielen Optionen</figcaption></figure>

#### periodPlot

Die Funktion `periodPlot` generiert ein [ggplot](http://ggplot2.org) Objekt und hat folgende Parameter und funktioniert im wesentlichen ähnlich wie `nightPlot`

----------------------------------------------------------------------------
Parameter          Was?                                Default
-----------------  ---------------------------------   ---------------------
plotData           `data.frame` generiert mit          -
                   `sumBatscopeData' 

start_date         Beginn der Zeitperiode              Anfang des Jahres des
                                                       ersten Datensatzes

end_date           Beginn der Zeitperiode              Ende des Jahres des
                                                       letzten Datensatzes

sel_species        Name der Spezies oder Namen von     `"every"`
                   mehreren Spezies als Vector oder
                   `"every"` für alle Spezies
                   einzeln oder `"all"` für die
                   Summe aller Spezies

x_limits           höchster und tiefster Wert der      `NULL`
                   x-Achse (ein Vector im POSIXct
                   Format), `NULL` für automatisch
                   (= start_date - end_date) 

y_limits           höchster und tiefster Wert der      `NULL`
                   y-Achse (ein Character oder
                   POSIXct Vector) mit Datum
                   1900-01-01 (abends) oder
                   1900-01-02 (morgens). `NULL`
                   für automatisch.

plotTitle          Title des Plots `NULL`              'NULL'
                   für automatisch.
----------------------------------------------------------------------------

Table: Tabelle 4: Parameter der Funktion `periodPlot`

Beispiel mit Standardwerten:


```r
periodPlot(daten_sum)
```

```
## Plotting number of sequences over period:
## [1] 2014-01-01 CET--2015-01-01 CET
```

```
## Warning: Removed 360 rows containing missing values (geom_point).
```

```
## Warning: Removed 357 rows containing missing values (geom_point).
```

```
## Warning: Removed 363 rows containing missing values (geom_point).
```

```
## Warning: Removed 362 rows containing missing values (geom_point).
```

<figure><img src="batplotr_anleitung_files/figure-html/periodPlot1-1.png"><figcaption>Figure 3: Beispiel eines `periodPlots`</figcaption></figure>

Die generierten Warnmeldungen hängeen damit zusammen, das nicht für alle Tage alle Daten vorhanden sind. 

`periodPlot`-Beispiel mit einigen Optionen. Man beachte auch, das der Plot hier zuerst als `ggplot` Objekt abgelegt wird und dann erst dargestellt wird.


```r
plot1 <- periodPlot(daten_sum,start_date="2014-03-01",end_date="2014-11-01",sel_species=unique(daten_sum$species)[3:5],y_limits=c("1900-01-01 17:30","1900-01-02 08:30"))
```

```
## Plotting number of sequences over period:
## [1] 2014-03-01 CET--2014-11-01 CET
```

Die effektiv geplotteten Daten können dann dem generierten `ggplot` Objekt entnommen werden:


```r
head(plot1$data)
```

```
##         ProjectName SurveyDate      lat     long              sunset
## 1 PT 2014 Limmattal 2014-03-01 47.38614 8.517271 2014-03-01 18:10:39
## 2 PT 2014 Limmattal 2014-03-02 47.38614 8.517271 2014-03-02 18:12:09
## 3 PT 2014 Limmattal 2014-03-03 47.38614 8.517271 2014-03-03 18:13:38
## 4 PT 2014 Limmattal 2014-03-04 47.38614 8.517271 2014-03-04 18:15:07
## 5 PT 2014 Limmattal 2014-03-05 47.38614 8.517271 2014-03-05 18:16:36
## 6 PT 2014 Limmattal 2014-03-06 47.38614 8.517271 2014-03-06 18:18:04
##               sunrise species bins_factor n_events sum_nCalls meanT_BL
## 1 2014-03-02 07:04:46    <NA>        <NA>       NA         NA       NA
## 2 2014-03-03 07:02:52    <NA>        <NA>       NA         NA       NA
## 3 2014-03-04 07:00:58    <NA>        <NA>       NA         NA       NA
## 4 2014-03-05 06:59:03    <NA>        <NA>       NA         NA       NA
## 5 2014-03-06 06:57:07    <NA>        <NA>       NA         NA       NA
## 6 2014-03-07 06:55:10    <NA>        <NA>       NA         NA       NA
##   bins time        sunrise_time         sunset_time
## 1 <NA> <NA> 1900-01-02 07:04:47 1900-01-01 18:10:40
## 2 <NA> <NA> 1900-01-02 07:02:53 1900-01-01 18:12:10
## 3 <NA> <NA> 1900-01-02 07:00:59 1900-01-01 18:13:39
## 4 <NA> <NA> 1900-01-02 06:59:04 1900-01-01 18:15:08
## 5 <NA> <NA> 1900-01-02 06:57:08 1900-01-01 18:16:37
## 6 <NA> <NA> 1900-01-02 06:55:11 1900-01-01 18:18:05
```

## Hilfe

Die Package sowie alle Funktionen haben eine Hilfeseite die mit dem R-Befehl `?` abgerufen werden kann, e.g.:


```r
?batplotr
?nightlot
```

## Entwicklung

Die Package wurde in R 3.1.2 getestet und der Quellcode ist auf [github](https://github.com/dcangst/batplotr) hinterlegt. Die Package kann also frei weiterentwickelt werden!

********************

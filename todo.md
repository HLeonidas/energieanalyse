# TODO: Analyse fachlich erweitern

## Umsetzungsstand 12.04.2026
Die erste Ausbaustufe aus diesem TODO ist umgesetzt.

Die Analysepipeline in `scripts/build_analysis.R` erzeugt jetzt:

- taegliche Roh- und Analysedatensaetze
- monatliche Analysedatensaetze
- einen saisonalen Datensatz fuer `winter`, `transition` und `summer`
- eine Ereigniszusammenfassung fuer Gasttage und Tage mit veraenderter bzw. sinkender Belegung
- ein HTML-Dashboard mit Highcharts unter `output/energy-analysis/dashboard.html`

Die Pipeline integriert inzwischen:

- Wetterdaten aus `data/wetter/`
- direkte Temperaturkennzahlen auf Tagesbasis
- Heizgradtage `HDD18`
- saisonale Cluster
- Vorher-Nachher-Kennzahlen bei Veraenderungen der Belegung
- temperaturbereinigte Fernwaerme ueber ein lineares Heizmodell
- Kennzahlen zu PV-Ueberschuss und theoretischem PV-zu-Waerme-Potenzial

Damit sind die Kernpunkte dieses TODOs fuer die erste fachliche Analyse umgesetzt.

## Ausgangslage
Die aktuelle Analyse aggregiert die vorhandenen Daten weitgehend über das gesamte Jahr 2025. Das ist für erste Übersichten nützlich, reicht aber für belastbare Aussagen über Zusammenhänge zwischen Belegung, Energieverbrauch, PV-Erzeugung und möglicher Optimierung noch nicht aus.

Insbesondere ist ein direkter Vergleich von Sommer- und Winterwerten bei Fernwärme fachlich nur eingeschränkt sinnvoll. Heizverbrauch muss stärker im saisonalen Kontext betrachtet werden.

## Zentrales Problem
Wir wollen nicht nur sehen, wie hoch der Verbrauch im Jahr insgesamt war, sondern verstehen:

- wie sich Verbrauch und Belegung innerhalb einzelner Monate verhalten
- ob der Verbrauch nach Abreise von Gästen zeitnah sinkt
- wie stark dieser Zusammenhang in den Wintermonaten ausgeprägt ist
- wie sich PV-Erzeugung und Verbrauch zueinander verhalten
- welches Optimierungspotenzial sich aus PV-Überschüssen und dem Zusammenspiel mit Wärme ergibt

## Fachliche Anforderungen an die naechste Analyse

### 1. Vergleich nicht nur auf Jahresbasis
Die Analyse soll nicht mehr primär das Gesamtjahr als eine einzige Vergleichseinheit betrachten.

Stattdessen sollen Vergleiche mindestens auf Monatsbasis und möglichst zusätzlich auf Tagesbasis erfolgen.

### 2. Monatsweise Analyse von Belegung, Wetter und Verbrauch
Innerhalb eines Monats sollen folgende Größen gemeinsam betrachtet werden:

- Belegung bzw. Nächtigungen
- Stromverbrauch
- Fernwärmeverbrauch
- PV-Erzeugung
- Wetterdaten bzw. Außentemperatur

Ziel ist ein statistisch sinnvollerer Vergleich innerhalb ähnlicher saisonaler Bedingungen.

### 3. Fokus auf Wintermonate
Für die Frage nach dem Heizverhalten sind vor allem die Wintermonate relevant.

Die Analyse soll daher besonders prüfen:

- ob Fernwärme mit höherer Belegung ansteigt
- ob Fernwärme nach Abreise von Gästen wieder sinkt
- wie stark dieser Effekt in den kalten Monaten sichtbar ist

### 4. Sommer und Winter nicht unreflektiert mischen
Ein direkter Vergleich von Fernwärme im Sommer mit Fernwärme im Winter ist inhaltlich wenig aussagekräftig.

Die Auswertung soll deshalb mindestens saisonal getrennt werden, zum Beispiel:

- Winter
- Übergangszeit
- Sommer

Alternativ kann dies über monatliche oder temperaturbasierte Cluster erfolgen.

### 5. Strom-Einspeisung vorerst aus dem Scope lassen
Die separate CSV zu `strom-einspeisung` soll aktuell nicht Teil der nächsten Ausbaustufe sein.

Für den Moment wird davon ausgegangen, dass die bestehende Sicht auf Stromverbrauch, PV-Erzeugung und PV-Überschüsse für die aktuelle Analyse ausreichend ist.

## Ergebnis der Erweiterung
Die Analyse ist jetzt so erweitert, dass wir nicht nur Visualisierungen haben, sondern erste belastbare Entscheidungsgrundlagen fuer Optimierungen.

Der aktuelle Datensatz deckt insbesondere ab:

- in welchen Monaten oder Zeitfenstern PV-Überschüsse auftreten
- ob und in welchem Umfang ueberschuessige PV-Energie potenziell fuer Waerme genutzt werden koennte
- wie Fernwaerme im Verhaeltnis zu Temperatur, Heizgradtagen und Belegung reagiert

## Neuer Stand Wetterdaten
Im Ordner `data/wetter/` liegt jetzt ein Wetterexport vor:

- Datei: `Messstationen Tagesdaten v2 Datensatz_20250101_20251231.csv`
- Zeitraum: `2025-01-01` bis `2025-12-31`
- Granularität: täglich
- Quelle: Geosphere, historische Stationsdaten
- Station laut Begleitdokumentation: `19821` `Weißensee-Gatschach`
- Variablen: `tl_mittel`, `tlmax`, `tlmin`, `rr`, `so_h`, `rf_mittel`, `vv_mittel`, `sh`

Damit haben wir jetzt eine konkrete und fachlich passende Wetterquelle auf Tagesbasis, die grundsätzlich direkt mit den übrigen Tagesdaten vergleichbar ist.

## Was mit den Wetterdaten bereits umgesetzt ist

### 1. Wetterdaten sind in die bestehende Analysepipeline integriert
Die Wetterdaten gehen als zusaetzliche Quelle in den taeglichen und monatlichen Analysedatensatz ein.

Dadurch sind unter anderem folgende Vergleichsgroessen vorhanden:

- Fernwaermeverbrauch in Abhaengigkeit von Temperatur
- Vergleich kalter und milder Tage innerhalb desselben Monats
- saisonale Auswertung auf Tages-, Monats- und Clusterebene

### 2. Temperaturkennzahlen werden direkt verwendet
Die Wetterquelle geht mit taeglichen Kennzahlen direkt in die Analyse ein, insbesondere:

- `tl_mittel` als Standardgröße für Tagesmitteltemperatur
- `tlmin` und `tlmax` für kalte bzw. warme Ausreißertage
- `rr` für Niederschlag
- `so_h` als Sonnenscheindauer
- `sh` als Schneehöhe

### 3. Winteranalyse ist temperaturbasiert erweitert
Die Winteranalyse erfolgt nicht nur nach Kalendermonat, sondern zusaetzlich anhand von Temperaturwerten und `HDD18`.

Damit laesst sich jetzt pruefen:

- ob hoher Fernwärmeverbrauch durch Belegung, Temperatur oder beides getrieben ist
- ob Verbrauch nach Abreise sinkt, wenn Temperatur ähnlich bleibt
- welche Grundlast auch ohne Gäste bei kaltem Wetter bestehen bleibt

### 4. Rohwerte sind technisch behandelt, fachlich aber noch zu pruefen
Einzelne Wetterfelder enthalten Sonder- oder Platzhalterwerte, zum Beispiel `rr = -1.0`.

Diese Werte werden in der Pipeline bereits konsistent behandelt und zusaetzlich markiert:

- `rr < 0` wird aktuell als `0 mm` uebernommen und ueber `precipitation_negative_flag` markiert
- leere `sh`-Werte bleiben `NA` und werden ueber `snow_depth_missing_flag` markiert

## Konkret umgesetzte Erweiterungen an der Pipeline
Die bestehende Pipeline wurde um folgende Punkte erweitert:

- Integration der Wetterdaten aus `data/wetter/`
- Verwendung der taeglichen Wetterwerte direkt auf Tagesbasis
- Aggregation der Wetterdaten von Tages- auf Monatsbasis fuer Monatsvergleiche
- Monatsweise Auswertung von Strom, Fernwaerme, PV, Belegung und Wetter
- Kennzahlen fuer Wintermonate und saisonale Cluster
- Analyse von Vorher-Nachher-Effekten bei Veraenderungen und sinkender Belegung
- Analyse von Fernwaermeverbrauch gegen Temperaturkennzahlen und `HDD18`
- aussentemperaturbereinigte Analyse der Fernwaerme pro Naechtigung
- Kennzahlen zu PV-Ueberschuss und potenzieller thermischer Nutzung
- Visualisierung der Rohdaten und der abgeleiteten Analysewerte in `dashboard.html`

## Offene Restpunkte
Nach der ersten Umsetzung bleiben noch fachliche und methodische Restpunkte:

- fachlich klaeren, was `rr = -1.0` in der Quelle exakt bedeutet
- pruefen, ob `sh` fuer 2025 komplett nicht verfuegbar ist oder nur leer exportiert wurde
- das Heizmodell spaeter um Belegung als zweite Einflussgroesse erweitern
- fuer echte Eigenverbrauchs- oder Einspeiseaussagen waeren zeitgleiche PV- und Netzwerte auf feinerer Granularitaet noetig
- die jetzigen Kennzahlen fachlich interpretieren und daraus konkrete Betriebs- oder Technikmassnahmen ableiten

## Erwartetes Ergebnis der naechsten Stufe
Die vorhandene Analyse beantwortet bereits einen grossen Teil der Fragen. Die naechste Stufe sollte aus den erzeugten Datensaetzen konkrete Entscheidungen ableiten:

- Wann steigt oder sinkt der Verbrauch in Zusammenhang mit der Belegung?
- Wie veraendert sich der Heizbedarf bei gleicher oder aehnlicher Aussentemperatur?
- Wie stark ist der Heizbedarf in den Wintermonaten belegungsabhaengig?
- Wie entwickeln sich PV-Erzeugung und Stromverbrauch im Verhaeltnis zueinander?
- Wo liegt konkretes Optimierungspotenzial, wenn PV-Ueberschuesse fuer Heizung oder Warmwasser nutzbar gemacht wuerden?


* wichtig -> wertschpfung -> wie groß ist der grenznutzen pro gast und wie groß sind potenziell die heizkosten wenn ein neuer gsat dazu kommt. im sommer so wie im winter 

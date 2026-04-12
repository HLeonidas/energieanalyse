# TODO: Analyse fachlich erweitern

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

## Fachliche Anforderungen an die nächste Analyse

### 1. Vergleich nicht nur auf Jahresbasis
Die Analyse soll nicht mehr primär das Gesamtjahr als eine einzige Vergleichseinheit betrachten.

Stattdessen sollen Vergleiche mindestens auf Monatsbasis und möglichst zusätzlich auf Tagesbasis erfolgen.

### 2. Monatsweise Analyse von Belegung, Wetter und Verbrauch
Innerhalb eines Monats sollen folgende Größen gemeinsam betrachtet werden:

- Belegung bzw. Nächtigungen und Anreisen
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

## Ziel der Erweiterung
Die Analyse soll so erweitert werden, dass wir nicht nur Visualisierungen haben, sondern belastbare Entscheidungsgrundlagen für Optimierungen.

Insbesondere soll geprüft werden:

- in welchen Monaten oder Zeitfenstern PV-Überschüsse auftreten
- ob und in welchem Umfang überschüssige PV-Energie potenziell für Wärme genutzt werden könnte
- ob sich daraus eine sinnvolle betriebliche oder technische Optimierung ableiten lässt

## Neuer Stand Wetterdaten
Im Ordner `data/wetter/` liegt jetzt ein Wetterexport vor:

- Datei: `Messstationen Tagesdaten v2 Datensatz_20250101_20251231.csv`
- Zeitraum: `2025-01-01` bis `2025-12-31`
- Granularität: täglich
- Quelle: Geosphere, historische Stationsdaten
- Station laut Begleitdokumentation: `19821` `Weißensee-Gatschach`
- Variablen: `tl_mittel`, `tlmax`, `tlmin`, `rr`, `so_h`, `rf_mittel`, `vv_mittel`, `sh`

Damit haben wir jetzt eine konkrete und fachlich passende Wetterquelle auf Tagesbasis, die grundsätzlich direkt mit den übrigen Tagesdaten vergleichbar ist.

## Was wir mit den Wetterdaten jetzt tun müssen

### 1. Wetterdaten in die bestehende Analysepipeline integrieren
Die Wetterdaten sollen als zusätzliche Quelle in den täglichen und monatlichen Analysedatensatz eingehen.

Dadurch sollen neue Vergleichsgrößen möglich werden:

- Fernwärmeverbrauch in Abhängigkeit von Temperatur
- Stromverbrauch in Abhängigkeit von Temperatur und Belegung
- Vergleich kalter und milder Tage innerhalb desselben Monats

### 2. Temperaturkennzahlen direkt verwenden
Da die Wetterquelle bereits tägliche Kennzahlen liefert, sollen diese direkt in die Analyse eingehen, insbesondere:

- `tl_mittel` als Standardgröße für Tagesmitteltemperatur
- `tlmin` und `tlmax` für kalte bzw. warme Ausreißertage
- `rr` für Niederschlag
- `so_h` als Sonnenscheindauer
- `sh` als Schneehöhe

### 3. Winteranalyse temperaturbasiert schärfen
Die Winteranalyse soll nicht nur nach Kalendermonat, sondern zusätzlich anhand von Temperaturwerten erfolgen.

So lässt sich besser prüfen:

- ob hoher Fernwärmeverbrauch durch Belegung, Temperatur oder beides getrieben ist
- ob Verbrauch nach Abreise sinkt, wenn Temperatur ähnlich bleibt
- welche Grundlast auch ohne Gäste bei kaltem Wetter bestehen bleibt

### 4. Rohwerte fachlich prüfen
Einzelne Wetterfelder enthalten offenbar Sonder- oder Platzhalterwerte, zum Beispiel `rr = -1.0`.

Diese Werte sollen vor der eigentlichen Auswertung fachlich geprüft und anschließend konsistent behandelt werden.

## Konkrete Erweiterungen an der Pipeline
Die bestehende Pipeline sollte deshalb um folgende Punkte erweitert werden:

- Integration der Wetterdaten aus `data/wetter/`
- Verwendung der täglichen Wetterwerte direkt auf Tagesbasis
- Aggregation der Wetterdaten von Tages- auf Monatsbasis nur dort, wo Monatsvergleiche gebraucht werden
- Dokumentation der Wetterfelder und fachliche Prüfung auffälliger Rohwerte
- Monatsweise Auswertung von Strom, Fernwärme, PV, Belegung und Wetter
- Kennzahlen für Wintermonate und saisonale Cluster
- Analyse von Vorher-Nachher-Effekten bei Anreise und Abreise
- Analyse von Fernwärmeverbrauch gegen Temperaturkennzahlen
- Kennzahlen zu PV-Überschuss und möglicher thermischer Nutzung
- Visualisierung der Rohdaten und der abgeleiteten Analysewerte

## Erwartetes Ergebnis
Am Ende soll die Analyse nicht nur zeigen, wie hoch Verbrauch und Erzeugung waren, sondern beantworten:

- Wann steigt oder sinkt der Verbrauch in Zusammenhang mit der Belegung?
- Wie verändert sich der Heizbedarf bei gleicher oder ähnlicher Außentemperatur?
- Wie stark ist der Heizbedarf in den Wintermonaten belegungsabhängig?
- Wie entwickeln sich PV-Erzeugung und Stromverbrauch im Verhältnis zueinander?
- Wo liegt konkretes Optimierungspotenzial, wenn PV-Überschüsse für Heizung oder Warmwasser nutzbar gemacht würden?

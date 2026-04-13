# Gästehaus Rader – Energie-Datenquellen & Analyse-Setup

## Zweck
Dieses Repository buendelt die Datenquellen und die aktuelle Analysepipeline fuer die gemeinsame Energie- und Betriebsanalyse des Gaestehauses Rader.

Ziel ist die Zusammenführung von:
- Heizung / Fernwärme
- Strom
- Solar / PV
- Meldewesen / Belegung
- Wetter / Außentemperatur

Stand dieser Übersicht: 12.04.2026

---

## Aktueller Datenstand im Repository

Der aktuelle Datenbestand deckt bereits große Teile des Analyseziels ab:

### 1) Meldewesen / Belegung
- Ablage: `data/meldewesen/`
- Vorhanden: Monatsdateien `2025-01.json` bis `2025-12.json`
- Struktur: Tageswerte je Datum, unter anderem mit Anreisen und Nächtigungen
- Status: vollständig für das Jahr 2025 im Repository vorhanden

### 2) Solar / PV
- Ablage: `data/fronius/`
- Vorhanden: Monatsdateien `2025-01.json` bis `2025-12.json`
- Struktur: JSON-Exporte mit Zeitreihenwerten aus dem Fronius-/Solar.web-Kontext
- Status: vollständig für das Jahr 2025 im Repository vorhanden

### 3) Fernwärme / Heizung
- Ablage: `data/fernwaerme/cns_device_export__20260410175322.csv`
- Zeitraum laut Export: `2025-01-01 00:00` bis `2025-12-31 23:59`
- Inhalt laut Datei: Jahres- und Monatssummen mit Energiemenge in `kWh` und Volumen in `m3`
- Status: ein CSV-Export für 2025 im Repository vorhanden

### 4) Strom
- Ablage: `data/strom/20260412-Lastprofil-AT007000096220000010190002540891A-20250101-20251231-QH.csv`
- Zeitraum laut Export: `01.01.2025` bis `31.12.2025`
- Struktur: 15-Minuten-Lastprofil mit den Spalten `Datum`, `Zeit`, `kWh`, `Status`
- Energierichtung laut Datei: `Verbrauch gemessen`
- Status: ein Jahresexport für 2025 im Repository vorhanden

### 5) Wetter / Außentemperatur
- Ablage: `data/wetter/Messstationen Tagesdaten v2 Datensatz_20250101_20251231.csv`
- Zeitraum laut Datei: `2025-01-01` bis `2025-12-31`
- Quelle: Geosphere-Datensatz für historische Stationsdaten
- Station laut Begleitdokumentation: `19821` `Weißensee-Gatschach`
- Struktur: täglicher CSV-Export mit den Feldern `tl_mittel`, `tlmax`, `tlmin`, `rr`, `so_h`, `rf_mittel`, `vv_mittel`, `sh`
- Status: tägliche Wetterdaten für 2025 sind im Repository vorhanden und grundsätzlich mit den übrigen Tagesdaten vergleichbar

---

## Bekannte Datenquellen / Portale

### Heizung / Fernwärme
- System / Portal: SIOCS
- URL: <https://weissbriach.siocs.eu/vis>
- Zweck: Heizungs- und Verbrauchsdaten prüfen bzw. später exportieren und analysieren

### Strom
- System / Portal: KELAG Services
- URL: <https://services.kelag.at/iss/Login>
- Zweck: Lastprofile bzw. Strombezugsdaten für die Analyse bereitstellen

### Solar / Photovoltaik
- System / Portal: Solar.web
- URL: <https://www.solarweb.com/>
- Zweck: PV-Erzeugung für die spätere Gegenüberstellung mit Verbrauch und Belegung

### Meldewesen / Belegung
- Quelle: Meldewesen-Export im Repository
- Zweck: Belegungsdaten als Kontextgröße für Heiz- und Stromverbrauch

### Wetter / Außentemperatur
- Quelle: Geosphere, historische Stationsdaten
- Referenz im Repository: `data/wetter/Messstationen Tagesdaten v2 Datensatz_20250101_20251231.csv`
- Station: `Weißensee-Gatschach` (`19821`)
- Zweck: Außentemperatur und weitere Wettergrößen als Referenz für Heiz-, Saison- und Belegungsanalyse

---

## Zielbild der späteren Analyse

Die spätere Analyse soll insbesondere folgende Fragen beantworten:
- Wie entwickeln sich Heizverbrauch, Stromverbrauch und PV-Erzeugung über die Zeit?
- Wie hängen Energieverbrauch und Belegung zusammen?
- Welche saisonalen Muster, Grundlasten und Auffälligkeiten sind erkennbar?
- Wie stark kompensiert die PV-Erzeugung den Strombezug?
- Wie verändert sich der Heizbedarf in Abhängigkeit von Außentemperatur und Belegung?

---

## Aktueller Analyse-Stand

Die aktuelle Pipeline liegt in `scripts/build_analysis.R` und erzeugt wiederverwendbare Ausgabedateien unter `output/energy-analysis/`.

Wesentliche Artefakte:
- `analysis_daily.csv`
- `analysis_monthly.csv`
- `analysis_seasonal.csv`
- `analysis_event_summary.csv`
- `raw_daily_sources.csv`
- `raw_monthly_sources.csv`
- `raw_strom_quarter_hour.csv`
- `metadata.json`
- `dashboard.html`

Die aktuelle Ausbaustufe integriert:
- Strom, Fernwaerme, PV, Meldewesen und Wetter auf Tagesbasis
- Monatsaggregation fuer Vergleiche
- saisonale Cluster `winter`, `transition`, `summer`
- Heizgradtage `HDD18`
- Vorher-Nachher-Kennzahlen bei Veraenderungen der Belegung
- Naechtigungen als massgebliche Belegungsgroesse; Anreisen werden aktuell nicht analysiert
- temperaturbereinigte Fernwaerme ueber ein lineares Wettermodell
- Kennzahlen zu PV-Ueberschuss und theoretischem PV-zu-Waerme-Potenzial
- HTML-Visualisierung mit Highcharts fuer Rohdaten und Analysewerte

---

## Wichtige technische Klärungen

Vor der eigentlichen Auswertung sollten die Datenquellen auf eine vergleichbare Basis gebracht werden:
- gemeinsame Zeitbasis festlegen, vorzugsweise Tag und Monat
- Zeitstempel und Zeitzonen konsistent prüfen
- Einheiten sauber dokumentieren (`kWh`, `m3`, Nächtigungen, Anreisen)
- PV-Erzeugung, Stromverbrauch und Fernwärme erst nach geklärter Zeitbasis vergleichen
- Wetterfelder und Parameterbedeutungen sauber dokumentieren
- auffaellige Rohwerte in den Wetterdaten fachlich pruefen, zum Beispiel `rr = -1.0`
- beachten, dass PV-Abdeckung und PV-zu-Waerme aktuell als theoretische Tagessalden und nicht als zeitgleiche Eigenverbrauchskennzahlen berechnet werden

---

## Nächste sinnvolle Ergänzungen

- Wetter-Sonderwerte fachlich validieren, insbesondere `rr = -1.0`
- Schneehoehe `sh` fachlich klaeren, da im aktuellen Export keine Werte vorliegen
- Heizmodell spaeter um weitere Einflussgroessen wie Belegung erweitern
- fuer echte Eigenverbrauchs- oder Einspeiseanalysen feinere zeitgleiche Daten ergaenzen
- die vorhandenen Datensaetze fuer Management-KPIs und konkrete Optimierungsszenarien weiter verdichten

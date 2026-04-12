# AGENTS.md

## Projektkontext
Dieses Repository dient der gemeinsamen Energie- und Betriebsanalyse fuer das Gaestehaus Rader.

Ziel ist die Zusammenfuehrung von:
- Heizung / Fernwaerme
- Strom
- Solar / PV
- Meldewesen / Belegung
- Wetter / Aussentemperatur

Die zentrale inhaltliche Beschreibung liegt in `readme.md`.

Die aktuelle Analysepipeline liegt in `scripts/build_analysis.R` und schreibt ihre Ergebnisse nach `output/energy-analysis/`.

## Aktueller Datenstand
Derzeit sind im Repository bereits folgende Datenbereiche vorhanden:
- `data/meldewesen/`: Monatsdateien `2025-01.json` bis `2025-12.json`
- `data/fronius/`: Monatsdateien `2025-01.json` bis `2025-12.json`
- `data/fernwaerme/`: derzeit ein CSV-Export
- `data/strom/`: derzeit ein CSV-Export mit Lastprofil fuer den Zeitraum `2025-01-01` bis `2025-12-31`
- `data/wetter/`: derzeit ein CSV-Export mit taeglichen Wetterdaten fuer den Zeitraum `2025-01-01` bis `2025-12-31`
- Wetterstation laut Begleitdokumentation: `19821` `Weissensee-Gatschach`

## Arbeitsregeln fuer Agents
- Behandle Dateien unter `data/` grundsaetzlich als Rohdaten oder vorbereitete Quelldaten.
- Rohdaten nicht umformatieren, verschieben oder ueberschreiben, ausser der Nutzer verlangt es explizit.
- Neue Datenquellen nach Moeglichkeit in einem eigenen Unterordner unter `data/` ablegen.
- Monatsdateien konsistent als `YYYY-MM.json` benennen.
- Bestehende Datenformate nur dann angleichen, wenn der Nutzer das ausdruecklich moechte oder eine Auswertung es zwingend erfordert.
- Bei spaeteren Importen auf Zeitstempel, Einheiten und Granularitaet achten.

## Erwartete Datenquellen
- Fernwaerme / Heizung: Portal SIOCS
- Strom: KELAG Services
- Solar / PV: Solar.web / Fronius
- Belegung: Meldewesen
- Wetter: Geosphere-Stationsdaten, bereits als CSV vorhanden

## Hinweise fuer spaetere Analysen
- Zeitreihen vorzugsweise auf Tages- oder Monatsbasis vergleichbar machen.
- Belegung, Heizverbrauch, Stromverbrauch und PV-Erzeugung nicht vermischen, bevor Zeitbasis und Einheiten geklaert sind.
- Wetterdaten als separate Quelle behandeln, zunaechst auf Tagesbasis mit den anderen Quellen vergleichen und erst danach auf Monatsebene aggregieren.
- Wetterparameter und auffaellige Rohwerte fachlich pruefen, zum Beispiel Sonderwerte wie `rr = -1.0`.
- Bestehende Analyse-Artefakte in `output/energy-analysis/` nur neu erzeugen oder erweitern, nicht manuell ueberschreiben.
- Kennzahlen wie PV-Abdeckung oder PV-zu-Waerme als theoretische Salden behandeln, solange keine zeitgleichen Last- und Erzeugungsdaten mit passender Granularitaet vorliegen.
- Wenn neue Quellen dazukommen, `readme.md` und diese Datei gemeinsam aktualisieren.

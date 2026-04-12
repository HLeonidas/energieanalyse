geholt von https://dataset.api.hub.geosphere.at/app/frontend/station/historical/klima-v2-1d

Aktuell verwendete Quelldatei im Projekt:
- `Messstationen Tagesdaten v2 Datensatz_20250101_20251231.csv`
- Zeitraum: `2025-01-01` bis `2025-12-31`
- Verwendung: aktuelle Wetterquelle fuer die gemeinsame Energieanalyse

Die Datei wird in der Analysepipeline `scripts/build_analysis.R` direkt auf Tagesbasis verarbeitet.
Offene fachliche Klaerungspunkte bleiben Sonderwerte wie `rr = -1.0` sowie die im Export leere Schneehoehe `sh`.

Ausgewählte Stationen (1 von 1097)
1 19821	Weißensee-Gatschach	14.10.1982	31.12.2100 

Ausgewählte Parameter (8 von 130)
tl_mittel	Lufttemperatur 2m Mittelwert	°C
tlmax	Lufttemperatur 2m Maximalwert	°C
tlmin	Lufttemperatur 2m Minimalwert	°C
rr	Niederschlag 24h Summe	mm
so_h	Sonnenscheindauer	h
rf_mittel	Relative Feuchte Mittelwert	%
vv_mittel	Windgeschwindigkeit Mittelwert	m/s
sh	Gesamtschneehöhe, Schneepegelmessung	cm

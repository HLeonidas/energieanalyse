# 🧠 Model Improvement Instructions – Guest Impact & Baseline

## 🎯 Ziel
Verbesserung der Modellierung zur sauberen Trennung von:

- **Eigenbedarf (Baseline)**
- **zusätzlichem Verbrauch durch Gäste**

Aktuell ist das Modell zu stark vereinfacht und kann den Gästeeinfluss falsch schätzen.

---

## 🚨 Problemübersicht

Aktuelles Modell:

```text
Verbrauch ~ hdd18 + nights
Risiken:
Überschätzung oder Unterschätzung des Gästeeffekts
Vermischung von:
Gästeverhalten
Saison
Betriebseffekten
unrealistische Annahmen (lineare Wirkung von nights)
✅ Was bleibt (nicht ändern)

Diese Teile sind gut und sollen beibehalten werden:

Tagesebene (kein Monatsmodell)
Nutzung von hdd18 für Wärme
saisonale Segmentierung
Vergleich:
Modell mit Gästen
Modell mit nights = 0
Grundidee: Baseline + Guest Impact
🔧 Konkrete Verbesserungen
1. nights nicht mehr linear modellieren

❌ aktuell:

+ nights

👉 Problem:

jeder zusätzliche Gast hat gleichen Effekt (unrealistisch)
✅ Ergänzen / testen:
has_guests_flag
log1p(nights)
nights^2

👉 Ziel:

abnehmender Grenzverbrauch
Schwellenverhalten abbilden
2. bessere Belegungsvariable einführen

👉 Neue Variable:

occupied_apartments = ceil(nights / 2)

👉 Verwendung im Modell:

+ occupied_apartments
Zusätzlich testen:
has_guests_flag
arrivals (Check-in Effekt)
3. Baseline sauberer definieren
Problem:

nights = 0 ist KEIN echter Leerlaufzustand

👉 enthält weiterhin:

Standby
Technik
Reinigung
Vorbereitung
Aktion:
Baseline explizit dokumentieren als:
"modellierte Referenz ohne Gäste"
nicht als "echter Eigenbedarf" interpretieren
4. Strommodell erweitern (WICHTIG)

❌ aktuell:

electricity_kwh ~ hdd18 + nights

👉 zu simpel

✅ neues Modell testen:
electricity_kwh ~ 
  has_guests_flag +
  nights +
  temp_mean_c +
  sunshine_hours +
  factor(season_cluster)
Optional erweitern:
weekday/weekend
arrivals
5. Wärmemodell verbessern

❌ aktuell:

heat_kwh ~ hdd18 + nights
✅ erweitern:
heat_kwh ~ 
  hdd18 +
  has_guests_flag +
  nights +
  factor(season_cluster)
zusätzlich testen:
heat_kwh ~ hdd18 + I(hdd18^2) + nights

oder:

heat_kwh ~ hdd18 + arrivals + nights
6. Betriebs-/Kalendereffekte einbauen

👉 aktuell fehlen:

Wochentag
Wochenende
Check-in / Check-out Tage
hinzufügen:
weekday = factor(Wochentag)
is_weekend = TRUE/FALSE
7. Baseload sauberer modellieren

❌ aktuell:

nur indirekt über Regression
Zielstruktur:
Gesamtverbrauch =
  fixe Grundlast +
  belegungsabhängige Last +
  wetterabhängige Last +
  Betriebs-/Kalendereffekte +
  Residual

👉 Agent soll prüfen:

ob baseload explizit modelliert werden kann
oder zumindest besser isoliert wird
8. Segmentmodell robuster machen

👉 aktuell:
Fallback auf All-Days Modell

Problem:
kann falsche Logik verstecken
Aktion:
prüfen:
wann Fallback greift
wie oft
in Output sichtbar machen
🔍 Validierungs-Check (Pflicht)

Agent soll folgende Modelle vergleichen:

Modell 1:
Verbrauch ~ hdd18
Modell 2:
Verbrauch ~ hdd18 + has_guests_flag
Modell 3:
Verbrauch ~ hdd18 + has_guests_flag + nights
Analyse:
Vergleich R²
Einfluss von has_guests_flag
Zusatznutzen von nights
🎯 Zielbild

Nach Umsetzung:

realistischere Trennung:
Eigenbedarf vs Gästeverbrauch
weniger Verzerrung durch Saison/Betrieb
stabilere Modelle
bessere Interpretierbarkeit
⚠️ Wichtige Interpretation
"Baseline" = Modell ohne Gäste
NICHT = echter physischer Leerlauf
🧠 Erwartetes Ergebnis
Strommodell verbessert sich deutlich
Wärmemodell leicht verbessert
Gästeeffekt wird realistischer
weniger Overfitting durch nights
### Vorschlag:
- `load_data.R` → alle Loader (strom, pv, wetter, etc.)
- `utils.R` → helper functions (safe_sum, rolling_mean, etc.)
- `models.R` → alle Regressionsmodelle
- `features.R` → Feature Engineering (z. B. PV, Eigenverbrauch, KPIs)
- `build_outputs.R` → Aggregation + CSV/JSON Output
- `main.R` → orchestriert alles

👉 Vorteil:
- leichter wartbar
- einzelne Teile separat testbar
- schneller iterierbar

---

## 2. Wertschöpfung korrekt berechnen (SEHR WICHTIG)

Aktuell:
```text
Revenue = Preis pro Nacht * Nächtigungen

❌ Das ist falsch

Hintergrund:
3 Appartements
jeweils 2–3 Betten
Preis ist pro Appartement (nicht pro Person)
Neue Logik:

👉 Ziel: Anzahl belegter Appartements schätzen

Einfachste Näherung:

occupied_apartments = ceil(nights / 2)

👉 Begründung:

durchschnittlich ~2 Personen pro Appartement
Neue Revenue-Formel:
Revenue = Preis pro Appartement * occupied_apartments

👉 Das muss in:

Guest Value Modell
alle Revenue-Berechnungen

angepasst werden

3. Strom + PV korrekt visualisieren (Stacked Chart)

Aktuell:

Strom und PV werden falsch interpretiert
Ziel:

👉 echte Darstellung des Verbrauchs

Neue Logik:
Eigenverbrauch = PV Produktion - Einspeisung
Netzstrom = Strom (Kelag)

👉 Visualisierung:

Stacked Chart:

Teil 1: Eigenverbrauch (PV)
Teil 2: Netzstrom
Gesamtverbrauch = Eigenverbrauch + Netzstrom
Input:
Tageswerte Strom (Kelag)
Tageswerte PV (Fronius)
Tageswerte Einspeisung (Kelag)

👉 Ergebnis:

echte Last sichtbar
Sommer/Winter korrekt interpretierbar
4. Residuals aus Visualisierung entfernen

Aktuell:

Residuals werden geplottet

Problem:

für Business-User schwer verständlich
erzeugt Verwirrung

👉 Aktion:

alle Residual-Charts entfernen
keine Residual-Linien mehr anzeigen
5. Widget entfernen

👉 Entfernen:

Analyse: Verbrauch pro Nächtigung

Begründung:

basiert auf falscher Logik (Nächtigungen ≠ Verbrauchstreiber allein)
wird durch bessere Modelle ersetzt
6. PV-Balance Chart anpassen / aufteilen

Aktuell:

basiert auf falscher Annahme (Strom vs PV direkt)
Neue Struktur:

Chart aufteilen in:

A) Energiefluss
Netzstrom
Eigenverbrauch (PV)
Einspeisung
B) PV Nutzung
Anteil PV → eigener Verbrauch
Anteil PV → Einspeisung
C) optional:
PV Überschuss vs Wärmebedarf

👉 Wichtig:

keine vereinfachte Rechnung mehr wie:
strom - pv
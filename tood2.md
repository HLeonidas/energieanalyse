# ⚡ Energy Data Correction – PV & Strom Modell

## 🎯 Ziel
Die aktuelle Analyse von Stromverbrauch vs. Nächtigungen ist verfälscht, da nur der Netzstrom betrachtet wird.  
PV-Produktion reduziert den Netzbezug und verzerrt dadurch die Korrelation.

Ziel ist es, den **realen Stromverbrauch korrekt zu berechnen**.

---

## 🚨 Problem

Aktuell wird angenommen:
```text
Strom (Kelag) = Verbrauch

Das ist falsch, da:

PV-Produktion einen Teil des Verbrauchs direkt deckt
dieser Anteil NICHT im Netzstrom sichtbar ist

➡️ Ergebnis:

Sommer: viele Gäste, aber niedriger Strom (falsch interpretiert)
Korrelation Nächtigungen vs Strom zu niedrig (~0,35)
✅ Lösung: Neues Energiemodell
Verfügbare Daten
strom/ → Netzstrom (Kelag)
fronius/ → PV Produktion
strom-einspeisung/ → Einspeisung ins Netz
auslastung/ → Nächtigungen
🔑 Neue Kennzahlen
1. Eigenverbrauch
eigenverbrauch = pv_produktion - einspeisung
2. Realer Stromverbrauch
real_strom = strom + eigenverbrauch

oder direkt:

real_strom = strom + pv_produktion - einspeisung
📊 Erwartetes Verhalten
Sommer:
PV hoch
Netzstrom niedrig
realer Stromverbrauch hoch
Winter:
PV niedrig
Netzstrom hoch
realer Stromverbrauch moderat

➡️ Der reale Verbrauch wird saisonal korrekt dargestellt

📈 Aufgaben für Agent
1. Daten vereinheitlichen
Alle Daten auf gleiche Zeitbasis bringen (Tag oder Monat)
Keys:
Datum
Strom
PV Produktion
Einspeisung
Nächtigungen
2. Neue Felder berechnen
eigenverbrauch = pv - einspeisung
real_strom = strom + eigenverbrauch
3. Validierung

Checks:

eigenverbrauch >= 0
einspeisung <= pv
keine fehlenden Werte
4. Analyse neu berechnen
Korrelation:
Nächtigungen vs real_strom
Vergleich:
alt: Nächtigungen vs strom
neu: Nächtigungen vs real_strom
5. Visualisierung anpassen

Charts:

Ersetze:
❌ Strom
Mit:
✅ Realer Stromverbrauch

Optional:

Eigenverbrauch anzeigen
Autarkiegrad berechnen
💡 Zusatz KPIs
autarkiegrad = eigenverbrauch / real_strom
eigenverbrauchsquote = eigenverbrauch / pv_produktion
⚠️ Wichtige Hinweise
Einspeisung ist NICHT gleich PV Produktion
Netzstrom ist bereits durch PV reduziert
Zeiträume müssen exakt übereinstimmen
🎯 Ergebnis

Nach Umsetzung:

korrekter Energieverbrauch
realistische Korrelation mit Nächtigungen
bessere Entscheidungsbasis

---

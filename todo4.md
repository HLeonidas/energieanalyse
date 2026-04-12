Ja, das Modell kann grob überschätzen oder unterschätzen, was “Eigenbedarf” ist und was durch Gäste kommt.
Vor allem weil es sehr stark vereinfacht:
Verbrauch ~ hdd18 + nights

Was am Modell gut ist

Ein paar Dinge sind schon sinnvoll:

Tagesebene statt nur Monatsebene
Wetterkontrolle über hdd18
Trennung in saisonale Segmente
Vergleich von Modell mit Nächtigungen vs. nights = 0
Bildung einer Baseline und eines Guest-Impacts

Das ist grundsätzlich ein brauchbarer Start.

Die wichtigsten Schwächen
1. nights als direkter linearer Treiber ist zu simpel

Ihr modelliert:

electricity_kwh ~ hdd18 + nights
heat_kwh ~ hdd18 + nights

Das heißt implizit:

jede zusätzliche Nächtigung erhöht Verbrauch immer gleich stark
1 → 2 Gäste hat denselben Mehrverbrauch wie 30 → 31 Gäste

Das ist oft nicht realistisch.

Warum:

Ein großer Teil des Verbrauchs ist fix
manche Lasten steigen stufenweise, nicht linear
zusätzliche Gäste haben oft abnehmenden Grenzverbrauch

Beispiel:

1 belegtes Apartment aktiviert Heizung, Licht, Warmwasser
Gast im gleichen Apartment erhöht Verbrauch deutlich weniger als der erste
Besser:
zusätzlich testen:
has_guests_flag
log1p(nights)
nights^2
Apartment-Belegung statt nur Nächtigungen
2. Nächtigungen sind möglicherweise die falsche operative Variable

Das ist ein sehr wichtiger Punkt.

nights misst Personen-/Gästenächte.
Aber Energie hängt oft eher an:

belegten Apartments / Zimmern
Check-ins / Check-outs
Anzahl belegter Einheiten
ob überhaupt Gäste da sind

Beispiel:

4 Gäste in 1 Apartment
4 Gäste in 4 Apartments

gleiche Nächtigungen, aber oft ganz anderer Energiebedarf.

Das heißt:

Für Grundbedarf vs. Zusatzbedarf wäre oft besser:

occupied_units
has_guests_flag
arrivals
departures

Falls du aus dem Meldewesen nur nights und arrivals hast, würde ich arrivals zumindest zusätzlich testen.

3. Der Baseline-Begriff ist modellseitig nicht ganz sauber

Im Skript ist die Logik:

Modell fitten auf allen Tagen
dann auf denselben Tagen mit nights = 0 vorhersagen
Differenz = Gästeeffekt

Das ist methodisch okay als Näherung, aber:

Problem:

Wenn nights mit anderen Zuständen gekoppelt ist, die ihr nicht modelliert, dann wird ein Teil dieser Effekte fälschlich den Gästen zugeschrieben.

Beispiele:

Gäste sind vor allem in Ferien / Hochsaison da
in dieser Zeit laufen andere Systeme anders
andere Betriebsmodi, längere Öffnungszeiten, Warmwasser, Lüftung etc.

Dann ist nights auch ein Proxy für Saison/Betrieb — nicht nur für echten Gastmehrverbrauch.

Folge:

Der “Guest Impact” kann zu hoch oder zu niedrig sein.

4. Für Strom ist hdd18 als Wettervariable zu schwach

Für Wärme ist hdd18 sinnvoll.
Für Strom eher nur begrenzt.

Strom hängt oft mehr an:

Tageslicht / Sonnenschein
Ferien / Wochentage
Belegung
Kühlung / Sommerhitze
Küche / Reinigung / Laundry / Betrieb

Wenn Strom nur mit

electricity_kwh ~ hdd18 + nights

modelliert wird, dann fehlt sehr viel Struktur.

Besser für Strom:

mindestens testen mit:

nights
temp_mean_c oder temp_max_c
sunshine_hours
weekday/weekend
Monat / Saison als Faktor
5. Für Fernwärme fehlt evtl. eine Nichtlinearität

heat_kwh ~ hdd18 + nights setzt voraus:

Wärme steigt linear mit Heizgradtagen

Das ist oft nicht ganz falsch, aber in Gebäuden mit Trägheit, Thermostatverhalten und Warmwasserlast kann das zu simpel sein.

Besser:

testen:

heat_kwh ~ hdd18 + nights + factor(month)
oder heat_kwh ~ hdd18 + I(hdd18^2) + nights

Vor allem an sehr kalten Tagen kann die Beziehung nicht streng linear sein.

6. Kein Wochentag / Betriebsrhythmus

Das Modell ignoriert, ob ein Tag:

Montag
Wochenende
An-/Abreisetag
Feiertag

ist.

Das kann gerade beim Strom stark relevant sein.

Beispiel:
Samstag mehr Reinigung / Wäsche / Küchenaktivität
Sonntag anderes Muster
Check-in-Tag anderes Profil

Wenn das nicht modelliert ist, landet ein Teil dieser Effekte fälschlich bei nights.

7. Baseload wird nicht wirklich kausal modelliert

Das Skript berechnet zwar:

electricity_baseload_kw_p10

aber der Wert wird später nicht wirklich als strukturelle Basis in das Strommodell eingebaut.
Die Trennung von Grundlast und Gastlast passiert primär über Regression, nicht über ein echtes Lastmodell.

Das ist okay für einen ersten Wurf, aber nicht ideal.

Besser wäre:

eine klare Zerlegung in:

Gesamtverbrauch =
feste Grundlast
+ belegungsabhängige Last
+ wetterabhängige Last
+ Betriebs-/Kalendereffekte
+ Restfehler

Im aktuellen Modell verschwimmt das alles.

8. Fallback auf All-Days-Modell kann Logik brechen

Das saisonale Modell fällt zurück, wenn ein saisonaler Koeffizient negativ oder instabil ist.

Das ist praktisch, aber fachlich auch ein Warnsignal:

Das Segmentmodell passt nicht sauber
oder die Datenlage reicht nicht
oder die Modellstruktur ist unpassend

Das ist kein Coding-Fehler, aber ein Hinweis, dass das Modell inhaltlich nicht robust genug ist.

Gibt es grobe Fehler bei der Erkennung von Eigenbedarf vs. Gästebedarf?
Ja, potenziell diese drei:
A. Grundbedarf wird vermutlich zu stark als glatt und konstant gedacht

Ein Teil des “Eigenbedarfs” ist nicht konstant, sondern saisonal und betriebsabhängig.

Zum Beispiel:

Winterbetrieb
Reinigung
Warmwasser
Küche
Lüftung
Pumpen
Technik

Wenn das Modell diese Dinge nicht separat kennt, kann es sie Gästen oder Wetter falsch zuschreiben.

B. Gästeeffekt wird vermutlich mit Saison-/Betriebseffekten vermischt

Wenn viele Gäste vor allem in bestimmten Perioden auftreten, dann erklärt nights auch indirekt:

Saison
Nutzungsmuster
organisatorische Abläufe

Dann ist der geschätzte “Gastmehrverbrauch” nicht rein gastbedingt.

C. Null-Nächtigungen bedeuten nicht automatisch echter Leerlauf

Das ist sehr wichtig.

Ein Tag mit nights = 0 heißt nicht:

keine Aktivität
kein Warmwasser
keine Reinigung
kein Vorheizen
kein Standby

Deshalb ist:

Vorhersage mit nights = 0

nur eine modellierte Referenz, aber kein echter physischer “reiner Eigenbedarf”.

Das sollte man in der Interpretation sauber sagen.

Was ich anders machen würde
Minimal besser, ohne alles neu zu bauen
Für Strom

Ich würde testen:

electricity_kwh ~ nights + temp_mean_c + sunshine_hours + factor(season_cluster)

oder besser:

electricity_kwh ~ has_guests_flag + nights + temp_mean_c + sunshine_hours + factor(season_cluster)

Warum has_guests_flag?
Weil oft schon die Tatsache, dass Gäste da sind, einen Sprung im Verbrauch auslöst.

Für Wärme

Ich würde eher testen:

heat_kwh ~ hdd18 + has_guests_flag + nights + factor(season_cluster)

oder

heat_kwh ~ hdd18 + arrivals + nights

weil Anreise-/Belegungseffekte bei Wärme oft relevant sind.

Noch besser

Die Trennung in zwei Komponenten:

1. Basisbetrieb
Wetter
Saison
Wochentag
2. Gastnutzung
Gäste da / nicht da
Nächtigungen
Anreisen
Was ich konkret prüfen würde
Sehr sinnvoll als schneller Modellcheck:
Modell 1
Verbrauch ~ hdd18
Modell 2
Verbrauch ~ hdd18 + has_guests_flag
Modell 3
Verbrauch ~ hdd18 + has_guests_flag + nights

Dann schaust du:

wie stark verbessert sich R²?
ist has_guests_flag schon stark?
bringt nights danach noch viel?

Das zeigt oft sehr schön:

ob nur “Gäste da / nicht da” zählt
oder ob zusätzliche Nächtigungen wirklich linear weiterziehen
Mein Gesamturteil
Kein kompletter Schrott

Das Modell ist als erster analytischer Prototyp okay.

Aber:

Für eine belastbare Aussage wie
„so viel ist Eigenbedarf, so viel kommt durch Gäste“
ist es noch zu simpel.

Die größten Schwächen sind:

lineare Behandlung von nights
fehlende Betriebs-/Kalendereffekte
potenzielle Verwechslung von Saison- und Gästeeffekten
nights = 0 als zu starke Vereinfachung für Baseline
Meine klare Einschätzung

Es gibt keinen brutalen Coding-Fehler in der Grundidee, aber es gibt ein methodisches Risiko, dass der Gästeeffekt falsch zugeschrieben wird.

Am wahrscheinlichsten ist:

Strom-Gästeeffekt aktuell zu unsauber
Wärme-Modell etwas solider, aber auch nicht perfekt
Baseline ist eher eine modellierte Referenz als echter Eigenbedarf

Wenn du willst, schreibe ich dir als Nächstes eine präzise Liste mit Modell-Verbesserungen für den Agenten als MD, damit morgen genau diese Punkte angepasst werden.
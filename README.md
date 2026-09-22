# OptiFert: Evaluation der Digit Soil Technologie

![Quarto](https://img.shields.io/badge/Rendered_with-Quarto-blue?logo=quarto)
![Status](https://img.shields.io/badge/Status-Active-success)

## Description
Dieses Repository enthält die Datenpipeline und das methodische Framework zur Evaluierung der [Digit Soil](https://www.digit-soil.com/productresearch) Sensortechnologie (SEAR) im Rahmen des Innosuisse-Projekts [OptiFert](https://www.zhaw.ch/en/research/project/74358) (ZHAW). 

Das Ziel dieser Kurzberichte ist es, die Vorhersagekraft der schnellen enzymatischen Vor-Ort-Messungen (z.B. Phosphatasen, Glucosidasen) mit klassischen, laborbasierten GRUD-Methoden (P-H2O, P-AAE, Nmin) sowie Ertragsdaten aus Langzeitversuchen zu vergleichen. 

Die Ergebnisse werden automatisiert als interaktives **Quarto-Book** auf GitHub Pages publiziert.
👉 **[Hier klicken, um die aktuellen Auswertungsberichte zu lesen]**(https://ptochosgeorgos.github.io/Optifert/)

## Visuals
*(Hier fügen wir später einen Screenshot des gerenderten Quarto-Books oder einen exemplarischen Korrelations-Plot ein).*

## Installation & Setup
Um dieses Projekt lokal auszuführen und die Daten zu analysieren, benötigen Sie [RStudio](https://posit.co/download/rstudio-desktop/) und [Quarto](https://quarto.org/).

1. Klonen Sie dieses Repository auf Ihren lokalen Rechner:
    
    git clone https://github.com/ptochosgeorgos/Optifert.git
    
2. Öffnen Sie die Datei `OptiFert_DigitSoil.Rproj` in RStudio.

## Usage
Das Projekt verwendet eine flache Datenstruktur (Tidy Data). Die Datenaufbereitung erfolgt vollständig modular in den jeweiligen Teilprojekt-Ordnern (`data/<Subprojekt>/<Subprojekt>_prep_data.R`) und legt harmonisierte Daten in `data/<Subprojekt>/prep_data/` ab.

Um einen neuen Versuchsbericht hinzuzufügen:
1. Erstellen Sie eine neue `.qmd` Datei im Ordner `reports/` (z. B. `reports/Fields25.qmd`).
2. Fügen Sie die Datei in der `_quarto.yml` unter `chapters:` hinzu.
3. Klicken Sie in RStudio auf den Button **"Render Book"** (oder nutzen Sie das Terminal: `quarto render`), um das HTML-Buch lokal zu generieren.

## Roadmap
- [x] Definition der Datenstruktur (Flat CSV/Parquet)
- [x] Setup der Quarto-Book Architektur
- [x] Erstellung der explorativen Basis-Plots (Kovariaten, Zeitreihen)
- [x] Implementierung der Hauptkomponentenanalyse (PCA) zur Lösung von Multikollinearität
- [x] Modulare Auslagerung von Wetter- (Open-Meteo) und Nmin-Berechnungen (`scripts/meteo_nmin_utils.R`)
- [x] Dezentrale Datenaufbereitung direkt in den Subprojekt-Ordnern (`data/<Subprojekt>/`)
- [ ] Integration einer optionalen Shinylive-App für explorative Datenfilterung durch Externe

## 📊 Datenstruktur & Aufbereitung

Die Daten jedes Teilprojekts werden autonom und reproduzierbar über das jeweilige Vorbereitungsskript aufbereitet:
* **DEMO:** `data/DEMO/DEMO_prep_data.R` $\rightarrow$ `data/DEMO/prep_data/DEMO_cleansed.csv`
* **Fields25:** `data/Fields25/Fields25_prep_data.R` $\rightarrow$ `data/Fields25/prep_data/Fields25_cleansed.csv`
* **Lysimeter:** `data/Lysimeter/Lysimeter_prep_data.R` $\rightarrow$ `data/Lysimeter/prep_data/Lysimeter_cleansed.csv`


### 📋 Standardisiertes Datenbankschema (Tidy Data)

Alle bereinigten Tabellen (`prep_data/`) und Master-Dateien folgen folgendem einheitlichen Benennungsschema:

| Spaltenname | Typ | Einheit / Format | Beschreibung |
| :--- | :--- | :--- | :--- |
| `date` | Date | `YYYY-MM-DD` | Datum der Probenahme bzw. Messung |
| `year` | integer | `YYYY` | Versuchsjahr |
| `plot_nr` | numeric | - | Parzellen-, Lysimeter- oder Standortnummer |
| `treatment` | factor | - | Düngungsverfahren / Behandlungsvariante (z.B. `null`, `ueblich`, `empfohlen`) |
| `crop` | factor | - | Kulturart (z.B. `MA` = Mais, `WW` = Winterweizen, `KM` = Körnermais) |
| `rep` | character | - | Feldreplikat bzw. Probenwiederholung (`A`, `B`, `1`, `2`) |
| `NH4` | numeric | mg/kg TS bzw. mg N/L | Ammonium-Stickstoff |
| `NO3` | numeric | mg/kg TS bzw. mg N/L | Nitrat-Stickstoff |
| `Ntot` | numeric | kg N/ha bzw. mg/kg TS | Gesamter mineralischer Stickstoff ($N_{min} = NH_4 + NO_3$) |
| `delta_nmin` | numeric | kg N/ha | Nettoveränderung des $N_{min}$ zwischen Messintervallen |
| `LAP` | numeric | pmol min⁻¹ | Leucin-Aminopeptidase (Peptidabbau / N-Zyklus) |
| `NAG` | numeric | pmol min⁻¹ | N-Acetyl-$\beta$-D-glucosaminidase (Chitinabbau / C- & N-Zyklus) |
| `GLS` | numeric | pmol min⁻¹ | $\beta$-Glucosidase (Celluloseabbau / C-Zyklus) |
| `MUP` | numeric | pmol min⁻¹ | Phosphatase (Phosphorzyklus) |
| `MUX` | numeric | pmol min⁻¹ | $\beta$-Xylosidase (Hemicelluloseabbau / C-Zyklus) |
| `yield` | numeric | variabel | Ertragsmenge der Ernte bzw. Biomasse |
| `yield_type` | character | - | Ertragsfraktion (`korn_TS`, `stroh_TS`, `silo_TS`, `total_hp_ts`) |
| `yield_unit` | character | `dt/ha`, `kg/ha`, `kg/a` | Physikalische Einheit des Ertrags |

## Contributing
Wir begrüssen die Zusammenarbeit mit Projektpartnern. Wenn Sie neue Sensordaten oder Referenzwerte hinzufügen möchten, stellen Sie bitte sicher, dass diese der Struktur in der `digitsoil_master.csv` entsprechen. 
Für Code-Änderungen oder methodische Vorschläge nutzen Sie bitte **Pull Requests** und eröffnen Sie im Vorfeld ein **Issue** zur Diskussion.

## Authors and Acknowledgment
* **Projektleitung/Analyse:** ZHAW / Agroscope
* **Technologie-Partner:** Digit Soil
* **Förderung:** Innosuisse (Projekt OptiFert)

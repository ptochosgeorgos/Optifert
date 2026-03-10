# OptiFert: Evaluation der Digit Soil Technologie

![Quarto](https://img.shields.io/badge/Rendered_with-Quarto-blue?logo=quarto)
![Status](https://img.shields.io/badge/Status-Active-success)

## Description
Dieses Repository enthält die Datenpipeline und das methodische Framework zur Evaluierung der [Digit Soil](https://www.digit-soil.com/productresearch) Sensortechnologie (SEAR) im Rahmen des Innosuisse-Projekts [OptiFert](https://www.zhaw.ch/en/research/project/74358) (ZHAW). 

Das Ziel dieser Kurzberichte ist es, die Vorhersagekraft der schnellen enzymatischen Vor-Ort-Messungen (z.B. Phosphatasen, Glucosidasen) mit klassischen, laborbasierten GRUD-Methoden (P-H2O, P-AAE, Nmin) sowie Ertragsdaten aus Langzeitversuchen zu vergleichen. 

Die Ergebnisse werden automatisiert als interaktives **Quarto-Book** auf GitHub Pages publiziert.
👉 **[Hier klicken, um die aktuellen Auswertungsberichte zu lesen]**(https://<ihr-github-username>.github.io/optifert-digitsoil) *(Link anpassen!)*

## Visuals
*(Hier fügen wir später einen Screenshot des gerenderten Quarto-Books oder einen exemplarischen Korrelations-Plot ein).*

## Installation & Setup
Um dieses Projekt lokal auszuführen und die Daten zu analysieren, benötigen Sie [RStudio](https://posit.co/download/rstudio-desktop/) und [Quarto](https://quarto.org/).

1. Klonen Sie dieses Repository auf Ihren lokalen Rechner:
    
    git clone https://github.com/<ihr-github-username>/optifert-digitsoil.git
    
2. Öffnen Sie die Datei `OptiFert_DigitSoil.Rproj` in RStudio.

## Usage
Das Projekt verwendet eine flache Datenstruktur (Tidy Data). Die Master-Datenbank (`data/digitsoil_master.csv`) enthält alle Sensor- und Labor-Referenzwerte.

Um einen neuen Kurzbericht (z. B. für ein neues Feld) hinzuzufügen:
1. Erstellen Sie eine neue `.qmd` Datei im Hauptverzeichnis (z. B. `02_feldversuch_xy.qmd`).
2. Fügen Sie die Datei in der `_quarto.yml` unter `chapters:` hinzu.
3. Klicken Sie in RStudio auf den Button **"Render Book"** (oder nutzen Sie das Terminal: `quarto render`), um das HTML-Buch lokal zu generieren.

## Roadmap
- [x] Definition der Datenstruktur (Flat CSV/Parquet)
- [ ] Setup der Quarto-Book Architektur
- [ ] Erstellung der explorativen Basis-Plots (Kovariaten, Zeitreihen)
- [ ] Implementierung der Hauptkomponentenanalyse (PCA) zur Lösung von Multikollinearität
- [ ] Integration einer optionalen Shinylive-App für explorative Datenfilterung durch Externe

## Contributing
Wir begrüssen die Zusammenarbeit mit Projektpartnern. Wenn Sie neue Sensordaten oder Referenzwerte hinzufügen möchten, stellen Sie bitte sicher, dass diese der Struktur in der `digitsoil_master.csv` entsprechen. 
Für Code-Änderungen oder methodische Vorschläge nutzen Sie bitte **Pull Requests** und eröffnen Sie im Vorfeld ein **Issue** zur Diskussion.

## Authors and Acknowledgment
* **Projektleitung/Analyse:** ZHAW / Agroscope
* **Technologie-Partner:** Digit Soil
* **Förderung:** Innosuisse (Projekt OptiFert)

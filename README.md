# Elm-Projekt-WineInformation
 Bei diesem Projekt handelt es sich um die Visualisierung von Weindaten mithilfe eines Scatterplots, Paralellen Koordinaten und einer Baumhierarchie.

## Inhaltsverzeichnis
[Einführung](#Einführung)
[Projektvorschau](#Projektvorschau)
[Datenherkunft](#Datenherkunft)
[Technologien](#Technologien)
[Projektinstallation](#Projektinstallation)
[Quellen](#Quellen)

## Einführung
Dies Projekt ist im Rahmen einer universitären Projektarbeit entstanden, in welcher verschiedene Visualsierungstechniken angewendet werden sollten.
Folgende wesentliche Anforderungen gab es dabei an diese Projektarbeit:
1. Visualsierungen mussten mithilfe der funktionalen Programmiersprache [Elm](https://elm-lang.org/) erfolgen
2. Es müssen drei verschiedene Visualsierungstechniken verwendet werden
   - Aus folgenden Visualsierungstechniken sollte ausgewählt werden:
     - Scatterplot, Zeitreihen-Diagramme
     - Mehrdimensionale Darstellungen
     - Icon und Pixel-orientierte Techniken
     - Bäume und Graphen  
3. Datensatz wurde vorgeben
4. Visualsierungen sollten verbunden zu einer Visualsierungsanwedung verbunden werden
In diesem Projekt wurde sich für den Scatterplot, die Paralellen Koordinaten und die Baumhierarchie entschieden.
Die Verbindung der verschiedenen Visualsierungen erfolgte mithilfe des Übersetzten Elm Codes in Java Script und HTML. 

## Projektvorschau
Die Vorschau für dieses Projekt wurde mithilfe von [GitHub Pages](https://pages.github.com/) erstellt und ist unter folgendem Link erreichbar: https://ricbre.github.io/Elm-Projekt-WineInformation/.

## Datenherkunft
 Die [Orginadaten](https://www.kaggle.com/dev7halo/wine-information) wurden auf der Plattform Kaggle zur verfügung gestellt. Diese wurde innerhalb des Projektes weiterverarbeitet. Die weiterverarbeiten Daten sind innerhalb des Projekts unter [Aufbereitete Daten](Daten/AufbereiteteDaten) zu finden. Die Visualisierungen verwenden dabei die Datei [WineInformationExcelAufbereitetKlein](Daten/AufbereiteteDaten/WineInformationExcelAufbereitetKlein.xlsx)

## Technologien
In diesem Projekt wurden folgende Programmiersprachen eingesetzt:
- [elm (Version: 0.19.1)](https://github.com/elm/compiler/releases/tag/0.19.1)
- [JavaScript (Version: ECMAScript 2018)](https://www.w3schools.com/Js/js_2018.asp)
- [HTML (Version: HTML5)](https://de.wikipedia.org/wiki/HTML5)

### Elm-Pakete
Folgende Elm-Pakete werden benötigt um das gesamte Projekt ausführen zu können:
- [alex-tan/elm-tree-diagram (Version: 1.0.0)](https://package.elm-lang.org/packages/alex-tan/elm-tree-diagram/1.0.0)
- [avh4/elm-color (Version: 1.0.0)](https://package.elm-lang.org/packages/avh4/elm-color/1.0.0)
- [elm/browser (Version: 1.0.2)](https://package.elm-lang.org/packages/elm/browser/1.0.2)
- [elm/core (Version: 1.0.5)](https://package.elm-lang.org/packages/elm/core/1.0.5)
- [elm/html (Version: 1.0.0)](https://package.elm-lang.org/packages/elm/html/1.0.0)
- [elm/http (Version: 2.0.0)](https://package.elm-lang.org/packages/elm/http/2.0.0)
- [elm/json (Version: 1.1.3)](https://package.elm-lang.org/packages/elm/json/1.1.3)
- [elm-community/list-extra (Version: 8.3.1)](https://package.elm-lang.org/packages/elm-community/list-extra/8.3.1)
- [elm-community/typed-svg (Version: 7.0.0)](https://package.elm-lang.org/packages/elm-community/typed-svg/7.0.0)
- [ericgj/elm-csv-decode (Version: 2.0.1)](https://package.elm-lang.org/packages/ericgj/elm-csv-decode/2.0.1)
- [folkertdev/one-true-path-experiment (Version: 6.0.0)](https://package.elm-lang.org/packages/folkertdev/one-true-path-experiment/6.0.0)
- [gampleman/elm-visualization (Version 2.3.0)](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.3.0)
- [lovasoa/elm-csv (Version: 1.1.7)](https://package.elm-lang.org/packages/lovasoa/elm-csv/1.1.7)
- [zwilias/elm-reorderable (Version: 1.3.0)](https://package.elm-lang.org/packages/zwilias/elm-reorderable/1.3.0)

## Projektinstallation
Da die Visualsierungen einmal im JavaScript/ HTMl Seite vorliegen und als Elm Code gibt es unterschiedliche möglichkeiten, dieses Projekt lokal bereitzustellen.

### Anschauen des Elm Codes und HTML-Webseiten
Um sich beide die Elm oder HTML Version des Projektes anzuschauen, muss innerhalb des Projektes der Terminalbefehl `elm reactor` ausgeführt werden.
Anschließend ist das Projekt unter `http://localhost:8000/` im Browser der Wahl zu erreichen.
Genauere Informationen zum `elm reactor` sind hier zu [finden](https://guide.elm-lang.org/install/elm.html).

### Übersetzten der Elm Dateien in HTML-Webseiten
Falls der Elm Code in eine Webseite mit eingebunden JavaScript übersetzt werden sollte muss innerhalb des Terminmal der Befehl `elm make [DATEINAME]` eingegeben werden.
Anschließend wird automatisch eine entsprechende HTMl Datei erstellt.
Genuere Informationen zu `elm make` sind hier zu [finden](https://hackage.haskell.org/package/elm-make)

## Quellen
- [Aufbau der Readme](https://bulldogjob.com/news/449-how-to-write-a-good-readme-for-your-github-project)
- [Readme Syntax](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)

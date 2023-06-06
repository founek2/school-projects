## Podporované operace
*  Převrácení obrazu směrem doprava o 90°; --rotate
*  zrcadlení; --mirror
*  inverzní obraz (negativ); --inverse
*  převod do odstínů šedi; --bw
*  zesvětlení; --lighten <percentage: 0-100>
*  ztmavení; --darken <percentage: 0-100>
*  zvýraznění hran (tzv. “unsharp mask”); --sharpen

## Operace navíc
* blur - Aplikace Gaussian blur s kernelem 3x3

Operace je možné libovolně kombinovat i aplikovat opakovaně (závisí na pořadí).
Parametr s INPUT_PATH a OUTPUT_PATH lze umístit na libovolnou pozici - pouze
je nutné zachovat pořadí.

## Ukázka
`cd project`  
`python3 . --inverse --bw --lighten 25 /path/to/input/img /path/to/output/img`


## Program
Program je možné spustit jako modul nebo spustit přímo soubor program.py. 
Implementace je umístěna ve složce src. Podporované operace lze jednoduše rozšiřovat
přidání nové funkce do filters.py - její docString se propíše do nápovědy
argumentu. Potom ji stačí přidat do dictu actions na konci souboru. K funkcním
lze přidat "metadata". Atuálně je podporován pouze type - zaručuje že parametr
argumentu bude daného typu. Počet parametrů si program odvozuje z počtu
parametrů dané filtrovací funkce.

Program lze využít i jako konvertor obrázků. Lze na vstup dát obrázek např.
s příponou ".png" a do výstupní cesty uvést img.jpg a program zajistí správnou
konverzi do jiného datového typu. Podporu zajišťuje knihovna ([Pillow](https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html))

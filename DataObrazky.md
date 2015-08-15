**(03)**
## Vstupní data/obrázky ##
  * TIFF, 300dpi, 4bit
  * pojmenování
    * v rámci názvu souboru je u monografií posledních 5 a u periodik 4 místa určená pro určení pořadí souboru v adresáři
    * ukládat se bude celý název
  * jsou uložená na serveru, do ASK4 budou nahrávány z určeného adresáře, vnořené adresáře budou ignorovány
  * po zpracování obrázků z daného adresáře možnost smazání (Defaultně vypnutá) a/nebo vkládání txt souboru se značkou, kdy byly obr. nahrané/zpracované

## Akce s daty/obrázky ##
  * generování náhledů a ikon
    * náhledy nebo ikony půjdou při exportech odfiltrovat
    * možnost výběru, zda chci náhled z původního skenu (TIFF) nebo upraveného skenu (JPG)
  * změny pořadí
  * mazání duplicit, nadbytečných stran
  * přejmenování souboru
  * změna formátu

## Publikovaná data/obrázky ##
  * JPG, rozlišení a hloubku přečíst z hlavičky pův. obr, možnost nastavení míry komprese
  * při publikaci budou spojeny do jednoho digitálního objektu jak původní skeny (TIFF), tak JPG (zpracované v ASK4, vstupní JPG budou smazány)

## Pojmenování ##

  * pojmenovává automaticky skener
  * **periodika**:
    * 6 míst SIGLA (ABA007)
    * 8 míst ISSN (00376736)
    * 4 místa ročník (1923)
    * 4 místa výtisk (0001)
    * 1 místo příloha (0)
    * 5 míst pořadové číslo souboru (00001)
    * např: ABA007003767360001000001.tif
  * **monografie**:
    * 6 míst SIGLA (ABA007)
    * 11 míst čárový kód (26000800652)
    * 4 místa rok vydání (1950)
    * 4 místa číslo souboru (0001)
    * např: ABA0072600080065219500001.tif

> Pozn.:
> > Pokud monografie nemá čárový kód, jsou jeho pozice nahrazeny jedenácti x (xxxxxxxxxxx). Monografie se pak rozlišují především názvema dresáře, ve kterém je popsán obsah, např.: AK\_1959\_Institute\_Of\_Physics\_Academy\_Of\_Sciences\_Of\_The\_Czech\_Republic, v němž "AK 1959" je signatura, nikoliv rok.
> > U starých tisků navíc není žádná konvence pro zapisování názvů. Někdy se používá signatura např. TOM\_II\_00001.tif, u externich zakázek třeba název případně část názvu předlohy např. kronika\_hlavenec.00001.tif
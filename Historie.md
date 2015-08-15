Vybráním **Historie** z roletky **Menu** dojde k přepnutí do obrazovky **Import – Správa importních dávek**, ve které jsou uloženy informace o provedených operacích se složkami dokumentů.

Na liště jsou umístěna tlačítka **Obnovit, Resetovat a Pokračovat**. Tlačítko Obnovit slouží k opětovnému načtení dat. Použitím tlačítka Resetovat se znovu načte importovaný adresář (již vytvořená metadata budou smazána). Pomocí tlačítka **Pokračovat** se lze u vybrané složky dostat do obrazovky **Import – Příprava metadat pro načtení do úložiště** a následně dokončit popisná metadata.

V levém sloupci **Složka** jsou jednotlivé importované složky. V dalších sloupcích **Datum importu, Stav a Uživatel** jsou uvedeny příslušné informace o jednotlivých načtených složkách.

Pro zpřehlednění práce lze zobrazené složky ve sloupcích filtrovat dle data importu a aktuálního stavu. Automaticky je přednastaven filtr Stav – Načteno. Tento status mají složky, které jsou načtené, ale nejsou archivované. K těmto složkám se lze (je nutné pro dokončení práce) se vrátit. Datum importu je nastaven na automatické řazení nejnověji načtené složky do prvního řádku.

Sloupec uživatel je informační, slouží pro práci administrátora. Jednotliví uživatelé vidí pouze své složky.

Archivované složky již NELZE znovu načíst ani u nich pokračovat v obrazovce **Import – Příprava metadat**. V tomto případě je tlačítko Pokračovat neaktivní.


http://wiki.archivacni-system.googlecode.com/git/images/04_Historie.JPG


**Stavy v obrazovce - Historie**

**Prázdné pole** - zobrazuje všechny stavy importní dávek.

**Načítání** - importní dávka právě běží a připravuje digitální obsah v daném adresáři po archivaci.

**Chyba načítání!** - Načítání neproběhlo úspěšně. Je nutné dávku nahrát a následně dávku načíst znova.

**Načteno** - digitální obsah (náhled, ikona, metadata, ...) je připraven pro uživatelské úpravy před archivací.

**Archivování** - importní dávka právě ukládá digitální obsah do úložiště Fedora.

**Chyba archivování** - Archivování neproběhlo úspěšně. Je nutné objekt archivovat znova.

**Archivováno** - importní dávka úspěšně skončila. Obsah je archivován. Metadata mohou být dále měněna pomocí editoru přímo v úložišti Fedora.
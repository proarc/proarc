# 1	Základní popis programu #
## 1.1	Osobní nastavení a uspořádání ##
Tato funkce je v současné době zajišťována individuálně firmou Incad.

## 1.2	Popis základních obrazovek ##
Po přihlášení do programu se uživateli objeví úvodní obrazovka s informací o ProArcu. V horní části obrazovky je lišta obsahující roletku Menu, roletku pro volbu jazyka a přihlašovací jméno uživatele. Roletka Menu obsahuje složky Import a Úložiště s jednotlivými pořadači a základní informací o ProArcu. Tato lišta je zobrazena ve všech obrazovkách.

### 1.2.1	Složka Import ###
Složka Import obsahuje dva pořadače určené pro práci s naskenovanými soubory. Umožňuje načtení a zpracování nové dávky a pomocí složky Historie návrat ke všem dávkám daného uživatele.
#### 1.2.1.1	Nová dávka ####
Po vybrání řádku Nová dávka z roletky Menu, která se nachází na liště v levé horní části obrazovky, se objeví obrazovka Import – Výběr adresáře s digitalizovanými daty pro přípravu metadat. Do této obrazovky se zrcadlí adresáře s jednotlivými složkami tak, jak jsou uloženy v importní části serveru.
Na horní liště jsou tlačítka Obnovit a Načíst adresář. Použitím tlačítka Načíst adresář se načtou soubory z předem vybrané a označené složky. Zároveň dojde k přepnutí do obrazovky Import – Připravit metadata pro import, kde lze doplnit popisná metadata.
V panelu Parametry je umístěno pole s roletkou Použitý skener s přednastavenými skenery a zaškrtávací pole pro volbu automatického generování indexů stran. Index je také možné generovat až při zpracování dokumentů v obrazovce Import – Připravit metadata pro import. V tom případě zůstane zaškrtávací pole prázdné.
Spodní část obrazovky je rozdělená do dvou sloupců. Ve sloupci Název jsou složky a soubory z importní části serveru uživatele. Ve sloupci Stav jsou informace o fázi importu jednotlivých složek (titulů). Složky, u kterých není stav uveden, čekají na načtení. U složek, ke kterým byl již přiřazen typ skeneru, a byly načteny do systému, je uveden stav Načteno. Z tohoto místa NELZE znovu otevřít již načtenou složku, to je možné jen z obrazovky Historie.


#### 1.2.1.2	Historie ####
Vybráním Historie z roletky Menu dojde k přepnutí do obrazovky Import – Správa importních dávek, ve které jsou uloženy informace o provedených operacích se složkami dokumentů.
Na liště jsou umístěna tlačítka Obnovit, Resetovat a Pokračovat. Tlačítko Obnovit slouží k opětovnému načtení dat. Použitím tlačítka Resetovat se znovu načte importovaný adresář (již vytvořená metadata budou smazána). Pomocí tlačítka Pokračovat se lze u vybrané složky dostat do obrazovky Import – Příprava metadat pro načtení do úložiště a následně dokončit popisná metadata.
V levém sloupci Složka jsou jednotlivé importované složky. V dalších sloupcích Datum importu, Stav a Uživatel jsou uvedeny příslušné informace o jednotlivých načtených složkách.
Pro zpřehlednění práce lze zobrazené složky ve sloupcích filtrovat dle data importu a aktuálního stavu. Automaticky je přednastaven filtr Stav – Načteno. Tento status mají složky, které jsou načtené, ale nejsou archivované. K těmto složkám se lze (je nutné pro dokončení práce) se vrátit. Datum importu je nastaven na automatické řazení nejnověji načtené složky do prvního řádku.
Sloupec uživatel je informační, slouží pro práci administrátora. Jednotlivý uživatelé vidí pouze své složky.
Archivované složky již NELZE znovu načíst ani u nich pokračovat v obrazovce Import – Příprava metadat. V tomto případě je tlačítko Pokračovat neaktivní.


Stavy v obrazovce - Historie
Prázdné pole - zobrazuje všechny stavy importní dávek.
Načítání - importní dávka právě běží a připravuje digitální obsah v daném adresáři po archivaci.
Chyba načítání! - Načítání neproběhlo úspěšně. Je nutné dávku nahrát a následně dávku načíst znova.
Načteno - digitální obsah (náhled, ikona, metadata, ...) je připraven pro uživatelské úpravy před archivací.
Archivování - importní dávka právě ukládá digitální obsah do úložiště Fedora.
Chyba archivování - Archivování neproběhlo úspěšně. Je nutné objekt archivovat znova.
Archivováno - importní dávka úspěšně skončila. Obsah je archivován. Metadata mohou být dále měněna pomocí editoru přímo v úložišti Fedora.

### 1.2.2	Úložiště ###
Ve složce Úložiště jsou dva pořadače určené pro zakládání nových objektů, editaci archivovaných objektů a exportování dat.
#### 1.2.2.1	Nový objekt ####
V pořadači Nový objekt se zakládá nadřazený objekt pro naskenované soubory.  V obrazovce Nový objekt – Vyberte typ nově vytvořeného objektu jsou v roletce Vybrat model přednastavené typy pro nově vytvářený objekt (Periodikum, Ročník, Výtisk, Monografie, Monografie – volná část, Strana).
Popisná metadata pro celý titul lze importovat z některého přednastaveného katalogu v roletce Vybrat katalog. Pro nalezení vyhledávaného objektu jsou přednastavené běžně používané identikátory (ISSN, ISBN, čČNB, Čárový kód, Signatura). Pomocí tlačítka Vytvořit nový objekt umístěného v horní liště pod názvem obrazovky dojde k vytvoření nového objektu.

#### 1.2.2.2	Hledat ####
Obrazovka Hledat slouží k vyhledávání, editaci a exportu již založených objektů. Tato obrazovka je rozdělena do dvou oken. V horním okně lze vyhledat všechny objekty podle typu modelu. V dolním okně se zobrazují označené objekty. Pro tyto činnosti slouží ovládací prvky na liště nástrojů (Filtr, Obnovit, MODS, Komentář, Vazby, FOXML, Exporty a Smazat). Vyhledávání jednotlivých objektů lze uskutečnit pomocí roletky Model, nebo označením v nabídce umístěné nad roletkou Model (Naposledy vytvořené, Naposledy editované, Dotaz, Rozšířený dotaz).

V horní části obrazovky je výběr pro další hledání a roletka Model s přednastavenými typy objektů (Vyhovuje vše, Periodikum, Ročník, Výtisk, Monografie, Monografie – volná část, Strana)
Vyhledávat lze dle několika kritérií.
a.	Naposledy vytvořené - roletka Model s typy objektů
b.	Naposledy editované - roletka Model s typy objektů
c.	Dotaz – roletka je nahrazena prázdným polem určeným k zadání hledaného textu
d.	Rozšířený dotaz – obsahuje různá vyhledávací pole a roletku Model

V horním okně je přehled všech titulů daného modelu. V dolním okně se zobrazí vybraný objekt, který lze rozbalit do stromové struktury až na úroveň jednotlivých stran. Pomocí nástrojů na liště je možné prohlížet, editovat a exportovat celý titul nebo jeho části.


Na liště nástrojů jsou umístěny tyto ikony:
Filtr pro hledaná data.
Načte znovu data.
Slouží k editaci bibliografických metadat vybraného objektu.
Umožňuje zapsat neveřejný komentář k titulu.
Přepne do obrazovky, kde lze editovat objekty navázané na daný titul.
Zobrazí administrativní a technická metadata objektu.
Zobrazí objekt ve formátu FOXML.
Roletka obsahuje typy exportů (skenů, původních skenů, pro Krameria 4).
Smaže vybrané objekty.
### 1.2.3	O ProArcu ###
Tento pořadač otvírá nové okno se základními informacemi o programu a odkazy na podrobnou dokumentaci.

## 1.3	Popis dalších obrazovek ##
### 1.3.1	Obrazovka MODS ###
Při tvorbě nového objektu se objeví dosud skrytá obrazovka MODS, která slouží pro založení nového objektu a případnou editaci již založeného objektu.
Tato obrazovka obsahuje lištu s typem a názvem zakládaného objektu, lištu nástrojů pro založení a editaci objektů a prázdná vybraná pole MODSu pro metadata titulu (nebo se staženými údaji z vybraného katalogu, která je možné rozšířit a doplnit).


Na liště nástrojů jsou umístěny tyto ikony:
Načte znovu data.
Roletka skrývající nabídku pro editaci:
Slouží k editaci vybraného objektu.
Umožňuje zapsat neveřejnou poznámku k titulu.
Přepne do obrazovky Editor připojených objektů.
Zobrazí administrativní a technická metadata objektu.
U provázaných objektů zobrazí nadřazenou úroveň.
Roletka skrývající nabídku druhů MODS.
Zobrazí vybraná pole MODSu pro editaci.
Zobrazí a umožní editovat plný MODS.
Zobrazí XML metadat titulu.
Uloží nový nebo editovaný objekt.
Umožňuje přidat nebo odebrat další pole v rámečku.

### 1.3.2	Import – Příprava metadat pro načtení do úložiště ###
Tato obrazovka slouží k tvorbě popisných dat jednotlivých stran – indexaci. Jedná se o skrytou obrazovku, do které se lze dostat ze složky Import přes pořadač Nová dávka, založením nové dávky, nebo přes pořadač Historie vybráním již založené dávky použitím tlačítka Pokračovat, které je umístěno na liště v horní části obrazovky.
Obrazovka Import – Příprava metadat pro načtení do úložiště je rozdělena do dvou částí. Levá část se dvěma sloupci a společnou lištou nástrojů slouží pro indexaci stran. Pravá část s jedním sloupcem a lištou nástrojů pro práci s náhledem zobrazuje jeden vybraný náhled.
Vlevo se v horním okně nachází sloupcový seznam souborů zvolené dávky s hodnotami: Název souboru, Číslo a Typ (strany). V dolním okně jsou dvě roletky a ikona Uložit. Roletka Další skrývá funkci Obnovit, MODS, Komentář, OCR a ATM. V roletce MODS je výběr různých forem MODSu. Změny se ukládají pomocí tlačítka Uložit nebo enterem.
V prostředním sloupci jsou umístěny náhledy stránek a v pravém sloupci je zobrazen detail zvolené stránky. V této části lze indexovat po vybrání jednoho nebo více obrázků pomocí tabulky v  dolním okně v levém sloupci sloužící pro indexaci.
Jednotlivé sloupce si může uživatel nastavit posunem svislých dělících lišt podle svých potřeb. Rozšířením prostředního sloupce dojde k navýšení počtu náhledů. Rozšířením pravého sloupce se zvětší zobrazení konkrétní strany. Prostřední a pravý sloupec lze úplně skrýt.



Na levé horní liště nástrojů jsou umístěny tyto ikony:
Načte znovu data.
Označí všechny strany.
Zobrazí objekt ve formátu FOXML v novém okně (záložce).
Smaže vybranou stranu.
Validuje metadata digitálních objektů.
Přepne do obrazovky Import-Vybrat nadřazený digitální objekt.
Na levé dolní liště nástrojů jsou umístěny tyto ikony:
Na liště nástrojů jsou umístěny tyto ikony:
Roletka skrývající nabídku pro editaci:
Načte znovu data.
Slouží k editaci vybraného objektu.
Umožňuje zapsat neveřejnou poznámku k titulu.
Tato funkce zatím není k dispozici.
Zobrazí administrativní a technická metadata objektu.
Roletka skrývající nabídku druhů MODS
Zobrazí vybraná pole MODSu pro editaci.
Zobrazí a umožní editovat plný MODS.
Zobrazí XML metadat titulu.
Uloží nový nebo editovaný objekt.
V pravé části, nad třetím sloupcem, se nalézají ikony pro zobrazení vybrané strany.
Zobrazí vybranou stranu v novém okně v plném rozlišení.
Umožňuje stažení a uložení vybrané strany v původním formátu nebo otevření v jiném programu.
Změní barvu pozadí zobrazovaných objektů podle přání uživatele.
Roletka s možnostmi velikostí zobrazení jednotlivé strany.

Indexovat lze jednotlivé strany nebo více stran najednou. Jednou možností je označit jeden nebo více řádků v horním okně levého sloupce. Druhou možností je označení jednoho nebo více náhledů v prostředním sloupci. U obou možností se používá pro indexaci tabulka v dolním okně v levém sloupci.
Tabulka pro editaci jednotlivých stran se objeví po označení jednoho řádku nebo jednoho náhledu. Obsahuje roletku Typ strany s různými typy stran. Dále pole Index strany s předvyplněným indexem strany (nebo prázdné pole pro doplnění indexu strany). Další prázdné pole Číslo strany pro vyplnění čísla strany. Pod tímto polem je identifikátor s automaticky generovaným typem - UUID a jeho hodnotou. Zcela dole je prostor pro Poznámku, která je veřejná.
Tabulka pro editaci více stran najednou se objeví po označení více stran. Je rozdělena do tří částí. V horní části je pole pro indexování, ve střední části se nacházejí pole pro editaci čísel stran a v dolní části lze editovat typy stran. Výběr editace provedeme označením ve vybraném zaškrtávacím poli.
Editovat index stran – do pole Index od lze zadat požadovanou číselnou hodnotu.
Editovat čísla stran
do pole Prefix lze zadat různé znaky
do pole Číslo od se zapisuje hodnota, od které se provádí číslování
v poli Přírůstek je přednastavena hodnota 1, značící číslování po jedné. Je možné nastavit i jinou periodu číslování
do pole Sufix lze zadat různé znaky
roletka Řada nabízí možnosti nejen číselných řad pro paginaci
v řádku Ukázka je náhled zvoleného číslování
Editovat typ stran obsahuje roletku s různými typy stran



### 1.3.3	Editor připojených objektů ###
Do obrazovky Editor připojených objektů se lze dostat z obrazovky Úložiště – Hledat použitím ikony Vazby umístěné na liště nástrojů. Tato obrazovka slouží k editaci objektů napojených na vybraný a označený objekt. Také je zde možnost vytvoření nových napojených objektů na vybraný hlavní objekt (ročník, výtisk) a přesun objektů.

Obrazovka je rozdělena do dvou oken. V levém okně se nachází tabulkový seznam s vybraným objektem. Pravé okno je určené pro editaci jednotlivých objektů.
Na horní liště jsou umístěny tyto ikony:
Načte znovu data.
Roletka skrývající nabídku pro editaci:
Slouží k editaci vybraného objektu.
Umožňuje zapsat neveřejnou poznámku k titulu.
Přepne do obrazovky Editor připojených objektů.
Zobrazí administrativní a technická metadata objektu.
U provázaných objektů zobrazí nadřazenou úroveň.
Roletka s volbou typu objektu (Ročník, Výtisk, Monografie-volná část, Strana)
Smaže vybraný objekt.
U provázaných objektů zobrazí podřazenou úroveň.
Validuje metadata digitálních objektů.

Na liště v pravém okně nalezneme tyto ikony:
Roletka skrývající nabídku pro editaci:
Načte znovu data.
Slouží k editaci vybraného objektu.
Umožňuje zapsat neveřejnou poznámku k titulu.
Tato funkce zatím není k dispozici.
Zobrazí administrativní a technická metadata objektu.
Roletka skrývající nabídku druhů MODS
Zobrazí vybraná pole MODSu pro editaci.
Zobrazí a umožní editovat plný MODS.
Zobrazí XML metadat titulu.
Uloží nový nebo editovaný objekt.
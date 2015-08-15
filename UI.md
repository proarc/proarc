# Editor #

_Umožní tvorbu a editaci metadat digitálních objektů, včetně importů a exportů a práci s primárními daty._

**(01)**
## Seznam vlastností ##

  * Systém bude využívat repozitář Fedora.
  * Bude podporovat stavající datové modely systému Kramerius 4.
  * Umožní editaci metadat.
  * Bude podporovat dávkové/hromadné úpravy.
  * Umožní vytváření zcela nových objektů.
  * Bude podporovat přebírání data z externích systémů (Aleph).
  * Fedora objekt bude integrovat metadata a digitalni obsah (sken, …)
  * Bude generovat UUID.
  * Bude podporovat ADM a TECH metadata (ATM).
  * Umožní vyhledávání v metadatech.
  * Do budoucna umožní rozšíření o dalších datové modely.


### Akce ###
#### [Popis workflow](Workflow.md) ####
  * uložení skenů do adresáře na serveru
  * import skenů do systému
    * výběr typu objektu
    * založení/zařazení do struktury
    * 
  * stažení metadat do editoru z Alephu
  * doplnění chybějících metadat (popisná, ATM)
  * vytvoření OCR
  * tvorba poznámek
  * export (publikace do Krameria)

#### Akce s obrázky ####
  * import
  * generování náhledů a ikon
  * změny pořadí
  * mazání duplicit, nadbytečných stran
  * přejmenování souboru
  * změna formátu
  * mazání zpracovaných skenů

#### Akce s metadaty ####
  * komunikace s Alpehem (Z39.50)
  * generování UUID
  * tvorba ATM
  * doplnění chybějících

#### OCR ####
  * plain text

#### Tvorba poznámek ####
  * interní i veřejné (dva datastreamy)
  * automaticky zaznamenávat operace: založil, editoval, smazal, exportoval, zalogoval se

#### Hromadné zpracování ####
  * některé akce bude možné provádět hromadně

#### Export ####
  * do Krameria
  * na pásku
  * tiffů pro třetí aplikace

#### Hledání ####
  * prohledávání ve všech údajích + filtrování:
    * naposledy editované dokumenty
    * dokumenty editované určitým uživatelem
    * podle údajů z bibl. záznamu

### Uživatelé ###
  * **administrátor:** přístup ke všem datům a akcím (import, export, editace, ...)
  * **zpracovatel:** omezený přístup nastavený defaultně nebo adminsitrátorem
  * **přístup k datům:**
    * vlastní uživatelské prostory pro import skenů a jejich export k editaci
    * k naimportovaným objektům budou mít přístup všichni
  * tiskové sestavy (pouze zobrazit)
    * tabulka zpracovaných titulů až na úroveň výtisků popřípadě článků (možnost definovat úroveň) za časové období (možnost nastavení intervalu)
    * operace provedené jednotlivým uživatelem za určené časové období (veškeré operace od založení dávky až po export - možnost vybrat operace)

### Další ###
  * rozhraní lokalizované do češtiny
  * klávesové zkratky (v rámci možností prohlížeče)
  * nápovědy po najetí kurzorem (využití popup)
  * tlačítkové formuláře (nebude vyžadovat podrobnou znalost metadat)
  * umožní snadné opravy včetně přesunů dat/metadat (grafické)
  * možnost automatického průběžného ukládání (časovač)
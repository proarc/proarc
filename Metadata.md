**(06)**
# Metadata #
**východiska:**
  * [materiál z NK ČR](http://www.ndk.cz/digitalizace/nove-standardy-digitalizace-od-roku-2011/metadata_periodika_v1-2_25.11) zpracovaný J. Hutařem
  * [pracovní materiál](https://docs.google.com/spreadsheet/ccc?key=0Aj7f9Vh8ZDbTdFZPbXc2WlFEUUFNNE40b2prZkRCTUE&hl=cs#gid=0) zpracovaný P. Švástovou z MZK
  * materiál určující metadata pro zpracování odborných článků v KnAV od L. Jansové
  * náhledy obrazovek sw Sirius
**výsledek:**
  * výběr metadat jako podklad pro editační formulář pro tvorbu metadat
    * [periodika](https://docs.google.com/spreadsheet/ccc?key=0Arylv_wbomHRdHpUZlNVMGt4U05MVjNEZjhjdVBCVUE&hl=cs#gid=0)
    * [monografie](https://docs.google.com/spreadsheet/ccc?key=0Arylv_wbomHRdFJmY0NjMzFuQURrUy0wZlhrdDRxbnc&hl=cs#gid=0)


## Verze z konce srpna 2011 NEAKTUÁLNÍ ##
  * vybraná "povinná" pole formátu MODS

### Periodikum - titul ###
| **prvek** | **podprvek** | **poznámka** |
|:----------|:-------------|:-------------|
| 

&lt;titleInfo&gt;

 | 

&lt;title&gt;

 |              |
| 

&lt;name&gt;

 | 

&lt;NamePart&gt;

 | např. redkator, editor, ... |
| 

&lt;typeOfResource&gt;

 |              | výběr z: text, cartographic, notated music, sound recording, sound recording – musical, sound recording – nonmusical, still image, moving image, three dimensional object, software, multimedia, mixed material |
| 

&lt;originInfo&gt;

 | 

&lt;issuance&gt;

 | continuing   |
|           | 

&lt;publisher&gt;

 |              |
| 

&lt;language&gt;

 | 

&lt;languageTerm&gt;

 | `<mods:languageTerm authority="iso639-2b" type="code">lat</mods:languageTerm>`|
| 

&lt;physicalDescription&gt;

 | 

&lt;form&gt;

 | bude se generovat hodnota: electronic; v MZK mají v současnosti: technique material; electronic |
|           | 

&lt;digitalOrigin&gt;

 | výběr z: reformated digital, digitized microfilm |
| 

&lt;classification&gt;

 |              | `<mods:classification authority="udc">025.171</mods:classification>`|
| 

&lt;identifier&gt;

 |              | bude se generovat UUID; **Pro potřeby propojení s RD by data měla obsahovat urn:nbn nebo číslo zakázky. KNAV zajistí proces získání některého z těchto identifikátorů** |
| 

&lt;location&gt;

 | 

&lt;physicalLocation&gt;

 |              |
|           | 

&lt;shelfLocator&gt;

 |              |
| 

&lt;part&gt;

 | 

&lt;detail&gt;

 |              |
|           | 

&lt;date&gt;

 |              |
| 

&lt;recordInfo&gt;

 | 

&lt;recordContentSource&gt;

 |              |
|           | 

&lt;recordCreationDate&gt;

 | bude se generovat |
|           | 

&lt;recordChangeDate&gt;

 | bude se generovat |
|           | 

&lt;recordIdentifier&gt;

 | bude se generovat |

### Periodikum - ročník, číslo, článek ###
| **prvek** | **podprvek** | **poznámka** |
|:----------|:-------------|:-------------|
| 

&lt;titleInfo&gt;

 | 

&lt;title&gt;

 |              |
| 

&lt;name&gt;

 | 

&lt;NamePart&gt;

 | např. redkator, editor, ... |
| 

&lt;typeOfResource&gt;

 |              | výběr z: text, cartographic, notated music, sound recording, sound recording – musical, sound recording – nonmusical, still image, moving image, three dimensional object, software, multimedia, mixed material |
| 

&lt;originInfo&gt;

 | 

&lt;issuance&gt;

 | continuing   |
|           | 

&lt;publisher&gt;

 |              |
| 

&lt;genre&gt;

 |              | u čl. se bude generovat: article |
| 

&lt;language&gt;

 | 

&lt;languageTerm&gt;

 | `<mods:languageTerm authority="iso639-2b" type="code">lat</mods:languageTerm>`|
| 

&lt;classification&gt;

 |              | `<mods:classification authority="udc">025.171</mods:classification>`|
| 

&lt;relatedItem&gt;

 |              |  type="host" |
|           | 

&lt;titleInfo&gt;

 |              |
|           | 

&lt;identifier&gt;

 | bude se generovat UUID; **Pro potřeby propojení s RD by data měla obsahovat urn:nbn nebo číslo zakázky. KNAV zajistí proces získání některého z těchto identifikátorů** |
|           | 

&lt;part&gt;



&lt;detail&gt;

  | type="chapter" |
|           | 

&lt;part&gt;



&lt;extent&gt;



&lt;start&gt;

 |              |
|           | 

&lt;part&gt;



&lt;extent&gt;



&lt;end&gt;

 |              |
| 

&lt;identifier&gt;

 |              | bude se generovat UUID; **Pro potřeby propojení s RD by data měla obsahovat urn:nbn nebo číslo zakázky. KNAV zajistí proces získání některého z těchto identifikátorů** |
| 

&lt;location&gt;

 | 

&lt;physicalLocation&gt;

 |              |
|           | 

&lt;shelfLocator&gt;

 |              |
| 

&lt;part&gt;

 | 

&lt;detail&gt;

 |              |
|           | 

&lt;date&gt;

 |              |
| 

&lt;recordInfo&gt;

 | 

&lt;recordContentSource&gt;

 |              |
|           | 

&lt;recordCreationDate&gt;

 | bude se generovat |
|           | 

&lt;recordChangeDate&gt;

 | bude se generovat |
|           | 

&lt;recordIdentifier&gt;

 | bude se generovat |


### Monografie - titul ###
| **prvek** | **podprvek** | **poznámka** |
|:----------|:-------------|:-------------|
| 

&lt;titleInfo&gt;

 | 

&lt;title&gt;

 |              |
| 

&lt;name&gt;

 | 

&lt;NamePart&gt;

 | osoba/autor  |
| 

&lt;typeOfResource&gt;

 |              | výběr z: text, cartographic, notated music, sound recording, sound recording – musical, sound recording – nonmusical, still image, moving image, three dimensional object, software, multimedia, mixed material |
| 

&lt;originInfo&gt;

 | 

&lt;issuance&gt;

 | monographic  |
|           | 

&lt;publisher&gt;

 |              |
| 

&lt;language&gt;

 | 

&lt;languageTerm&gt;

 | `<mods:languageTerm authority="iso639-2b" type="code">lat</mods:languageTerm>`|
| 

&lt;physicalDescription&gt;

 | 

&lt;form&gt;

 | bude se generovat hodnota: electronic; v MZK mají v současnosti: technique material; electronic |
|           | 

&lt;digitalOrigin&gt;

 | výběr z: reformated digital, digitized microfilm |
| 

&lt;classification&gt;

 |              | `<mods:classification authority="udc">025.171</mods:classification>` |
| 

&lt;identifier&gt;

 |              | bude se generovat UUID; **Pro potřeby propojení s RD by data měla obsahovat urn:nbn nebo číslo zakázky. KNAV zajistí proces získání některého z těchto identifikátorů** |
| 

&lt;location&gt;

 | 

&lt;physicalLocation&gt;

 |              |
|           | 

&lt;shelfLocator&gt;

 |              |
| 

&lt;recordInfo&gt;

 | 

&lt;recordContentSource&gt;

 |              |
|           | 

&lt;recordCreationDate&gt;

 | bude se generovat |
|           | 

&lt;recordChangeDate&gt;

 | bude se generovat |
|           | 

&lt;recordIdentifier&gt;

 | bude se generovat |

### Monografie - kapitola ###
| **prvek** | **podprvek** | **poznámka** |
|:----------|:-------------|:-------------|
| 

&lt;titleInfo&gt;

 | 

&lt;title&gt;

 |              |
| 

&lt;name&gt;

 | 

&lt;NamePart&gt;

 | osoba/autor  |
| 

&lt;typeOfResource&gt;

 |              | výběr z: text, cartographic, notated music, sound recording, sound recording – musical, sound recording – nonmusical, still image, moving image, three dimensional object, software, multimedia, mixed material |
| 

&lt;language&gt;

 | 

&lt;languageTerm&gt;

 | `<mods:languageTerm authority="iso639-2b" type="code">lat</mods:languageTerm>`|
| 

&lt;relatedItem&gt;

 |              |  type="host" |
|           | 

&lt;titleInfo&gt;

 |              |
|           | 

&lt;identifier&gt;

 | bude se generovat UUID; **Pro potřeby propojení s RD by data měla obsahovat urn:nbn nebo číslo zakázky. KNAV zajistí proces získání některého z těchto identifikátorů** |
|           | 

&lt;part&gt;



&lt;detail&gt;

  | type="chapter" |
|           | 

&lt;part&gt;



&lt;extent&gt;



&lt;start&gt;

 |              |
|           | 

&lt;part&gt;



&lt;extent&gt;



&lt;end&gt;

 |              |
| 

&lt;identifier&gt;

 |              |              |
| 

&lt;location&gt;

 | 

&lt;url&gt;

 |              |
| 

&lt;recordInfo&gt;

 | 

&lt;recordContentSource&gt;

 |              |
|           | 

&lt;recordCreationDate&gt;

 | bude se generovat |
|           | 

&lt;recordChangeDate&gt;

 | bude se generovat |
|           | 

&lt;recordIdentifier&gt;

 | bude se generovat |

### Monografie, Periodikum - strana ###
| **prvek** | **podprvek** | **poznámka** |
|:----------|:-------------|:-------------|
| 

&lt;typeOfResource&gt;

 |              | výběr z: text, cartographic, notated music, sound recording, sound recording – musical, sound recording – nonmusical, still image, moving image, three dimensional object, software, multimedia, mixed material |
| 

&lt;relatedItem&gt;

 |              |  type="host" |
|           | 

&lt;titleInfo&gt;

 |              |
|           | 

&lt;identifier&gt;

 | bude se generovat UUID; **Pro potřeby propojení s RD by data měla obsahovat urn:nbn nebo číslo zakázky. KNAV zajistí proces získání některého z těchto identifikátorů** |
| 

&lt;part&gt;

 |              | type="NormalPage" a asi nějaké další |
|           | 

&lt;part&gt;



&lt;detail&gt;

 | type="pageNumber" |
|           | 

&lt;part&gt;



&lt;number&gt;

 |              |
|           | 

&lt;part&gt;



&lt;detail&gt;

 | type="pageIndex" |
| 

&lt;identifier&gt;

 |              |              |



### ATM ###
  * typ skeneru, požadavky pro optimální zobrazení, vlastník, donátor copyright
  * ve fedoře ručně předpřipravené objekty, na které se bude odkazovat
    * KnAV dodá XML s ATM daty
  * v editoru zobrazitelné jako needitovatelné XML
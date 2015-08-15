

# Konfigurace systému #

Po spuštění aplikačního serveru se v adresáři `$PROARC_HOME` vytvoří soubor [proarc.cfg.template](http://archivacni-system.googlecode.com/git/proarc-common/src/main/resources/cz/cas/lib/proarc/common/config/proarc.properties) s přednastavenými parametry systému. Pro vlastní nastavení vytvořte soubor `proarc.cfg` s parametry, které chcete změnit. Případné změny souboru `proarc.cfg.template` budou přepsány.

**Číslo v závorkách** znamená verzi, od které lze nastavení použít.

## Úložiště Fedora Commons ##

Parametry pro připojení k úložišti Fedora.
```
fedora.client.url=http://localhost:8080/fedora
fedora.client.username=fedoraAdmin
fedora.client.password=fedoraAdmin
```

## Katalogy s metadaty ##

Při vytváření nových digitálních objektů lze využít existující popisná metadata z různých katalogů.

Seznam ID deklarovaných katalogů oddělených čárkou.
```
## Bibliographic metadata catalogs
catalogs=aleph_nkp,rdcz,z3950,oai
```

Deklarace daného katalogu je prefixovaná pomocí `catalog.<ID>`. Názvy katalogů (parametr `name`) se zobrazí uživateli.

#### Registr digitalizace ####
```
catalog.rdcz.url=http://registrdigitalizace.cz/soapservices
catalog.rdcz.name=registrdigitalizace.cz
catalog.rdcz.user=
catalog.rdcz.password=
catalog.rdcz.type=DigitizationRegistryCatalog
# (2.0) fields - seznam všech možných polí: barcode, ccnb, isbn, issn, signature, title
catalog.rdcz.fields=isbn
# (2.0) title - změna názvu pole v UI
catalog.rdcz.field.isbn.title=International Standard Book Number
```

#### Aleph - XServer ####
```
# Aleph NKP
catalog.aleph_nkp.url=http://aleph.nkp.cz/X?base=nkc
catalog.aleph_nkp.name=Aleph NKP
catalog.aleph_nkp.type=AlephXServer
```

#### Aleph - Z39.50 ####
```
# Z39.50
# <port> is mandatory!
catalog.z3950.url=<hostname>:<port>
catalog.z3950.base=DATABASE_ID
catalog.z3950.name=Z39.50 Catalog
catalog.z3950.type=Z3950Catalog
# recordCharset is optional; use in case the server returns records with unexpected charset
# http://docs.oracle.com/javase/6/docs/api/java/nio/charset/Charset.html
#catalog.z3950.recordCharset=UTF-8

# fields - seznam polí zobrazených uživateli v daném pořadí
catalog.z3950.fields=issn, isbn
# query - RPN dotaz poslaný Aleph serveru. %s je nahrazeno hledaným textem
#         viz. http://www.indexdata.com/zebra/doc/querymodel-rpn.html
catalog.z3950.field.issn.query=@attrset bib-1 @attr 1=8 "%s"
## (2.0) title - název pole zobrazený v UI
catalog.z3950.field.issn.title=International Standard Serial Number
catalog.z3950.field.isbn.query=@attrset bib-1 @attr 1=7 "%s"
```

#### OAI ####
OAI katalog umožňuje převzít popisná metadata pomocí protokolu [OAIPMH](http://www.openarchives.org/OAI/openarchivesprotocol.html).
```
# (2.1) povinné URL OAI serveru
catalog.oai.url=http://arXiv.org/oai2
# (2.1) povinné označení katalogu
catalog.oai.type=OAICatalog
# (2.1) název katalogu zobrazený uživateli
catalog.oai.name=OAI Katalog
# (2.1) povinný OAI metadata prefix pro data ve formátu http://www.loc.gov/standards/marcxml/schema/MARC21slim.xsd
catalog.oai.metadataPrefix=marc21
# (2.1) nepovinný prefix identifikátoru, který se přidá před hodnotu vyplněnou uživatelem
catalog.oai.identifierPrefix=oai:arXiv.org:
# (2.1) povolená pole: id
catalog.oai.fields=id
# (2.1) název pole zobrazený uživateli
catalog.oai.field.id.title=Identifikátor
```

## Import skenů ##
### Kopie JPEG ###
Metoda použitá pro zmenšování obrázků. Metodu lze nastavit pro náhled i ikonu zvlášť viz. níže.
```
# Algorithm to scale an image with java.image.
# Supported values:
#   AREA_AVERAGING, BICUBIC_STEPPED, BILINEAR, BICUBIC, BILINEAR_STEPPED,
#   NEAREST_NEIGHBOR, NEAREST_NEIGHBOR_STEPPED, REPLICATE
import.image.java.scalingMethod=BICUBIC_STEPPED
```

Parametry pro vytvoření náhledu z původního skenu.
```
## PREVIEW datastream
# max width to scale origin scan
#import.image.preview.maxWidth=
# max height to scale origin scan
import.image.preview.maxHeight=1000
# Algorithm to scale an image.
import.image.preview.java.scalingMethod=${import.image.java.scalingMethod}
```

Parametry pro vytvoření ikony z původního skenu.
```
## THUMBNAIL datastream
# max width to scale origin scan
import.image.thumbnail.maxWidth=120
# max height to scale origin scan
import.image.thumbnail.maxHeight=128
# Algorithm to scale an image.
import.image.thumbnail.java.scalingMethod=${import.image.java.scalingMethod}
```

### Kopie JPEG2000 ###
Parametry pro načtení externě připravených kopií skenu ve formátu JPEG2000 pro standard NDK.
```
# (2.0) Koncovka archivní kopie.
import.ndk_archival.file.suffix=.ac.jp2
# (2.0) Koncovka uživatelské kopie.
import.ndk_user.file.suffix=.uc.jp2
```

Pro generování kopií ve formátu JPEG2000, lze použít externí aplikaci [www.kakadusoftware.com Kakadu] doporučenou NDK (i s parametry) pro převod TIFF/JP2.
```
# (2.0) ID profilu externího procesu pro vytvoření archivní kopie.
import.ndk_archival.processor=kdu_ac
# (2.0) ID profilu externího procesu pro vytvoření uživatelské kopie.
import.ndk_user.processor=kdu_uc
```

Profily externího procesu lze definovat následovně:
```
KDU_HOME=<KAKADU_HOME>
KDU_EXEC=${KDU_HOME}/kdu_compress
KDU_LIBPATH=${KDU_HOME}/lib

## Ukázka nastavení pro archivní kopii dle NDK.
# (2.0) Plná cesta spustitelného externího procesu.
processor.kdu_ac.exec=${KDU_EXEC}
# (2.0) Plná cesta k adresáři s knihovnami kdu. Nepovinné, pokud jsou knihovny dostupné pro tomcat.
processor.kdu_ac.environment.LD_LIBRARY_PATH=${KDU_LIBPATH}
# (2.0) Typ procesu.
processor.kdu_ac.type=kdu_compress
# (2.0) Parametry externího procesu.
processor.kdu_ac.arg=-quiet
processor.kdu_ac.arg=Cblk\={64\,64}
processor.kdu_ac.arg=Corder\=RPCL
processor.kdu_ac.arg=Stiles\={4096\,4096}
processor.kdu_ac.arg=Cprecincts\={256\,256}\,{256\,256}\,{128\,128}
processor.kdu_ac.arg=ORGtparts\=R
processor.kdu_ac.arg=Creversible\=no
processor.kdu_ac.arg=Clayers\=1
processor.kdu_ac.arg=Clevels\=5
processor.kdu_ac.arg=Cmodes\={BYPASS}
processor.kdu_ac.arg=Cuse_sop\=yes
processor.kdu_ac.arg=Cuse_eph\=yes

## Podobně se konfiguruje profil pro uživatelskou kopii.
processor.kdu_uc.exec=${KDU_EXEC}
processor.kdu_uc.environment.LD_LIBRARY_PATH=${KDU_LIBPATH}
processor.kdu_uc.type=kdu_compress
...
```

Při testech na platformě Windows se výjimečně stávalo, že proces `kdu_compress.exe` náhodně zamrzl nebo skončil předčasně. Proto byly přidány následující parametry:
```
# (2.0) Maximální očekávaná doba běhu procesu v ms; 0 - znamená že se čeká věčně; standardně se čeká 2 min.
processor.<id>.timeout=120000
# (2.0) počet pokusů opětovného spuštění procesu, pokud skončil neočekávaně; standardně 0
processor.<id>.retry=1
```

## OCR ##

Při importu skenů lze k nově vytvářeným digitálním objektům přiřadit předem připravené OCR jako neformátovaný text nebo [ALTO](http://www.loc.gov/alto). Pro správné spárování skenu a OCR je potřeba dodržet formát `<název_skenu>.tif` a `<název_skenu>.<ocr_koncovka>`, kde koncovka OCR je konfigurovatelná.
```
# Koncovka pro textový obsah.
import.text_ocr.file.suffix=.ocr.txt
# (2.0) Koncovka pro obsah s ALTO.
import.alto.file.suffix=.ocr.xml
```

Pokud předem připravené OCR je v jiném kódování než UTF-8, je potřeba změnit následující parametr. Do úložiště je OCR uloženo vždy v kódování UTF-8. Popis možných hodnot na [zde](http://docs.oracle.com/javase/6/docs/api/java/nio/charset/Charset.html).
```
import.text_ocr.file.charset=UTF-8
```

## URN:NBN ##

Pro tvorbu NDK balíčků je nutné získávat identifikátory URN:NBN. Identifikátory přiděluje služba [CZIDLO](https://resolver.nkp.cz), kde se producent balíčků zaregistruje.
```
# (2.0) Čárkou oddělený seznam profilů. Zatím se využívá jen první.
urnnbn.resolvers=knav

# (2.0) ID registrátora
urnnbn.resolver.knav.registrar=aba007
# (2.0) Nepovinné ID archivátora.
urnnbn.resolver.knav.archiver=18
# (2.0) Uživatelský popis profilu.
urnnbn.resolver.knav.title=KNAV Test
# (2.0) URL kde běží služba resolveru.
urnnbn.resolver.knav.url=https://resolver-test.nkp.cz/api/v3
# (2.0) Uživatelské jméno pro přihlášení k resolveru.
urnnbn.resolver.knav.user=<username>
# (2.0) Heslo pro přihlášení k resolveru.
urnnbn.resolver.knav.passwd=***
```

## Exporty ##
### K4 export ###

Při exportu hierarchie digitálních lze nastavit přístup k daným objektům.
```
# default access policy used for exported object. Values: policy:private|policy:public|<empty>
export.kramerius4.policy=policy:private
```
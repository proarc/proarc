# Mets Corrector #

Nástroj pro opravu issues:
> [280 - chybně generované soubory md5 a info.xml](https://code.google.com/p/archivacni-system/issues/detail?id=280)

> [290 -Spatny ty METS baliku - namisto Monograph je monograph](https://code.google.com/p/archivacni-system/issues/detail?id=290)

> [291 - AMD Mets - chybi typ fyzicke mapy](https://code.google.com/p/archivacni-system/issues/detail?id=291)


Opravný nástroj je určen pro balíčky, které již v info.xml obsahují md5 sum


# Spouštění #

Nástroj je napsaný v jazyku Java a je spustitelný přímo z jar souboru
```

java -jar metsCorrector.jar [sourceDirectory] [backupDirectory]
```

  * sourceDirectory - kořenový adresář, který je třeba opravit
  * backupDirectory - adresář, kam jsou uloženy původní soubory a žurnál opravy

# Běh #
Program se snaží projít zadaný zdrojový adresář a v něm najít PSP balíčky. Pokud je nalezen PSP balíček, založí se bez ohledu na to, zda je balíček validní nebo ne záložní adresář jako podadresář zadaný parametrem pro spuštění.

Kontrolují se soubory v adresáři amdSec, hlavní METS soubor, MD5 a info.xml

Pokud se vyskytne chyba, je toto zalogováno do žurnálového souboru, který se nachází v adresáři se zálohami a chyba je opravena.
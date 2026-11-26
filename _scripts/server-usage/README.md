# Využití disků serveru

`server_usage.sh` měří kapacitu a obsazenost 1–N pojmenovaných souborových systémů
na Linuxu. Vyžaduje Bash 4.4+, `jq`, GNU coreutils (`df`, `date` atd.) a `flock`
z util-linux. Na Debianu/Ubuntu lze nástroje doplnit příkazem
`sudo apt-get install bash jq coreutils util-linux`.

Pro Windows je k dispozici `server_usage.ps1`. Vyžaduje Windows PowerShell 5.1
nebo PowerShell 7 na Windows, bez dalších balíčků. Používá Windows API
`GetDiskFreeSpaceEx`, včetně cest na připojených svazcích a UNC sdíleních.
Formát konfigurace i výstupních JSON souborů je stejný jako u Bash verze.

## Konfigurace a spuštění

Zkopírujte `disks.example.json` a nastavte pro každý disk:

- `id`: jedinečný stabilní identifikátor pro spojení bodů v historickém grafu;
- `name`: zobrazovaný název, může obsahovat diakritiku;
- `path`: absolutní cesta k existujícímu adresáři na měřeném souborovém systému.

Více cest na témže souborovém systému vrací stejnou obsazenost celého systému.
Pokud měříte připojený disk, zajistěte jeho připojení před měřením: existující
prázdný mountpoint po odpojení disku vrací kapacitu nadřazeného souborového systému.

```sh
/bin/bash /opt/proarc/scripts/server-usage/server_usage.sh \
  --config /etc/proarc/disks.json \
  --output-dir /data/proarc/.proarc
```

Výchozí výstupní adresář je `.proarc` v domovském adresáři uživatele spouštějícího
skript. V cronu nastavte `--output-dir` explicitně na konfigurační adresář ProArcu.
Výstupní soubory mají pevná jména:

- `server-usage-actual.json`: poslední měření, při každém spuštění se přepisuje;
- `server-usage-historical.json`: denní vzorky, mění se jen s `--historical`.

Přepínač `--historical` provede nové měření a zapíše oba soubory. Opakované
spuštění během stejného místního kalendářního dne nahradí vzorek tohoto dne.
Starší dny zůstávají zachovány; historie nemá automatické mazání.

## Cron

Příklad crontabu pro uživatele, který může zapisovat do `/data/proarc/.proarc`:

```cron
15,30,45 * * * * /bin/bash /opt/proarc/scripts/server-usage/server_usage.sh --config /etc/proarc/disks.json --output-dir /data/proarc/.proarc
0 1-23 * * * /bin/bash /opt/proarc/scripts/server-usage/server_usage.sh --config /etc/proarc/disks.json --output-dir /data/proarc/.proarc
0 0 * * * /bin/bash /opt/proarc/scripts/server-usage/server_usage.sh --config /etc/proarc/disks.json --output-dir /data/proarc/.proarc --historical
```

Tím vzniká aktuální vzorek každých 15 minut a historický o půlnoci, bez souběhu
dvou úloh v 00:00. Datum vzorku se řídí lokální časovou zónou procesu; nastavte
stejnou zónu pro cron i skript (například `Europe/Prague`). Půlnoční měření má datum
právě začínajícího dne. Skript nedoplňuje dny, kdy cron neběžel.

## PowerShell a Plánovač úloh ve Windows

Zkopírujte `disks.windows.example.json` a nastavte existující absolutní cesty
ve Windows. Zpětná lomítka se v JSON zapisují zdvojeně: `"C:\\"` nebo
`"D:\\ProArc"`. Připojené svazky se měří podle zadaného adresáře, 
nikoliv pouze podle písmene nadřazeného disku.

```powershell
powershell.exe -NoProfile -File .\server_usage.ps1 `
  -Config .\disks.windows.json `
  -OutputDir C:\ProArc\.proarc

# Nové měření, zápis aktuálního souboru i denního vzorku:
powershell.exe -NoProfile -File .\server_usage.ps1 `
  -Config .\disks.windows.json `
  -OutputDir C:\ProArc\.proarc -Historical
```

Pro PowerShell 7 použijte `pwsh.exe` se stejnými argumenty. Výchozí výstupní
adresář je i zde `$HOME\.proarc`. Výstup má kódování UTF-8 bez BOM.
Zápis nahrazuje soubor atomicky pomocí .NET a při přepisu zachovává jeho ACL;
nové soubory dědí oprávnění výstupního adresáře. Proces chrání exkluzivně otevřený
soubor `.server-usage.lock`. Souběžný druhý proces skončí s kódem `1`.
Výstupní souborový systém musí podporovat atomické nahrazení souboru.

V Plánovači úloh nastavte dvě úlohy pod účtem, který může číst měřené disky
a zapisovat do cílového adresáře:

- aktuální měření každých 15 minut, například od 00:01 (00:01, 00:16, …);
- historii každý den v 00:00, s přidaným argumentem `-Historical`.

Posun o jednu minutu předchází souběhu obou úloh o půlnoci. Program je
`powershell.exe` a argumenty aktuální úlohy například:

```text
-NoProfile -NonInteractive -File "C:\ProArc\scripts\server_usage.ps1" -Config "C:\ProArc\disks.windows.json" -OutputDir "C:\ProArc\.proarc"
```

Použijte absolutní cesty; čas denního vzorku odpovídá lokální zóně Windows.
Pro síťová úložiště používejte UNC cesty dostupné účtu úlohy: interaktivně
namapované písmeno síťového disku nemusí být v Plánovači úloh dostupné.
Skript sám úlohy neregistruje ani nemění zásady spouštění PowerShellu.
Stejně jako v Bash verzi se chybějící dny nedoplňují a denní opakování nahrazuje
vzorek stejného data. Windows a linuxový sběrač směrujte do samostatných
výstupních adresářů; jejich mechanismy zámků se mezi platformami nesdílejí.

## Formát JSON

Příklad `server-usage-actual.json` (velikosti jsou v bajtech):

```json
{
  "schemaVersion": 1,
  "measuredAt": "2026-11-24T23:00:00+00:00",
  "date": "2026-11-25",
  "disks": [
    {
      "id": "archive",
      "name": "Archiv ProArc",
      "path": "/mnt/proarc",
      "totalBytes": 1000000000,
      "usedBytes": 600000000,
      "freeBytes": 400000000,
      "error": null
    }
  ]
}
```

`measuredAt` je čas v UTC s časovým posunem, `date` je místní datum měření.
`freeBytes` odpovídá místu dostupnému uživateli podle `df --block-size=1 --output=avail`.
PowerShellová verze používá hodnoty vrácené Windows API `GetDiskFreeSpaceEx`.
Na Unixu kvůli rezervovaným blokům nemusí součet `usedBytes + freeBytes`
odpovídat `totalBytes`. Procento obsazení lze počítat jako
`100 * usedBytes / totalBytes` pro kladnou kapacitu a úspěšné měření.

Historický soubor obsahuje `schemaVersion: 1` a pole `samples`. Každá položka
`samples` má celý výše uvedený formát aktuálního měření. Pro graf použijte
`measuredAt` jako čas a `disks[].id` jako identitu řady; názvy i kapacity se
mohou v čase měnit. Chybějící disk nebo chybu zobrazte jako mezeru v grafu.

Při chybě konkrétního disku jsou jeho tři velikosti `null` a `error` obsahuje
popis chyby. Ostatní disky se zapíší a skript skončí s kódem `1` a chybou na
stderr. Kód `0` znamená úspěch všech měření. Neplatná konfigurace, poškozená
historie nebo chyba zápisu také vrací kód `1`. Poškozená historie se nemaže.

## Docker a Tomcat

Pro měření hostitelských disků spouštějte cron na hostiteli. Adresář obsahující
oba JSON soubory musí být dostupný aplikaci v jejím konfiguračním adresáři
`.proarc`. U Dockeru použijte sdílený bind mount tohoto adresáře. Připojujte celý
adresář, protože atomické přepisování nahrazuje jednotlivé soubory novými inody.
Cesty v konfiguraci skriptu patří hostiteli; klient je nemusí vyhodnocovat.

Při spuštění skriptu uvnitř kontejneru budou měřeny pouze souborové systémy
viditelné v kontejneru, podle tamních cest. U samostatného Tomcatu nastavte
výstup přímo do jeho adresáře `.proarc`. Uživatel Tomcatu musí mít právo číst
soubory i procházet nadřazené adresáře. Nové JSON soubory mají oprávnění `0644`;
při přepisu se zachovají existující oprávnění souboru. Vlastníkem nového souboru
je uživatel spouštějící skript.

Každý soubor se zapisuje atomicky a souběžné zápisy chrání souborový zámek
`.server-usage.lock`; souběžný druhý proces skončí kódem `1`. Adresář musí
podporovat souborové zámky a atomické přejmenování. Dvojice JSON souborů není
jedna transakce: při chybě mezi dvěma zápisy může mít každý jiný čas měření.

## Načítání v ProArcu a klient

ProArc čte soubory při požadavku přes REST API. Výchozí názvy v konfiguračním
adresáři lze změnit v souboru `proarc.cfg`:

```properties
statistics.serverUsage.actualFile=server-usage-actual.json
statistics.serverUsage.historicalFile=server-usage-historical.json
```

Relativní cesty se vyhodnocují vůči `PROARC_HOME` (konfigurační adresář), absolutní
cesty umožňují číst soubory i z jiného adresáře. Pokud změníte názvy, zajistěte
odpovídající publikování souborů sběračem; jeho výstupní názvy zůstávají pevné.

- `GET /rest/v2/statistics/server-usage`: poslední měření v `actual`, stav
  v `error` a již vypočtené velikosti adresářů v `storageUsage`.
- `GET /rest/v2/statistics/server-usage/history`: seřazená denní měření
  v `samples` a stav v `error`.

Oba endpointy existují také pod `/rest/v1` a používají běžnou obálku
`response.data[0]`. Stav `error` je `null`, `missing` (soubor neexistuje)
nebo `invalid` (nečitelný či neplatný soubor). Nedostupná měření se nenahrazují
nulami. Čtení API neprochází úložiště; velikosti adresářů pocházejí z posledního
měření ProArcu a mají samostatný čas `storageUsage.calculatedAt`.

V klientovi otevřete **Statistiky → Obecné** pro přehled 1–N disků s pruhem
obsazenosti. Kliknutí na disk otevře **Využití úložiště**, kde jsou velikosti
`datastore`, `objectstore` a `users`, aktuální kapacita a historický graf.
Přehled se obnovuje každou minutu; měření starší než hodina je označeno.
Tlačítko **Obnovit** v detailu načte i historii.

Graf přepíná obsazenost a přírůstky po dnech, týdnech (od pondělí) či měsících.
Obsazenost odpovídá poslednímu dni v období; přírůstek je součet denních rozdílů
obsazenosti a může být záporný. Chybějící den, disk či chyba měření znamená mezeru,
ne nulovou hodnotu. Přírůstek potřebuje předchozí den jako základ; přes chybějící
dny se neodhaduje. První a probíhající období mohou být neúplná.
Hodnoty i časy měření jsou dostupné také v rozbalitelné tabulce.
Historické velikosti jednotlivých adresářů sběrač nezaznamenává; graf zobrazuje
obsazenost souborových systémů.

## Ověření

```sh
cd _scripts/server-usage
bash test_server_usage.sh
```
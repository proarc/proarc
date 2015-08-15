[Issue 109](https://code.google.com/p/archivacni-system/issues/detail?id=109). Připomínky a návrhy můžete přidat jako komentář níže.

# Motivace #
V případě instalace se složitější uživatelskou strukturou, je potřeba oddělit digitální objekty mezi skupinami uživatelů. Pro tento účel se zavede [organizace](#Organizace.md).

# Organizace #

Organizace je skupina uživatelů, kteří sdílí množinu digitálních objektů, případně různých nastavení.

Uživatel bude členem jedné nebo žádné organizace.

Digitální objekt bude patřit jedné nebo žádné organizaci.


# Správa organizací #

Pro správu organizací bude sloužit nový editor "Správa organizací", který umožní:
  * přidat organizaci
  * smazat organizaci, pokud není členem žádný uživatel nebo nevlastní digitální objekt
  * editace
    * název organizace
    * případně další; navrhněte


# Uživatelé #

V editoru Uživatelé přibude ve formuláři pole Organizace s přepínačem dostupných organizací. Bude možné organizaci nepřiřadit.


# Digitální objekt #

Při vytvoření digitálního objektu se automaticky přidá vazba na organizaci přihlášeného uživatele.

V editoru Technická metadata bude nové pole Organizace s výběrem dostupných organizací. Hodnotu může měnit jen administrátor.


# Úložiště Hledat #

Při hledání digitálních objektů se zobrazí jen objekty organizace přihlášeného uživatele. Uživatelé bez organizace uvidí vše.

V tabulce výsledků přidat sloupec Organizace. Standardně skrytý.

Napojené objekty se zobrazí bez ohledu na organizaci. _Nemělo by vznikat._


# Globální menu #

Změna položky Uživatelé v globálním menu na

Menu
  * ...
  * Správa uživatelů
    * Uživatelé
    * Organizace
  * ...
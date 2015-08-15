

# Požadavky na server #

  * OS `*`NIX 64 bit, případně MS Windows
  * RAM doporučeno 8 GB
  * Disk min. OS + aplikace + RDBMS data + uživatelský prostor pro import digitalizovaných dat a exporty

# Instalace #

Instalační balík je k dispozici [zde](https://drive.google.com/folderview?id=0B_yPKwM70Ky1WHMtVHdBXzczcTQ&usp=sharing).

Před instalací aplikace je nutné instalovat následující komponenty:

  * [Java](http://www.java.com) Sun/Oracle JDK 1.6 nejnovější verzi
  * Databáze [PostgreSQL 8.4](#PostgreSQL.md) nebo novější
  * Úložiště [Fedora Commons 3.5](#Fedora_Commons.md)
  * Aplikační server [Apache Tomcat](#Tomcat.md) (volitelné)

a vytvořit systémového uživatele pod kterým poběží aplikační server.

Do připraveného aplikačního serveru se nainstaluje [ProArc](#ProArc.md).

## PostgreSQL ##

  1. Vytvořit uživatele **`fedoraAdmin`** s přístupem k databázi **`fedora3`**
  1. a uživatele **`proarcAdmin`** s přístupem k databázi **`proarc`**.

## Fedora Commons ##

Postup instalace viz. https://wiki.duraspace.org/display/FEDORA35/Installation+and+Configuration.

Parametry:

  * typ instalace custom
  * bez SSL
  * bez FESL
  * uživatel `fedoraAdmin`
  * databáze postgres (fedora3)
  * standardní resource index Mulgara

Po instalaci je nutné změnit nebo zrušit pravidlo `$FEDORA_HOME/data/fedora-xacml-policies/repository-policies/default/deny-unallowed-file-resolution.xml` tak, aby Fedora umožnila import FOXML souborů z uživatelského prostoru ProArcu. Deaktivaci lze provést buď editací XACML pravidel v souboru nebo jeho prostým odstraněním. Editace XACML pravidel je popsána v dokumentaci úložiště Fedora Commons.

## Tomcat ##

Tomcat verze 6 je součástí instalačního balíku Fedora 3.5. Použít lze ale i samostatnou instalaci. Z bezpečnostních důvodů by uživatelé neměli přistupovat přímo na aplikační server a komunikace by měla probíhat s využitím HTTPS. Záleží na konkrétní instalaci.

### Konfigurace Tomcat ###
  1. Do souboru `$CATALINA_HOME/conf/context.xml` přidat definici úložiště:
```
<Resource name="jdbc/proarc" auth="Container"
        type="javax.sql.DataSource" initialSize="3"
        maxActive="100" maxIdle="30" maxWait="10000"
        username="proarcAdmin" password="FIXME"
        driverClassName="org.postgresql.Driver"
        url="jdbc:postgresql://localhost/proarc"/>
```
  1. Do souboru `$CATALINA_HOME/conf/server.xml` přidat URIEncoding:
```
<Connector port="8080" protocol="HTTP/1.1" 
               connectionTimeout="20000" URIEncoding="UTF-8"
               redirectPort="8443" />
```
  1. ~~a nahradit `<Realm className="org.apache.catalina.realm.UserDatabaseRealm" .../>` následujícím elementem Realm:~~ Ne od verze 2.
```
<Realm className="org.apache.catalina.realm.JDBCRealm"
           driverName="org.postgresql.Driver"
           connectionURL="jdbc:postgresql://localhost/proarc"
           connectionName="proarcAdmin" connectionPassword="FIXME"
           userTable="tomcat_users" userNameCol="username" userCredCol="userpass" 
           userRoleTable="tomcat_roles" roleNameCol="rolename"
           digest="SHA" />
```
  1. Do adresáře `$CATALINA_HOME/lib` zkopírovat JDBC ovladač podle verze instalované databáze.
  1. Přidat systémové proměnné:
```
export JAVA_HOME=/usr/java/java-6
export CATALINA_HOME=$HOME/tomcat
export FEDORA_HOME=$HOME/fedora
export JAVA_OPTS=" -XX:MaxPermSize=128m -Djava.awt.headless=true -XX:+HeapDumpOnOutOfMemoryError"
export PATH=$PATH:$FEDORA_HOME/server/bin:$FEDORA_HOME/client/bin:$CATALINA_HOME/bin:$JAVA_HOME/bin
```

## ProArc ##

  1. Nastavit umístění konfigurace pomocí systémové proměnné. Pokud proměnná nebude definována, použije se:
```
export PROARC_HOME=$HOME/.proarc
```
  1. Zpřístupnit adresář `$PROARC_HOME/users`, do kterého se budou ukládat data uživatelů (exporty, dávkové importy). Adresář by měl být dostupný jak pro systémového uživatele, pod kterým poběží Tomcat, tak pro uživatele ProArcu (např. pomocí Samba, FTP, ...). Adresář je vytvořen při prvním spuštění.
  1. Do adresáře `$CATALINA_HOME/endorsed` zkopírovat obsah `proarc-version-release.zip/tomcat/endorsed/*`.
  1. `proarc.war` uložit do `$CATALINA_HOME/webapps`
  1. V `$PROARC_HOME/proarc.cfg` aktualizovat nastavení podle provedené instalace. Více [zde](Konfigurace.md).

Login administrátora po instalaci je proarc/proarcAdmin. Heslo lze změnit v aplikaci https://server/proarc v sekci Uživatelé nebo ve formuláři po kliknutí na jméno uživatele na začátku stránky.

# Upgrade z verze 1.`*` #

  1. Nový `proarc.war` uložit do `$CATALINA_HOME/webapps`. Pokud se původní war rozbalil do adresáře, tak smazat adresář.
  1. V `$PROARC_HOME/proarc.cfg` aktualizovat nastavení podle provedené instalace. Více [zde](Konfigurace.md).
  1. Po prvním úspěšném spuštění nové verze je možno odstranit `<Realm className="org.apache.catalina.realm.JDBCRealm" .../>` v souboru `$CATALINA_HOME/conf/server.xml`. Nadále se nevyužívá.
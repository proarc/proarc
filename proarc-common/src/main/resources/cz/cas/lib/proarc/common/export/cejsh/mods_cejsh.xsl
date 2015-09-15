<?xml version="1.0" encoding="utf-8"?>
<!--
Creates the bwmeta document from a MODS collection of articles.

Author Miroslav Pavelka
-->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:mods="http://www.loc.gov/mods/v3"
                xmlns="http://yadda.icm.edu.pl/bwmeta-1.0.5.xsd"
                xmlns:xlink="http://www.w3.org/2001/XMLSchema-instance"
                exclude-result-prefixes="mods xlink"
>
    <xsl:import href="iso-639-2b-to-639-1.xsl"/>
    <xsl:output method="xml" indent="yes" encoding="utf-8" omit-xml-declaration="yes"/>

    <!-- volume year - mods/originInfo/dateIssued[0] -->
    <xsl:param name="year"/>
    <!-- volume number - mods/titleInfo/partNumber[0]
         NA if missing -->
    <xsl:param name="volume"/>
    <!-- volume UUID - mods/identifier["uuid"] -->
    <xsl:param name="volumeId"/>
    <!-- issue number - mods/titleInfo/partNumber[0] -->
    <xsl:param name="issue"/>
    <!-- issue UUID - mods/identifier[type="uuid"] -->
    <xsl:param name="issueId"/>
    <!-- title issn - mods/identifier[type="issn"][0] -->
    <xsl:param name="issn"/>
    <!-- supplement UUID - mods/identifier[type="uuid"] -->
    <xsl:param name="supplementId"/>
    <!-- supplement title - mods/titleInfo/title -->
    <xsl:param name="supplementTitle" />
    <!-- supplement genre - mods/genre -->
    <xsl:param name="supplementType"/>

    <xsl:param name="remote_link">http://kramerius.lib.cas.cz/search/handle/uuid:</xsl:param>

    <xsl:param name="category_prefix">bwmeta1.category.cejsh-</xsl:param>
    <xsl:param name="element_prefix">bwmeta1.element.</xsl:param>

    <xsl:param name="parentId">
        <xsl:choose>
            <xsl:when test="$issueId">
                <xsl:value-of select="$issueId"/>
            </xsl:when>
            <xsl:when test="$supplementId">
                <xsl:value-of select="$supplementId"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$volumeId"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:param>

    <xsl:variable name="lookupDoc" select="document('cejsh_journals.xml')" />
    <xsl:variable name="journalId" select="$lookupDoc/cejsh/journal[@issn=$issn]/journalId"/>
    <xsl:variable name="discipline" select="$lookupDoc/cejsh/journal[@issn=$issn]/discipline"/>
    <xsl:variable name="publisherAddress" select="$lookupDoc/cejsh/journal[@issn=$issn]/publisherAddress"/>

    <xsl:param name="yearId">
        <xsl:value-of select="$journalId"/>-<xsl:value-of select="$year"/>
    </xsl:param>


    <xsl:template match="text()">
        <xsl:if test="normalize-space(.) != ''">
            <xsl:value-of select="."/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="mods:modsCollection">
        <xsl:if test="not($lookupDoc)">
            <xsl:message terminate="yes">ERROR: Missing cejsh_journals.xml</xsl:message>
        </xsl:if>
        <xsl:if test="$issn='' or $year='' ">
            <xsl:message terminate="yes">ERROR: Missing hierarchy parameters: issn or year</xsl:message>
        </xsl:if>
        <xsl:if test="($volumeId='' and $issueId='')">
            <xsl:message terminate="yes">ERROR: Missing hierarchy parameters: volume or issue</xsl:message>
        </xsl:if>
        <xsl:if test="($volumeId!='' and $volume='') or ($issueId!='' and $issue='')">
            <xsl:message terminate="yes">ERROR: Missing hierarchy parameters name: volume or issue</xsl:message>
        </xsl:if>
        <xsl:if test="not($journalId)">
            <xsl:message terminate="yes">ERROR: Missing journalId for ISSN: '<xsl:value-of select="$issn"/>'!</xsl:message>
        </xsl:if>
        <xsl:if test="($supplementType='volume_supplement' and not($volumeId)) or ($supplementType='issue_supplement' and not($issueId))">
            <xsl:message terminate="yes">ERROR: Missing hierarchy parameters!</xsl:message>
        </xsl:if>
        <xsl:if test="($supplementId and not($supplementTitle))">
            <xsl:message terminate="yes">ERROR: Missing supplement title!</xsl:message>
        </xsl:if>

        <bwmeta>
            <!-- year -->
            <xsl:element name="element">
                <xsl:attribute name="id">
                    <xsl:value-of select="$element_prefix"/>
                    <xsl:value-of select="$yearId"/>
                </xsl:attribute>
                <xsl:element name="name">
                    <xsl:value-of select="$year"/>
                </xsl:element>

                <xsl:element name="hierarchy">
                    <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                    <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Year</xsl:attribute>
                    <xsl:element name="element-ref">
                        <xsl:attribute name="ref">bwmeta1.element.cejsh-<xsl:value-of select="$journalId"/></xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </xsl:element>

            <!-- volume -->
            <xsl:if test="$volumeId">
                <xsl:element name="element">
                    <xsl:attribute name="id">
                        <xsl:value-of select="$element_prefix"/>
                        <xsl:value-of select="$volumeId"/>
                    </xsl:attribute>
                    <xsl:element name="name">
                        <xsl:value-of select="$volume"/>
                    </xsl:element>

                    <xsl:element name="hierarchy">
                        <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                        <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Volume</xsl:attribute>
                        <xsl:element name="element-ref">
                            <xsl:attribute name="ref">
                                <xsl:value-of select="$element_prefix"/>
                                <xsl:value-of select="$yearId"/>
                            </xsl:attribute>
                        </xsl:element>
                    </xsl:element>
                </xsl:element>
            </xsl:if>

            <!-- issue or supplement-->

            <xsl:choose>
                <xsl:when test="$issueId">
                    <xsl:element name="element">
                        <xsl:attribute name="id">
                            <xsl:value-of select="$element_prefix"/>
                            <xsl:value-of select="$issueId"/>
                        </xsl:attribute>
                        <xsl:element name="name">
                            <xsl:value-of select="$issue"/>
                        </xsl:element>
                        <xsl:element name="hierarchy">
                            <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                            <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Number</xsl:attribute>
                            <xsl:element name="element-ref">
                                <xsl:choose>
                                    <xsl:when test="$volumeId">
                                        <xsl:attribute name="ref">
                                            <xsl:value-of select="$element_prefix"/>
                                            <xsl:value-of select="$volumeId"/>
                                        </xsl:attribute>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:attribute name="ref">
                                            <xsl:value-of select="$element_prefix"/>
                                            <xsl:value-of select="$yearId"/>
                                        </xsl:attribute>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:element>
                        </xsl:element>
                    </xsl:element>
                </xsl:when>
                <xsl:when test="$supplementId and $supplementType='volume_supplement'">
                    <xsl:element name="element">
                        <xsl:attribute name="id">
                            <xsl:value-of select="$element_prefix"/>
                            <xsl:value-of select="$supplementId"/>
                        </xsl:attribute>
                        <xsl:element name="name">
                            <xsl:value-of select="$supplementTitle"/>
                        </xsl:element>
                        <xsl:element name="hierarchy">
                            <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                            <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Number</xsl:attribute>
                            <xsl:element name="element-ref">
                                <xsl:choose>
                                    <xsl:when test="$volumeId">
                                        <xsl:attribute name="ref">
                                            <xsl:value-of select="$element_prefix"/>
                                            <xsl:value-of select="$volumeId"/>
                                        </xsl:attribute>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:attribute name="ref">
                                            <xsl:value-of select="$element_prefix"/>
                                            <xsl:value-of select="$yearId"/>
                                        </xsl:attribute>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:element>
                        </xsl:element>
                        <xsl:element name="relations">
                            <xsl:attribute name="type">supplement</xsl:attribute>
                            <xsl:element name="element-ref">
                                <xsl:attribute name="ref">
                                    <xsl:value-of select="$element_prefix"/>
                                    <xsl:choose>
                                        <xsl:when test="$volumeId">
                                            <xsl:value-of select="$volumeId"/>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:value-of select="$yearId"/>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:attribute>
                            </xsl:element>
                        </xsl:element>
                    </xsl:element>
                </xsl:when>
            </xsl:choose>

            <xsl:apply-templates/>

        </bwmeta>

    </xsl:template>

    <xsl:template match="mods:mods">
        <xsl:if test="not($parentId)">
            <xsl:message terminate="yes">ERROR: Missing id of parent level</xsl:message>
        </xsl:if>

        <xsl:choose>
            <xsl:when test="mods:identifier[@type='uuid'] != '' ">
                    <xsl:element name="element">
                        <xsl:attribute name="id">
                            <xsl:value-of select="$element_prefix"/>
                            <xsl:value-of select="mods:identifier[@type='uuid']"/>
                        </xsl:attribute>
                        <xsl:if test="mods:language/mods:languageTerm">
                            <xsl:attribute name="langs">
                                <xsl:call-template name="iso-639-2b-converter">
                                    <xsl:with-param name="languageCode" select="mods:language/mods:languageTerm"/>
                                </xsl:call-template>
                            </xsl:attribute>
                        </xsl:if>

                        <xsl:for-each select="mods:titleInfo">
                            <xsl:element name="name">
                                <xsl:if test="./@lang">
                                    <xsl:attribute name="lang">
                                        <xsl:call-template name="iso-639-2b-converter">
                                            <xsl:with-param name="languageCode" select="./@lang"/>
                                        </xsl:call-template>
                                    </xsl:attribute>
                                </xsl:if>
                                <xsl:choose>
                                    <xsl:when test="./mods:nonSort!='' and ./mods:title!='' ">
                                        <!-- &#160; je mezera -->
                                        <xsl:value-of select="./mods:nonSort"/>
                                        <xsl:text>&#160;</xsl:text>
                                        <xsl:value-of select="./mods:title"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:if test="./mods:title!=''">
                                            <xsl:value-of select="./mods:title"/>
                                        </xsl:if>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:element>
                        </xsl:for-each>

                        <xsl:for-each select="mods:abstract">
                            <xsl:if test="./@lang='eng'">
                                <xsl:element name="description">
                                    <xsl:attribute name="lang">EN</xsl:attribute>
                                    <xsl:value-of select="normalize-space(.)"/>
                                </xsl:element>
                            </xsl:if>
                        </xsl:for-each>

                        <xsl:element name="attribute">
                            <xsl:attribute name="key">cejsh.paper-type</xsl:attribute>
                            <xsl:attribute name="value">ARTICLE</xsl:attribute>
                        </xsl:element>

                        <xsl:if test="$discipline">
                            <xsl:element name="categories">
                                <xsl:attribute name="ids">
                                    <xsl:value-of select="$category_prefix"/>
                                    <xsl:value-of select="$discipline"/>
                                </xsl:attribute>
                            </xsl:element>
                        </xsl:if>

                        <xsl:for-each select="mods:name">
                            <xsl:element name="contributor">
                                <xsl:attribute name="index">
                                    <xsl:value-of select="position()" />
                                </xsl:attribute>
                                <xsl:if test="./mods:role/mods:roleTerm[@type='code']='aut'">
                                    <xsl:attribute name="role">author</xsl:attribute>
                                </xsl:if>
                                <xsl:if test="./mods:role/mods:roleTerm[@type='code']='edt'">
                                    <xsl:attribute name="role">editor</xsl:attribute>
                                </xsl:if>

                                <xsl:if test="./@type='personal'">
                                    <xsl:if test="./mods:namePart[@type='family']!='' and ./mods:namePart[@type='given']!=''">
                                        <xsl:attribute name="title">
                                            <xsl:value-of select="./mods:namePart[@type='given']"/>
                                            <xsl:text>&#160;</xsl:text>
                                            <xsl:value-of select="./mods:namePart[@type='family']"/>
                                        </xsl:attribute>
                                    </xsl:if>
                                </xsl:if>

                                <xsl:if test="not(./mods:namePart[@type='family'] and ./mods:namePart[@type='given'])">
                                    <xsl:attribute name="title">
                                        <xsl:value-of select="./mods:namePart"/>
                                    </xsl:attribute>
                                </xsl:if>

                                <xsl:if test="./@type='corporate' and ./mods:namePart ">
                                    <xsl:attribute name="title">
                                        <xsl:value-of select="./mods:namePart"/>
                                    </xsl:attribute>
                                </xsl:if>

                                <xsl:if test="$publisherAddress">
                                    <xsl:element name="affiliation">
                                        <xsl:element name="attribute">
                                            <xsl:attribute name="key">text</xsl:attribute>
                                            <xsl:attribute name="value">
                                                <xsl:value-of select="$publisherAddress"/>
                                            </xsl:attribute>
                                        </xsl:element>
                                    </xsl:element>
                                </xsl:if>

                                <xsl:if test="./@type='personal'">
                                    <xsl:element name="attribute">
                                        <xsl:attribute name="key">person</xsl:attribute>
                                        <xsl:if test="./mods:namePart[@type='family'] != '' ">
                                            <xsl:element name="attribute">
                                                <xsl:attribute name="key">person.surname</xsl:attribute>
                                                <xsl:attribute name="value">
                                                    <xsl:value-of select="./mods:namePart[@type='family']"/>
                                                </xsl:attribute>
                                            </xsl:element>
                                        </xsl:if>
                                        <xsl:if test="./mods:namePart[@type='given'] != '' ">
                                            <xsl:element name="attribute">
                                                <xsl:attribute name="key">person.firstname</xsl:attribute>
                                                <xsl:attribute name="value">
                                                    <xsl:value-of select="./mods:namePart[@type='given']"/>
                                                </xsl:attribute>
                                            </xsl:element>
                                        </xsl:if>
                                    </xsl:element>
                                </xsl:if>

                                <xsl:if test="./@type='corporate' and ./mods:namePart">
                                    <xsl:element name="attribute">
                                        <xsl:attribute name="key">institution</xsl:attribute>
                                        <xsl:attribute name="value">_</xsl:attribute>
                                        <xsl:element name="attribute">
                                            <xsl:attribute name="key">institution.name</xsl:attribute>
                                            <xsl:attribute name="value">
                                                <xsl:value-of select="./mods:namePart"/>
                                            </xsl:attribute>
                                        </xsl:element>
                                    </xsl:element>
                                </xsl:if>

                            </xsl:element>
                        </xsl:for-each>

                        <xsl:element name="hierarchy">
                            <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                            <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Article</xsl:attribute>
                            <xsl:element name="element-ref">
                                <xsl:attribute name="ref">
                                    <xsl:value-of select="$element_prefix"/>
                                    <xsl:value-of select="$parentId"/>
                                </xsl:attribute>
                            </xsl:element>
                            <xsl:if test="mods:part/mods:extent/mods:start and mods:part/mods:extent/mods:end">
                                <xsl:element name="position">
                                    <xsl:attribute name="value">
                                        <xsl:value-of select="mods:part/mods:extent/mods:start"/>-<xsl:value-of select="mods:part/mods:extent/mods:end"/>
                                    </xsl:attribute>
                                </xsl:element>
                            </xsl:if>
                        </xsl:element>

                        <xsl:if test="$supplementId and $supplementType='issue_supplement'">
                            <xsl:element name="relations">
                                <xsl:attribute name="type">supplement</xsl:attribute>
                                <xsl:element name="element-ref">
                                    <xsl:attribute name="ref">
                                        <xsl:value-of select="$element_prefix"/>
                                        <xsl:value-of select="$supplementId"/>
                                    </xsl:attribute>
                                </xsl:element>
                            </xsl:element>
                        </xsl:if>

                        <xsl:element name="contents">
                            <xsl:attribute name="index">1</xsl:attribute>
                            <xsl:attribute name="type">full-text</xsl:attribute>
                            <xsl:element name="location">
                                <xsl:attribute name="name">Digitální knihovna AV ČR</xsl:attribute>
                                <xsl:element name="localisation">
                                    <xsl:attribute name="type">URL</xsl:attribute>
                                    <xsl:attribute name="remote">yes</xsl:attribute>
                                    <xsl:value-of select="$remote_link"/>
                                    <xsl:value-of select="mods:identifier[@type='uuid']"/>
                                </xsl:element>
                                <xsl:element name="format">
                                    <xsl:attribute name="type">application/pdf</xsl:attribute>
                                </xsl:element>
                            </xsl:element>
                        </xsl:element>

                        <xsl:if test="mods:subject/mods:topic/@lang='eng'">
                            <xsl:element name="keywords">
                                <xsl:attribute name="lang">EN</xsl:attribute>
                                <xsl:for-each select="mods:subject/mods:topic">
                                    <xsl:if test="./@lang='eng'">
                                        <xsl:element name="k">
                                            <xsl:value-of select="."/>
                                        </xsl:element>
                                    </xsl:if>
                                </xsl:for-each>
                            </xsl:element>
                        </xsl:if>

                    </xsl:element>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message terminate="yes">ERROR: Missing article UUID</xsl:message>
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>

</xsl:stylesheet>
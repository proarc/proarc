<?xml version="1.0" encoding="utf-8"?>
<!--
Creates the Crossref document from a MODS collection of articles.

Author Miroslav Pavelka
-->
<xsl:stylesheet version="1.0"
                xmlns="http://www.crossref.org/schema/4.3.6"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:mods="http://www.loc.gov/mods/v3"
                >
    <xsl:import href="../cejsh/iso-639-2b-to-639-1.xsl"/>
    <xsl:output method="xml" indent="yes" encoding="utf-8" />

    <xsl:key name="selected_contributors" match="mods:name"
             use="concat(../mods:identifier[@type='uuid'], boolean((mods:role/mods:roleTerm = 'aut') or (mods:role/mods:roleTerm = 'rev') or (mods:role/mods:roleTerm = 'edt') or (mods:role/mods:roleTerm = 'trl')))" />

    <!-- z úrovně volume -->
    <!-- volume number - mods/titleInfo/partNumber -->
    <xsl:param name="volume"/>

    <!-- z úrovně issue -->
    <!-- issue number - mods/titleInfo/partNumber -->
    <xsl:param name="issue"/>

    <!-- issue date - mods/originInfo/dateIssued -->
    <!-- kdyz není úroveň issue, tak z úrovně volume -->
    <xsl:param name="publication_date"/>

    <!-- z úrovně titulu -->
    <!-- title issn - mods/identifier[type="issn"][0] -->
    <xsl:param name="issn"/>
    <!-- název titulu - mods/titleInfo/title -->
    <xsl:param name="full_title"/>
    <!-- zkrácený název titulu - mods/titleInfo/title/@type="abbreviated" -->
    <xsl:param name="abbrev_title"/>
    <!-- formát titulu - mods/physicalDescription/form -->
    <xsl:param name="media_type"/>

    <!-- UUID exportované úrovně (issue nebo článku)-->
    <xsl:param name="export_uuid"/>
    <!-- čas exportu - formát RRRRMMDDhhmm -->
    <xsl:param name="export_time"/>

    <xsl:param name="kramerius_link">http://kramerius.lib.cas.cz/search/handle/uuid:</xsl:param>

    <xsl:variable name="lookupDoc" select="document('../cejsh/cejsh_journals.xml')" />
    <xsl:variable name="depositor_name" select="$lookupDoc/cejsh/journal[@issn=$issn]/depositorName"/>
    <xsl:variable name="email_address" select="$lookupDoc/cejsh/journal[@issn=$issn]/emailAddress"/>

    <xsl:template match="text()">
        <xsl:if test="normalize-space(.) != ''">
            <xsl:value-of select="."/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="mods:modsCollection">
        <xsl:if test="not($lookupDoc)">
            <xsl:message terminate="yes">ERROR: Missing cejsh_journals.xml</xsl:message>
        </xsl:if>
        <xsl:if test="not($full_title)">
            <xsl:message terminate="yes">ERROR: Missing parameter: full_title</xsl:message>
        </xsl:if>
        <xsl:if test="not($issn)">
            <xsl:message terminate="yes">ERROR: Missing parameter: issn</xsl:message>
        </xsl:if>
        <xsl:if test="not($publication_date)">
            <xsl:message terminate="yes">ERROR: Missing parameter: publication_date</xsl:message>
        </xsl:if>
        <xsl:if test="not($export_uuid)">
            <xsl:message terminate="yes">ERROR: Missing parameter: export_uuid</xsl:message>
        </xsl:if>
        <xsl:if test="not($export_time)">
            <xsl:message terminate="yes">ERROR: Missing parameter: export_time</xsl:message>
        </xsl:if>
        <xsl:if test="not($depositor_name) or not($email_address)">
            <xsl:message terminate="yes">ERROR: Missing cejsh_journals parameter: depositor_name or email_address, for ISSN: <xsl:value-of select="$issn"/></xsl:message>
        </xsl:if>

        <xsl:element name="doi_batch">
            <xsl:attribute name="xsi:schemaLocation">http://www.crossref.org/schema/4.3.6 http://www.crossref.org/schemas/crossref4.3.6.xsd</xsl:attribute>
            <xsl:attribute name="version">4.3.6</xsl:attribute>

            <xsl:element name="head">
                <xsl:element name="doi_batch_id">
                    <xsl:value-of select="$export_uuid"/>
                </xsl:element>
                <xsl:element name="timestamp">
                    <xsl:value-of select="$export_time"/>
                </xsl:element>
                <xsl:element name="depositor">
                    <xsl:element name="depositor_name">
                        <xsl:value-of select="$depositor_name"/>
                    </xsl:element>
                    <xsl:element name="email_address">
                        <xsl:value-of select="$email_address"/>
                    </xsl:element>
                </xsl:element>
                <xsl:element name="registrant">
                    <xsl:text>Knihovna AV ČR, v. v. i.</xsl:text>
                </xsl:element>
            </xsl:element>

            <xsl:element name="body">
                <xsl:element name="journal">
                    <xsl:element name="journal_metadata">
                        <xsl:element name="full_title">
                            <xsl:value-of select="$full_title"/>
                        </xsl:element>
                        <xsl:element name="abbrev_title">
                            <xsl:choose>
                                <xsl:when test="$abbrev_title">
                                    <xsl:value-of select="$abbrev_title"/>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="$full_title"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:element>
                        <xsl:element name="issn">
                            <xsl:if test="$media_type">
                                <xsl:attribute name="media_type">
                                    <xsl:value-of select="$media_type"/>
                                </xsl:attribute>
                            </xsl:if>
                            <xsl:value-of select="$issn"/>
                        </xsl:element>
                    </xsl:element>

                    <xsl:element name="journal_issue">
                        <xsl:element name="publication_date">
                            <xsl:if test="$media_type">
                                <xsl:attribute name="media_type">
                                    <xsl:value-of select="$media_type"/>
                                </xsl:attribute>
                            </xsl:if>

                            <xsl:call-template name="pubdate"/>

                        </xsl:element>
                        <xsl:if test="$volume">
                            <xsl:element name="journal_volume">
                                <xsl:element name="volume">
                                    <xsl:value-of select="$volume"/>
                                </xsl:element>
                            </xsl:element>
                        </xsl:if>
                        <xsl:if test="$issue">
                            <xsl:element name="issue">
                                <xsl:value-of select="$issue"/>
                            </xsl:element>
                        </xsl:if>
                    </xsl:element>

                    <xsl:apply-templates/>
                </xsl:element>
            </xsl:element>
        </xsl:element>
    </xsl:template>

    <xsl:template match="mods:titleInfo">
        <xsl:element name="titles">
            <xsl:element name="title">
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
            <xsl:if test="./mods:subTitle">
                <xsl:element name="subtitle">
                    <xsl:value-of select="./mods:subTitle"/>
                </xsl:element>
            </xsl:if>
        </xsl:element>
    </xsl:template>

    <xsl:template match="mods:mods">
        <xsl:choose>
            <xsl:when test="./mods:identifier[@type='doi']">
                <xsl:variable name="uuid" select="mods:identifier[@type='uuid']"/>
                <xsl:element name="journal_article">
                    <xsl:attribute name="publication_type">
                        <xsl:text>full_text</xsl:text>
                    </xsl:attribute>
                    <xsl:if test="./mods:language/mods:languageTerm">
                        <xsl:variable name="langTerm">
                            <xsl:call-template name="iso-639-2b-converter">
                                <xsl:with-param name="languageCode" select="./mods:language/mods:languageTerm"/>
                            </xsl:call-template>
                        </xsl:variable>
                        <xsl:if test="$langTerm!=''">
                            <xsl:attribute name="language">
                                <xsl:value-of select="$langTerm"/>
                            </xsl:attribute>
                        </xsl:if>
                    </xsl:if>

                    <xsl:apply-templates select="mods:titleInfo[not(@type)]" />

                    <xsl:if test="boolean(./mods:name/mods:namePart!='')">
                        <xsl:if test="boolean((./mods:name/mods:role/mods:roleTerm = 'aut') or (./mods:name/mods:role/mods:roleTerm = 'rev') or (./mods:name/mods:role/mods:roleTerm = 'edt') or (./mods:name/mods:role/mods:roleTerm = 'trl'))">
                            <xsl:element name="contributors">
                                <xsl:for-each select="key('selected_contributors', concat($uuid, 'true'))">
                                    <xsl:if test="./@type='personal' and ./mods:namePart!='' ">
                                        <xsl:element name="person_name">
                                            <xsl:choose>
                                                <xsl:when test="position() = 1">
                                                    <xsl:attribute name="sequence">
                                                        <xsl:text>first</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:attribute name="sequence">
                                                        <xsl:text>additional</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                            <xsl:choose>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='aut'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>author</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='rev'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>author</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='edt'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>editor</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='trl'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>translator</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                            </xsl:choose>
                                            <xsl:choose>
                                                <xsl:when test="./mods:namePart[@type='family']">
                                                    <xsl:if test="./mods:namePart[@type='given']">
                                                        <xsl:element name="given_name">
                                                            <xsl:value-of select="./mods:namePart[@type='given']"/>
                                                        </xsl:element>
                                                    </xsl:if>
                                                    <xsl:element name="surname">
                                                        <xsl:value-of select="./mods:namePart[@type='family']"/>
                                                    </xsl:element>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:if test="./mods:namePart[not(@type)]">
                                                        <xsl:element name="surname">
                                                            <xsl:value-of select="./mods:namePart"/>
                                                        </xsl:element>
                                                    </xsl:if>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                        </xsl:element>
                                    </xsl:if>
                                    <xsl:if test="./@type='corporate' and ./mods:namePart!=''">
                                        <xsl:element name="organization">
                                            <xsl:choose>
                                                <xsl:when test="position() = 1">
                                                    <xsl:attribute name="sequence">
                                                        <xsl:text>first</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:attribute name="sequence">
                                                        <xsl:text>additional</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                            <xsl:choose>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='aut'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>author</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='rev'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>author</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='edt'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>editor</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                                <xsl:when test="./mods:role/mods:roleTerm[@type='code']='trl'">
                                                    <xsl:attribute name="contributor_role">
                                                        <xsl:text>translator</xsl:text>
                                                    </xsl:attribute>
                                                </xsl:when>
                                            </xsl:choose>
                                            <xsl:value-of select="./mods:namePart"/>
                                        </xsl:element>
                                    </xsl:if>
                                </xsl:for-each>
                            </xsl:element>
                        </xsl:if>
                    </xsl:if>

                    <xsl:element name="publication_date">
                        <xsl:if test="$media_type">
                            <xsl:attribute name="media_type">
                                <xsl:value-of select="$media_type"/>
                            </xsl:attribute>
                        </xsl:if>
                        <xsl:call-template name="pubdate"/>
                    </xsl:element>

                    <xsl:variable name="start_page" select="./mods:relatedItem/mods:part/mods:extent/mods:start" />
                    <xsl:variable name="end_page" select="./mods:relatedItem/mods:part/mods:extent/mods:end" />

                    <xsl:if test="$start_page or $end_page">
                        <xsl:element name="pages">
                            <xsl:if test="$start_page">
                                <xsl:element name="first_page">
                                    <xsl:value-of select="$start_page"/>
                                </xsl:element>
                            </xsl:if>
                            <xsl:if test="$end_page">
                                <xsl:element name="last_page">
                                    <xsl:value-of select="$end_page"/>
                                </xsl:element>
                            </xsl:if>
                        </xsl:element>
                    </xsl:if>

                    <xsl:variable name="start_page_part" select="./mods:part/mods:extent/mods:start" />
                    <xsl:variable name="end_page_part" select="./mods:part/mods:extent/mods:end" />

                    <xsl:if test="$start_page_part or $end_page_part">
                        <xsl:element name="pages">
                            <xsl:if test="$start_page_part">
                                <xsl:element name="first_page">
                                    <xsl:value-of select="$start_page_part"/>
                                </xsl:element>
                            </xsl:if>
                            <xsl:if test="$end_page_part">
                                <xsl:element name="last_page">
                                    <xsl:value-of select="$end_page_part"/>
                                </xsl:element>
                            </xsl:if>
                        </xsl:element>
                    </xsl:if>

                    <xsl:element name="doi_data">
                        <xsl:if test="./mods:identifier[@type='doi']">
                            <xsl:element name="doi">
                                <xsl:value-of select="./mods:identifier[@type='doi']"/>
                            </xsl:element>
                        </xsl:if>
                        <xsl:element name="resource">
                            <xsl:value-of select="$kramerius_link"/>
                            <xsl:value-of select="./mods:identifier[@type='uuid']"/>
                        </xsl:element>
                    </xsl:element>

                </xsl:element>

            </xsl:when>
        </xsl:choose>

    </xsl:template>


    <!-- rozebere datum podle "." -->
    <xsl:template name="splitdate">
        <xsl:param name="workdate"/>
        <xsl:choose>
            <xsl:when test="contains($workdate,'.')">
                <xsl:variable name="before" select="substring-before($workdate,'.')" />
                <xsl:variable name="after" select="substring-after($workdate,'.')" />
                <xsl:choose>
                    <xsl:when test="contains($after,'.')">
                        <xsl:element name="month">
                            <xsl:value-of select="substring-before($after,'.')"/>
                        </xsl:element>
                        <xsl:element name="day">
                            <xsl:value-of select="$before"/>
                        </xsl:element>
                        <xsl:element name="year">
                            <xsl:value-of select="substring-after($after,'.')"/>
                        </xsl:element>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:element name="month">
                            <xsl:value-of select="$before"/>
                        </xsl:element>
                        <xsl:element name="year">
                            <xsl:value-of select="$after"/>
                        </xsl:element>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:element name="year">
                    <xsl:value-of select="$workdate"/>
                </xsl:element>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!-- crossref ocekava v datu integer, ne rozsah -->
    <xsl:template name="pubdate">
        <xsl:choose>
            <xsl:when test="contains($publication_date , '-')">
                <xsl:call-template name="splitdate">
                    <xsl:with-param name="workdate">
                        <xsl:value-of select="substring-after($publication_date,'-')"/>
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="splitdate">
                    <xsl:with-param name="workdate">
                        <xsl:value-of select="$publication_date"/>
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
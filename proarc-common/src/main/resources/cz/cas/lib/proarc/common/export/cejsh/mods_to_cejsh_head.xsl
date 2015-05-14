<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" 
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:mods="http://www.loc.gov/mods/v3"
                xmlns="http://yadda.icm.edu.pl/bwmeta-1.0.5.xsd"
                xmlns:xlink="http://www.w3.org/2001/XMLSchema-instance">
    <xsl:output method="xml" indent="yes" encoding="utf-8" omit-xml-declaration="yes"/>

    <!-- volume year - mods/originInfo/dateIssued[0] -->
    <xsl:param name="year"/>
    <!-- volume number - mods/titleInfo/partNumber[0]
         NA if missing -->
    <xsl:param name="volume"/>
    <!-- vlume UUID - mods/identifier["uuid"] -->
    <xsl:param name="volumeId"/>
    <!-- issue number - mods/titleInfo/partNumber[0] -->
    <xsl:param name="issue"/>
    <!-- issue UUID - mods/identifier[type="uuid"] -->
    <xsl:param name="issueId"/>
    <!-- title issn - mods/identifier[type="issn"][0] -->
    <xsl:param name="issn"/>

    <xsl:variable name="lookupDoc" select="document('cejsh_journals.xml')" />

    <xsl:variable name="journalId" select="$lookupDoc/cejsh/journal[@issn=$issn]/journalId"/>

    <xsl:template match="text()">
        <xsl:if test="normalize-space(.) != ''">
            <xsl:value-of select="."/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="/">

        <xsl:if test="not($lookupDoc)">
            <xsl:message terminate="yes">ERROR: Missing cejsh_journals.xml</xsl:message>
        </xsl:if>
        <xsl:if test="$issn='' or $year='' or $volume='' or $volumeId='' or $issue='' or $issueId=''">
            <xsl:message terminate="yes">ERROR: Missing hierarchy parameters</xsl:message>
        </xsl:if>
        <xsl:if test="not($journalId)">
            <xsl:message terminate="yes">ERROR: Missing journalId for ISSN: '<xsl:value-of select="$issn"/>'!</xsl:message>
        </xsl:if>

        <bwmeta>
            <!-- year -->
            <xsl:element name="element">
                <xsl:attribute name="id">bwmeta1.element.<xsl:value-of select="$volumeId"/>-<xsl:value-of select="$year"/></xsl:attribute>
                <xsl:element name="name">
                    <xsl:value-of select="$year"/>
                </xsl:element>

                <xsl:element name="hierarchy">
                    <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                    <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Year</xsl:attribute>
                    <xsl:element name="element-ref">
                        <xsl:attribute name="ref">bwmeta1.element.<xsl:value-of select="$journalId"/></xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </xsl:element>

            <!-- volume -->
            <xsl:element name="element">
                <xsl:attribute name="id">bwmeta1.element.<xsl:value-of select="$volumeId"/></xsl:attribute>
                <xsl:element name="name">
                    <xsl:value-of select="$volume"/>
                </xsl:element>

                <xsl:element name="hierarchy">
                    <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                    <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Volume</xsl:attribute>
                    <xsl:element name="element-ref">
                        <xsl:attribute name="ref">bwmeta1.element.<xsl:value-of select="$volumeId"/>-<xsl:value-of select="$year"/></xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </xsl:element>

            <!-- issue -->
            <xsl:if test="$issue">
                <xsl:element name="element">
                    <xsl:attribute name="id">bwmeta1.element.<xsl:value-of select="$issueId"/></xsl:attribute>
                    <xsl:element name="name">
                        <xsl:value-of select="$issue"/>
                    </xsl:element>

                    <xsl:element name="hierarchy">
                        <xsl:attribute name="class">bwmeta1.hierarchy-class.hierarchy_Journal</xsl:attribute>
                        <xsl:attribute name="level">bwmeta1.level.hierarchy_Journal_Number</xsl:attribute>
                        <xsl:element name="element-ref">
                            <xsl:attribute name="ref">bwmeta1.element.<xsl:value-of select="$volumeId"/></xsl:attribute>
                        </xsl:element>
                    </xsl:element>
                </xsl:element>
            </xsl:if>
        </bwmeta>

    </xsl:template>

</xsl:stylesheet>

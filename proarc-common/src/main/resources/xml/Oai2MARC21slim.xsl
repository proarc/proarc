<?xml version="1.0" encoding="UTF-8"?>
<!--
Extracts MARC metadata from OAI GetRecord response.
It stops with error message when an OAI error or an unexpected metadata format occures.
-->
<xsl:stylesheet
    xmlns:marc="http://www.loc.gov/MARC21/slim"
    xmlns:oai="http://www.openarchives.org/OAI/2.0/"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="oai" version="1.0">

    <xsl:output encoding="UTF-8" indent="yes" method="xml"/>
    <xsl:strip-space elements="*"/>

    <!-- copy metadata subtree -->
    <xsl:template match="oai:metadata/marc:record">
        <xsl:copy-of select="." />
    </xsl:template>

    <!-- stop on invalid metadata; it expects http://www.loc.gov/MARC21/slim -->
    <xsl:template match="oai:metadata/*[not(marc:*)]">
        <xsl:message terminate="yes">
            Invalid metadata format: <xsl:value-of select ="namespace-uri(.)"/>, element <xsl:value-of select ="name(.)"/>
        </xsl:message>
    </xsl:template>

    <!--stop on OAI result error-->
    <xsl:template match="oai:error">
        <xsl:message terminate="yes">
            <xsl:value-of select="@code"/>: <xsl:value-of select="."/>
        </xsl:message>
    </xsl:template>

    <!-- ignore OAI contents -->
    <xsl:template match="text() | @*">
    </xsl:template>

</xsl:stylesheet>

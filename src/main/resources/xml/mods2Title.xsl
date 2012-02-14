<?xml version="1.0" encoding="UTF-8"?>

<!--
    Extracts tilte as plain text
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:mods="http://www.loc.gov/mods/v3"
                >
    <xsl:output method="text"/>

    <xsl:template match="/">
        <xsl:value-of select="//mods:mods/mods:titleInfo/mods:title"/>
    </xsl:template>

</xsl:stylesheet>

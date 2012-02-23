<?xml version="1.0" encoding="UTF-8"?>

<!--
    Fixes Aleph XServer response that does not declare oai_marc namespace
    to simplify processing.
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                >
    <xsl:output encoding="UTF-8" indent="no" method="xml"/>

    <xsl:template match="oai_marc">
        <oai_marc xmlns="http://www.openarchives.org/OAI/1.1/oai_marc">
            <xsl:apply-templates select="*" />
        </oai_marc>
    </xsl:template>

    <xsl:template match="* | @*">
        <xsl:copy>
            <xsl:apply-templates select="* | @* | text()"/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>

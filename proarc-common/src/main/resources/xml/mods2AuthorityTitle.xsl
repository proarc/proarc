<?xml version="1.0" encoding="UTF-8"?>

<!--
    Extracts title as plain text
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:mods="http://www.loc.gov/mods/v3">
    <xsl:output method="text"/>

    <xsl:variable name="family" select="//mods:mods/mods:name[@type='personal']/mods:namePart[@type='family']"/>
    <xsl:variable name="given" select="//mods:mods/mods:name[@type='personal']/mods:namePart[@type='given']"/>
    <xsl:variable name="date" select="//mods:mods/mods:name[@type='personal']/mods:namePart[@type='date']"/>
    <xsl:variable name="identifier" select="//mods:mods/mods:name[@type='personal']/mods:nameIdentifier"/>
    <xsl:variable name="description" select="//mods:mods/mods:name/mods:description"/>
    <xsl:variable name="namePart" select="//mods:mods/mods:name/mods:namePart"/>
    <xsl:variable name="topicOfSubject" select="//mods:mods/mods:subject/mods:topic"/>
    <xsl:variable name="topicIdentifier" select="//mods:mods/mods:subject/mods:topic/@valueURI"/>
    <xsl:variable name="geographicCode" select="//mods:mods/mods:subject/mods:geographicCode"/>
    <xsl:variable name="geographic" select="//mods:mods/mods:subject/mods:geographic"/>
    <xsl:variable name="geographicIdentifier" select="//mods:mods/mods:subject/mods:geographic/@valueURI"/>

    <xsl:template match="/">
        <xsl:choose>
            <xsl:when test="boolean(normalize-space($geographic))">
                <xsl:choose>
                    <xsl:when test="boolean(normalize-space($geographicIdentifier))">
                        <xsl:value-of select="concat('S:', $geographic, ' (', $geographicIdentifier,')')"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="concat('S:', $geographic)"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="boolean(normalize-space($geographicCode))">
                <xsl:value-of select="concat('S:Geographic code: ', $geographicCode)"/>
            </xsl:when>
            <xsl:when test="boolean(normalize-space($topicOfSubject))">
                <xsl:choose>
                    <xsl:when test="boolean(normalize-space($topicIdentifier))">
                        <xsl:value-of select="concat('S:', $topicOfSubject, ' (', $topicIdentifier,')')"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="concat('S:', $topicOfSubject)"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="boolean(normalize-space($family))">
                    <xsl:choose>
                        <xsl:when test="boolean(normalize-space($date))">
                            <xsl:choose>
                                <xsl:when test="boolean(normalize-space($description))">
                                    <xsl:value-of select="concat('N:', $family, ' ', $given, ' (',$date, '; ', $identifier,' - ', $description, ')')"/>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="concat('N:', $family, ' ', $given, ' (',$date, '; ',$identifier ,')')"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:choose>
                                <xsl:when test="boolean(normalize-space($description))">
                                    <xsl:value-of select="concat('N:', $family, ' ', $given, ' (',$identifier ,' - ', $description, ')')"/>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="concat('N:', $family, ' ', $given, ' (',$identifier ,')')"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:otherwise>
                    </xsl:choose>
            </xsl:when>
            <xsl:when test="boolean(normalize-space($namePart))">
                <xsl:value-of select="concat('N:', $namePart)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="concat('--', '--- Nedefinováno ---')"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>

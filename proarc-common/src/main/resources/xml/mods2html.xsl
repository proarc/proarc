<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:mods="http://www.loc.gov/mods/v3"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="mods" >
    <xsl:output method="html" indent="yes" encoding="UTF-8"/>
    <!-- MODS2 records to html
    ntra added 4th child level 4/2/04
    -->

    <!-- localized dictionary URI -->
    <xsl:param name="MODS_DICTIONARY" select="'modsDictionary.xml'"/>

    <xsl:variable name="dictionary" select="document($MODS_DICTIONARY)/dictionary"/>

    <xsl:template match="/">

<!--        <html>
            <head>
                <style type="text/css">TD {vertical-align:top}</style>
            </head>
            <body>-->
                <xsl:choose>
                    <xsl:when test="mods:modsCollection">
                        <xsl:apply-templates select="mods:modsCollection"/>
                    </xsl:when>
                    <xsl:when test="mods:mods">
                        <xsl:apply-templates select="mods:mods"/>
                    </xsl:when>
                </xsl:choose>
<!--            </body>
        </html>-->
    </xsl:template>

    <xsl:template match="mods:modsCollection">
        <xsl:apply-templates select="mods:mods"/>
    </xsl:template>

    <xsl:template match="mods:mods">
        <table>
            <xsl:apply-templates select="mods:titleInfo"/>
            <xsl:apply-templates select="mods:name"/>
            <xsl:apply-templates select="mods:originInfo"/>
            <xsl:apply-templates select="mods:location"/>
            <xsl:apply-templates select="mods:identifier"/>
            <xsl:apply-templates select="mods:language"/>
            <xsl:apply-templates select="mods:physicalDescription"/>
            <xsl:apply-templates select="mods:abstract"/>
            <xsl:apply-templates select="mods:note"/>
            <xsl:apply-templates select="mods:typeOfResource"/>
            <xsl:apply-templates select="mods:genre"/>
            <xsl:apply-templates select="mods:classification"/>
            <xsl:apply-templates select="mods:subject"/>
            <xsl:apply-templates select="mods:part"/>
            <xsl:apply-templates select="mods:tableOfContents"/>
            <xsl:apply-templates select="mods:accessCondition"/>
            <xsl:apply-templates select="mods:extension"/>
            <xsl:apply-templates select="mods:targetAudience"/>
            <xsl:apply-templates select="mods:recordInfo"/>
            <xsl:apply-templates select="mods:relatedItem"/>
            <xsl:apply-templates select="*[not(self::mods:titleInfo
                    or self::mods:name
                    or self::mods:originInfo
                    or self::mods:location
                    or self::mods:identifier
                    or self::mods:language
                    or self::mods:physicalDescription
                    or self::mods:abstract
                    or self::mods:note
                    or self::mods:typeOfResource
                    or self::mods:genre
                    or self::mods:classification
                    or self::mods:subject
                    or self::mods:part
                    or self::mods:tableOfContents
                    or self::mods:accessCondition
                    or self::mods:extension
                    or self::mods:targetAudience
                    or self::mods:recordInfo
                    or self::mods:relatedItem)]"/>
        </table>
        <hr/>
    </xsl:template>

    <xsl:template match="*">

        <xsl:choose>

            <xsl:when test="child::*">
                <tr>
                    <td colspan="2">
                        <b>
                            <xsl:call-template name="longName">
                                <xsl:with-param name="name">
                                    <xsl:value-of select="local-name()"/>
                                </xsl:with-param>
                            </xsl:call-template>

                            <xsl:call-template name="attr"/>
                        </b>
                    </td>
                </tr>
                <xsl:apply-templates mode="nested">
                    <xsl:with-param name="depth" select="1"/>
                </xsl:apply-templates>
            </xsl:when>

            <xsl:otherwise>
                <tr>
                    <td width="300pt">
                        <b>
                            <xsl:call-template name="longName">
                                <xsl:with-param name="name">
                                    <xsl:value-of select="local-name()"/>
                                </xsl:with-param>
                            </xsl:call-template>

                            <xsl:call-template name="attr"/>
                        </b>
                    </td>
                    <td>
                        <xsl:call-template name="formatValue"/>
                    </td>
                </tr>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="formatValue">

        <xsl:choose>

            <xsl:when test="@type='uri'">
                <a href="{text()}">
                    <xsl:value-of select="text()"/>
                </a>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="text()"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="*" mode="nested">
        <xsl:param name="depth" select="1"/>

        <xsl:choose>

            <xsl:when test="child::*">
                <tr>
                    <td colspan="2">
                        <div>
                            <xsl:call-template name="indent">
                                <xsl:with-param name="depth" select="$depth"/>
                            </xsl:call-template>
                            <xsl:call-template name="longName">
                                <xsl:with-param name="name">
                                    <xsl:value-of select="local-name()"/>
                                </xsl:with-param>
                            </xsl:call-template>

                            <xsl:call-template name="attr"/>
                        </div>
                    </td>
                </tr>
                <xsl:apply-templates mode="nested">
                    <xsl:with-param name="depth" select="$depth + 1"/>
                </xsl:apply-templates>
            </xsl:when>

            <xsl:otherwise>
                <tr>
                    <td>
                        <div>
                            <xsl:call-template name="indent">
                                <xsl:with-param name="depth" select="$depth"/>
                            </xsl:call-template>
                            <xsl:call-template name="longName">
                                <xsl:with-param name="name">
                                    <xsl:value-of select="local-name()"/>
                                </xsl:with-param>
                            </xsl:call-template>

                            <xsl:call-template name="attr"/>
                        </div>
                    </td>
                    <td>
                        <xsl:call-template name="formatValue"/>
                    </td>
                </tr>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="indent">
        <xsl:param name="depth"/>
        <xsl:if test="$depth &gt; 0">
            <xsl:text>&#160;</xsl:text>
            <xsl:call-template name="indent">
                <xsl:with-param name="depth" select="$depth - 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>


    <xsl:template name="longName">
        <xsl:param name="name"/>

        <xsl:choose>

            <xsl:when test="$dictionary/entry[@key=$name]">
                <xsl:value-of select="$dictionary/entry[@key=$name]"/>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$name"/>
            </xsl:otherwise>

        </xsl:choose>

    </xsl:template>

    <xsl:template name="attr">

        <xsl:for-each select="@type|@point">
            <xsl:text>: </xsl:text>
            <xsl:call-template name="longName">
                <xsl:with-param name="name">
                    <xsl:value-of select="."/>
                </xsl:with-param>
            </xsl:call-template>
        </xsl:for-each>

        <xsl:if test="@authority or @edition">

            <xsl:for-each select="@authority">(
                <xsl:call-template name="longName">
                    <xsl:with-param name="name">
                        <xsl:value-of select="."/>
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:for-each>
            <xsl:if test="@edition">Edition
                <xsl:value-of select="@edition"/>
            </xsl:if>)
        </xsl:if>

        <xsl:variable name="attrStr">

            <xsl:for-each select="@*[local-name()!='edition' and local-name()!='type' and local-name()!='authority' and local-name()!='point' and local-name()!='usage' and local-name()!='eventType' and local-name()!='encoding' and local-name()!='objectPart' and local-name()!='source']">

                <xsl:value-of select="local-name()"/>="
                <xsl:value-of select="."/>",
            </xsl:for-each>
        </xsl:variable>

        <xsl:variable name="nattrStr" select="normalize-space($attrStr)"/>

        <xsl:if test="string-length($nattrStr)">(
            <xsl:value-of select="substring($nattrStr,1,string-length($nattrStr)-1)"/>)
        </xsl:if>

        <xsl:for-each select="@usage|@eventType|@encoding|@objectPart|@source">
            <xsl:text> (</xsl:text>
            <xsl:call-template name="longName">
                <xsl:with-param name="name">
                    <xsl:value-of select="."/>
                </xsl:with-param>
            </xsl:call-template>
            <xsl:text>)</xsl:text>
        </xsl:for-each>
    </xsl:template>

</xsl:stylesheet><!-- Stylus Studio meta-information - (c)1998-2003 Copyright Sonic Software Corporation. All rights reserved.
<metaInformation>
<scenarios ><scenario default="no" name="Scenario2" userelativepaths="yes" externalpreview="no" url="http://www.loc.gov/standards/mods/instances/mods99042030.xml" htmlbaseurl="" outputurl="" processortype="internal" commandline="" additionalpath="" additionalclasspath="" postprocessortype="none" postprocesscommandline="" postprocessadditionalpath="" postprocessgeneratedext=""/><scenario default="yes" name="MODS to HTML" userelativepaths="yes" externalpreview="no" url="..\..\..\temp\1toc.iflasubj.xml" htmlbaseurl="" outputurl="..\test_files\modshtml.html" processortype="internal" commandline="" additionalpath="" additionalclasspath="" postprocessortype="none" postprocesscommandline="" postprocessadditionalpath="" postprocessgeneratedext=""/></scenarios><MapperInfo srcSchemaPath="" srcSchemaRoot="" srcSchemaPathIsRelative="yes" srcSchemaInterpretAsXML="no" destSchemaPath="" destSchemaRoot="" destSchemaPathIsRelative="yes" destSchemaInterpretAsXML="no"/>
</metaInformation>
-->

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:mods="http://www.loc.gov/mods/v3" exclude-result-prefixes="mods"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:oai_dc="http://www.openarchives.org/OAI/2.0/oai_dc/"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--
This stylesheet transforms MODS version 3.2 records and collections of records to simple Dublin Core (DC) records,
based on the Library of Congress' MODS to simple DC mapping <http://www.loc.gov/standards/mods/mods-dcsimple.html>

The stylesheet will transform a collection of MODS 3.2 records into simple Dublin Core (DC)
as expressed by the SRU DC schema <http://www.loc.gov/standards/sru/dc-schema.xsd>

The stylesheet will transform a single MODS 3.2 record into simple Dublin Core (DC)
as expressed by the OAI DC schema <http://www.openarchives.org/OAI/2.0/oai_dc.xsd>

Because MODS is more granular than DC, transforming a given MODS element or subelement to a DC element frequently results in less precise tagging,
and local customizations of the stylesheet may be necessary to achieve desired results.

This stylesheet makes the following decisions in its interpretation of the MODS to simple DC mapping:

When the roleTerm value associated with a name is creator, then name maps to dc:creator
When there is no roleTerm value associated with name, or the roleTerm value associated with name is a value other than creator, then name maps to dc:contributor
Start and end dates are presented as span dates in dc:date and in dc:coverage
When the first subelement in a subject wrapper is topic, subject subelements are strung together in dc:subject with hyphens separating them
Some subject subelements, i.e., geographic, temporal, hierarchicalGeographic, and cartographics, are also parsed into dc:coverage
The subject subelement geographicCode is dropped in the transform


Revision 1.1    2007-05-18 <tmee@loc.gov>
                Added modsCollection conversion to DC SRU
                Updated introductory documentation

Version 1.0    2007-05-04 Tracy Meehleib <tmee@loc.gov>

-->

    <xsl:output encoding="UTF-8" indent="yes" method="xml"/>

    <xsl:param name="MODEL" />

    <xsl:template match="/">
        <oai_dc:dc>
            <xsl:apply-templates select="//mods:mods[1]/*"/>
            <dc:type><xsl:value-of select="$MODEL"/></dc:type>
        </oai_dc:dc>
    </xsl:template>

    <xsl:template match="mods:titleInfo">
        <xsl:variable name="title">
            <xsl:value-of select="mods:nonSort"/>
            <xsl:if test="string(mods:nonSort)">
                <xsl:text> </xsl:text>
            </xsl:if>
            <xsl:value-of select="mods:title"/>
            <xsl:if test="string(mods:subTitle)">
                <xsl:text>: </xsl:text>
                <xsl:value-of select="mods:subTitle"/>
            </xsl:if>
            <xsl:if test="string(mods:partNumber)">
                <xsl:text>. </xsl:text>
                <xsl:value-of select="mods:partNumber"/>
            </xsl:if>
            <xsl:if test="string(mods:partName)">
                <xsl:text>. </xsl:text>
                <xsl:value-of select="mods:partName"/>
            </xsl:if>
        </xsl:variable>

        <xsl:if test="string($title)">
            <dc:title>
                <xsl:value-of select="$title"/>
            </dc:title>
        </xsl:if>
    </xsl:template>

    <!-- title for: page, periodicalvolume, periodicalitem, monographunit -->
    <xsl:template match="mods:part">
        <xsl:apply-templates select="mods:detail/mods:number"/>
    </xsl:template>

    <!-- title for: page, periodicalvolume, periodicalitem -->
    <xsl:template match="mods:detail[@type='pageNumber' or @type='volume' or @type='issue']/mods:number[1]">
        <dc:title>
            <xsl:value-of select="."/>
        </dc:title>
    </xsl:template>

    <!-- title for: monographunit -->
    <xsl:template match="mods:part[@type='Volume']/mods:detail/mods:number[1]">
        <dc:title>
            <xsl:value-of select="."/>
        </dc:title>
    </xsl:template>

    <xsl:template match="mods:name">
        <xsl:choose>
            <xsl:when
                test="mods:role/mods:roleTerm[@type='text']='creator' or mods:role/mods:roleTerm[@type='code']='cre' ">
                <xsl:variable name="creator">
                    <xsl:call-template name="name"/>
                </xsl:variable>
                <xsl:if test="string($creator)">
                    <dc:creator>
                        <xsl:value-of select="$creator"/>
                    </dc:creator>
                </xsl:if>
            </xsl:when>

            <xsl:otherwise>
                <xsl:variable name="contributor">
                    <xsl:call-template name="name"/>
                </xsl:variable>
                <xsl:if test="string($contributor)">
                    <dc:contributor>
                        <xsl:value-of select="$contributor"/>
                    </dc:contributor>
                </xsl:if>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="mods:classification">
        <dc:subject>
            <xsl:value-of select="."/>
        </dc:subject>
    </xsl:template>

    <xsl:template match="mods:subject[mods:topic | mods:name | mods:occupation | mods:geographic | mods:hierarchicalGeographic | mods:cartographics | mods:temporal] ">
        <dc:subject>
            <xsl:for-each select="mods:topic">
                <xsl:value-of select="."/>
                <xsl:if test="position()!=last()">--</xsl:if>
            </xsl:for-each>

            <xsl:for-each select="mods:occupation">
                <xsl:value-of select="."/>
                <xsl:if test="position()!=last()">--</xsl:if>
            </xsl:for-each>

            <xsl:for-each select="mods:name">
                <xsl:call-template name="name"/>
            </xsl:for-each>
        </dc:subject>

        <xsl:for-each select="mods:titleInfo/mods:title">
            <dc:subject>
                <xsl:value-of select="mods:titleInfo/mods:title"/>
            </dc:subject>
        </xsl:for-each>

        <xsl:for-each select="mods:geographic">
            <dc:coverage>
                <xsl:value-of select="."/>
            </dc:coverage>
        </xsl:for-each>

        <xsl:for-each select="mods:hierarchicalGeographic">
            <dc:coverage>
                <xsl:for-each
                    select="mods:continent|mods:country|mods:provence|mods:region|mods:state|mods:territory|mods:county|mods:city|mods:island|mods:area">
                    <xsl:value-of select="."/>
                    <xsl:if test="position()!=last()">--</xsl:if>
                </xsl:for-each>
            </dc:coverage>
        </xsl:for-each>

        <xsl:for-each select="mods:cartographics/*">
            <dc:coverage>
                <xsl:value-of select="."/>
            </dc:coverage>
        </xsl:for-each>

        <xsl:if test="mods:temporal">
            <dc:coverage>
                <xsl:for-each select="mods:temporal">
                    <xsl:value-of select="."/>
                    <xsl:if test="position()!=last()">-</xsl:if>
                </xsl:for-each>
            </dc:coverage>
        </xsl:if>

        <xsl:if test="*[1][local-name()='topic'] and *[local-name()!='topic']">
            <dc:subject>
                <xsl:for-each select="*[local-name()!='cartographics' and local-name()!='geographicCode' and local-name()!='hierarchicalGeographic'] ">
                    <xsl:value-of select="."/>
                    <xsl:if test="position()!=last()">--</xsl:if>
                </xsl:for-each>
            </dc:subject>
        </xsl:if>
    </xsl:template>

    <xsl:template match="mods:abstract | mods:tableOfContents | mods:note">
        <dc:description>
            <xsl:value-of select="."/>
        </dc:description>
    </xsl:template>

    <xsl:template match="mods:originInfo">
        <xsl:apply-templates select="*[@point='start']"/>
        <xsl:for-each
            select="mods:dateIssued[@point!='start' and @point!='end'] |mods:dateCreated[@point!='start' and @point!='end'] | mods:dateCaptured[@point!='start' and @point!='end'] | mods:dateOther[@point!='start' and @point!='end']">
            <dc:date>
                <xsl:value-of select="."/>
            </dc:date>
        </xsl:for-each>

        <xsl:for-each select="mods:publisher">
            <dc:publisher>
                <xsl:value-of select="."/>
            </dc:publisher>
        </xsl:for-each>
    </xsl:template>

    <xsl:template match="mods:dateIssued | mods:dateCreated | mods:dateCaptured">
        <dc:date>
            <xsl:choose>
                <xsl:when test="@point='start'">
                    <xsl:value-of select="."/>
                    <xsl:text> - </xsl:text>
                </xsl:when>
                <xsl:when test="@point='end'">
                    <xsl:value-of select="."/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="."/>
                </xsl:otherwise>
            </xsl:choose>
        </dc:date>
    </xsl:template>

    <xsl:template match="mods:genre">
        <xsl:choose>
            <xsl:when test="@authority='dct'">
                <dc:type>
                    <xsl:value-of select="."/>
                </dc:type>
                <xsl:for-each select="mods:typeOfResource">
                    <dc:type>
                        <xsl:value-of select="."/>
                    </dc:type>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <dc:type>
                    <xsl:value-of select="."/>
                </dc:type>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="mods:typeOfResource">
        <xsl:if test="@collection='yes'">
            <dc:type>Collection</dc:type>
        </xsl:if>
        <xsl:if test=". ='software' and ../mods:genre='database'">
            <dc:type>DataSet</dc:type>
        </xsl:if>
        <xsl:if test=".='software' and ../mods:genre='online system or service'">
            <dc:type>Service</dc:type>
        </xsl:if>
        <xsl:if test=".='software'">
            <dc:type>Software</dc:type>
        </xsl:if>
        <xsl:if test=".='cartographic material'">
            <dc:type>Image</dc:type>
        </xsl:if>
        <xsl:if test=".='multimedia'">
            <dc:type>InteractiveResource</dc:type>
        </xsl:if>
        <xsl:if test=".='moving image'">
            <dc:type>MovingImage</dc:type>
        </xsl:if>
        <xsl:if test=".='three-dimensional object'">
            <dc:type>PhysicalObject</dc:type>
        </xsl:if>
        <xsl:if test="starts-with(.,'sound recording')">
            <dc:type>Sound</dc:type>
        </xsl:if>
        <xsl:if test=".='still image'">
            <dc:type>StillImage</dc:type>
        </xsl:if>
        <xsl:if test=". ='text'">
            <dc:type>Text</dc:type>
        </xsl:if>
        <xsl:if test=".='notated music'">
            <dc:type>Text</dc:type>
        </xsl:if>
    </xsl:template>

    <xsl:template match="mods:physicalDescription">
        <xsl:if test="mods:extent">
            <dc:format>
                <xsl:value-of select="mods:extent"/>
            </dc:format>
        </xsl:if>
        <xsl:if test="mods:form">
            <dc:format>
                <xsl:value-of select="mods:form"/>
            </dc:format>
        </xsl:if>
        <xsl:if test="mods:internetMediaType">
            <dc:format>
                <xsl:value-of select="mods:internetMediaType"/>
            </dc:format>
        </xsl:if>
    </xsl:template>

    <xsl:template match="mods:mimeType">
        <dc:format>
            <xsl:value-of select="."/>
        </dc:format>
    </xsl:template>

    <xsl:template match="mods:identifier">
        <xsl:variable name="type" select="translate(@type,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')"/>
        <xsl:choose>
            <!--issue 111-->
            <!--<xsl:when test="contains ('isbn issn uri doi lccn uri uuid', $type)">-->
            <xsl:when test="normalize-space($type)">
                <dc:identifier>
                    <xsl:value-of select="$type"/>:<xsl:value-of select="."/>
                </dc:identifier>
            </xsl:when>
            <xsl:otherwise>
                <dc:identifier>
                    <xsl:value-of select="."/>
                </dc:identifier>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="mods:location">
        <dc:identifier>
            <xsl:for-each select="mods:url">
                <xsl:value-of select="."/>
            </xsl:for-each>
        </dc:identifier>
    </xsl:template>

    <xsl:template match="mods:language">
        <dc:language>
            <xsl:value-of select="normalize-space(.)"/>
        </dc:language>
    </xsl:template>

    <xsl:template match="mods:relatedItem[mods:titleInfo | mods:name | mods:identifier | mods:location]">
        <xsl:choose>
            <xsl:when test="@type='original'">
                <dc:source>
                    <xsl:for-each
                        select="mods:titleInfo/mods:title | mods:identifier | mods:location/mods:url">
                        <xsl:if test="normalize-space(.)!= ''">
                            <xsl:value-of select="."/>
                            <xsl:if test="position()!=last()">--</xsl:if>
                        </xsl:if>
                    </xsl:for-each>
                </dc:source>
            </xsl:when>
            <xsl:when test="@type='series'"/>
            <xsl:otherwise>
                <dc:relation>
                    <xsl:for-each
                        select="mods:titleInfo/mods:title | mods:identifier | mods:location/mods:url">
                        <xsl:if test="normalize-space(.)!= ''">
                            <xsl:value-of select="."/>
                            <xsl:if test="position()!=last()">--</xsl:if>
                        </xsl:if>
                    </xsl:for-each>
                </dc:relation>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="mods:accessCondition">
        <dc:rights>
            <xsl:value-of select="."/>
        </dc:rights>
    </xsl:template>

    <xsl:template name="name">
        <xsl:variable name="name">
            <xsl:for-each select="mods:namePart[not(@type)]/text()">
                <xsl:value-of select="."/>
                <xsl:text> </xsl:text>
            </xsl:for-each>
            <xsl:value-of select="mods:namePart[@type='family']"/>
            <xsl:if test="mods:namePart[@type='given']/text()">
                <xsl:text>, </xsl:text>
                <xsl:value-of select="mods:namePart[@type='given']"/>
            </xsl:if>
            <xsl:if test="mods:namePart[@type='date']/text()">
                <xsl:text>, </xsl:text>
                <xsl:value-of select="mods:namePart[@type='date']"/>
                <xsl:text/>
            </xsl:if>
            <xsl:if test="mods:displayForm">
                <xsl:text> (</xsl:text>
                <xsl:value-of select="mods:displayForm"/>
                <xsl:text>) </xsl:text>
            </xsl:if>
            <xsl:for-each select="mods:role[mods:roleTerm[@type='text']!='creator']/text()">
                <xsl:text> (</xsl:text>
                <xsl:value-of select="normalize-space(.)"/>
                <xsl:text>) </xsl:text>
            </xsl:for-each>
        </xsl:variable>
        <xsl:value-of select="normalize-space($name)"/>
    </xsl:template>

    <xsl:template match="mods:dateIssued[@point='start'] | mods:dateCreated[@point='start'] | mods:dateCaptured[@point='start'] | mods:dateOther[@point='start'] ">
        <xsl:variable name="dateName" select="local-name()"/>
        <dc:date>
            <xsl:value-of select="."/>-
            <xsl:value-of select="../*[local-name()=$dateName][@point='end']"/>
        </dc:date>
    </xsl:template>

    <xsl:template match="mods:temporal[@point='start']  ">
        <xsl:value-of select="."/>-
        <xsl:value-of select="../mods:temporal[@point='end']"/>
    </xsl:template>

    <xsl:template match="mods:temporal[@point!='start' and @point!='end']  ">
        <xsl:value-of select="."/>
    </xsl:template>

    <!-- suppress all else:-->
    <xsl:template match="*"/>


</xsl:stylesheet>

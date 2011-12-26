<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- 
    Second pass of lowering:
    - Detect incorrectly placed fields
    - Fold container item fields into a single ld:item subelement.
 -->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ld="http://github.com/peterix/dfhack/lowered-data-definition">
    <xsl:template match="@*">
        <xsl:copy-of select='.'/>
    </xsl:template>

    <xsl:template match="*">
        <xsl:copy>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="ld:field[not(@ld:level) or (@ld:level &lt; 0)]" priority='10'>
        <xsl:message terminate='yes'>
            Unexpected field: <xsl:copy-of select='.'/>
        </xsl:message>
    </xsl:template>

    <xsl:template match="ld:field" priority='8'>
        <xsl:param name="tag" select="'field'"/>
        <xsl:element name='ld:{$tag}'>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="ld:field[@ld:is-container]" priority='9'>
        <xsl:param name="tag" select="'field'"/>
        <xsl:element name='ld:{$tag}'>
            <xsl:apply-templates select='@*'/>
            <xsl:choose>
                <xsl:when test='count(ld:field) &lt;= 1'>
                    <xsl:apply-templates select='node()'>
                        <xsl:with-param name='tag' select="'item'"/>
                    </xsl:apply-templates>
                </xsl:when>
                <xsl:otherwise>
                    <!-- This destroys formatting, but it seems inevitable. -->
                    <ld:item ld:meta='compound'>
                        <xsl:attribute name='ld:level'><xsl:value-of select='@ld:level'/></xsl:attribute>
                        <xsl:apply-templates select='ld:field|text()'/>
                    </ld:item>
                    <xsl:apply-templates select='node()[not(self::ld:field)]'/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>

<!--
Local Variables:
indent-tabs-mode: nil
nxml-child-indent: 4
End:
-->

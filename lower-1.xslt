<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- 
  The original XML format is good for human use, but
  difficult to interpret during code generation. This
  lowers it to more repetitive & verbose, but easier
  for the programs to interpret.
  
  This is the first pass that folds all field tags into ld:field.
 -->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ld="http://github.com/peterix/dfhack/lowered-data-definition">
    <!-- 
        Global templates:

        - Copy attributes and simple tags
        - Bail out on unexpected tags
     -->

    <xsl:template match="@*">
        <xsl:copy-of select='.'/>
    </xsl:template>

    <xsl:template match="*">
        <xsl:message terminate="yes">
            Error: Unexpected tag: <xsl:value-of select='name(.)'/>
        </xsl:message>
    </xsl:template>

    <xsl:template match="/data-definition">
        <ld:data-definition>
            <xsl:apply-templates select='@*|node()'/>
        </ld:data-definition>
    </xsl:template>

    <xsl:template match="comment|code-helper|enum-attr|enum-item">
        <xsl:copy-of select='.'/>
    </xsl:template>

    <!-- Type defs: convert to one common 'global-type' tag name. -->
    
    <xsl:template match='enum-type|bitfield-type|class-type|struct-type'>
        <ld:global-type>
            <xsl:attribute name='ld:meta'><xsl:value-of select='name(.)'/></xsl:attribute>
            <xsl:attribute name='ld:level'>0</xsl:attribute>
            <xsl:apply-templates select='@*|node()'>
                <xsl:with-param name='level' select="1"/>
            </xsl:apply-templates>
        </ld:global-type>
    </xsl:template>

    <!-- Code to properly annotate references to types by name -->

    <xsl:key name="primitive-type-lookup" match="prim-type" use="@name"/>
    <xsl:variable name="primitive-types-top" select="document('')/*/ld:primitive-types"/>

    <xsl:template match="ld:primitive-types">
        <xsl:param name="name"/>
        <xsl:choose>
            <xsl:when test="$name = 'pointer'">
                <xsl:attribute name='ld:meta'>pointer</xsl:attribute>
            </xsl:when>
            <xsl:when test="key('primitive-type-lookup', $name)">
                <xsl:attribute name='ld:meta'>primitive</xsl:attribute>
                <xsl:attribute name='type-name'><xsl:value-of select='$name'/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
                <xsl:attribute name='ld:meta'>global</xsl:attribute>
                <xsl:attribute name='type-name'><xsl:value-of select='$name'/></xsl:attribute>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="lookup-type-ref">
        <xsl:param name='name'/>
        <xsl:apply-templates select="$primitive-types-top">
            <xsl:with-param name="name" select="$name"/>
        </xsl:apply-templates>
    </xsl:template>

    <!--
        Fields:

        - Fold into one generic 'field' tag.
        - Add a 'level' attribute to assist in searching by name.
    -->

    <!-- Primitive types: meta='primitive' type-name='blah' -->
    <ld:primitive-types>
        <prim-type name='int8_t'/>
        <prim-type name='uint8_t'/>
        <prim-type name='int16_t'/>
        <prim-type name='uint16_t'/>
        <prim-type name='int32_t'/>
        <prim-type name='uint32_t'/>
        <prim-type name='int64_t'/>
        <prim-type name='uint64_t'/>
        <prim-type name='bool'/>
        <prim-type name='padding'/>
        <prim-type name='stl-string'/>
        <prim-type name='flag-bit'/>
        <prim-type name='s-float'/>
        <prim-type name='static-string'/>
    </ld:primitive-types>

    <xsl:template match='int8_t|uint8_t|int16_t|uint16_t|int32_t|uint32_t|int64_t|uint64_t|bool|padding|stl-string|flag-bit|s-float|static-string'>
        <xsl:param name='level' select='-1'/>
        <ld:field ld:meta='primitive'>
            <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
            <xsl:attribute name='type-name'><xsl:value-of select='name(.)'/></xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </ld:field>
    </xsl:template>

    <!--
        Compound, enum or bitfield:
        
        - When a proxy: meta='global' subtype='$tag' type-name='blah'
        - When an ad-hoc compound: meta='compound' subtype='$tag'
        - Level not incremented unless it has a name.
    -->
    <xsl:template match='compound|bitfield|enum'>
        <xsl:param name='level' select='-1'/>
        <ld:field>
            <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
            <xsl:attribute name='ld:subtype'><xsl:value-of select='name(.)'/></xsl:attribute>
            <xsl:choose>
                <xsl:when test='@type-name'>
                    <xsl:call-template name='lookup-type-ref'>
                        <xsl:with-param name='name' select="@type-name"/>
                    </xsl:call-template>
                    <xsl:apply-templates select='@*|node()'/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name='ld:meta'>compound</xsl:attribute>
                    <xsl:apply-templates select='@*|node()'>
                        <xsl:with-param name='level' select="$level+count(@name)"/>
                    </xsl:apply-templates>
                </xsl:otherwise>
            </xsl:choose>
        </ld:field>
    </xsl:template>

    <!-- Generic container helper: resolve type-name to a field, then process subtags. -->
    <xsl:template name='container'>
        <xsl:param name='level' select='-1'/>
        <xsl:param name='type' select='@type-name'/>
        <xsl:attribute name='ld:is-container'>true</xsl:attribute>
        <xsl:choose>
            <xsl:when test='$type'>
                <ld:field>
                    <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
                    <xsl:call-template name='lookup-type-ref'>
                        <xsl:with-param name='name' select="$type"/>
                    </xsl:call-template>
                </ld:field>
                <xsl:apply-templates select='node()'/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates select='node()'>
                    <xsl:with-param name='level' select="$level"/>
                </xsl:apply-templates>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- Special containers:  meta='$tag' -->
    <xsl:template match='static-array|pointer'>
        <xsl:param name='level' select='-1'/>
        <ld:field>
            <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
            <xsl:attribute name='ld:meta'><xsl:value-of select='name(.)'/></xsl:attribute>
            <xsl:apply-templates select='@*'/>
            <xsl:call-template name='container'>
                <xsl:with-param name='level' select="$level+1"/>
            </xsl:call-template>
        </ld:field>
    </xsl:template>

    <!-- Misc containers: meta='container' subtype='$tag' -->
    <xsl:template match='stl-vector|df-flagarray|stl-bit-vector'>
        <xsl:param name='level' select='-1'/>
        <ld:field ld:meta='container'>
            <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
            <xsl:attribute name='ld:subtype'><xsl:value-of select='name(.)'/></xsl:attribute>
            <xsl:apply-templates select='@*'/>
            <xsl:call-template name='container'>
                <xsl:with-param name='level' select="$level+1"/>
            </xsl:call-template>
        </ld:field>
    </xsl:template>
</xsl:stylesheet>

<!--
Local Variables:
indent-tabs-mode: nil
nxml-child-indent: 4
End:
-->

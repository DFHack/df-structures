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

    <xsl:template match="comment|code-helper|enum-attr|enum-item|item-attr|extra-include">
        <xsl:copy>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="virtual-methods|custom-methods|cond-if|cond-else">
        <xsl:param name='level' select='-1'/>
        <xsl:copy>
            <xsl:apply-templates select='@*|node()'>
                <xsl:with-param name='level' select="$level"/>
            </xsl:apply-templates>
        </xsl:copy>
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

    <xsl:template match='df-linked-list-type'>
        <ld:global-type ld:meta='struct-type' ld:subtype='df-linked-list-type' ld:level='0' type-name='{@type-name}' item-type='{@item-type}'>
            <code-helper name="describe"><xsl:text>(describe-obj $.item)</xsl:text></code-helper>
            <ld:field name='item' type-name='{@item-type}' ld:level='1' ld:meta='pointer' ld:is-container='true'>
                <ld:item ld:level='2' ld:meta='global' type-name='{@item-type}'/>
            </ld:field>
            <ld:field name='prev' type-name='{@type-name}' ld:level='1' ld:meta='pointer' ld:is-container='true'>
                <ld:item ld:level='2' ld:meta='global' type-name='{@type-name}'/>
            </ld:field>
            <ld:field name='next' type-name='{@type-name}' ld:level='1' ld:meta='pointer' ld:is-container='true'>
                <ld:item ld:level='2' ld:meta='global' type-name='{@type-name}'/>
            </ld:field>
        </ld:global-type>
    </xsl:template>

    <xsl:template match='df-other-vectors-type'>
        <xsl:variable name='vectors' select='./stl-vector'/>
        <xsl:variable name='enum-name' select='@index-enum'/>
        <xsl:variable name='item-type' select='@item-type'/>

        <xsl:for-each select="*[not(name()='stl-vector' or name()='comment' or name()='code-helper')]">
            <xsl:message terminate="yes">
Error: df-other-vectors-type may only contain stl-vector fields. (<xsl:value-of select='name(.)'/>)
            </xsl:message>
        </xsl:for-each>

        <xsl:for-each select="$vectors">
            <xsl:variable name='enum-key' select='@name'/>
            <xsl:variable name='enum-item' select='/data-definition/enum-type[@type-name=($enum-name)]/enum-item[@name=($enum-key)]'/>

            <xsl:choose>
                <xsl:when test='count($enum-item) = 0'>
                    <xsl:message terminate="yes">
Error: field <xsl:value-of select='$enum-key'/> is not a member of enum <xsl:value-of select='$enum-name'/>
                    </xsl:message>
                </xsl:when>
                <xsl:when test="starts-with(($enum-item/@value), '-')">
                    <xsl:message terminate="yes">
Error: field <xsl:value-of select='$enum-key'/> corresponds to an enum value of <xsl:value-of select='$enum-item/@value'/>
                    </xsl:message>
                </xsl:when>
            </xsl:choose>
        </xsl:for-each>

        <ld:global-type ld:meta='struct-type' ld:subtype='df-other-vectors-type' ld:level='0' type-name='{@type-name}' index-enum='{$enum-name}' item-type='{$item-type}'>
            <xsl:apply-templates select="@comment|@since"/>
            <xsl:apply-templates select="comment|code-helper">
                <xsl:with-param name='level' select="1"/>
            </xsl:apply-templates>

            <xsl:for-each select="/data-definition/enum-type[@type-name=($enum-name)]/enum-item[not(starts-with(@value, '-'))]">
                <xsl:variable name='enum-key' select='@name'/>
                <xsl:variable name='defined' select='$vectors[@name=($enum-key)]'/>

                <xsl:choose>
                    <xsl:when test='count($defined)'>
                        <xsl:apply-templates select='$defined'>
                            <xsl:with-param name='level' select='1'/>
                        </xsl:apply-templates>
                    </xsl:when>
                    <xsl:otherwise>
                        <ld:field ld:meta='container' ld:level='1' ld:subtype='stl-vector' name='{$enum-key}' pointer-type='{$item-type}'>
                            <ld:item ld:meta='pointer' ld:is-container='true' ld:level='2' type-name='{$item-type}'>
                                <ld:item>
                                    <xsl:call-template name='lookup-type-ref'>
                                        <xsl:with-param name='name' select='$item-type'/>
                                        <xsl:with-param name='level' select='3'/>
                                    </xsl:call-template>
                                </ld:item>
                            </ld:item>
                        </ld:field>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>
        </ld:global-type>
    </xsl:template>

    <!-- Code to properly annotate references to types by name -->

    <xsl:key name="primitive-type-lookup" match="prim-type" use="@ld:subtype"/>
    <xsl:variable name="primitive-types-top" select="document('')/*/ld:primitive-types"/>

    <xsl:template match="ld:primitive-types">
        <xsl:param name="name"/>
        <xsl:param name="level"/>
        <xsl:param name="rq_global"/>
        <xsl:variable name='item' select="key('primitive-type-lookup', $name)"/>
        <xsl:choose>
            <xsl:when test="$item">
                <xsl:if test='$rq_global'>
                    <xsl:message terminate="yes">
                        Error: Cannot refer to primitive types from <xsl:value-of select='$rq_global'/>
                    </xsl:message>
                </xsl:if>
                <xsl:apply-templates select="$item/@*"/>
                <xsl:apply-templates select="$item/*">
                    <xsl:with-param name='level' select="$level+1"/>
                </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
                <xsl:attribute name='ld:meta'>global</xsl:attribute>
                <xsl:attribute name='type-name'><xsl:value-of select='$name'/></xsl:attribute>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="lookup-type-ref">
        <xsl:param name='name'/>
        <xsl:param name='level' select='-1'/>
        <xsl:param name='rq_global'/>
        <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
        <xsl:apply-templates select="$primitive-types-top">
            <xsl:with-param name="name" select="$name"/>
            <xsl:with-param name="level" select="$level"/>
            <xsl:with-param name="rq_global" select="$rq_global"/>
        </xsl:apply-templates>
    </xsl:template>

    <!--
        Fields:

        - Fold into one generic 'field' tag.
        - Add a 'level' attribute to assist in searching by name.
    -->

    <!-- Primitive types: -->
    <ld:primitive-types>
        <prim-type ld:meta='number' ld:subtype='int8_t' ld:bits='8'/>
        <prim-type ld:meta='number' ld:subtype='uint8_t' ld:unsigned='true' ld:bits='8'/>
        <prim-type ld:meta='number' ld:subtype='int16_t' ld:bits='16'/>
        <prim-type ld:meta='number' ld:subtype='uint16_t' ld:unsigned='true' ld:bits='16'/>
        <prim-type ld:meta='number' ld:subtype='int32_t' ld:bits='32'/>
        <prim-type ld:meta='number' ld:subtype='uint32_t' ld:unsigned='true' ld:bits='32'/>
        <prim-type ld:meta='number' ld:subtype='int64_t' ld:bits='64'/>
        <prim-type ld:meta='number' ld:subtype='uint64_t' ld:unsigned='true' ld:bits='64'/>
        <prim-type ld:meta='number' ld:subtype='ssize_t' ld:bits=''/>
        <prim-type ld:meta='number' ld:subtype='size_t' ld:unsigned='true' ld:bits=''/>
        <prim-type ld:meta='number' ld:subtype='long' ld:bits=''/>
        <prim-type ld:meta='number' ld:subtype='ulong' ld:bits=''/>
        <prim-type ld:meta='number' ld:subtype='bool' ld:bits='8'/>
        <prim-type ld:meta='number' ld:subtype='s-float' ld:bits='32'/>
        <prim-type ld:meta='number' ld:subtype='d-float' ld:bits='64'/>
        <prim-type ld:meta='number' ld:subtype='flag-bit' ld:bits='1'/>

        <prim-type ld:meta='bytes' ld:subtype='padding'/>
        <prim-type ld:meta='bytes' ld:subtype='static-string'/>

        <prim-type ld:meta='pointer' ld:subtype='pointer'/>
        <prim-type ld:meta='pointer' ld:subtype='ptr-string' ld:is-container='true'>
            <static-string/>
        </prim-type>

        <prim-type ld:meta='primitive' ld:subtype='stl-string'/>
        <prim-type ld:meta='primitive' ld:subtype='stl-fstream'/>
        <prim-type ld:meta='primitive' ld:subtype='stl-mutex'/>
        <prim-type ld:meta='primitive' ld:subtype='stl-condition-variable'/>
        <prim-type ld:meta='primitive' ld:subtype='stl-future'/>
    </ld:primitive-types>

    <xsl:template match='int8_t|uint8_t|int16_t|uint16_t|int32_t|uint32_t|int64_t|uint64_t|size_t|ssize_t|long|ulong|bool|flag-bit|s-float|d-float|padding|static-string|ptr-string|stl-string|stl-fstream|stl-mutex|stl-condition-variable|stl-future'>
        <xsl:param name='level' select='-1'/>
        <ld:field>
            <xsl:apply-templates select='@*'/>
            <xsl:call-template name='lookup-type-ref'>
                <xsl:with-param name="name" select="name(.)"/>
                <xsl:with-param name="level" select="$level"/>
            </xsl:call-template>
            <xsl:apply-templates select='node()'/>
        </ld:field>
    </xsl:template>

    <!--
        Compound, enum or bitfield:

        - When a proxy: meta='global' subtype='$tag' type-name='blah'
        - When an ad-hoc compound: meta='compound' subtype='$tag'
        - Level not incremented unless it has a name.
    -->
    <xsl:template name='compound'>
        <xsl:param name='level' select='-1'/>
        <xsl:param name='level_shift' select='1'/>
        <xsl:param name='rq_global'/>
        <xsl:apply-templates select='@*'/>
        <xsl:choose>
            <xsl:when test='@type-name'>
                <xsl:call-template name='lookup-type-ref'>
                    <xsl:with-param name='name' select="@type-name"/>
                    <xsl:with-param name="level" select="$level"/>
                    <xsl:with-param name='rq_global' select="$rq_global"/>
                </xsl:call-template>
                <xsl:apply-templates select='node()'/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
                <xsl:attribute name='ld:meta'>compound</xsl:attribute>
                <xsl:apply-templates select='node()'>
                    <xsl:with-param name='level' select="$level+$level_shift"/>
                </xsl:apply-templates>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match='compound'>
        <xsl:param name='level' select='-1'/>
        <ld:field>
            <xsl:if test="not(@name|@type-name)">
                <xsl:attribute name='ld:anon-compound'>true</xsl:attribute>
            </xsl:if>
            <xsl:call-template name='compound'>
                <xsl:with-param name='level' select="$level"/>
                <xsl:with-param name='level_shift' select="count(@name)"/>
            </xsl:call-template>
        </ld:field>
    </xsl:template>

    <xsl:template match='bitfield|enum'>
        <xsl:param name='level' select='-1'/>
        <ld:field>
            <xsl:attribute name='ld:subtype'><xsl:value-of select='name(.)'/></xsl:attribute>
            <xsl:call-template name='compound'>
                <xsl:with-param name='level' select="$level"/>
                <xsl:with-param name='rq_global' select="name(.)"/>
            </xsl:call-template>
        </ld:field>
    </xsl:template>

    <!-- Generic container helper: resolve type-name to a field, then process subtags. -->
    <xsl:template name='container'>
        <xsl:param name='level' select='-1'/>
        <xsl:attribute name='ld:is-container'>true</xsl:attribute>
        <xsl:choose>
            <xsl:when test='@pointer-type'>
                <ld:field ld:meta='pointer' ld:is-container='true'>
                    <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
                    <xsl:attribute name='type-name'><xsl:value-of select='@pointer-type'/></xsl:attribute>
                    <xsl:apply-templates select='@refers-to|@ref-target|@aux-value'/>
                    <ld:field>
                        <xsl:apply-templates select='@refers-to|@ref-target|@aux-value'/>
                        <xsl:call-template name='lookup-type-ref'>
                            <xsl:with-param name='name' select="@pointer-type"/>
                            <xsl:with-param name="level" select="$level+1"/>
                        </xsl:call-template>
                    </ld:field>
                </ld:field>
                <xsl:apply-templates select='node()'/>
            </xsl:when>
            <xsl:when test='@type-name'>
                <ld:field>
                    <xsl:apply-templates select='@refers-to|@ref-target|@aux-value'/>
                    <xsl:call-template name='lookup-type-ref'>
                        <xsl:with-param name='name' select="@type-name"/>
                        <xsl:with-param name="level" select="$level"/>
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
    <xsl:template match='stl-vector|stl-deque|stl-set|stl-bit-vector|stl-map|stl-unordered-map|stl-optional|stl-variant|stl-shared-ptr|stl-function|df-flagarray|df-static-flagarray|df-array|df-linked-list'>
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

    <!-- Virtual methods -->

    <xsl:template match='vmethod|cmethod'>
        <xsl:param name='level' select='-1'/>
        <xsl:copy>
            <xsl:attribute name='ld:level'><xsl:value-of select='$level'/></xsl:attribute>
            <xsl:apply-templates select='@*'/>
            <xsl:if test='@ret-type'>
                <xsl:copy-of select='text()[1]'/>
                <ret-type>
                    <xsl:call-template name='lookup-type-ref'>
                        <xsl:with-param name='name' select="@ret-type"/>
                        <xsl:with-param name="level" select="$level+1"/>
                    </xsl:call-template>
                </ret-type>
            </xsl:if>
            <xsl:apply-templates select='node()'>
                <xsl:with-param name='level' select="$level+1"/>
            </xsl:apply-templates>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='ret-type'>
        <xsl:param name='level' select='-1'/>
        <xsl:copy>
            <xsl:call-template name='compound'>
                <xsl:with-param name='level' select="$level"/>
            </xsl:call-template>
        </xsl:copy>
    </xsl:template>

    <!-- Global objects: treat as container fields -->
    <xsl:template match='global-object'>
        <ld:global-object>
            <xsl:attribute name='ld:level'>0</xsl:attribute>
            <xsl:apply-templates select='@*'/>
            <xsl:call-template name='container'>
                <xsl:with-param name='level' select="1"/>
            </xsl:call-template>
        </ld:global-object>
    </xsl:template>
</xsl:stylesheet>

<!--
Local Variables:
indent-tabs-mode: nil
nxml-child-indent: 4
End:
-->

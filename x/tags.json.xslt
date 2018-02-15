<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="*|/">
    <xsl:param name="hierarchy"/>
    <xsl:apply-templates>
      <xsl:with-param name="hierarchy" select="$hierarchy"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="text()">
  </xsl:template>
  <xsl:template match="/">
    {
    <xsl:apply-templates>
      <xsl:sort select="@id" data-type="number"/>
      <xsl:with-param name="hierarchy" select="@name"/>
    </xsl:apply-templates>
    "":null
    }
  </xsl:template>
  <xsl:template match="category">
    <xsl:param name="hierarchy"/>
    <xsl:if test="@id">"<xsl:value-of select="@id"/>":{"name":"<xsl:value-of select="concat($hierarchy,@name)"/>",
    "description":"<xsl:value-of select="description/text()" />"},
    </xsl:if>
    <xsl:apply-templates>
      <xsl:sort select="@id" data-type="number"/>
      <xsl:with-param name="hierarchy" select="concat($hierarchy,@name,':')"/>
    </xsl:apply-templates>
  </xsl:template>
</xsl:stylesheet>

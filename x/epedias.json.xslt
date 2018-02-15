<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="text()">
  </xsl:template>
  <xsl:template match="/">
    {
    <xsl:apply-templates />
    "":null
    }
  </xsl:template>
  <xsl:template match="entry">
    "<xsl:value-of select="@id" />":{"tag":"<xsl:value-of select="@tag"/>","name":"<xsl:value-of select="link/@name"/>","base":"<xsl:value-of select="urlbase/@href"/>","description":"<xsl:value-of select="description/text()"/>"},
  </xsl:template>
</xsl:stylesheet>

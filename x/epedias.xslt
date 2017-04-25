<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml"/>
  <xsl:template match="description">
    <div class="description">
      <xsl:apply-templates />
    </div>
  </xsl:template>
  <xsl:template match="/">
    <html>
      <head>
	<title>
	  Piperka external entries on other sites
	</title>
      </head>
      <body>
	<table id="epedias">
	  <xsl:apply-templates />
	</table>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="entry">
    <tr>
      <td>
	<a>
	  <xsl:attribute name="href"><xsl:value-of select="link/@href" /></xsl:attribute>
	  <xsl:attribute name="title"><xsl:value-of select="description/text()" /></xsl:attribute>
	  <xsl:value-of select="link/@name"/>
	</a>
      </td>
      <td class="urlbase"><xsl:value-of select="urlbase/@href" /></td>
      <td>
	<input type="text">
	  <xsl:attribute name="name">epedia-entry-<xsl:value-of select="@id" /></xsl:attribute>
	</input>
	<input type="hidden" name="epedia">
	  <xsl:attribute name="value"><xsl:value-of select="@id" /></xsl:attribute>
	</input>
      </td>
    </tr>
  </xsl:template>
</xsl:stylesheet>

<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml"/>
  <xsl:template match="*|/">
    <xsl:param name="hierarchy"/>
    <xsl:apply-templates>
      <xsl:with-param name="hierarchy" select="$hierarchy"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="description">
    <div class="description">
      <xsl:apply-templates />
    </div>
  </xsl:template>
  <xsl:template match="/">
    <html>
      <head>
	<title>
	  Piperka tags, HTMLified
	</title>
      </head>
      <body>
	<select id="tagselect">
	  <xsl:for-each select="tags/category">
	    <option>
	      <xsl:attribute name="value">
		<xsl:value-of select="@name"/>
	      </xsl:attribute>
	      <xsl:value-of select="@name"/>
	    </option>
	  </xsl:for-each>
	</select>
	<xsl:for-each select="tags/category">
	  <div class="maincategory script">
	    <xsl:attribute name="id">cat-<xsl:value-of select="@name"/></xsl:attribute>
	    <xsl:apply-templates>
	      <xsl:with-param name="hierarchy" select="concat(@name,':')"/>
	    </xsl:apply-templates>
	  </div>
	</xsl:for-each>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="category">
    <xsl:param name="hierarchy"/>
    <span class="category">
      <xsl:if test="@id">
	<input type="checkbox" name="tags">
	  <xsl:attribute name="value">
	    <xsl:value-of select="@id"/>
	  </xsl:attribute>
	  <xsl:attribute name="id">cat-sel-<xsl:value-of select="@id"/></xsl:attribute>
	  <xsl:attribute name="name">category</xsl:attribute>
	  <xsl:attribute name="value"><xsl:value-of select="@id"/></xsl:attribute>
	</input>
	<label>
	  <xsl:attribute name="for">cat-sel-<xsl:value-of select="@id"/></xsl:attribute>
	  <xsl:value-of select="concat($hierarchy,@name)"/>
	</label>
      </xsl:if>
      <xsl:apply-templates>
	<xsl:with-param name="hierarchy" select="concat($hierarchy,@name,':')"/>
      </xsl:apply-templates>
    </span>
  </xsl:template>
</xsl:stylesheet>

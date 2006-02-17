<!--

/*
 * Basic SAF to MAF converter
 *
 * Author:
 * Ben Waldron <benjamin.waldron@cl.cam.ac.uk>
 *
 */
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output encoding="UTF-8" method="xml" indent="yes" doctype-system="maf.dtd" media-type="text/xml"/>
  <xsl:strip-space elements="*"/>
  
  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="saf">
    <maf document='{@document}' addressing='{@addressing}'>
      <xsl:copy-of select="olac"/>
      <xsl:apply-templates/>
    </maf>
  </xsl:template>
  
  <xsl:template match="fsm">
    <fsm init='{@init}' final='{@final}'>
      <xsl:apply-templates select="annot[@type='token']"/>
      <xsl:apply-templates select="annot[@type='pos']"/>
    </fsm>
  </xsl:template>
  
  <xsl:template match="annot[@type='token']">
    <token id='{@id}' from='{@from}' to='{@to}' source='{@source}' target='{@target}' form='{@value}'/>
  </xsl:template>
  
  <xsl:template match="annot[@type='pos']">
    <wordForm id='{@id}' source='{@source}' target='{@target}' tag='{@value}'/>
  </xsl:template>
  
</xsl:stylesheet>

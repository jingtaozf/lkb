<!--
 *
 * SAF to PIC converter
 *
 * Author:
 * Ben Waldron <benjamin.waldron@cl.cam.ac.uk>
 *
 * NOTES: 
 *  - currently very basic
 *  - cheap should be called in '-tok=xml' mode to ensure "character pointer" interpretation
 *     of cfrom/cto
 *
 * INPUT: SAF containing pos annotations, each with a single dep to a token annotation, 
 *          each with a single dep to a sentence annotation
 * OUTPUT: sequence of pet-input-chart's (Note: before piping to PET two newlines must be 
 *          inserted after each pet-input-chart, and ech pet-input-chart must by preceeded 
 *          by an XML declaration.)
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output encoding="UTF-8" method="xml" indent="yes" media-type="text/xml"/>
  <!-- <xsl:strip-space elements="*"/> -->
  
  <!-- create keys so we can cross-reference token and pos annotations later -->
  <xsl:key name="token" match="//annot[@type='token']" use="@deps"/>
  <xsl:key name="pos" match="//annot[@type='pos']" use="@deps"/>

  <!-- create a pet-input-chart for each sentence -->
  <xsl:template match="/">
    <xsl:for-each select="//annot[@type='sentence']">
      <pet-input-chart>
        <xsl:comment>SAF sentence: <xsl:value-of select="@id"/></xsl:comment>
        <xsl:apply-templates select="key('token',@id)"/>
      </pet-input-chart>
    </xsl:for-each>
  </xsl:template>
  
  <!-- for each token, find pos annotation -->
  <xsl:template match="annot[@type='token']">
    <xsl:apply-templates select="key('pos',@id)">
      <xsl:with-param name="token" select="."/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- for each pos, create w element -->
  <xsl:template match="annot[@type='pos']">
    <xsl:param name="token"/>
    <w id='{@id}' cstart='{$token/@from}' cend='{$token/@to}'>
      <surface><xsl:value-of select="id(@deps)/@value"/></surface>
      <pos tag='{@value}' prio='1.0'/>
    </w>
  </xsl:template>
  
</xsl:stylesheet>

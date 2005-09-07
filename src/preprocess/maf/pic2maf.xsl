<!--

/*
 * Basic pet-input-chart to maf converter
 *
 * Author:
 * Ben Waldron <benjamin.waldron@cl.cam.ac.uk>
 *
 */
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output encoding="UTF-8" method="xml" indent="yes" doctype-system="maf.dtd" media-type="text/xml"/>
  <xsl:strip-space elements="*"/>
  
  <!-- ========== -->
  <!-- MAF TOKENS -->
  <!-- ========== -->

  <!--maf addressing='Xpoint' creator='lkb-maf-tokens' date='14:25:37 9/06/2005 (UTC)' language='en.US'-->
  <xsl:template match="/">
    <xsl:apply-templates select="pet-input-chart"/>
  </xsl:template>

  <xsl:template match="pet-input-chart">
        <xsl:element name="maf">
      <xsl:attribute name="addressing">CharPos</xsl:attribute>
      <xsl:attribute name="creator">pic2maf-wf.xsl</xsl:attribute>
      <xsl:attribute name="date">...</xsl:attribute>
      <xsl:attribute name="language">en.US</xsl:attribute>
      <xsl:apply-templates mode="t"/>
      <xsl:apply-templates mode="wf" select="."/>
    </xsl:element>
  </xsl:template>

  <!--token id='t1' from='?' to='?' value='the'/-->
  <xsl:template match="w" mode="t">
        <xsl:element name="token">
      <xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
      <xsl:attribute name="from"><xsl:value-of select="@cstart"/></xsl:attribute>
      <xsl:attribute name="to"><xsl:value-of select="@cend"/></xsl:attribute>
      <xsl:attribute name="value"><xsl:value-of select="surface"/></xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- no output -->
  <xsl:template match="ne" mode="t">
  </xsl:template>

  <!-- ============== -->
  <!-- MAF WORD FORMS -->
  <!-- ============== -->

  <xsl:template match="pet-input-chart" mode="wf">
        <xsl:element name="fsm">
      <xsl:attribute name="init">...</xsl:attribute>
      <xsl:attribute name="final">...</xsl:attribute>

            <xsl:element name="state">
        <xsl:attribute name="id">_START_</xsl:attribute>
      </xsl:element>
      <xsl:apply-templates mode="wf-state"/>

      <xsl:call-template name="transition-w">
        <xsl:with-param name="source" select="_START_"/>
        <xsl:with-param name="list" select="w"/>
        <xsl:with-param name="w-list" select="w"/>
      </xsl:call-template>

    </xsl:element>
  </xsl:template>

  <xsl:template match="w" mode="wf-state">
        <xsl:element name="state">
      <xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- no output -->
  <xsl:template match="ne" mode="wf-state">
  </xsl:template>

  <xsl:template name="transition-w">
    <xsl:param name="list"/>
    <xsl:param name="w-list"/>
    <xsl:variable name="first" select="$list[1]"/>
    <xsl:variable name="rest" select="$list[position()!=1]"/>
        <xsl:element name="transition">
      <xsl:attribute name="source">
        <xsl:call-template name="source">
          <xsl:with-param name="w-id" select="$first/@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:value-of select="$first/@id"/>
      </xsl:attribute>
      <xsl:element name="wordForm">
        <xsl:attribute name="form">
          <xsl:value-of select="$first/surface"/>
        </xsl:attribute>
        <xsl:attribute name="tag"/>
        <xsl:attribute name="tokens">
          <xsl:value-of select="$first/@id"/>
        </xsl:attribute>
        <xsl:element name="fs">
          <xsl:element name="f">
            <xsl:attribute name="name">metaPic</xsl:attribute>
            <xsl:if test="$first/@constant">
            <xsl:element name="f"><xsl:attribute name="name">constant</xsl:attribute><xsl:value-of select="$first/@constant"/></xsl:element>              
            </xsl:if>
            <xsl:if test="$first/@prio">
            <xsl:element name="f"><xsl:attribute name="name">prio</xsl:attribute><xsl:value-of select="$first/@prio"/></xsl:element>
            </xsl:if>
            <xsl:apply-templates select="$first/pos"/>
          </xsl:element>
        </xsl:element>
      </xsl:element>
    </xsl:element>
    <xsl:if test="$rest">
      <xsl:call-template name="transition-w">
        <xsl:with-param name="list" select="$rest"/>
        <xsl:with-param name="w-list" select="$w-list"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="pos">
    <xsl:element name="f">
      <xsl:attribute name="name">pos</xsl:attribute>
      <xsl:element name="f"><xsl:attribute name="name">tag</xsl:attribute><xsl:value-of select="@tag"/></xsl:element>
      <xsl:element name="f"><xsl:attribute name="name">prio</xsl:attribute><xsl:value-of select="@prio"/></xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template name="source">
    <xsl:param name="w-id"/>
    <xsl:param name="w-list"/>
    <xsl:variable name="w-first" select="$w-list[1]"/>
    <xsl:variable name="w-first-id">
      <xsl:value-of select="$w-first/@id"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$w-id=$w-first-id">_START_</xsl:when>
      <xsl:otherwise>
        <xsl:if test="$w-list">
          <xsl:call-template name="source-aux">
            <xsl:with-param name="w-id" select="$w-id"/>
            <xsl:with-param name="w-list" select="$w-list"/>
          </xsl:call-template>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="source-aux">
    <xsl:param name="w-id"/>
    <xsl:param name="w-list"/>
    <xsl:variable name="w-first" select="$w-list[1]"/>
    <xsl:variable name="w-second" select="$w-list[2]"/>
    <xsl:variable name="w-second-id">
      <xsl:value-of select="$w-second/@id"/>
    </xsl:variable>
    <xsl:variable name="w-rest" select="$w-list[position()!=1]"/>
    <xsl:choose>
      
      <xsl:when test="$w-id=$w-second-id">
        <xsl:value-of select="$w-first/@id"/>
      </xsl:when>
    
      <xsl:otherwise>
        <xsl:if test="$w-list">
          <xsl:call-template name="source-aux">
            <xsl:with-param name="w-id" select="$w-id"/>
            <xsl:with-param name="w-list" select="$w-rest"/>
          </xsl:call-template>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
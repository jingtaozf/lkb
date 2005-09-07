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
        <xsl:with-param name="list" select="w"/>
        <xsl:with-param name="w-list" select="w"/>
      </xsl:call-template>

      <xsl:call-template name="transition-w-pos">
        <xsl:with-param name="list" select="w/pos"/>
        <xsl:with-param name="w-list" select="w"/>
      </xsl:call-template>

      <xsl:call-template name="transition-w-typeinfo">
        <xsl:with-param name="list" select="w/typeinfo"/>
        <xsl:with-param name="w-list" select="w"/>
      </xsl:call-template>

      <xsl:call-template name="transition-ne">
        <xsl:with-param name="list" select="ne"/>
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
    <xsl:for-each select="$list">      
    <xsl:element name="transition">
      <xsl:attribute name="source">
        <xsl:call-template name="source">
          <xsl:with-param name="w-id" select="@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:value-of select="@id"/>
      </xsl:attribute>
      <xsl:element name="wordForm">
        <xsl:attribute name="form">
          <xsl:value-of select="surface"/>
        </xsl:attribute>
        <xsl:attribute name="tag"/>
        <xsl:attribute name="tokens">
          <xsl:value-of select="@id"/>
        </xsl:attribute>
        <xsl:element name="fs">
          <xsl:if test="@constant">
            <xsl:element name="f"><xsl:attribute name="name">constant</xsl:attribute><xsl:value-of select="@constant"/></xsl:element>              
          </xsl:if>
          <xsl:if test="@prio">
            <xsl:element name="f"><xsl:attribute name="name">prio-w</xsl:attribute><xsl:value-of select="@prio"/></xsl:element>
          </xsl:if>
        </xsl:element>
      </xsl:element>
    </xsl:element>
    </xsl:for-each>
  </xsl:template>

 <xsl:template name="transition-w-pos">
    <xsl:param name="list"/>
    <xsl:param name="w-list"/>
    <xsl:for-each select="$list">      
    <xsl:element name="transition-POS">
      <xsl:attribute name="source">
        <xsl:call-template name="source">
          <xsl:with-param name="w-id" select="../@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:value-of select="../@id"/>
      </xsl:attribute>
      <xsl:element name="wordForm">
        <xsl:attribute name="form">
          <xsl:value-of select="../surface"/>
        </xsl:attribute>
        <xsl:attribute name="tag">
          <xsl:value-of select="@tag"/>
        </xsl:attribute>
        <xsl:attribute name="tokens">
          <xsl:value-of select="../@id"/>
        </xsl:attribute>
        <xsl:element name="fs">
          <xsl:if test="@prio">
            <xsl:element name="f"><xsl:attribute name="name">prio-pos</xsl:attribute><xsl:value-of select="@prio"/></xsl:element>
          </xsl:if>
        </xsl:element>
      </xsl:element>
    </xsl:element>
    </xsl:for-each>
  </xsl:template>

 <xsl:template name="transition-w-typeinfo">
   <!-- ignore typeinfo/@id for now -->
    <xsl:param name="list"/>
    <xsl:param name="w-list"/>
    <xsl:for-each select="$list">      
    <xsl:element name="transition-TYPEINFO">
      <xsl:attribute name="source">
        <xsl:call-template name="source">
          <xsl:with-param name="w-id" select="../@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:value-of select="../@id"/>
      </xsl:attribute>
      <xsl:element name="wordForm">
        <xsl:attribute name="form">
          <xsl:value-of select="../surface"/>
        </xsl:attribute>
        <xsl:attribute name="tag"/>
        <xsl:attribute name="tokens">
          <xsl:value-of select="../@id"/>
        </xsl:attribute>
        <xsl:element name="fs">
          <xsl:apply-templates select="." mode="fs"/>
        </xsl:element>
      </xsl:element>
    </xsl:element>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="typeinfo" mode="fs">
    <xsl:element name="f"><xsl:attribute name="name">stem</xsl:attribute><xsl:value-of select="stem"/></xsl:element>
    <xsl:apply-templates select="infl" mode="fs"/>
    <xsl:apply-templates select="fsmod" mode="fs"/>
    <xsl:if test="@prio">
      <xsl:element name="f"><xsl:attribute name="name">prio-typeinfo</xsl:attribute><xsl:value-of select="@prio"/></xsl:element>
    </xsl:if>
    <xsl:if test="@baseform">
      <xsl:element name="f"><xsl:attribute name="name">baseform</xsl:attribute><xsl:value-of select="@baseform"/></xsl:element>
    </xsl:if>
  </xsl:template>
  
  <xsl:template name="transition-ne">
    <!-- ignore @id for now -->
    <xsl:param name="list"/>
    <xsl:param name="w-list"/>
    <xsl:for-each select="$list">      
    <xsl:element name="transition-NE">
      <xsl:attribute name="source">
        <xsl:call-template name="source-multi">
          <xsl:with-param name="w-ids" select="ref/@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:call-template name="target-multi">
          <xsl:with-param name="w-id" select="ref/@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:element name="wordForm">
        <!-- ommit form -->
        <xsl:attribute name="tag">
          <xsl:value-of select="pos/@tag"/>
        </xsl:attribute>
        <xsl:attribute name="tokens">
          <xsl:value-of select="ref/@id"/>
        </xsl:attribute>
        <xsl:element name="fs">
          <xsl:if test="pos/@prio">
            <xsl:element name="f"><xsl:attribute name="name">prio-pos</xsl:attribute><xsl:value-of select="pos/@prio"/></xsl:element>
            </xsl:if>
            <xsl:apply-templates select="typeinfo" mode="fs"/>
          </xsl:element>
        </xsl:element>
      </xsl:element>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="infl" mode="fs">
    <xsl:element name="f"><xsl:attribute name="name">infl</xsl:attribute><xsl:value-of select="@name"/></xsl:element>
  </xsl:template>

  <xsl:template match="fsmod" mode="fs">
    <xsl:element name="f">
      <xsl:attribute name="name">fsmod</xsl:attribute>
      <xsl:element name="f"><xsl:attribute name="name">path</xsl:attribute><xsl:value-of select="@path"/></xsl:element>
      <xsl:element name="f"><xsl:attribute name="name">value</xsl:attribute><xsl:value-of select="@value"/></xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template name="source-multi">
    <xsl:param name="w-ids"/>
    <xsl:param name="w-list"/>...</xsl:template>

  <xsl:template name="target-multi">
    <xsl:param name="w-ids"/>
    <xsl:param name="w-list"/>...</xsl:template>

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
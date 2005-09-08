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

  <xsl:template match="/">
    <xsl:apply-templates select="pet-input-chart"/>
  </xsl:template>
  
  <xsl:template match="pet-input-chart">
    <xsl:element name="maf">
      <xsl:attribute name="addressing">CharPos</xsl:attribute>
      <xsl:attribute name="creator">pic2maf-wf.xsl</xsl:attribute>
      <xsl:attribute name="date">...</xsl:attribute>
      <xsl:attribute name="language">en.US</xsl:attribute>
      <xsl:apply-templates mode="token"/>
      <xsl:apply-templates mode="wf" select="."/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="w" mode="token">
        <xsl:element name="token">
      <xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
      <xsl:attribute name="from"><xsl:value-of select="@cstart"/></xsl:attribute>
      <xsl:attribute name="to"><xsl:value-of select="@cend"/></xsl:attribute>
      <xsl:attribute name="value"><xsl:value-of select="surface"/></xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- no output -->
  <xsl:template match="ne" mode="token">
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
      
      <xsl:call-template name="transition-w-pos_typeinfo">
        <xsl:with-param name="w-list" select="w"/>
      </xsl:call-template>

      <!-- assume at most 1 pos and 1 typrinfo in ne element -->
      <xsl:call-template name="transition-ne">
        <xsl:with-param name="list" select="ne"/>
        <xsl:with-param name="w-list" select="w"/>
      </xsl:call-template>

    </xsl:element>
  </xsl:template>

  <!-- ========== -->
  <!-- = states = -->
  <!-- ========== -->

  <xsl:template match="w" mode="wf-state">
    <xsl:element name="state">
      <xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- no output -->
  <xsl:template match="ne" mode="wf-state">
  </xsl:template>

  <!-- =============== -->
  <!-- = transitions = -->
  <!-- =============== -->

  <xsl:template name="transition-w-pos_typeinfo">
    <xsl:param name="w-list"/>
    <xsl:for-each select="$w-list">
      <xsl:call-template name="transition-w-pos_typeinfo-aux">
        <xsl:with-param name="w" select="."/>
        <xsl:with-param name="pos-list" select="pos"/>
        <xsl:with-param name="typeinfo-list" select="typeinfo"/>
        <xsl:with-param name="w-list" select="$w-list"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="transition-w-pos_typeinfo-aux">
    <xsl:param name="pos-list"/>
    <xsl:param name="typeinfo-list"/>
    <xsl:param name="w"/>
    <xsl:param name="w-list"/>

    <xsl:variable name="pos-first" select="$pos-list[1]"/>
    <xsl:variable name="pos-rest" select="$pos-list[position()!=1]"/>

    <xsl:variable name="typeinfo-first" select="$typeinfo-list[1]"/>
    <xsl:variable name="typeinfo-rest" select="$typeinfo-list[position()!=1]"/>

    <xsl:element name="transition">
      <xsl:attribute name="source">
        <xsl:call-template name="source">
          <xsl:with-param name="w-id" select="$w/@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:value-of select="$w/@id"/>
      </xsl:attribute>
      <xsl:element name="wordForm">
        <xsl:attribute name="form">
          <xsl:value-of select="$w/surface"/>
        </xsl:attribute>
        <xsl:attribute name="tag">
          <xsl:value-of select="$pos-first/@tag"/>
        </xsl:attribute>
        <xsl:attribute name="tokens">
          <xsl:value-of select="$w/@id"/>
        </xsl:attribute>
        <xsl:element name="fs">
          <xsl:if test="$w/@constant">
            <xsl:element name="f"><xsl:attribute name="name">constant</xsl:attribute><xsl:value-of select="$w/@constant"/></xsl:element>              
          </xsl:if>
          <xsl:if test="$w/@prio">
            <xsl:element name="f"><xsl:attribute name="name">prio-w</xsl:attribute><xsl:value-of select="$w/@prio"/></xsl:element>
          </xsl:if>
        </xsl:element>
        <xsl:element name="fs">
          <xsl:apply-templates select="$pos-first" mode="fs"/>
          <xsl:apply-templates select="$typeinfo-first" mode="fs"/>
        </xsl:element>
      </xsl:element>
    </xsl:element>

    <xsl:if test="$pos-rest or $typeinfo-rest">
      <xsl:call-template name="transition-w-pos_typeinfo-aux">
        <xsl:with-param name="pos-list" select="$pos-rest"/>
        <xsl:with-param name="typeinfo-list" select="$typeinfo-rest"/>
        <xsl:with-param name="w" select="$w"/>
        <xsl:with-param name="w-list" select="$w-list"/>
      </xsl:call-template>
    </xsl:if>

  </xsl:template>

  <!-- assume at most 1 pos and 1 typrinfo in ne element -->
  <xsl:template name="transition-ne">
    <xsl:param name="list"/>
    <xsl:param name="w-list"/>
    <xsl:for-each select="$list">      
    <xsl:element name="transition">
      <xsl:attribute name="source">
        <xsl:call-template name="source">
          <xsl:with-param name="w-id" select="ref[1]/@id"/>
          <xsl:with-param name="w-list" select="$w-list"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:value-of select="ref[last()]/@id"/> 
      </xsl:attribute>
      <xsl:element name="wordForm">
        <!-- ommit form -->
        <xsl:attribute name="tag">
          <xsl:value-of select="pos/@tag"/>
        </xsl:attribute>
        <xsl:attribute name="tokens">
          <xsl:for-each select="ref">
            <xsl:value-of select="@id"/><xsl:text> </xsl:text>            
          </xsl:for-each>
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
  
  <!-- ====================== -->
  <!-- = feature structures = -->
  <!-- ====================== -->

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
  
  <xsl:template match="pos" mode="fs">
    <xsl:if test="@prio">
      <xsl:element name="f"><xsl:attribute name="name">prio-pos</xsl:attribute><xsl:value-of select="@prio"/></xsl:element>
    </xsl:if>
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

  <!-- ============================== -->
  <!-- = obtain source/target nodes = -->
  <!-- ============================== -->

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
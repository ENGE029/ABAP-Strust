REPORT /apmg/strust_info LINE-SIZE 255.

************************************************************************
* Trust Management: Certificate Information
*
* Lists all certificates in a PSE categorized by type:
* - Own certificate
* - Root certificates (self-signed)
* - Intermediate certificates (CA-signed)
* - Domain/Peer certificates
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS:
    p_cont TYPE psecontext DEFAULT 'SSLC' OBLIGATORY,
    p_appl TYPE ssfappl DEFAULT 'ANONYM' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
  PARAMETERS:
    p_passwd TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
  PARAMETERS:
    p_own    AS CHECKBOX DEFAULT abap_true,
    p_root   AS CHECKBOX DEFAULT abap_true,
    p_inter  AS CHECKBOX DEFAULT abap_true,
    p_domain AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.

  DATA(subrc) = cl_abap_pse=>authority_check( iv_activity = '03' ).
  IF subrc <> 0.
    MESSAGE 'You are not authorized to display certificates' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

START-OF-SELECTION.

  IF p_own IS INITIAL AND p_root IS INITIAL AND p_inter IS INITIAL AND p_domain IS INITIAL.
    MESSAGE 'No certificate types selected for display' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  DATA:
    psename     TYPE ssfpsename,
    profile     TYPE localfile,
    profiledata TYPE ssfpab.

  CALL FUNCTION 'SSFPSE_FILENAME'
    EXPORTING
      context       = p_cont
      applic        = p_appl
    IMPORTING
      psename       = psename
      profile       = profile
    EXCEPTIONS
      pse_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE 'PSE not found' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " Read the profile content
  profiledata = profile.

  TRY.

      " Display PSE header
      WRITE: / 'PSE Context/Application:' COLOR COL_KEY,
        AT 30 p_cont, '/', p_appl COLOR COL_POSITIVE.
      WRITE: / 'Date/Time:' COLOR COL_KEY,
        AT 30 |{ sy-datum DATE = ISO } { sy-uzeit TIME = ISO }|.
      SKIP 2.

      " Get and display own certificate
      IF p_own = abap_true.
        TRY.
            DATA cert_own TYPE /apmg/cl_strust=>ty_certattr.
            DATA cert_own_bin TYPE xstring.

            CALL FUNCTION 'SSFC_GET_OWNCERTIFICATE'
              EXPORTING
                profile               = profiledata
                profilepw             = p_passwd
              IMPORTING
                certificate           = cert_own_bin
              EXCEPTIONS
                ssf_krn_error         = 1
                ssf_krn_nomemory      = 2
                ssf_krn_nossflib      = 3
                ssf_krn_invalid_par   = 4
                ssf_krn_nocertificate = 5
                OTHERS                = 6.

            IF sy-subrc = 0.
              CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
                EXPORTING
                  certificate         = cert_own_bin
                IMPORTING
                  subject             = cert_own-subject
                  issuer              = cert_own-issuer
                  serialno            = cert_own-serialno
                  validfrom           = cert_own-validfrom
                  validto             = cert_own-validto
                EXCEPTIONS
                  ssf_krn_error       = 1
                  ssf_krn_nomemory    = 2
                  ssf_krn_nossflib    = 3
                  ssf_krn_invalid_par = 4
                  OTHERS              = 5.

              IF sy-subrc = 0.
                cert_own-date_from = cert_own-validfrom(8).
                cert_own-date_to   = cert_own-validto(8).

                WRITE / 'Own Certificate' COLOR COL_HEADING.
                ULINE.

                PERFORM display_certificate USING cert_own 'OWN'.
                SKIP 2.
              ENDIF.
            ELSE.
              WRITE / 'Own Certificate' COLOR COL_HEADING.
              ULINE.
              WRITE /5 'No own certificate found' COLOR COL_TOTAL.
              SKIP 2.
            ENDIF.

          CATCH cx_root INTO DATA(error_own).
            WRITE / 'Own Certificate' COLOR COL_HEADING.
            ULINE.
            WRITE: /5 'Error:' COLOR COL_TOTAL, error_own->get_text( ).
            SKIP 2.
        ENDTRY.
      ENDIF.

      " Get certificate list
      DATA:
        certlist TYPE ssfbintab,
        certs    TYPE /apmg/cl_strust=>ty_certattr_tt.

      CALL FUNCTION 'SSFC_GET_CERTIFICATELIST'
        EXPORTING
          profile               = profiledata
          profilepw             = p_passwd
        IMPORTING
          certificatelist       = certlist
        EXCEPTIONS
          ssf_krn_error         = 1
          ssf_krn_nomemory      = 2
          ssf_krn_nossflib      = 3
          ssf_krn_invalid_par   = 4
          ssf_krn_nocertificate = 5
          OTHERS                = 6.

      IF sy-subrc <> 0.
        WRITE / 'Error reading certificate list' COLOR COL_NEGATIVE.
        STOP.
      ENDIF.

      LOOP AT certlist ASSIGNING FIELD-SYMBOL(<certbin>).
        DATA cert TYPE /apmg/cl_strust=>ty_certattr.
        CLEAR cert.

        CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
          EXPORTING
            certificate         = <certbin>
          IMPORTING
            subject             = cert-subject
            issuer              = cert-issuer
            serialno            = cert-serialno
            validfrom           = cert-validfrom
            validto             = cert-validto
          EXCEPTIONS
            ssf_krn_error       = 1
            ssf_krn_nomemory    = 2
            ssf_krn_nossflib    = 3
            ssf_krn_invalid_par = 4
            OTHERS              = 5.

        IF sy-subrc = 0.
          cert-date_from = cert-validfrom(8).
          cert-date_to   = cert-validto(8).
          APPEND cert TO certs.
        ENDIF.
      ENDLOOP.

      IF lines( certs ) = 0.
        WRITE: / 'No certificates found in PSE' COLOR COL_TOTAL.
        STOP.
      ENDIF.

      " Categorize certificates
      DATA:
        certs_root   TYPE /apmg/cl_strust=>ty_certattr_tt,
        certs_inter  TYPE /apmg/cl_strust=>ty_certattr_tt,
        certs_domain TYPE /apmg/cl_strust=>ty_certattr_tt.

      LOOP AT certs ASSIGNING FIELD-SYMBOL(<cert>).

        " Parse subject to determine if it's a domain certificate
        DATA(subject_cn) = VALUE string( ).

        TRY.
            DATA(subject_dn) = /apmg/cl_distinguished_name=>parse( <cert>-subject ).

            IF line_exists( subject_dn[ key = 'CN' ] ).
              subject_cn = subject_dn[ key = 'CN' ]-name.
            ENDIF.
          CATCH cx_root.
            " If DN parsing fails, use empty CN
            CLEAR subject_cn.
        ENDTRY.

        " Categorize based on self-signed and CN pattern
        IF <cert>-subject = <cert>-issuer.
          " Root certificate (self-signed)
          APPEND <cert> TO certs_root.
        ELSEIF subject_cn CA '*.' OR subject_cn CA '.'.
          " Domain certificate (contains domain pattern with dot)
          APPEND <cert> TO certs_domain.
        ELSE.
          " Intermediate certificate
          APPEND <cert> TO certs_inter.
        ENDIF.

      ENDLOOP.

      " Sort all categories by expiry date (most urgent first)
      SORT certs_root BY date_to date_from.
      SORT certs_inter BY date_to date_from.
      SORT certs_domain BY date_to date_from.

      " Display Root Certificates
      IF p_root = abap_true AND lines( certs_root ) > 0.
        WRITE: / |Root Certificates ({ lines( certs_root ) })| COLOR COL_HEADING.
        ULINE.

        LOOP AT certs_root ASSIGNING FIELD-SYMBOL(<root>).
          PERFORM display_certificate USING <root> 'ROOT'.
        ENDLOOP.
        SKIP 2.
      ENDIF.

      " Display Intermediate Certificates
      IF p_inter = abap_true AND lines( certs_inter ) > 0.
        WRITE: / |Intermediate Certificates ({ lines( certs_inter ) })| COLOR COL_HEADING.
        ULINE.

        LOOP AT certs_inter ASSIGNING FIELD-SYMBOL(<inter>).
          PERFORM display_certificate USING <inter> 'INTER'.
        ENDLOOP.
        SKIP 2.
      ENDIF.

      " Display Domain Certificates
      IF p_domain = abap_true AND lines( certs_domain ) > 0.
        WRITE: / |Domain Certificates ({ lines( certs_domain ) })| COLOR COL_HEADING.
        ULINE.

        LOOP AT certs_domain ASSIGNING FIELD-SYMBOL(<domain>).
          PERFORM display_certificate USING <domain> 'DOMAIN'.
        ENDLOOP.
        SKIP 2.
      ENDIF.

      " Summary
      ULINE.
      WRITE: / 'Total Certificates:' COLOR COL_KEY,
        AT 30 lines( certs ) COLOR COL_POSITIVE.
      WRITE: / 'Root:' COLOR COL_KEY,
        AT 30 lines( certs_root ) COLOR COL_NORMAL.
      WRITE: / 'Intermediate:' COLOR COL_KEY,
        AT 30 lines( certs_inter ) COLOR COL_NORMAL.
      WRITE: / 'Domain:' COLOR COL_KEY,
        AT 30 lines( certs_domain ) COLOR COL_NORMAL.

    CATCH /apmg/cx_error INTO DATA(error).
      WRITE: / 'Error loading PSE or certificates:' COLOR COL_NEGATIVE, error->get_text( ).
  ENDTRY.

FORM display_certificate USING cert TYPE /apmg/cl_strust=>ty_certattr
                               cert_type TYPE string.

  DATA(days_until_expire) = cert-date_to - sy-datum.

  " Shorten subject if too long
  DATA(subject_short) = cert-subject.
  IF strlen( subject_short ) > 75.
    subject_short = subject_short(72) && '...'.
  ENDIF.

  " Display certificate details
  WRITE: /5 subject_short,
    AT 130 |{ cert-date_from DATE = ISO }|,
    AT 145 |{ cert-date_to DATE = ISO }|,
    AT 158 ''.

  " Display status text with appropriate color
  IF days_until_expire < 0.
    WRITE |EXPIRED ({ abs( days_until_expire ) } days ago)| COLOR COL_NEGATIVE.
  ELSEIF days_until_expire = 0.
    WRITE 'EXPIRES TODAY' COLOR COL_NEGATIVE.
  ELSEIF days_until_expire <= 7.
    WRITE |{ days_until_expire } days| COLOR COL_GROUP.
  ELSEIF days_until_expire <= 30.
    WRITE |{ days_until_expire } days| COLOR COL_TOTAL.
  ELSE.
    WRITE 'valid' COLOR COL_POSITIVE.
  ENDIF.

  " Display issuer on next line (indented)
  IF cert_type <> 'OWN'.
    DATA(issuer_short) = cert-issuer.
    IF strlen( issuer_short ) > 75.
      issuer_short = issuer_short(72) && '...'.
    ENDIF.
    WRITE: /10 'Issuer:' COLOR COL_KEY, issuer_short COLOR COL_NORMAL.
  ENDIF.

ENDFORM.

REPORT /apmg/strust_info LINE-SIZE 255.

"----------------------------------------------------------------------
" Trust Management: Certificate Information (Read-Only)
"
" Lists all certificates in a PSE categorized by type WITHOUT acquiring
" exclusive locks. Uses direct SSF read-only functions to avoid lock
" conflicts when other users have the PSE open.
"
" Copyright 2025 apm.to Inc. <https://apm.to>
" SPDX-License-Identifier: MIT
"----------------------------------------------------------------------

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

  CALL FUNCTION 'SSFPSE_PARAMETER'
    EXPORTING
      context       = p_cont
      applic        = p_appl
    EXCEPTIONS
      pse_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE 'PSE not found' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  " Display PSE header
  WRITE / 'PSE Context/Application:' COLOR COL_KEY.
  WRITE AT 30 p_cont COLOR COL_POSITIVE.
  WRITE ' / ' COLOR COL_POSITIVE.
  WRITE p_appl COLOR COL_POSITIVE.

  WRITE / 'Date/Time:' COLOR COL_KEY.
  WRITE AT 30 sy-datum COLOR COL_NORMAL.
  WRITE ' ' COLOR COL_NORMAL.
  WRITE sy-uzeit COLOR COL_NORMAL.
  SKIP 2.

  " Get and display own certificate (read-only, no lock)
  IF p_own = abap_true.
    PERFORM read_own_certificate USING p_cont p_appl p_passwd.
    SKIP 2.
  ENDIF.

  " Get certificate list (read-only, no lock)
  PERFORM read_certificate_list USING p_cont p_appl p_passwd p_root p_inter p_domain.

*---------- FORMS ----------

FORM read_own_certificate USING i_context TYPE psecontext
                                 i_applic TYPE ssfappl
                                 i_passwd TYPE string.

  DATA lv_cert_binary TYPE xstring.

  WRITE / 'Own Certificate' COLOR COL_HEADING.
  ULINE.

  CALL FUNCTION 'SSFC_GET_OWNCERTIFICATE'
    EXPORTING
      context    = i_context
      applic     = i_applic
      password   = i_passwd
    IMPORTING
      certificate = lv_cert_binary
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc <> 0.
    WRITE /5 'No own certificate found' COLOR COL_TOTAL.
    RETURN.
  ENDIF.

  PERFORM parse_and_display USING lv_cert_binary 'OWN'.

ENDFORM.

FORM read_certificate_list USING i_context TYPE psecontext
                                  i_applic TYPE ssfappl
                                  i_passwd TYPE string
                                  i_root TYPE abap_bool
                                  i_inter TYPE abap_bool
                                  i_domain TYPE abap_bool.

  DATA lt_cert_list TYPE STANDARD TABLE OF xstring WITH KEY table_line.
  DATA lt_certs_root TYPE /apmg/cl_strust=>ty_certattr_tt.
  DATA lt_certs_inter TYPE /apmg/cl_strust=>ty_certattr_tt.
  DATA lt_certs_domain TYPE /apmg/cl_strust=>ty_certattr_tt.
  DATA lv_cert_item TYPE xstring.
  DATA lv_subject TYPE string.
  DATA lv_issuer TYPE string.
  DATA lv_validfrom TYPE string.
  DATA lv_validto TYPE string.
  DATA lv_cert_attr TYPE /apmg/cl_strust=>ty_certattr.
  DATA total_certs TYPE i.

  CALL FUNCTION 'SSFC_GET_CERTIFICATELIST'
    EXPORTING
      context      = i_context
      applic       = i_applic
      password     = i_passwd
    IMPORTING
      certificate_list = lt_cert_list
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc <> 0 OR lines( lt_cert_list ) = 0.
    WRITE / 'No certificates found in PSE' COLOR COL_TOTAL.
    RETURN.
  ENDIF.

  " Process each certificate
  LOOP AT lt_cert_list INTO lv_cert_item.
    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate = lv_cert_item
      IMPORTING
        subject    = lv_subject
        issuer     = lv_issuer
        validfrom  = lv_validfrom
        validto    = lv_validto
      EXCEPTIONS
        OTHERS     = 1.

    IF sy-subrc = 0.
      CLEAR lv_cert_attr.
      lv_cert_attr-subject = lv_subject.
      lv_cert_attr-issuer = lv_issuer.
      lv_cert_attr-certificate = lv_cert_item.
      lv_cert_attr-validfrom = lv_validfrom.
      lv_cert_attr-validto = lv_validto.

      IF strlen( lv_validto ) >= 8.
        lv_cert_attr-date_to = lv_validto(8).
      ENDIF.
      IF strlen( lv_validfrom ) >= 8.
        lv_cert_attr-date_from = lv_validfrom(8).
      ENDIF.

      PERFORM categorize_cert USING lv_cert_attr lv_subject
        CHANGING lt_certs_root lt_certs_inter lt_certs_domain.
    ENDIF.
  ENDLOOP.

  " Sort all categories by expiry date
  SORT lt_certs_root BY date_to date_from.
  SORT lt_certs_inter BY date_to date_from.
  SORT lt_certs_domain BY date_to date_from.

  " Display Root Certificates
  IF i_root = abap_true AND lines( lt_certs_root ) > 0.
    WRITE / 'Root Certificates' COLOR COL_HEADING.
    WRITE AT 30 '(' COLOR COL_HEADING.
    WRITE lines( lt_certs_root ) COLOR COL_HEADING.
    WRITE ')' COLOR COL_HEADING.
    ULINE.
    PERFORM display_certs USING lt_certs_root 'ROOT'.
    SKIP 2.
  ENDIF.

  " Display Intermediate Certificates
  IF i_inter = abap_true AND lines( lt_certs_inter ) > 0.
    WRITE / 'Intermediate Certificates' COLOR COL_HEADING.
    WRITE AT 30 '(' COLOR COL_HEADING.
    WRITE lines( lt_certs_inter ) COLOR COL_HEADING.
    WRITE ')' COLOR COL_HEADING.
    ULINE.
    PERFORM display_certs USING lt_certs_inter 'INTER'.
    SKIP 2.
  ENDIF.

  " Display Domain Certificates
  IF i_domain = abap_true AND lines( lt_certs_domain ) > 0.
    WRITE / 'Domain Certificates' COLOR COL_HEADING.
    WRITE AT 30 '(' COLOR COL_HEADING.
    WRITE lines( lt_certs_domain ) COLOR COL_HEADING.
    WRITE ')' COLOR COL_HEADING.
    ULINE.
    PERFORM display_certs USING lt_certs_domain 'DOMAIN'.
    SKIP 2.
  ENDIF.

  " Summary
  total_certs = lines( lt_certs_root ) + lines( lt_certs_inter ) + lines( lt_certs_domain ).

  ULINE.
  WRITE / 'Total Certificates:' COLOR COL_KEY.
  WRITE AT 30 total_certs COLOR COL_POSITIVE.
  WRITE / 'Root:' COLOR COL_KEY.
  WRITE AT 30 lines( lt_certs_root ) COLOR COL_NORMAL.
  WRITE / 'Intermediate:' COLOR COL_KEY.
  WRITE AT 30 lines( lt_certs_inter ) COLOR COL_NORMAL.
  WRITE / 'Domain:' COLOR COL_KEY.
  WRITE AT 30 lines( lt_certs_domain ) COLOR COL_NORMAL.

ENDFORM.

FORM categorize_cert USING i_cert TYPE /apmg/cl_strust=>ty_certattr
                           i_subject TYPE string
                     CHANGING c_certs_root TYPE /apmg/cl_strust=>ty_certattr_tt
                              c_certs_inter TYPE /apmg/cl_strust=>ty_certattr_tt
                              c_certs_domain TYPE /apmg/cl_strust=>ty_certattr_tt.

  DATA lv_subject_cn TYPE string.

  TRY.
      DATA(lv_subject_dn) = /apmg/cl_distinguished_name=>parse( i_subject ).
      IF line_exists( lv_subject_dn[ key = 'CN' ] ).
        lv_subject_cn = lv_subject_dn[ key = 'CN' ]-name.
      ENDIF.
    CATCH cx_root.
      CLEAR lv_subject_cn.
  ENDTRY.

  IF i_cert-subject = i_cert-issuer.
    APPEND i_cert TO c_certs_root.
  ELSEIF lv_subject_cn CA '*.' OR lv_subject_cn CA '.'.
    APPEND i_cert TO c_certs_domain.
  ELSE.
    APPEND i_cert TO c_certs_inter.
  ENDIF.

ENDFORM.

FORM display_certs USING i_certs TYPE /apmg/cl_strust=>ty_certattr_tt
                        i_cert_type TYPE string.

  LOOP AT i_certs INTO DATA(lv_cert).
    PERFORM display_certificate USING lv_cert i_cert_type.
  ENDLOOP.

ENDFORM.

FORM display_certificate USING i_cert TYPE /apmg/cl_strust=>ty_certattr
                               i_cert_type TYPE string.

  DATA lv_days_expire TYPE i.
  DATA lv_subj_short TYPE string.
  DATA lv_iss_short TYPE string.

  lv_days_expire = i_cert-date_to - sy-datum.

  lv_subj_short = i_cert-subject.
  IF strlen( lv_subj_short ) > 75.
    lv_subj_short = lv_subj_short(72) && '...'.
  ENDIF.

  WRITE /5 lv_subj_short.
  WRITE AT 130 i_cert-date_from.
  WRITE AT 145 i_cert-date_to.
  WRITE AT 158 ''.

  IF lv_days_expire < 0.
    WRITE 'EXPIRED (' COLOR COL_NEGATIVE.
    WRITE abs( lv_days_expire ) COLOR COL_NEGATIVE.
    WRITE ' days ago)' COLOR COL_NEGATIVE.
  ELSEIF lv_days_expire = 0.
    WRITE 'EXPIRES TODAY' COLOR COL_NEGATIVE.
  ELSEIF lv_days_expire <= 7.
    WRITE lv_days_expire COLOR COL_GROUP.
    WRITE ' days' COLOR COL_GROUP.
  ELSEIF lv_days_expire <= 30.
    WRITE lv_days_expire COLOR COL_TOTAL.
    WRITE ' days' COLOR COL_TOTAL.
  ELSE.
    WRITE 'valid' COLOR COL_POSITIVE.
  ENDIF.

  IF i_cert_type <> 'OWN'.
    lv_iss_short = i_cert-issuer.
    IF strlen( lv_iss_short ) > 75.
      lv_iss_short = lv_iss_short(72) && '...'.
    ENDIF.
    WRITE /10 'Issuer:' COLOR COL_KEY.
    WRITE lv_iss_short COLOR COL_NORMAL.
  ENDIF.

ENDFORM.

FORM parse_and_display USING i_cert_binary TYPE xstring
                             i_cert_type TYPE string.

  DATA lv_subject TYPE string.
  DATA lv_issuer TYPE string.
  DATA lv_validfrom TYPE string.
  DATA lv_validto TYPE string.
  DATA lv_cert TYPE /apmg/cl_strust=>ty_certattr.

  CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
    EXPORTING
      certificate = i_cert_binary
    IMPORTING
      subject    = lv_subject
      issuer     = lv_issuer
      validfrom  = lv_validfrom
      validto    = lv_validto
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0.
    CLEAR lv_cert.
    lv_cert-subject = lv_subject.
    lv_cert-issuer = lv_issuer.
    lv_cert-certificate = i_cert_binary.
    lv_cert-validfrom = lv_validfrom.
    lv_cert-validto = lv_validto.

    IF strlen( lv_validto ) >= 8.
      lv_cert-date_to = lv_validto(8).
    ENDIF.
    IF strlen( lv_validfrom ) >= 8.
      lv_cert-date_from = lv_validfrom(8).
    ENDIF.

    PERFORM display_certificate USING lv_cert i_cert_type.
  ELSE.
    WRITE /5 'Cannot parse certificate' COLOR COL_NEGATIVE.
  ENDIF.

ENDFORM.

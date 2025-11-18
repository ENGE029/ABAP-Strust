REPORT /apmg/strust_info LINE-SIZE 255.

"----------------------------------------------------------------------
" Trust Management: Certificate Information (Read-Only)
"
" Lists all certificates in a PSE categorized by type.
" Uses read-only methods to avoid long-term locks on PSE.
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

  TRY.
      DATA(strust) = /apmg/cl_strust=>create(
        context     = p_cont
        application = p_appl
        password    = p_passwd )->load( ).

      " Get and display own certificate (read-only method after load)
      IF p_own = abap_true.
        PERFORM read_own_cert USING strust.
        SKIP 2.
      ENDIF.

      " Get certificate list (read-only method after load)
      PERFORM read_cert_list USING strust p_root p_inter p_domain.

    CATCH /apmg/cx_error INTO DATA(error).
      WRITE / 'Error loading PSE:' COLOR COL_NEGATIVE.
      WRITE error->get_text( ) COLOR COL_NEGATIVE.
  ENDTRY.

*---------- FORMS ----------

FORM read_own_cert USING i_strust TYPE REF TO /apmg/cl_strust.

  WRITE / 'Own Certificate' COLOR COL_HEADING.
  ULINE.

  TRY.
      DATA(cert_own) = i_strust->get_own_certificate( ).
      PERFORM display_certificate USING cert_own 'OWN'.
    CATCH /apmg/cx_error.
      WRITE /5 'No own certificate found' COLOR COL_TOTAL.
  ENDTRY.

ENDFORM.

FORM read_cert_list USING i_strust TYPE REF TO /apmg/cl_strust
                          i_root TYPE abap_bool
                          i_inter TYPE abap_bool
                          i_domain TYPE abap_bool.

  DATA certs_root TYPE /apmg/cl_strust=>ty_certattr_tt.
  DATA certs_inter TYPE /apmg/cl_strust=>ty_certattr_tt.
  DATA certs_domain TYPE /apmg/cl_strust=>ty_certattr_tt.
  DATA total_certs TYPE i.

  DATA(certs) = i_strust->get_certificate_list( ).

  IF lines( certs ) = 0.
    WRITE / 'No certificates found in PSE' COLOR COL_TOTAL.
    RETURN.
  ENDIF.

  " Categorize certificates
  LOOP AT certs ASSIGNING FIELD-SYMBOL(<cert>).
    DATA(subject_cn) = VALUE string( ).

    TRY.
        DATA(subject_dn) = /apmg/cl_distinguished_name=>parse( <cert>-subject ).
        IF line_exists( subject_dn[ key = 'CN' ] ).
          subject_cn = subject_dn[ key = 'CN' ]-name.
        ENDIF.
      CATCH cx_root.
        CLEAR subject_cn.
    ENDTRY.

    IF <cert>-subject = <cert>-issuer.
      APPEND <cert> TO certs_root.
    ELSEIF subject_cn CA '*.' OR subject_cn CA '.'.
      APPEND <cert> TO certs_domain.
    ELSE.
      APPEND <cert> TO certs_inter.
    ENDIF.
  ENDLOOP.

  " Sort all categories by expiry date
  SORT certs_root BY date_to date_from.
  SORT certs_inter BY date_to date_from.
  SORT certs_domain BY date_to date_from.

  " Display Root Certificates
  IF i_root = abap_true AND lines( certs_root ) > 0.
    WRITE / 'Root Certificates' COLOR COL_HEADING.
    WRITE AT 30 '(' COLOR COL_HEADING.
    WRITE lines( certs_root ) COLOR COL_HEADING.
    WRITE ')' COLOR COL_HEADING.
    ULINE.
    PERFORM display_certs USING certs_root 'ROOT'.
    SKIP 2.
  ENDIF.

  " Display Intermediate Certificates
  IF i_inter = abap_true AND lines( certs_inter ) > 0.
    WRITE / 'Intermediate Certificates' COLOR COL_HEADING.
    WRITE AT 30 '(' COLOR COL_HEADING.
    WRITE lines( certs_inter ) COLOR COL_HEADING.
    WRITE ')' COLOR COL_HEADING.
    ULINE.
    PERFORM display_certs USING certs_inter 'INTER'.
    SKIP 2.
  ENDIF.

  " Display Domain Certificates
  IF i_domain = abap_true AND lines( certs_domain ) > 0.
    WRITE / 'Domain Certificates' COLOR COL_HEADING.
    WRITE AT 30 '(' COLOR COL_HEADING.
    WRITE lines( certs_domain ) COLOR COL_HEADING.
    WRITE ')' COLOR COL_HEADING.
    ULINE.
    PERFORM display_certs USING certs_domain 'DOMAIN'.
    SKIP 2.
  ENDIF.

  " Summary
  total_certs = lines( certs_root ) + lines( certs_inter ) + lines( certs_domain ).

  ULINE.
  WRITE / 'Total Certificates:' COLOR COL_KEY.
  WRITE AT 30 total_certs COLOR COL_POSITIVE.
  WRITE / 'Root:' COLOR COL_KEY.
  WRITE AT 30 lines( certs_root ) COLOR COL_NORMAL.
  WRITE / 'Intermediate:' COLOR COL_KEY.
  WRITE AT 30 lines( certs_inter ) COLOR COL_NORMAL.
  WRITE / 'Domain:' COLOR COL_KEY.
  WRITE AT 30 lines( certs_domain ) COLOR COL_NORMAL.

ENDFORM.

FORM display_certs USING i_certs TYPE /apmg/cl_strust=>ty_certattr_tt
                        i_cert_type TYPE string.

  LOOP AT i_certs INTO DATA(cert).
    PERFORM display_certificate USING cert i_cert_type.
  ENDLOOP.

ENDFORM.

FORM display_certificate USING i_cert TYPE /apmg/cl_strust=>ty_certattr
                               i_cert_type TYPE string.

  DATA days_expire TYPE i.
  DATA subj_short TYPE string.
  DATA iss_short TYPE string.

  days_expire = i_cert-date_to - sy-datum.

  subj_short = i_cert-subject.
  IF strlen( subj_short ) > 75.
    subj_short = subj_short(72) && '...'.
  ENDIF.

  WRITE /5 subj_short.
  WRITE AT 130 i_cert-date_from.
  WRITE AT 145 i_cert-date_to.
  WRITE AT 158 ''.

  IF days_expire < 0.
    WRITE 'EXPIRED (' COLOR COL_NEGATIVE.
    WRITE abs( days_expire ) COLOR COL_NEGATIVE.
    WRITE ' days ago)' COLOR COL_NEGATIVE.
  ELSEIF days_expire = 0.
    WRITE 'EXPIRES TODAY' COLOR COL_NEGATIVE.
  ELSEIF days_expire <= 7.
    WRITE days_expire COLOR COL_GROUP.
    WRITE ' days' COLOR COL_GROUP.
  ELSEIF days_expire <= 30.
    WRITE days_expire COLOR COL_TOTAL.
    WRITE ' days' COLOR COL_TOTAL.
  ELSE.
    WRITE 'valid' COLOR COL_POSITIVE.
  ENDIF.

  IF i_cert_type <> 'OWN'.
    iss_short = i_cert-issuer.
    IF strlen( iss_short ) > 75.
      iss_short = iss_short(72) && '...'.
    ENDIF.
    WRITE /10 'Issuer:' COLOR COL_KEY.
    WRITE iss_short COLOR COL_NORMAL.
  ENDIF.

ENDFORM.

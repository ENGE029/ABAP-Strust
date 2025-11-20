REPORT /apmg/strust_cleaner LINE-SIZE 255.

************************************************************************
* Trust Management: Certificate Cleaner
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
  SELECT-OPTIONS s_subj FOR ('STRING') NO INTERVALS.
  SELECTION-SCREEN SKIP.
  PARAMETERS p_text TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
  PARAMETERS:
    p_passwd TYPE string LOWER CASE,
    p_expire AS CHECKBOX DEFAULT abap_true,
    p_dupl   AS CHECKBOX DEFAULT abap_true,
    p_super  AS CHECKBOX DEFAULT abap_true,
    p_root   AS CHECKBOX,
    p_inter  AS CHECKBOX,
    p_main   AS CHECKBOX,
    p_test   AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.

  DATA(subrc) = cl_abap_pse=>authority_check( iv_activity = '01' )
    + cl_abap_pse=>authority_check( iv_activity = '02' )
    + cl_abap_pse=>authority_check( iv_activity = '06' ).
  IF subrc <> 0.
    MESSAGE 'You are not authorized to clean certificates' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

START-OF-SELECTION.

  IF p_expire IS INITIAL AND p_dupl IS INITIAL AND p_super IS INITIAL.
    MESSAGE 'No cleanup options selected' TYPE 'I' DISPLAY LIKE 'E'.
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

  TRY.
      DATA(strust) = /apmg/cl_strust=>create(
        context     = p_cont
        application = p_appl
        password    = p_passwd )->load( ).

      DATA(certs) = strust->get_certificate_list( ).
    CATCH /apmg/cx_error INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
  ENDTRY.

  SORT certs BY date_to date_from subject.

  DATA:
    certs_to_remove TYPE /apmg/cl_strust=>ty_certattr_tt,
    total_removed   TYPE i,
    total_expired   TYPE i,
    total_dupl      TYPE i,
    total_super     TYPE i.

  " Display current certificates
  WRITE: / 'Current certificates in PSE:' COLOR COL_HEADING.
  SKIP.

  LOOP AT certs ASSIGNING FIELD-SYMBOL(<cert>) WHERE subject IN s_subj.

    DATA(days_until_expire) = <cert>-date_to - sy-datum.
    DATA(cert_type) = VALUE string( ).
    DATA(subject_cn) = VALUE string( ).
    DATA(reason) = VALUE string( ).
    DATA(lv_color) = VALUE i( ).
    DATA(to_remove) = abap_false.

    TRY.
        DATA(dn) = /apmg/cl_distinguished_name=>parse( <cert>-subject ).
        IF line_exists( dn[ key = 'CN' ] ).
          subject_cn = dn[ key = 'CN' ]-name.
        ENDIF.
      CATCH cx_root.
        CLEAR subject_cn.
    ENDTRY.

    " Categorize certificate
    IF <cert>-subject = <cert>-issuer.
      cert_type = 'ROOT'.
    ELSEIF subject_cn CA '*.' OR subject_cn CA '.'.
      cert_type = 'DOMAIN'.
    ELSE.
      cert_type = 'INTER'.
    ENDIF.

    " Check if expired
    IF p_expire = abap_true AND days_until_expire < 0.
      to_remove = abap_true.
      reason = 'expired'.
      lv_color = col_negative.
      total_expired = total_expired + 1.
    ENDIF.

    " Check for duplicates (same subject, issuer, but older validTo date)
    IF p_dupl = abap_true.
      LOOP AT certs ASSIGNING FIELD-SYMBOL(<cert_dup>)
        WHERE subject = <cert>-subject
          AND issuer  = <cert>-issuer
          AND date_to > <cert>-date_to.
        to_remove = abap_true.
        reason = 'duplicate (newer exists)'.
        lv_color = col_total.
        total_dupl = total_dupl + 1.
        EXIT.
      ENDLOOP.
    ENDIF.

    " Check for superfluous certificates
    " Domain certs are superfluous if their issuer (intermediate) exists
    " Intermediate certs are superfluous if their issuer (root) exists
    " Root certs are never superfluous (they're trust anchors)
    IF p_super = abap_true AND to_remove = abap_false.
      IF cert_type = 'DOMAIN' OR cert_type = 'INTER'.
        " Check if the issuing certificate exists in PSE
        LOOP AT certs ASSIGNING FIELD-SYMBOL(<cert_issuer>)
          WHERE subject = <cert>-issuer.
          " Issuer found: this certificate is superfluous (issuer is sufficient for validation)
          to_remove = abap_true.
          IF cert_type = 'INTER'.
            reason = 'superfluous (root CA present)'.
          ELSE.
            reason = 'superfluous (intermediate present)'.
          ENDIF.
          lv_color = col_group.
          total_super = total_super + 1.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " Apply certificate type filter
    IF to_remove = abap_true.
      IF ( cert_type = 'ROOT' AND p_root IS INITIAL ) OR
         ( cert_type = 'INTER' AND p_inter IS INITIAL ) OR
         ( cert_type = 'DOMAIN' AND p_main IS INITIAL ).
        to_remove = abap_false.
        CLEAR: reason, lv_color.
      ENDIF.
    ENDIF.

    " Display certificate
    WRITE: / <cert>-subject,
      AT 130 |{ <cert>-date_from DATE = ISO }|,
      AT 145 |{ <cert>-date_to DATE = ISO }|,
      AT 158 ''.

    IF to_remove = abap_true.
      WRITE reason COLOR = lv_color.
      APPEND <cert> TO certs_to_remove.
      total_removed = total_removed + 1.
    ELSEIF days_until_expire > 30.
      WRITE 'valid' COLOR COL_POSITIVE.
    ELSEIF days_until_expire > 7.
      WRITE 'expires in a month' COLOR COL_TOTAL.
    ELSEIF days_until_expire > 0.
      WRITE 'expires in a week' COLOR COL_GROUP.
    ELSEIF days_until_expire = 0.
      WRITE 'expires today' COLOR COL_NEGATIVE.
    ELSE.
      WRITE 'expired' COLOR COL_NEGATIVE.
    ENDIF.

  ENDLOOP.

  SKIP.
  ULINE.
  SKIP.

  " Display summary
  IF total_removed = 0.
    WRITE: / 'No certificates to remove' COLOR COL_POSITIVE.
    STOP.
  ENDIF.

  WRITE: / 'Summary:' COLOR COL_HEADING.
  SKIP.
  WRITE: /5 'Total certificates:' COLOR COL_NORMAL, AT 40 lines( certs ).
  IF total_expired > 0.
    WRITE: /5 'Expired certificates:' COLOR COL_NEGATIVE, AT 40 total_expired.
  ENDIF.
  IF total_dupl > 0.
    WRITE: /5 'Duplicate certificates:' COLOR COL_TOTAL, AT 40 total_dupl.
  ENDIF.
  IF total_super > 0.
    WRITE: /5 'Superfluous certificates:' COLOR COL_GROUP, AT 40 total_super.
  ENDIF.
  WRITE: /5 'Certificates to remove:' COLOR COL_GROUP, AT 40 total_removed.

  SKIP.
  ULINE.

  IF p_test = abap_true.
    WRITE: / 'Test run' COLOR COL_TOTAL, '(changes were not saved)'.
    STOP.
  ENDIF.

  " Remove certificates
  TRY.
      LOOP AT certs_to_remove ASSIGNING <cert>.
        strust->remove(
          subject = <cert>-subject
          comment = p_text ).
      ENDLOOP.

      SKIP.
      WRITE: / 'Cleanup completed successfully' COLOR COL_POSITIVE.
      WRITE: /5 |{ total_removed } certificate(s) removed|.

    CATCH /apmg/cx_error INTO error.
      SKIP.
      WRITE: / 'Error during cleanup:' COLOR COL_NEGATIVE, error->get_text( ).
  ENDTRY.

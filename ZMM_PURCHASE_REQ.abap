*&---------------------------------------------------------------------*
*& Report  ZMM_PURCHASE_REQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmm_purchase_req.

TABLES: eban, ebkn, t023t, t007a, lfa1.

TYPES: BEGIN OF ty_alv ,
         werks TYPE eban-werks,  "Plant
         banfn TYPE eban-banfn,  "Purchase Req.
         bnfpo TYPE eban-bnfpo,  "Requisn Item
         txz01 TYPE eban-txz01,  "Short Text
         afnam TYPE eban-afnam,  "Requisitioner
         menge TYPE ebkn-menge,  "Quantity Requested
         meins TYPE eban-meins,  "Unit of Measure
         lifnr TYPE eban-lifnr,  "Desired Vendor
         lfdat TYPE eban-lfdat,  "Delivery Date
         prctr TYPE ebkn-prctr,  "Profit Center
         matkl TYPE t023t-matkl, "Material Group
         mwskz TYPE t007s-text1, "Tax Code
         headt TYPE char70,  "Header text
         check TYPE char1,
       END OF ty_alv.

DATA: lt_alv TYPE TABLE OF ty_alv,
      ls_alv TYPE ty_alv.


DATA: ls_t007s TYPE t007s.
DATA: lt_lfa1 TYPE TABLE OF lfa1,
      lt_adr6 TYPE TABLE OF adr6,
      ls_lfa1 TYPE lfa1,
      ls_adr6 TYPE adr6.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_werks LIKE eban-werks DEFAULT '1841' OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION .

  "Requisiciones de compra en planta 0547
  SELECT *
   FROM eban
   INTO TABLE @DATA(lt_eban)
    WHERE werks EQ @p_werks
      AND banpr EQ '05'
      AND matnr EQ ' '.

  IF sy-subrc EQ 0.

    SELECT *
      FROM ebkn
      INTO TABLE @DATA(lt_ebkn)
      FOR ALL ENTRIES IN @lt_eban
      WHERE banfn EQ @lt_eban-banfn
        AND kokrs EQ 'CEMM'.

    SELECT *
      FROM t007s
      INTO TABLE @DATA(lt_t007s)
        WHERE kalsm EQ 'TAXMX'
          AND spras EQ 'E'.

    SELECT *
      FROM lfa1
      INTO TABLE lt_lfa1
      FOR ALL ENTRIES IN lt_eban
      WHERE lifnr EQ lt_eban-lifnr.


  ENDIF.

  LOOP AT lt_eban INTO DATA(ls_eban).

    ls_alv-werks  = ls_eban-werks.
    ls_alv-banfn  = ls_eban-banfn.
    ls_alv-bnfpo  = ls_eban-bnfpo.
    ls_alv-txz01  = ls_eban-txz01.
    ls_alv-afnam  = ls_eban-afnam.
    ls_alv-menge  = ls_eban-menge.
    ls_alv-meins  = ls_eban-meins.
    ls_alv-lifnr  = ls_eban-lifnr.
    ls_alv-lfdat  = ls_eban-lfdat.
    ls_alv-lfdat  = ls_eban-lfdat + ls_eban-badat.
    ls_alv-check  = ' '.

    READ TABLE lt_ebkn INTO DATA(ls_ebkn) WITH KEY banfn = ls_eban-banfn.
    IF sy-subrc EQ 0.
      ls_alv-prctr  = ls_ebkn-prctr.
    ENDIF.

    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_eban-lifnr.
    IF sy-subrc EQ 0.

      CASE ls_lfa1-lifnr(1).
        WHEN 3.
          IF ls_lfa1-land1 NE 'MX'.

            READ TABLE lt_t007s INTO ls_t007s WITH KEY mwskz = 'V3'.
            IF sy-subrc EQ 0.
              CONCATENATE ls_t007s-mwskz ls_t007s-text1 INTO ls_alv-mwskz.
            ENDIF.

          ELSE.

            READ TABLE lt_t007s INTO ls_t007s WITH KEY mwskz = 'V0'.
            IF sy-subrc EQ 0.
              CONCATENATE ls_t007s-mwskz ls_t007s-text1 INTO ls_alv-mwskz.
            ENDIF.

          ENDIF.
        WHEN 1 OR 9.
          "Por defecto ME21N
      ENDCASE.

    ENDIF.

*
    ls_alv-matkl  = 'SERV'.
    CONCATENATE ls_alv-afnam ' / ' ls_alv-prctr INTO ls_alv-headt.

    APPEND ls_alv TO lt_alv.
    CLEAR ls_alv.
  ENDLOOP.

  IF lt_alv[] IS NOT INITIAL.
    PERFORM alv_report USING lt_alv[].      "LLamado al ALV
  ELSE.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*&---------------------------------------------------------------------*
  TYPE-POOLS: slis.

  DATA: lf_sp_group  TYPE slis_t_sp_group_alv,  "MANEJAR GRUPOS DE CAMPOS
        lf_layout    TYPE slis_layout_alv,      "MANEJAR DISEÑO DE LAYOUT
        it_topheader TYPE slis_t_listheader,    "MANEJAR CABECERA DEL REP
        wa_top       LIKE LINE OF it_topheader. "LÍNEA PARA CABECERA

  DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.     "Parametros del catalogo


*///////////////////////////////      ALV PERFORMS
FORM alv_report  USING  pp_itab LIKE lt_alv[].

  PERFORM sp_group_build USING lf_sp_group[].         "1
  PERFORM alv_ini_fieldcat.                           "2
  PERFORM layout_build USING lf_layout.               "3
  PERFORM alv_listado USING pp_itab[].                "4

ENDFORM.

*///////////////////////////////      ALV PERFORM_1
FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = text-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.


*///////////////////////////////      ALV PERFORM_2
FORM alv_ini_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '6'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'WERKS'.
  alv_git_fieldcat-seltext_l = 'Plant'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'BANFN'.
  alv_git_fieldcat-seltext_l = 'Purchase Req.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'BNFPO'.
  alv_git_fieldcat-seltext_s = 'Req. Item'.
  alv_git_fieldcat-seltext_l = 'Requisition Item'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '40'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'TXZ01'.
  alv_git_fieldcat-seltext_l = 'Short Text'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'AFNAM'.
  alv_git_fieldcat-seltext_l = 'Requisitioner'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'PRCTR'.
  alv_git_fieldcat-seltext_l = 'Profit Center'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MENGE'.
  alv_git_fieldcat-seltext_s = 'Quantity'.
  alv_git_fieldcat-seltext_l = 'Quantity Requested'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MEINS'.
  alv_git_fieldcat-seltext_l = 'UoM'.
  alv_git_fieldcat-seltext_l = 'Unit of Measure'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'LIFNR'.
  alv_git_fieldcat-seltext_l = 'Desired Vendor'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MATKL'.
  alv_git_fieldcat-seltext_l = 'Material Group'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MWSKZ'.
  alv_git_fieldcat-seltext_l = 'Tax Code'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'LFDAT'.
  alv_git_fieldcat-seltext_l = 'Delivery Date'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '25'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'HEADT'.
  alv_git_fieldcat-seltext_l = 'Header text'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '3'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'CHECK'.
  alv_git_fieldcat-seltext_l = ''.
  alv_git_fieldcat-checkbox  = 'X'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.


ENDFORM.

*///////////////////////////////      ALV PERFORM_3
FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.

*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.

*///////////////////////////////      ALV PERFORM_4
FORM alv_listado  USING ppp_itab LIKE lt_alv[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = lf_layout
      it_fieldcat              = alv_git_fieldcat[]
*     it_special_groups        = lf_sp_group
      i_save                   = 'X'
    TABLES
      t_outtab                 = ppp_itab.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1
               sy-msgv2
               sy-msgv3
               sy-msgv4.
  ENDIF.

ENDFORM.

*///////////////////////////////      Standard Buttons
FORM pf_status_set USING extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD'.

ENDFORM.

FORM generate_oc.

ENDFORM.

FORM send_mail.


data: ans type char1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Title POPUP_TO_CONFIRM'
      text_question         = 'Click Cancel to Exit'
      text_button_1         = 'OK'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'CANCEL'
      icon_button_2         = 'ICON_CANCEL'
      display_cancel_button = ' '
      popup_type            = 'ICON_MESSAGE_ERROR'
    IMPORTING
      answer                = ans.
  IF ans = 2.
    LEAVE PROGRAM.
  ENDIF.


  DATA: w_size TYPE sood-objlen,
        w_file TYPE xstring.

  DATA: mail_prov   TYPE ad_smtpadr,
        mail_user   TYPE ad_smtpadr,
        mail_comp   TYPE ad_smtpadr,
        lv_filename TYPE string.

  DATA: it_text TYPE soli_tab,
        wa_text LIKE LINE OF it_text.


  wa_text-line = '<HTML><BODY>'.
  APPEND wa_text TO it_text.
  wa_text-line = 'Buen día,'.
  APPEND wa_text TO it_text.
  wa_text-line = '<BR></BR>'.
  APPEND wa_text TO it_text.
  wa_text-line = 'Envío nueva orden de compra.'.
  APPEND wa_text TO it_text.
  wa_text-line = 'Favor de confirmar recibido.'.
  APPEND wa_text TO it_text.
  wa_text-line = 'Para cualquier detalle, favor ponerse en contacto con el usuario.'.
  APPEND wa_text TO it_text.
  wa_text-line = '<BR></BR>'.
  APPEND wa_text TO it_text.
  wa_text-line = '<i>¡RECUERDA!</i>'.
  APPEND wa_text TO it_text.
  wa_text-line = 'Al momento de entregar material, por favor hacerlo <u>exclusivamente</u> en <b>ventanilla de Almacén,</b> o bien, si es un <b>servicio</b> entregar facturas directamente al usuario.'.
  APPEND wa_text TO it_text.
  wa_text-line = '<BR></BR>'.
  APPEND wa_text TO it_text.
  wa_text-line = 'Saludos.'.
  APPEND wa_text TO it_text.
  wa_text-line = '<BR></BR>'.
  APPEND wa_text TO it_text.
  wa_text-line = '<b>Arturo Luna<b>'.
  APPEND wa_text TO it_text.
  wa_text-line = 'Compras'.
  APPEND wa_text TO it_text.


* to reflect the data changed into internal table
  DATA : ref_grid TYPE REF TO cl_gui_alv_grid. "new

  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.

  IF NOT ref_grid IS INITIAL.
    CALL METHOD ref_grid->check_changed_data.
  ENDIF.

  SORT lt_alv BY check DESCENDING.

  DELETE lt_alv WHERE check EQ ' '.

  REFRESH: lt_lfa1[].
  CLEAR: ls_lfa1.

  SELECT *
   FROM lfa1
   INTO TABLE lt_lfa1
   FOR ALL ENTRIES IN lt_alv
   WHERE lifnr EQ lt_alv-lifnr.

  SELECT *
    FROM adr6
    INTO TABLE lt_adr6
    FOR ALL ENTRIES IN lt_lfa1
    WHERE addrnumber EQ lt_lfa1-adrnr.
  "Obtener lt_adr6-SMTP_ADDR (email proveedor)

  LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE check = 'X'.

    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = <fs_alv>-lifnr.
    IF sy-subrc EQ 0.

      READ TABLE lt_adr6 INTO ls_adr6 WITH KEY addrnumber = ls_lfa1-adrnr.
      IF sy-subrc EQ 0.
        mail_prov = ls_adr6-smtp_addr.
        mail_user = ls_adr6-smtp_addr. "temporal
        mail_comp = ls_adr6-smtp_addr. "temporal

        CALL FUNCTION 'ZFM_SEND_EMAIL_ATTACH'
          EXPORTING
            email    = mail_prov
            subject  = '45007_TEST 43523 PROVEEDOR SYSTEMS SA. DE CV.'
            filename = lv_filename
            bin_file = w_file
            bin_size = w_size
            format   = 'PDF'
          TABLES
            body     = it_text.

*        CALL FUNCTION 'ZFM_SEND_EMAIL_ATTACH'
*          EXPORTING
*            email    = mail_user
*            subject  = '45007_TEST 43523 PROVEEDOR SYSTEMS SA. DE CV.'
*            filename = lv_filename
*            bin_file = w_file
*            bin_size = w_size
*            format   = 'PDF'
*          TABLES
*            body     = it_text.

*         CALL FUNCTION 'ZFM_SEND_EMAIL_ATTACH'
*          EXPORTING
*            email    = mail_comp
*            subject  = '45007_TEST 43523 PROVEEDOR SYSTEMS SA. DE CV.'
*            filename = lv_filename
*            bin_file = w_file
*            bin_size = w_size
*            format   = 'PDF'
*          TABLES
*            body     = it_text.

      ENDIF.

    ENDIF.

  ENDLOOP.



*  lv_email    = lt_adr6-smtp_addr.
*  lv_filename = mkpf-mblnr.




ENDFORM.


FORM user_command USING p_ucomm TYPE sy-ucomm
                         p_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN '&GEN_OC'.
      PERFORM generate_oc.
    WHEN '&MAIL'.
      PERFORM send_mail.
  ENDCASE.
ENDFORM.

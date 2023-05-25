*&---------------------------------------------------------------------*
*& Report  ZMM_PURCHASE_REQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmm_purchase_req.

TABLES: eban, ebkn, t023t, t007a, lfa1.

DATA: ref_grid TYPE REF TO cl_gui_alv_grid.

TYPES: BEGIN OF ty_alv ,
         werks   TYPE eban-werks,  "Plant
         banfn   TYPE eban-banfn,  "Purchase Req.
         bnfpo   TYPE eban-bnfpo,  "Requisn Item
         txz01   TYPE eban-txz01,  "Short Text
         afnam   TYPE eban-afnam,  "Requisitioner
         menge   TYPE ebkn-menge,  "Quantity Requested
         meins   TYPE eban-meins,  "Unit of Measure
         lifnr   TYPE eban-lifnr,  "Desired Vendor
         lfdat   TYPE eban-lfdat,  "Delivery Date
         prctr   TYPE ebkn-prctr,  "Profit Center
         matkl   TYPE t023t-matkl, "Material Group
         mwskz   TYPE t007s-mwskz, "Tax Code
         mwskz_t TYPE t007s-text1, "Tax Code Desc.
         headt   TYPE char70,      "Header text
         check   TYPE char1,       "Check Box
         ocgen   TYPE bstnr,       "Orden de Compra Generada
         email   TYPE zemail_sent, "Email enviado

         "Anexados en fondo para procesar BAPI
         bsart   TYPE eban-bsart,
         ekorg   TYPE eban-ekorg,
         ekgrp   TYPE eban-ekgrp,
         waers   TYPE eban-waers,
         preis   TYPE eban-preis,
         knttp   TYPE eban-knttp,
         bednr   TYPE eban-bednr,
         wepos   TYPE eban-wepos,
         repos   TYPE eban-repos,
         sakto   TYPE ebkn-sakto,
         kokrs   TYPE ebkn-kokrs,
         kostl   TYPE ebkn-kostl,
         aufnr   TYPE ebkn-aufnr,
         anln1   TYPE ebkn-anln1,
         ernam   TYPE eban-ernam,
         bname   TYPE usr21-bname,
         adrnr   TYPE lfa1-adrnr,
         name1   TYPE lfa1-name1,
         name2   TYPE lfa1-name2,
         land1   TYPE lfa1-land1,
       END OF ty_alv.

DATA: lt_alv   TYPE TABLE OF ty_alv,
      lt_genoc TYPE TABLE OF ty_alv,
      ls_alv   TYPE ty_alv.

DATA: lt_eban  TYPE TABLE OF eban,
      lt_ebkn  TYPE TABLE OF ebkn,
      lt_t007s TYPE TABLE OF t007s,
      lt_lfa1  TYPE TABLE OF lfa1,
      lt_adr6  TYPE TABLE OF adr6.

DATA: ls_t007s TYPE t007s,
      ls_lfa1  TYPE lfa1,
      ls_adr6  TYPE adr6.

DATA: lt_reqoc TYPE TABLE OF zmm_reqoc,
      ls_reqoc TYPE zmm_reqoc.

DATA: lv_days TYPE pea_scrdd.

DATA: lt_auxoc TYPE TABLE OF ty_alv,
      ls_auxoc TYPE ty_alv.

DATA: lt_auxmail TYPE TABLE OF ty_alv,
      ls_auxmail TYPE ty_alv.

DATA: lt_reqemail TYPE TABLE OF zmm_reqemail,
      ls_reqemail TYPE zmm_reqemail.

DATA: lt_usr21 TYPE TABLE OF usr21,
      ls_usr21 TYPE usr21.

DATA: lv_answer TYPE char1.
DATA: message_e TYPE char255.
DATA: any_ocgen TYPE char1.

DATA: lt_zmm_reqoc TYPE TABLE OF zmm_reqoc.

DATA: FLAG_OC TYPE CHAR1,
      FLAG_MAIL TYPE CHAR1.

*&---------------------------------------------------------------------*

CONSTANTS: c_x        VALUE 'X',
           gc_refresh TYPE syucomm VALUE '&REFRESH'.

TYPE-POOLS: slis.

DATA: lf_sp_group   TYPE slis_t_sp_group_alv,  "MANEJAR GRUPOS DE CAMPOS
      lf_layout     TYPE slis_layout_alv,      "MANEJAR DISEÑO DE LAYOUT
      it_topheader  TYPE slis_t_listheader,    "MANEJAR CABECERA DEL REP
      wa_top        LIKE LINE OF it_topheader, "LÍNEA PARA CABECERA
      lt_event_exit TYPE slis_t_event_exit,    "Event
      ls_event_exit TYPE slis_event_exit.      "Event

DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.     "Parametros del catalogo

SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_werks LIKE eban-werks DEFAULT '1841' OBLIGATORY.
*SELECT-OPTIONS: p_banfn FOR eban-banfn.
SELECTION-SCREEN: END OF BLOCK b1.


*---------------------------------------------------------------------*

START-OF-SELECTION .

  PERFORM show_data.

*---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM f_display_data.

*---------------------------------------------------------------------*

FORM show_data.

  REFRESH: lt_zmm_reqoc,
           lt_eban,
           lt_reqoc,
           lt_ebkn,
           lt_t007s,
           lt_lfa1,
           lt_adr6,
           lt_alv.


  SELECT *
   FROM zmm_reqoc
   INTO TABLE lt_zmm_reqoc
     WHERE ocgen NE ' '
       AND email EQ ' '.

  SELECT *
   FROM eban
   APPENDING TABLE lt_eban
    WHERE werks EQ p_werks
*      AND banfn IN @p_banfn
      AND ebeln EQ ' '
      AND banpr EQ '05'
      AND matnr EQ ' '
      AND loekz EQ ' '.

  IF lt_eban[] IS NOT INITIAL.

    IF lt_zmm_reqoc[] IS NOT INITIAL.

      SELECT *
       FROM eban
       APPENDING TABLE lt_eban
        FOR ALL ENTRIES IN lt_zmm_reqoc
        WHERE banfn EQ lt_zmm_reqoc-banfn
          AND ebeln EQ lt_zmm_reqoc-ocgen
          AND banpr EQ '05'
          AND matnr EQ ' '
          AND loekz EQ ' '.

    ENDIF.

    SELECT *
      FROM zmm_reqoc
      INTO TABLE lt_reqoc
      FOR ALL ENTRIES IN lt_eban
      WHERE banfn EQ lt_eban-banfn.

    SELECT *
      FROM ebkn
      INTO TABLE lt_ebkn
      FOR ALL ENTRIES IN lt_eban
      WHERE banfn EQ lt_eban-banfn
        AND kokrs EQ 'CEMM'.

    SELECT *
      FROM t007s
      INTO TABLE lt_t007s
        WHERE kalsm EQ 'TAXMX'
          AND spras EQ 'E'.

    SELECT *
      FROM lfa1
      INTO TABLE lt_lfa1
      FOR ALL ENTRIES IN lt_eban
      WHERE lifnr EQ lt_eban-lifnr.

    IF lt_lfa1[] IS NOT INITIAL.

      SELECT *
        FROM adr6
        INTO TABLE lt_adr6
        FOR ALL ENTRIES IN lt_lfa1
        WHERE addrnumber EQ lt_lfa1-adrnr.

    ENDIF.

  ENDIF.


*  SORT lt_eban BY banfn bnfpo DESCENDING.
  SORT lt_eban BY banfn bnfpo ASCENDING.

  LOOP AT lt_eban INTO DATA(ls_eban).

    "Anexados en fondo para procesar BAPI
    ls_alv-bsart = ls_eban-bsart.
    ls_alv-ekorg = ls_eban-ekorg.
    ls_alv-ekgrp = ls_eban-ekgrp.
    ls_alv-lifnr = ls_eban-lifnr.
    ls_alv-waers = ls_eban-waers.
*    ls_alv-matnr = ls_eban-matnr.
    ls_alv-preis = ls_eban-preis.
    ls_alv-knttp = ls_eban-knttp.
    ls_alv-bednr = ls_eban-bednr.
    ls_alv-wepos = ls_eban-wepos.
    ls_alv-repos = ls_eban-repos.
    ls_alv-ernam = ls_eban-ernam.
    ls_alv-bname = sy-uname.

    "Columnas a mostrar en ALV
    ls_alv-werks = ls_eban-werks.
    ls_alv-banfn = ls_eban-banfn.
    ls_alv-bnfpo = ls_eban-bnfpo.
    ls_alv-txz01 = ls_eban-txz01.
    ls_alv-afnam = ls_eban-afnam.
    ls_alv-menge = ls_eban-menge.
    ls_alv-meins = ls_eban-meins.
    ls_alv-lifnr = ls_eban-lifnr.

*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = ls_eban-lifnr
*      IMPORTING
*        output = ls_alv-lifnr.

*    ls_alv-lfdat = ls_eban-lfdat.
    CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
      EXPORTING
        date1         = sy-datum
        date2         = ls_eban-badat
        output_format = '02'
      IMPORTING
        days          = lv_days.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    ls_alv-lfdat = lv_days + ls_eban-lfdat.
    ls_alv-check = ' '.

    READ TABLE lt_reqoc INTO ls_reqoc  WITH KEY banfn = ls_eban-banfn.
    IF sy-subrc EQ 0.
      ls_alv-ocgen = ls_reqoc-ocgen.
      ls_alv-email = ls_reqoc-email.
    ENDIF.

    READ TABLE lt_ebkn INTO DATA(ls_ebkn) WITH KEY banfn = ls_eban-banfn.
    IF sy-subrc EQ 0.
      ls_alv-prctr = ls_ebkn-prctr.

      "Anexados en fondo para procesar BAPI
      ls_alv-sakto = ls_ebkn-sakto.
      ls_alv-kokrs = ls_ebkn-kokrs.
      ls_alv-kostl = ls_ebkn-kostl.
      ls_alv-aufnr = ls_ebkn-aufnr.
      ls_alv-anln1 = ls_ebkn-anln1.
    ENDIF.

    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_eban-lifnr.
    IF sy-subrc EQ 0.

      ls_alv-land1 = ls_lfa1-land1.
      ls_alv-adrnr = ls_lfa1-adrnr.
      ls_alv-name1 = ls_lfa1-name1.
      ls_alv-name2 = ls_lfa1-name2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_lfa1-lifnr
        IMPORTING
          output = ls_lfa1-lifnr.

      CASE ls_lfa1-lifnr(1).
        WHEN 3.
          IF ls_lfa1-land1 NE 'MX'.

            READ TABLE lt_t007s INTO ls_t007s WITH KEY mwskz = 'V3'.
            IF sy-subrc EQ 0.
              CONCATENATE ls_t007s-mwskz ls_t007s-text1 INTO ls_alv-mwskz.
              ls_alv-mwskz = ls_t007s-mwskz.
            ENDIF.

          ELSE.

            READ TABLE lt_t007s INTO ls_t007s WITH KEY mwskz = 'V0'.
            IF sy-subrc EQ 0.
              CONCATENATE ls_t007s-mwskz ls_t007s-text1 INTO ls_alv-mwskz_t.
              ls_alv-mwskz = ls_t007s-mwskz.
            ENDIF.

          ENDIF.

        WHEN 1 OR 9. "Nacional con IVA

          READ TABLE lt_t007s INTO ls_t007s WITH KEY mwskz = 'V3'.
          IF sy-subrc EQ 0.
            CONCATENATE ls_t007s-mwskz ls_t007s-text1 INTO ls_alv-mwskz.
            ls_alv-mwskz = ls_t007s-mwskz.
          ENDIF.

        WHEN OTHERS.

          READ TABLE lt_t007s INTO ls_t007s WITH KEY mwskz = 'V0'.
          IF sy-subrc EQ 0.
            CONCATENATE ls_t007s-mwskz ls_t007s-text1 INTO ls_alv-mwskz_t.
            ls_alv-mwskz = ls_t007s-mwskz.
          ENDIF.

      ENDCASE.

    ENDIF.

*
    ls_alv-matkl  = 'SERV'.
    CONCATENATE ls_alv-afnam ' / ' ls_alv-prctr INTO ls_alv-headt.

    APPEND ls_alv TO lt_alv.
    CLEAR ls_alv.

  ENDLOOP.

  SORT lt_alv BY banfn bnfpo ASCENDING.

*  IF flag_oc EQ 'X'.
*    MESSAGE 'Purchase orders have been created. Please check the view ZMM_REQOC or reload report' TYPE 'S'.
*  ELSEIF flag_mail EQ 'X'.
*    MESSAGE 'Notification sent to vendor email address.' TYPE 'S'.
*  ENDIF.

*  CLEAR: flag_oc, flag_mail.

ENDFORM.


FORM f_display_data.

  IF lt_alv[] IS NOT INITIAL.
    PERFORM alv_report USING lt_alv[].      "LLamado al ALV
  ELSE.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.


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
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'CHECK'.
  alv_git_fieldcat-seltext_l = 'Seleccionar'.
  alv_git_fieldcat-checkbox  = 'X'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'WERKS'.
  alv_git_fieldcat-seltext_l = 'Plant'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'BANFN'.
  alv_git_fieldcat-seltext_l = 'Purchase Req.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'BNFPO'.
  alv_git_fieldcat-seltext_s = 'Req. Item'.
  alv_git_fieldcat-seltext_l = 'Req. Item'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '40'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'TXZ01'.
  alv_git_fieldcat-seltext_l = 'Short Text'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'AFNAM'.
  alv_git_fieldcat-seltext_l = 'Requisitioner'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'PRCTR'.
  alv_git_fieldcat-seltext_l = 'Profit Center'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MENGE'.
  alv_git_fieldcat-seltext_s = 'Quantity'.
  alv_git_fieldcat-seltext_l = 'Quantity Requested'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MEINS'.
  alv_git_fieldcat-seltext_l = 'UoM'.
  alv_git_fieldcat-seltext_l = 'UoM'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'LIFNR'.
  alv_git_fieldcat-seltext_l = 'Vendor'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MATKL'.
  alv_git_fieldcat-seltext_l = 'Material Group'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'MWSKZ'.
  alv_git_fieldcat-seltext_l = 'Tax Code'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
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
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'OCGEN'.
  alv_git_fieldcat-seltext_l = 'Purchase Order'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV '.
  alv_git_fieldcat-fieldname = 'EMAIL'.
  alv_git_fieldcat-seltext_l = 'Sent Email'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR ls_event_exit.
  ls_event_exit-ucomm        = gc_refresh.    " Refresh
  ls_event_exit-after        = c_x.
  APPEND ls_event_exit TO lt_event_exit.

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
      it_event_exit            = lt_event_exit
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


FORM pf_status_set USING extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.


FORM popup_mail.

  CLEAR: lv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmación '
      text_question         = '¿Quieres enviar por correo la orden de compra?'
      text_button_1         = 'Si'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_CANCEL'
      display_cancel_button = ' '
      popup_type            = 'ICON_HINT' "'ICON_MESSAGE_ERROR'
    IMPORTING
      answer                = lv_answer.
  IF lv_answer NE 1.
    LEAVE SCREEN.
  ELSE.
    PERFORM send_mail.
  ENDIF.

ENDFORM.

FORM popup_oc.

  CLEAR: lv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmación '
      text_question         = '¿Quieres generar las ordenes de compra selecionadas?'
      text_button_1         = 'Si'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_CANCEL'
      display_cancel_button = ' '
      popup_type            = 'ICON_MESSAGE_ERROR'
    IMPORTING
      answer                = lv_answer.
  IF lv_answer NE 1.
    LEAVE SCREEN.
  ELSE.
    PERFORM generate_oc.
  ENDIF.

ENDFORM.

FORM send_mail.

* to reflect the data changed into internal table
  CLEAR: ref_grid.

  DATA ls_ref1 TYPE REF TO cl_gui_alv_grid .

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ls_ref1.

  CALL METHOD ls_ref1->check_changed_data.

*  LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
*    CLEAR: <fs_alv>-check.
*  ENDLOOP.
*
*  IF ref_grid IS INITIAL.
*    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*      IMPORTING
*        e_grid = ref_grid.
*  ENDIF.

*  IF NOT ref_grid IS INITIAL.
*    CALL METHOD ref_grid->check_changed_data.
*  ENDIF.

  "Temporal para enviar mail
  REFRESH lt_auxmail.
  CLEAR: ls_auxmail.
  lt_auxmail[] = lt_alv[].

  DELETE ADJACENT DUPLICATES FROM lt_auxmail COMPARING banfn.
  DELETE lt_auxmail WHERE check EQ ' '.
  DELETE lt_auxmail WHERE ocgen EQ ' '.


  IF lt_auxmail[] IS INITIAL.

    MESSAGE 'Please select at least one record.' TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.

    SORT lt_auxmail BY banfn bnfpo check ASCENDING.

*  LOOP AT lt_auxmail ASSIGNING FIELD-SYMBOL(<fs_auxmail>).
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = <fs_auxmail>-lifnr
*      IMPORTING
*        output = <fs_auxmail>-lifnr.
*  ENDLOOP.

    IF lt_auxmail[] IS NOT INITIAL.
      SELECT *
        FROM zmm_reqoc
        INTO TABLE @DATA(lt_reqoc)
        FOR ALL ENTRIES IN @lt_auxmail
        WHERE banfn EQ @lt_auxmail-banfn
          AND email NE 'X'.

      IF sy-subrc NE 0.
        MESSAGE 'The purchase order has already been notified to the vendor by email.' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE SCREEN.
      ELSE.
        PERFORM print_form.
      ENDIF.

    ELSE.
      MESSAGE 'Purchase requisitions must generate a purchase order before notifying the vendor.' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.

  ENDIF.

ENDFORM.


FORM print_form.

  "SSF Function Declarations
  DATA: ls_ctrl_form  TYPE ssfctrlop, "Smart Form Control Structure
        ls_output_opt TYPE ssfcompop, "Smart Form Transfer Options
        ls_job_output TYPE ssfcrescl, "Structure to return value at the end of form printing
        lv_fname      TYPE rs38l_fnam.

* Internal Table declarations
  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.

  DATA: ls_lfa1  TYPE lfa1,
        ls_t001  TYPE t001,
        ls_ekko  TYPE ekko,
        ls_sadr  TYPE sadr,
        ls_pekko TYPE pekko,
        ls_ekpo  TYPE ekpo,
        ls_t024  TYPE t024,
        ls_komk  TYPE komk,
        ls_lfm1  TYPE lfm1,
        lt_ekpo  TYPE TABLE OF ekpo.

  DATA: it_lfa1 TYPE TABLE OF lfa1,
        wa_lfa1 TYPE lfa1.


  DATA: nast          LIKE nast,
        l_druvo       LIKE t166k-druvo,
        l_nast        LIKE nast,
        l_from_memory,
        l_doc         TYPE meein_purchase_doc_print.

  DATA: ent_retco  TYPE char1 VALUE '0',
        ent_screen TYPE char1 VALUE 'X'.

  DATA: tnapr      TYPE tnapr,
        arc_params TYPE arc_params,
        toa_dara   TYPE toa_dara.

  DATA: lw_return TYPE ssfcrescl.

*  SORT lt_auxmail BY banfn bnfpo ASCENDING.

*  SELECT *
*    FROM lfa1
*    INTO TABLE it_lfa1
*    FOR ALL ENTRIES IN lt_auxmail
*    WHERE lifnr EQ lt_auxmail-lifnr.

  REFRESH: lt_reqemail, lt_usr21, lt_adr6.
  CLEAR: ls_reqemail, ls_usr21, ls_adr6.

  SELECT *
    FROM zmm_reqemail
    INTO TABLE lt_reqemail
    FOR ALL ENTRIES IN lt_auxmail
    WHERE lifnr EQ lt_auxmail-lifnr.

  SELECT *
    FROM usr21
    APPENDING TABLE lt_usr21
    FOR ALL ENTRIES IN lt_auxmail
    WHERE bname EQ lt_auxmail-ernam.

  SELECT *
    FROM usr21
    APPENDING TABLE lt_usr21
    FOR ALL ENTRIES IN lt_auxmail
    WHERE bname EQ lt_auxmail-bname.

  IF lt_usr21[] IS NOT INITIAL.
    SELECT *
      FROM adr6
      INTO TABLE lt_adr6
      FOR ALL ENTRIES IN lt_usr21
      WHERE persnumber EQ lt_usr21-persnumber
        AND addrnumber EQ lt_usr21-addrnumber.

  ENDIF.

  LOOP AT lt_auxmail INTO DATA(ls_auxmail).

*    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = ls_auxmail-lifnr.
*    IF sy-subrc EQ 0.
    nast-adrnr = ls_auxmail-adrnr. "wa_lfa1-adrnr.
*    ENDIF.

    tnapr-fonam = 'Z_MEDRUCK'.

    nast-mandt = sy-mandt.
    nast-kappl = 'EF'.
    nast-objky = ls_auxmail-ocgen.
    nast-kschl = 'NEU'.
    nast-spras = 'E'.
    nast-parnr = ls_auxmail-lifnr.
    nast-parvw = 'LF'.
    nast-erdat = ls_auxmail-lfdat.                          "20230516
    nast-eruhr = '150514'.
*    nast-adrnr = 0000106120
    nast-nacha = '1'.
    nast-anzal = '1'.
    nast-vsztp = '3'.
    nast-usnam = sy-uname.
    nast-ldest = 'LP01'.
    nast-dimme = 'X'.
    nast-dimme = 'X'.
    nast-nauto = 'X'.
    nast-tdreceiver = sy-uname.
    nast-tdarmod = '1'.
    nast-objtype = 'BUS2012'.

    CLEAR ent_retco.
    IF nast-aende EQ space.
      l_druvo = '1'.
    ELSE.
      l_druvo = '2'.
    ENDIF.

    CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
      EXPORTING
        ix_nast        = nast
        ix_screen      = ent_screen
      IMPORTING
        ex_retco       = ent_retco
        ex_nast        = l_nast
        doc            = l_doc
      CHANGING
        cx_druvo       = l_druvo
        cx_from_memory = l_from_memory.

*LLamado a smartforms para impresión de OC
    IF l_doc-xekko-frggr = '  ' OR l_doc-xekko-frggr = '00'.
      CALL FUNCTION 'ZME_PRINT_PO_ZMM_GENERA_OC'
        EXPORTING
          ix_nast        = l_nast
          ix_druvo       = l_druvo
          doc            = l_doc
          ix_screen      = ent_screen
          ix_from_memory = l_from_memory
          ix_toa_dara    = toa_dara
          ix_arc_params  = arc_params
          ix_fonam       = tnapr-fonam
          get_otf        = 'X'
        IMPORTING
          ex_retco       = ent_retco
          output_options = ls_job_output.
    ENDIF.

    i_otf[] = ls_job_output-otfdata[].

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
      IMPORTING
        bin_filesize          = v_len_in
      TABLES
        otf                   = i_otf
        lines                 = i_tline
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    LOOP AT i_tline.
      TRANSLATE i_tline USING '~'.
      CONCATENATE wa_buffer i_tline INTO wa_buffer.
    ENDLOOP.

    TRANSLATE wa_buffer USING '~'.
    DO.
      i_record = wa_buffer.
      APPEND i_record.
      SHIFT wa_buffer LEFT BY 255 PLACES.
      IF wa_buffer IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

* Attachment
    REFRESH: i_reclist,
             i_objtxt,
             i_objbin,
             i_objpack.
    CLEAR wa_objhead.

    i_objbin[] = i_record[].

    IF ls_auxmail-land1 EQ 'MX'.

* Create Message Body Title and Description
      i_objtxt = '<HTML><BODY>'.
      APPEND i_objtxt.
      i_objtxt = 'Buen día,'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'Envío nueva orden de compra.'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'Favor de confirmar recibido.'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'Para cualquier detalle, favor ponerse en contacto con el usuario.'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<b><i>¡RECUERDA!</i></b>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'Al momento de entregar material, por favor hacerlo <u>exclusivamente</u> en <b>ventanilla de Almacén,</b> o bien, si es un <b>servicio</b> entregar facturas directamente al usuario.'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'Saludos.'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
*      i_objtxt = '<b>Arturo Luna<b>'.
*      APPEND i_objtxt.
*      i_objtxt = '<BR></BR>'.
*      APPEND i_objtxt.
      i_objtxt = 'Compras Estampado Magna'.
      APPEND i_objtxt.
      i_objtxt = '</BODY></HTML>'.
      APPEND i_objtxt.

    ELSE.

      i_objtxt = '<HTML><BODY>'.
      APPEND i_objtxt.
      i_objtxt = 'Hi,'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'Find attached a new purchase order'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'Please confirm received.'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = 'For any details, the user is copied on this mail.'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
      APPEND i_objtxt.
      i_objtxt = '<b><i>Regards.</i></b>'.
      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
*      APPEND i_objtxt.
*      i_objtxt = 'Al momento de entregar material, por favor hacerlo <u>exclusivamente</u> en <b>ventanilla de Almacén,</b> o bien, si es un <b>servicio</b> entregar facturas directamente al usuario.'.
*      APPEND i_objtxt.
      i_objtxt = '<BR></BR>'.
*      APPEND i_objtxt.
*      i_objtxt = '<BR></BR>'.
*      APPEND i_objtxt.
*      i_objtxt = 'Saludos.'.
*      APPEND i_objtxt.
*      i_objtxt = '<BR></BR>'.
*      APPEND i_objtxt.
*      i_objtxt = '<BR></BR>'.
*      APPEND i_objtxt.
*      i_objtxt = '<b>Arturo Luna<b>'.
*      APPEND i_objtxt.
*      i_objtxt = '<BR></BR>'.
*      APPEND i_objtxt.
      i_objtxt = 'Compras Estampado Magna'.
      APPEND i_objtxt.
      i_objtxt = '</BODY></HTML>'.
      APPEND i_objtxt.

    ENDIF.

    DESCRIBE TABLE i_objtxt LINES v_lines_txt.
    READ TABLE i_objtxt INDEX v_lines_txt.

    wa_doc_chng-obj_name = 'Delivery Order1'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_auxmail-lifnr
      IMPORTING
        output = ls_auxmail-lifnr.

    CONCATENATE ls_auxmail-ocgen
                ls_auxmail-lifnr
                ls_auxmail-name1
                ls_auxmail-name2
           INTO wa_doc_chng-obj_descr SEPARATED BY space.
    wa_doc_chng-doc_size = v_lines_txt * 255.

* Main Text
    CLEAR i_objpack-transf_bin.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    i_objpack-body_num = v_lines_txt.
    i_objpack-doc_type = 'HTM'.
    APPEND i_objpack.

* Attachment (pdf-Attachment)
    i_objpack-transf_bin = 'X'.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    DESCRIBE TABLE i_objbin LINES v_lines_bin.
    READ TABLE i_objbin INDEX v_lines_bin.

    i_objpack-doc_size = v_lines_bin * 255 .
    i_objpack-body_num = v_lines_bin.
    i_objpack-doc_type = 'PDF'.
    i_objpack-obj_name = 'smart'.

    CONCATENATE ls_auxmail-ocgen
                ls_auxmail-lifnr
                ls_auxmail-name1
                ls_auxmail-name2
           INTO i_objpack-obj_descr SEPARATED BY space.
    APPEND i_objpack.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_auxmail-lifnr
      IMPORTING
        output = ls_auxmail-lifnr.

    READ TABLE lt_reqemail INTO ls_reqemail WITH KEY lifnr = ls_auxmail-lifnr.
    IF sy-subrc EQ 0.

      CLEAR i_reclist.
      i_reclist-receiver = ls_reqemail-zprov_smtpadr1.
      i_reclist-rec_type = 'U'.
      APPEND i_reclist.

      CLEAR i_reclist.
      i_reclist-receiver = ls_reqemail-zprov_smtpadr2.
      i_reclist-rec_type = 'U'.
      i_reclist-copy     = 'X'.
      APPEND i_reclist.

      CLEAR i_reclist.
      i_reclist-receiver = ls_reqemail-zprov_smtpadr3.
      i_reclist-rec_type = 'U'.
      i_reclist-copy     = 'X'.
      APPEND i_reclist.

      CLEAR i_reclist.
      i_reclist-receiver = ls_reqemail-zprov_smtpadr4.
      i_reclist-rec_type = 'U'.
      i_reclist-copy     = 'X'.
      APPEND i_reclist.

      CLEAR i_reclist.
      i_reclist-receiver = ls_reqemail-zprov_smtpadr5.
      i_reclist-rec_type = 'U'.
      i_reclist-copy     = 'X'.
      APPEND i_reclist.

      READ TABLE lt_usr21 INTO ls_usr21 WITH KEY bname = ls_auxmail-ernam.
      IF sy-subrc EQ 0.
        READ TABLE lt_adr6 INTO ls_adr6 WITH KEY addrnumber = ls_usr21-addrnumber  persnumber = ls_usr21-persnumber.
        IF sy-subrc EQ 0.
          CLEAR i_reclist.
          i_reclist-receiver = ls_adr6-smtp_addr.
          i_reclist-rec_type = 'U'.
          i_reclist-copy     = 'X'.
          APPEND i_reclist.
        ENDIF.
      ENDIF.

      READ TABLE lt_usr21 INTO ls_usr21 WITH KEY bname = ls_auxmail-bname.
      IF sy-subrc EQ 0.
        READ TABLE lt_adr6 INTO ls_adr6 WITH KEY addrnumber = ls_usr21-addrnumber  persnumber = ls_usr21-persnumber.
        IF sy-subrc EQ 0.
          CLEAR i_reclist.
          i_reclist-receiver = ls_adr6-smtp_addr.
          i_reclist-rec_type = 'U'.
          i_reclist-copy     = 'X'.
          APPEND i_reclist.
        ENDIF.
      ENDIF.

    ENDIF.

    DELETE i_reclist WHERE receiver EQ ' '.

    CLEAR: ls_reqemail, ls_usr21, ls_adr6.
    CLEAR:ls_reqoc.
    REFRESH: lt_reqoc.

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = wa_doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = i_objpack
        object_header              = wa_objhead
        contents_bin               = i_objbin
        contents_txt               = i_objtxt
        receivers                  = i_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc <> 0.
      MESSAGE 'Vendor must have an email address. Please check the view ZMM_REQEMAIL.' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE 'Notification sent to vendor email address.' TYPE 'S'.

      ls_reqoc-banfn = ls_auxmail-banfn.
      ls_reqoc-ocgen = ls_auxmail-ocgen.
      ls_reqoc-email = 'X'.
      APPEND ls_reqoc TO lt_reqoc.

      MODIFY zmm_reqoc FROM TABLE lt_reqoc.
      COMMIT WORK AND WAIT.

*      flag_mail = 'X'.
*      PERFORM show_data.
*      p_selfield-refresh    = c_x.
*      p_selfield-col_stable = c_x.
*      p_selfield-row_stable = c_x.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " GENERAR_CORREO


FORM generate_oc.

* Declaraciones BAPI_PO_CREATE1
  DATA: BEGIN OF t_return OCCURS 10.
          INCLUDE STRUCTURE bapiret2.
  DATA  END OF t_return.

  DATA: BEGIN OF t_return_log OCCURS 10.
          INCLUDE STRUCTURE bapiret2.
  DATA  END OF t_return_log.

  TYPES: BEGIN OF ty_pritem.
          INCLUDE STRUCTURE bapimepoitem.
  TYPES: END OF ty_pritem.

  TYPES: BEGIN OF ty_pritemx.
          INCLUDE STRUCTURE bapimepoitemx.
  TYPES: END OF ty_pritemx.

  TYPES: BEGIN OF ty_praccount.
          INCLUDE STRUCTURE bapimepoaccount.
  TYPES: END OF ty_praccount.

  TYPES: BEGIN OF ty_praccountx.
          INCLUDE STRUCTURE bapimepoaccountx.
  TYPES: END OF ty_praccountx.

  DATA: t_prheader     LIKE bapimepoheader,
        t_prheaderx    LIKE bapimepoheaderx,
        v_number       LIKE bapimereqheader-preq_no,
        t_pritem       TYPE TABLE OF ty_pritem,
        w_pritem       LIKE LINE OF t_pritem,
        t_pritemx      TYPE TABLE OF ty_pritemx,
        w_pritemx      LIKE LINE OF t_pritemx,
        t_potextitem   TYPE TABLE OF bapimepotext,
        t_prheadertext TYPE TABLE OF  bapimepotextheader,
        w_prheadertext TYPE bapimepotextheader,
        t_praccount    TYPE TABLE OF ty_praccount,
        w_praccount    LIKE LINE OF t_praccount,
        t_praccountx   TYPE TABLE OF ty_praccountx,
        w_praccountx   LIKE LINE OF t_praccountx,
        t_pocond       TYPE TABLE OF  bapimepocond,
        t_pocondx      TYPE TABLE OF   bapimepocondx,

        t_possched     TYPE TABLE OF bapimeposchedule,
        t_posschedx    TYPE TABLE OF bapimeposchedulx,
        w_possched     TYPE bapimeposchedule,
        w_posschedx    TYPE bapimeposchedulx,

        wa_pocond      TYPE bapimepocond,
        wa_pocondx     TYPE bapimepocondx,
        v_tabix        TYPE sy-tabix,
        lv_tabix       TYPE sy-tabix,
        w_return       LIKE LINE OF t_return,
        v_tabixc       TYPE c LENGTH 10,
        v_req.

  DATA: t_item_schedules LIKE bapimeposchedule OCCURS 0 WITH HEADER LINE.

  CLEAR: any_ocgen.
*  CLEAR: ref_grid.


  DATA ls_ref1 TYPE REF TO cl_gui_alv_grid .

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ls_ref1.

  CALL METHOD ls_ref1->check_changed_data.
*
*
*  LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
*    CLEAR: <fs_alv>-check.
*  ENDLOOP.
*  UNASSIGN <fs_alv>.
*
** Lectura de datos
*  IF ref_grid IS INITIAL.
*    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*      IMPORTING
*        e_grid = ref_grid.
*  ENDIF.
*
*  IF NOT ref_grid IS INITIAL.
*    CALL METHOD ref_grid->check_changed_data.
*  ENDIF.
  SORT lt_alv BY banfn bnfpo check ASCENDING.

  "Temporal para OC
  REFRESH lt_auxoc.
  CLEAR: ls_auxoc.
  lt_auxoc[] = lt_alv[].

* Eliminar registros que no esten selecionados
  DELETE lt_auxoc WHERE check EQ ' '.

* Eliminar registros que ya tiene OC generada
  DELETE lt_auxoc WHERE ocgen NE ''.

  IF lt_auxoc[] IS INITIAL.

    MESSAGE 'Please select at least one record.' TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.

    CLEAR: w_pritem-po_item,
           w_pritemx-po_item,
           w_praccount-po_item,
           w_praccountx-po_item,
           wa_pocond-itm_number.

    REFRESH: t_potextitem.


    SORT lt_auxoc BY banfn bnfpo check ASCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_auxoc COMPARING banfn.

    IF lt_auxoc[] IS INITIAL.

      MESSAGE 'Purchase requisitions already generated a purchase order.' TYPE 'S' DISPLAY LIKE 'E'.

    ELSE.

      LOOP AT lt_auxoc INTO ls_auxoc.

        CLEAR: t_prheader,
               t_prheaderx.

        REFRESH: t_return,
                 t_pritem,
                 t_pritemx,
                 t_prheadertext,
                 t_potextitem,
                 t_praccount,
                 t_praccountx,
                 t_pocond,
                 t_pocondx,
                 t_item_schedules,
                 t_posschedx.

        LOOP AT lt_auxoc INTO DATA(ls_auxoc) WHERE banfn EQ ls_auxoc-banfn.

          v_tabix = sy-tabix.

          t_prheader-doc_type   = ls_auxoc-bsart.
          t_prheader-purch_org  = ls_auxoc-ekorg.
          t_prheader-pur_group  = ls_auxoc-ekgrp.
          t_prheader-currency   = ls_auxoc-waers.
          t_prheader-vendor     = ls_auxoc-lifnr.

*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_genoc-lifnr
*          IMPORTING
*            output = t_prheader-vendor.

          t_prheaderx-doc_type  = 'X'.
          t_prheaderx-purch_org = 'X'.
          t_prheaderx-pur_group = 'X'.
          t_prheaderx-vendor    = 'X'.
          t_prheaderx-currency  = 'X'.

          w_pritem-po_item      = ls_auxoc-bnfpo.
          w_pritem-plant        = ls_auxoc-werks.
          w_pritem-material     = ' '. "ls_auxoc-matnr. "(vacio)
          w_pritem-short_text   = ls_auxoc-txz01.
          w_pritem-matl_group   = ls_auxoc-matkl.
          w_pritem-quantity     = ls_auxoc-menge.
          w_pritem-po_unit      = ls_auxoc-meins.
          w_pritem-price_unit   = 1.
          w_pritem-po_price     = '2'. "w_tabinm-preis.
          w_pritem-net_price    = ls_auxoc-preis.
          w_pritem-preq_no      = ls_auxoc-banfn.
          w_pritem-preq_item    = ls_auxoc-bnfpo.
          w_pritem-preq_name    = ls_auxoc-afnam.
          w_pritem-acctasscat   = ls_auxoc-knttp.
          w_pritem-trackingno   = ls_auxoc-bednr. "w_tabinm-codco. "No existe
          w_pritem-price_date   = sy-datum.
          w_pritem-tax_code     = ls_auxoc-mwskz.
          w_pritem-gr_ind       = ls_auxoc-wepos.
          w_pritem-ir_ind       = ls_auxoc-repos.
          w_pritem-gr_basediv   = ' '.            "w_tabinm-webre. "No existe
          w_pritem-no_rounding  = 'X'.
          APPEND w_pritem TO t_pritem.

          w_pritemx-po_item     = ls_auxoc-bnfpo. "w_pritemx-po_item + 10.
          w_pritemx-plant       = 'X'.
          w_pritemx-tax_code    = 'X'.
          w_pritemx-material    = 'X'.
          w_pritemx-short_text  = 'X'.
          w_pritemx-quantity    = 'X'.
          w_pritemx-matl_group  = 'X'.
          w_pritemx-po_unit     = 'X'.
          w_pritemx-price_unit  = 'X'.
          w_pritemx-po_price    = 'X'.
          w_pritemx-preq_no     = 'X'.
          w_pritemx-preq_item   = 'X'.
          w_pritemx-preq_name   = 'X'.
          w_pritemx-acctasscat  = 'X'.
          w_pritemx-trackingno  = 'X'.
          w_pritemx-gr_ind      = 'X'.
          w_pritemx-ir_ind      = 'X'.
          w_pritemx-gr_basediv  = 'X'.
          w_pritemx-net_price   = 'X'.
          w_pritemx-no_rounding = 'X'.
          APPEND w_pritemx TO t_pritemx.


          w_praccount-po_item   = ls_auxoc-bnfpo. "w_praccount-po_item + 10.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_auxoc-sakto
            IMPORTING
              output = ls_auxoc-sakto.

          w_praccount-serial_no  = '01'.
          w_praccount-gl_account = ls_auxoc-sakto.
          w_praccount-co_area    = ls_auxoc-kokrs.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_auxoc-kostl
            IMPORTING
              output = ls_auxoc-kostl.

          IF ls_auxoc-knttp NE 'F'.
            w_praccount-costcenter = ls_auxoc-kostl.
          ENDIF.

          w_praccount-quantity  = ls_auxoc-menge.
          w_praccount-net_value = ls_auxoc-preis.
          w_praccount-orderid   = ls_auxoc-aufnr.

          IF ls_auxoc-knttp EQ 'A'.
            w_praccount-asset_no = ls_auxoc-anln1. "w_tabinm-asset.
          ENDIF.

          APPEND w_praccount TO t_praccount.

          w_praccountx-co_area    = 'X'.
          w_praccountx-po_item    = ls_auxoc-bnfpo. "w_praccountx-po_item + 10.
          w_praccountx-serial_no  = '01'.
          w_praccountx-gl_account = 'X'.

          IF ls_auxoc-knttp NE 'F'.
            w_praccountx-costcenter = 'X'.
          ENDIF.
          w_praccountx-orderid   = 'X'.
          w_praccountx-quantity  = 'X'.
          w_praccountx-net_value = 'X'.

          IF ls_auxoc-knttp EQ 'A'.
            w_praccountx-asset_no = 'X'.
          ENDIF.
          APPEND w_praccountx TO t_praccountx.

          wa_pocond-itm_number = ls_auxoc-bnfpo. "wa_pocond-itm_number + 10.
          wa_pocond-cond_type  = 'PBXX'.
          wa_pocond-cond_value = ls_auxoc-preis.
          wa_pocond-currency   = ls_auxoc-waers.
          APPEND wa_pocond TO t_pocond.

          wa_pocondx-itm_number = ls_auxoc-bnfpo. "wa_pocondx-itm_number + 10.
          wa_pocondx-cond_type  = 'X'.
          wa_pocondx-cond_value = 'X'.
          wa_pocondx-currency   = 'X'.
          APPEND wa_pocondx TO t_pocondx.


************************
          w_possched-po_item  = ls_auxoc-bnfpo. "w_possched-po_item + 10.

          CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
            EXPORTING
              input  = sy-datum
            IMPORTING
              output = w_possched-delivery_date.

*    w_possched-delivery_date  = sy-datum. "w_tabinm-fecha_des.
          w_possched-quantity       = ls_auxoc-menge.
          w_possched-del_datcat_ext = 'D'.
          APPEND w_possched TO t_possched.

          t_item_schedules-po_item        = ls_auxoc-bnfpo.
          t_item_schedules-delivery_date  = ls_auxoc-lfdat.
          t_item_schedules-quantity       = ls_auxoc-menge.
*          t_item_schedules-del_datcat_ext = '1'.
*          t_item_schedules-sched_line     = '0001'.
          APPEND t_item_schedules.

          w_posschedx-po_item        = ls_auxoc-bnfpo. "w_posschedx-po_item + 10.
          w_posschedx-delivery_date  = 'X'.
          w_posschedx-quantity       = 'X'.
*          w_posschedx-del_datcat_ext = 'X'.
*          w_posschedx-sched_line     = '0001'.
          APPEND w_posschedx TO t_posschedx.
************************

          w_prheadertext-po_number = ls_auxoc-banfn.
*        w_prheadertext-po_item   = ls_auxoc-bnfpo.
          w_prheadertext-text_id   = 'F01'.
*        w_prheadertext-text_form = '*'.
          w_prheadertext-text_line = ls_auxoc-headt.
          APPEND w_prheadertext TO t_prheadertext.

          DATA: ls_tdname TYPE stxh-tdname,
                lt_lines  TYPE TABLE OF tline.

          CONCATENATE '1' ls_auxoc-bnfpo INTO ls_tdname.
*    CONCATENATE w_tabkey2-banfno w_pritem-po_item INTO ls_tdname.

          SELECT SINGLE *
            FROM stxh
            INTO @DATA(ls_stxh)
            WHERE tdname EQ @ls_tdname.

          IF sy-subrc EQ 0.
            REFRESH: lt_lines.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = ls_stxh-tdid
                language                = ls_stxh-tdspras
                name                    = ls_stxh-tdname
                object                  = ls_stxh-tdobject
              TABLES
                lines                   = lt_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

            LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
              APPEND INITIAL LINE TO t_potextitem ASSIGNING FIELD-SYMBOL(<fs_potextitem>).
*          <fs_potextitem>-po_number = w_tabkey2-banfno.
              <fs_potextitem>-po_item =  w_pritem-po_item.
*          <fs_potextitem>-text_id = ls_stxh-tdid.
              <fs_potextitem>-text_form = <fs_lines>-tdformat.
              <fs_potextitem>-text_line = <fs_lines>-tdline.
            ENDLOOP.
          ENDIF.

        ENDLOOP.

        CALL FUNCTION 'BAPI_PO_CREATE1'
          EXPORTING
            poheader         = t_prheader
            poheaderx        = t_prheaderx
            no_price_from_po = 'X'
          TABLES
            return           = t_return
            poitem           = t_pritem
            poitemx          = t_pritemx
            potextheader     = t_prheadertext
            potextitem       = t_potextitem
            poaccount        = t_praccount
            poaccountx       = t_praccountx
            pocond           = t_pocond
            pocondx          = t_pocondx
            poschedule       = t_item_schedules "t_possched
            poschedulex      = t_posschedx.

        CLEAR: ls_reqoc.
        REFRESH: lt_reqoc.

        READ TABLE t_return INTO w_return  WITH KEY type = 'S' id = '06' number = '017'.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = t_return.

          SELECT *
            FROM ekpo
            INTO TABLE @DATA(lt_ekpo)
            WHERE ebeln EQ @w_return-message_v2.

          READ TABLE lt_ekpo INTO DATA(ls_ekpo) INDEX 1.
          IF sy-subrc EQ 0.
            ls_reqoc-banfn = ls_auxoc-banfn.
            ls_reqoc-ocgen = ls_ekpo-ebeln.
            any_ocgen      = 'X'.
            APPEND ls_reqoc TO lt_reqoc.
          ENDIF.

          MODIFY zmm_reqoc FROM TABLE lt_reqoc.
          COMMIT WORK AND WAIT.

        ELSE.

          LOOP AT t_return INTO w_return WHERE type EQ 'E'.

            CASE w_return-id.
              WHEN '06'.
                CONCATENATE w_return-message ' ' INTO message_e SEPARATED BY space.
              WHEN 'ME'.
                CONCATENATE message_e w_return-message ' ' INTO message_e SEPARATED BY space.
*      	WHEN OTHERS.
            ENDCASE.

          ENDLOOP.

        ENDIF.

        IF message_e IS NOT INITIAL.
          MESSAGE message_e TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

        CLEAR: message_e.

      ENDLOOP.

      IF any_ocgen EQ 'X'.
        MESSAGE 'Purchase orders have been created. Please check the view ZMM_REQOC or reload report' TYPE 'S'." DISPLAY LIKE 'I'.

*        flag_oc = 'X'.
*        PERFORM show_data.
*        p_selfield-refresh    = c_x.
*        p_selfield-col_stable = c_x.
*        p_selfield-row_stable = c_x.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.


FORM user_command USING p_ucomm TYPE sy-ucomm
                         p_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN '&GEN_OC'.
      PERFORM popup_oc.
    WHEN '&MAIL'.
      PERFORM popup_mail.
    WHEN '&REFRESH'.
      PERFORM show_data.
      p_selfield-refresh    = c_x.
      p_selfield-col_stable = c_x.
      p_selfield-row_stable = c_x.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Report  ZMM_REPLICATE_MB52
*&
*&---------------------------------------------------------------------*
*& Creación de Johan Rodríguez
*& Consultor ABAP 16/03/2023
*&---------------------------------------------------------------------*
REPORT zmm_replicate_mb52.

TABLES: mard, mara, makt, mslb, mbew, mkol,ekbe.

*TYPES: BEGIN OF  st_alv ,
*         werks   TYPE mard-werks,  "Plant
*         mtart   TYPE mara-mtart,  "Material Type
*         matkl   TYPE mara-matkl,  "Material Group
*         matnr   TYPE mard-matnr,  "Material Number
*         maktx   TYPE makt-maktx,  "Material Description
*         lgort   TYPE mard-lgort,  "Storage Location
*         sobkz   TYPE mslb-sobkz,  "Special Stock Number
*         meins   TYPE mara-meins,  "Base Unit
*         waers   TYPE ekbe-waers,  "Currency
*         speme   TYPE mard-speme,  "lock
*         retme   TYPE mard-retme,  "Returns
*         price   TYPE mbew-stprs,  "Price
*         trans   TYPE mard-umlme,  "Transit / Transfer
*         unres   TYPE mard-labst,  "Unrestricted
*         vunre   TYPE mard-labst,  "Value Unrestricted
*         vtran   TYPE mard-umlme,  "Value in Transit/transfer
*         quali   TYPE mard-insme,  "Quality Inspection
*         vqual   TYPE mard-insme,  "Value of Quality Inspection
*         restr   TYPE mard-einme,  "Restricted
*         vrest   TYPE mard-einme,  "Value of Restricted
*         vbloc   TYPE mard-speme,  "Value of Blocked
*         vretu   TYPE mard-retme,  "Value of Returns Blocked
*         yearly  TYPE ryear,       "Year
*         monthly TYPE month,       "Month
*         weekly  TYPE weekn,       "Week
*       END OF  st_alv.
*DATA: it_alv  TYPE TABLE OF st_alv, " Layout
*       wa_alv  TYPE st_alv.

DATA: it_alv TYPE TABLE OF zmm_stock_matnr,
      wa_alv TYPE zmm_stock_matnr.

DATA: it_mard  TYPE TABLE OF mard,
      it_mara  TYPE TABLE OF mara,
      it_makt  TYPE TABLE OF makt,
      it_mslb  TYPE TABLE OF mslb,
      it_mbew  TYPE TABLE OF mbew,
      it_mkol  TYPE TABLE OF mkol,
      it_ekbe  TYPE TABLE OF ekbe,
      it_t001l TYPE TABLE OF t001l.

DATA:
  wa_mara  TYPE mara,
  wa_mard  TYPE mard,
  wa_makt  TYPE makt,
  wa_mslb  TYPE mslb,
  wa_mbew  TYPE mbew,
  wa_mkol  TYPE mkol,
  wa_ekbe  TYPE ekbe,
  wa_t001l TYPE t001l.

DATA: week_number TYPE scal-week,
      price_d4    TYPE dec11_4.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_matnr FOR mard-matnr.
SELECT-OPTIONS: s_werks FOR mard-werks DEFAULT '0547' OBLIGATORY.
SELECT-OPTIONS: s_lgort FOR mard-lgort DEFAULT '2002' OPTION NE.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.
SELECT-OPTIONS: s_mtart FOR mara-mtart.
SELECT-OPTIONS: s_matkl FOR mara-matkl DEFAULT 'STEEL' OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-t03.
PARAMETERS:     p_box1 AS CHECKBOX DEFAULT 'X'.  "Also Select Special Stocks
SELECT-OPTIONS: s_sobkz FOR mslb-sobkz.
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-t04.
PARAMETERS: p_box2 AS CHECKBOX.      "No zero stock lines
SELECTION-SCREEN: END OF BLOCK b4.


START-OF-SELECTION .

  " Validar existencia de materiales
  SELECT *
   FROM mard
   INTO TABLE @DATA(lt_mard)
    WHERE matnr IN @s_matnr
      AND werks IN @s_werks
      AND lgort IN @s_lgort.

  IF lt_mard[] IS NOT INITIAL.

    SELECT *
      FROM mara
      INTO TABLE it_mara
      FOR ALL ENTRIES IN lt_mard
        WHERE matnr EQ lt_mard-matnr
          AND mtart IN s_mtart
          AND matkl IN s_matkl.

    IF it_mara[] IS NOT INITIAL.

      SELECT *
        FROM makt
        INTO TABLE it_makt
         FOR ALL ENTRIES IN it_mara
         WHERE matnr EQ it_mara-matnr
           AND spras EQ sy-langu.

      " Valida datos filtrados desde MARA
      SELECT *
       FROM mard
       INTO TABLE it_mard
        FOR ALL ENTRIES IN it_mara
        WHERE matnr EQ it_mara-matnr
          AND werks IN s_werks
          AND lgort IN s_lgort.

      IF it_mard[] IS NOT INITIAL.

        SELECT *
          FROM mslb
          INTO TABLE it_mslb
          FOR ALL ENTRIES IN it_mard
            WHERE matnr EQ it_mard-matnr
              AND werks EQ it_mard-werks
*          AND lfgja EQ it_mard-lfgja
*          AND lfmon EQ it_mard-lfmon
              AND sobkz IN s_sobkz.

        SELECT *
          FROM mbew
          INTO TABLE it_mbew
          FOR ALL ENTRIES IN it_mard
          WHERE matnr EQ it_mard-matnr
            AND vprsv = 'S'.

        SELECT *
          FROM t001l
          INTO TABLE it_t001l
          FOR ALL ENTRIES IN it_mard
          WHERE werks EQ it_mard-werks
            AND lgort EQ it_mard-lgort.

      ENDIF.

    ENDIF.

  ENDIF.


  LOOP AT it_mard INTO wa_mard.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mard-matnr
      IMPORTING
        output = wa_alv-matnr.                      "Material

    wa_alv-waers = 'MXN'.                           "Currency
    wa_alv-werks = wa_mard-werks.                   "Plant
    wa_alv-lgort = wa_mard-lgort.                   "Storage Location

    READ TABLE it_t001l INTO wa_t001l WITH KEY werks = wa_mard-werks lgort = wa_mard-lgort.
    IF sy-subrc EQ 0.
      wa_alv-lgobe = wa_t001l-lgobe.                "Storage Description
    ENDIF.

    "Week Number
    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = sy-datum
      IMPORTING
        week         = week_number
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

    wa_alv-weekly = week_number+4(2).               "Week
    wa_alv-monthly = sy-datum+4(2).                 "Month
    wa_alv-yearly = sy-datum(4).                    "Year

    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mard-matnr.
    IF sy-subrc = 0.
      wa_alv-mtart = wa_mara-mtart.                 "Material Type
      wa_alv-matkl = wa_mara-matkl.                 "Material Group
      wa_alv-meins = wa_mara-meins.                 "Base Unit
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_mard-matnr.
    IF sy-subrc = 0.
      wa_alv-maktx = wa_makt-maktx.                 "Material Description
    ENDIF.

    CLEAR: wa_mbew, price_d4.
    READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_mard-matnr. "BWKEY search later
    IF sy-subrc = 0.
*      wa_alv-mat_price = wa_mbew-stprs / wa_mbew-peinh. "Material Price
      price_d4 = wa_mbew-stprs / wa_mbew-peinh. "Material Price
    ENDIF.

    wa_alv-labst    = wa_mard-labst.                      "Unrestricted
    wa_alv-salk3_vu = wa_alv-labst * price_d4. "wa_alv-mat_price.    "Value Unrestricted
    wa_alv-trauml   = wa_mard-umlme.                      "Transit / Transfer
    wa_alv-salk3_vt = wa_alv-trauml * price_d4. "wa_alv-mat_price.   "Value in Transit/transfer
    wa_alv-insme    = wa_mard-insme.                      "Quality Inspection
    wa_alv-salk3_vq = wa_alv-insme * price_d4. "wa_alv-mat_price.    "Value of Quality Inspection
    wa_alv-einme    = wa_mard-einme.                      "Restricted
    wa_alv-salk3_vr = wa_alv-einme * price_d4. "wa_alv-mat_price.    "Value of Restricted
    wa_alv-speme    = wa_mard-speme.                      "Block
    wa_alv-salk3_vb = wa_alv-speme * price_d4. "wa_alv-mat_price.    "Value of Blocked
    wa_alv-retme    = wa_mard-retme.                      "Returns
    wa_alv-salk3_rb = wa_alv-retme * price_d4. "wa_alv-mat_price.    "Value of Returns Blocked

    "Save data from MARD
    APPEND wa_alv TO it_alv.

*    IF p_box1 EQ 'X'.
*      READ TABLE it_mslb INTO wa_mslb WITH KEY matnr = wa_mard-matnr
*                                                werks = wa_mard-werks
*                                                lfgja = wa_mard-lfgja
*                                                lfmon = wa_mard-lfmon.
*      IF sy-subrc = 0.
*        wa_alv-labst = wa_mslb-lbvla.                      "Unrestricted
*        wa_alv-ssnum = wa_mslb-lifnr. "ssnum.              "Special Stock Number
*        wa_alv-insme = wa_mslb-lbins.                      "Quality Inspection
*        wa_alv-einme = wa_mslb-lbein.                      "Restricted
*
*        wa_alv-salk3_vu = wa_alv-labst * wa_alv-mat_price. "Value Unrestricted
*        wa_alv-salk3_vq = wa_alv-insme * wa_alv-mat_price. "Value of Quality Inspection
*        wa_alv-salk3_vr = wa_alv-einme * wa_alv-mat_price. "Value of Restricted
*
*        "Save data from MSLB when special stock is marked
*        APPEND wa_alv TO it_alv.
*      ENDIF.
*    ENDIF.

    CLEAR wa_alv.

  ENDLOOP.

  IF p_box1 EQ 'X'.
    LOOP AT it_mslb INTO wa_mslb.

      READ TABLE it_alv INTO wa_alv WITH KEY matnr = wa_mslb-matnr werks = wa_mslb-werks.
      IF sy-subrc EQ 0.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_mslb-lifnr
          IMPORTING
            output = wa_alv-ssnum.                         "Special Stock Number

        CLEAR: wa_mbew, price_d4.
        READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_mslb-matnr bwkey = wa_mslb-werks..
        IF sy-subrc = 0.
*          wa_alv-mat_price = wa_mbew-stprs / wa_mbew-peinh.  "Material Price
          price_d4 = wa_mbew-stprs / wa_mbew-peinh.           "Material Price
        ENDIF.

        wa_alv-labst = wa_mslb-lblab. "lbvla.              "Unrestricted
        wa_alv-insme = wa_mslb-lbins.                      "Quality Inspection
        wa_alv-einme = wa_mslb-lbein.                      "Restricted

        wa_alv-salk3_vu = wa_alv-labst * price_d4. "wa_alv-mat_price. "Value Unrestricted
        wa_alv-salk3_vq = wa_alv-insme * price_d4. "wa_alv-mat_price. "Value of Quality Inspection
        wa_alv-salk3_vr = wa_alv-einme * price_d4. "wa_alv-mat_price. "Value of Restricted

        CLEAR: wa_alv-lgort, wa_alv-lgobe.

        "Save data from MSLB when special stock is marked
        APPEND wa_alv TO it_alv.
        CLEAR wa_alv.

      ENDIF.

    ENDLOOP.
  ENDIF.


  IF p_box2 EQ 'X'.
    DELETE it_alv WHERE labst < 1.
  ENDIF.

*  DELETE it_alv WHERE matkl NE 'STEEL'.

  SORT it_alv ASCENDING BY matnr werks lgort ssnum.
  IF it_alv[] IS NOT INITIAL.

*    DELETE FROM zmm_stock_matnr.
*    COMMIT WORK AND WAIT.

    MODIFY zmm_stock_matnr FROM TABLE it_alv.
    COMMIT WORK AND WAIT.

    PERFORM alv_report USING it_alv[].      "LLamado al ALV

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
FORM alv_report  USING  pp_itab LIKE it_alv[].

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
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'WERKS'.
  alv_git_fieldcat-seltext_l = 'Plnt'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'MTART'.
  alv_git_fieldcat-seltext_l = 'MTyp'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'MATKL'.
  alv_git_fieldcat-seltext_l = 'Matl Group'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '14'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'MATNR'.
  alv_git_fieldcat-seltext_l = 'Material Number'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '30'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'MAKTX'.
  alv_git_fieldcat-seltext_l = 'Material Description'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'LGORT'.
  alv_git_fieldcat-seltext_l = 'SLoc'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '20'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'LGOBE'.
  alv_git_fieldcat-seltext_l = 'SLoc Description'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SSNUM'.
  alv_git_fieldcat-seltext_l = 'Special Stock'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'MEINS'.
  alv_git_fieldcat-seltext_l = 'BUn'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'WAERS'.
  alv_git_fieldcat-seltext_l = 'Crcy'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'LABST'.
  alv_git_fieldcat-seltext_l = 'Unrestricted'. "'Valuated Unrestricted-Use Stock'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SALK3_VU'.
  alv_git_fieldcat-seltext_l = 'Value Unrestricted'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'TRAUML'.
  alv_git_fieldcat-seltext_l = 'Transit/Transf.'. "'Total Stock in Transit and in Transfer'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SALK3_VT'.
  alv_git_fieldcat-seltext_l = 'Val. in Trans/Tfr'. "'Value in Transit/Transfer'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'INSME'.
  alv_git_fieldcat-seltext_l = 'In Quality Insp.'. "'Stock in Quality Inspection'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SALK3_VQ'.
  alv_git_fieldcat-seltext_l = 'Value in QualInsp.'. "'Value of Quality Inspection '.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'EINME'.
  alv_git_fieldcat-seltext_l = 'Restricted-Use'. "'Total Stock of All Restricted Batches'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SALK3_VR'.
  alv_git_fieldcat-seltext_l = 'Value of Restricted'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SPEME'.
  alv_git_fieldcat-seltext_l = 'Blocked'. "'Blocked Stock'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SALK3_VB'.
  alv_git_fieldcat-seltext_l = 'Value BlockedStock'. "'Value of Blocked'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'RETME'.
  alv_git_fieldcat-seltext_l = 'Returns'. "'Blocked Stock Returns'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = ''.
  alv_git_fieldcat-tabname   = 'IT_ALV '.
  alv_git_fieldcat-fieldname = 'SALK3_RB'.
  alv_git_fieldcat-seltext_l = 'Value Rets Blocked'. "'Value of Returns Blocked'.
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
FORM alv_listado  USING ppp_itab LIKE it_alv[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      is_layout          = lf_layout
      it_fieldcat        = alv_git_fieldcat[]
*     it_special_groups  = lf_sp_group
      i_save             = 'X'
    TABLES
      t_outtab           = ppp_itab.
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
*FORM pf_status USING rt_extab TYPE slis_t_extab.
*
*  SET PF-STATUS 'ZSTANDARD'.
*
*ENDFORM.

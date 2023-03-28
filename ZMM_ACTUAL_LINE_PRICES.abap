*&---------------------------------------------------------------------*
*& Report  ZMM_ACTUAL_LINE_PRICES
*&
*&---------------------------------------------------------------------*
*& Creación de Johan Rodríguez
*& Consultor ABAP 09/03/2023
*&---------------------------------------------------------------------*
REPORT zmm_actual_line_prices.

TABLES: ekbe, glpca, lfa1, tcurr, mara.

DATA: t_ekbe  TYPE TABLE OF ekbe,
      t_glpca TYPE TABLE OF glpca,
      t_tcurr TYPE TABLE OF tcurr,
      t_lfa1  TYPE TABLE OF lfa1,
      t_mara  TYPE TABLE OF mara,
      t_skat  TYPE TABLE OF skat.

DATA: wa_ekbe  TYPE ekbe,
      wa_glpca TYPE glpca,
      wa_tcurr TYPE tcurr,
      wa_lfa1  TYPE lfa1,
      wa_mara  TYPE mara,
      wa_skat  TYPE skat.

DATA: week_number TYPE scal-week.

DATA: it_alv TYPE TABLE OF zmm_actual_line,
      wa_alv TYPE zmm_actual_line.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME." TITLE text-t01.
PARAMETERS: p_rbukrs TYPE glpca-rbukrs DEFAULT '0547' OBLIGATORY.                       "Company Code
PARAMETERS: p_fiscal TYPE glpca-ryear DEFAULT sy-datum(4) OBLIGATORY.    "Fiscal Year
PARAMETERS: p_number TYPE glpca-racct DEFAULT '210004010'.                 "Account Number

PARAMETERS: p_rad1 TYPE c RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND u1.
PARAMETERS: p_rad2 TYPE c RADIOBUTTON GROUP rb1.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME." TITLE text-t01.
SELECT-OPTIONS: s_month FOR glpca-poper MODIF ID aa.                     "Montly
SELECT-OPTIONS s_weekly FOR glpca-budat MODIF ID bb.                     "Weekly
SELECTION-SCREEN: END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_rad1 EQ 'X'.
      REFRESH s_weekly.
      IF screen-group1 EQ 'BB'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_rad2 EQ 'X'.
      REFRESH s_month.
      IF screen-group1 EQ 'AA'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  IF p_rad1 EQ 'X' AND s_month IS INITIAL.
    MESSAGE 'Posting Period must be filled' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ELSEIF p_rad2 EQ 'X' AND s_weekly IS INITIAL..
    MESSAGE 'Posting Date must be filled' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF s_month IS NOT INITIAL.

    SELECT *
     FROM glpca
     INTO TABLE t_glpca
      WHERE rbukrs EQ p_rbukrs
       AND  poper  IN s_month
       AND  ryear  EQ p_fiscal
       AND  racct  EQ p_number
       AND  bwart IN ( '101', '102' ).

  ELSE.

    SELECT *
    FROM glpca
    INTO TABLE t_glpca
    WHERE rbukrs EQ p_rbukrs
      AND ryear  EQ p_fiscal
      AND racct  EQ p_number
      AND budat  IN s_weekly.

  ENDIF.

  IF t_glpca[] IS NOT INITIAL.

    SELECT *
      FROM skat
      INTO TABLE t_skat
      FOR ALL ENTRIES IN t_glpca
      WHERE saknr EQ t_glpca-racct.

    SELECT *
      FROM ekbe
      INTO TABLE t_ekbe
      FOR ALL ENTRIES IN t_glpca
      WHERE ebeln = t_glpca-ebeln
        AND gjahr = t_glpca-ryear.

    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1
      FOR ALL ENTRIES IN t_glpca
      WHERE lifnr = t_glpca-lifnr.

    SELECT *
      FROM mara
      INTO TABLE t_mara
      FOR ALL ENTRIES IN t_glpca
      WHERE matnr = t_glpca-matnr.

  ENDIF.

  REFRESH it_alv.
  DATA: lv_do TYPE i.

  LOOP AT t_glpca INTO wa_glpca.

    wa_alv-client   = sy-mandt.
    wa_alv-refdocnr = wa_glpca-refdocnr. "Ref. Document
    wa_alv-matnr    = wa_glpca-matnr.    "Material
    wa_alv-rprctr   = wa_glpca-rprctr.   "Profit Center
    wa_alv-kostl    = wa_glpca-kostl.    "Cost Center
    wa_alv-racct    = wa_glpca-racct.    "Account Number
    wa_alv-sgtxt    = wa_glpca-sgtxt.    "Acc.Text
    wa_alv-rtcur01  = wa_glpca-rtcur.    "Curr. Key of CoCd Curr.
    wa_alv-hsl      = wa_glpca-hsl.      "In company code currency
    wa_alv-tsl      = wa_glpca-tsl.      "In transaction currency
    wa_alv-rtcur02  = wa_glpca-rtcur.    "Curr. Key Trans. Curr.
    wa_alv-poper    = wa_glpca-poper.    "Posting period
    wa_alv-budat    = wa_glpca-budat.    "Posting Date
    wa_alv-bldat    = wa_glpca-bldat.    "Document Date
    wa_alv-ebeln    = wa_glpca-ebeln.    "Purchasing Document
    wa_alv-usnam    = wa_glpca-usnam.    "User name
    wa_alv-lifnr    = wa_glpca-lifnr.    "Vendor
    wa_alv-bwart    = wa_glpca-bwart.    "MM Trans. Type
    wa_alv-bukrs    = wa_glpca-rbukrs.
    wa_alv-yearly   = wa_glpca-ryear.

    CONCATENATE wa_alv-refdocnr  wa_alv-matnr INTO  wa_alv-conct. "Concatenado

    READ TABLE t_skat INTO wa_skat WITH KEY saknr = wa_glpca-racct.
    IF sy-subrc EQ 0.
      wa_alv-sgtxt = wa_skat-txt20.
    ENDIF.

    READ TABLE t_ekbe INTO wa_ekbe WITH KEY ebeln = wa_glpca-ebeln.
    IF sy-subrc EQ 0.

      wa_alv-menge   = wa_ekbe-menge.  "CANTIDAD
      wa_alv-wrbtr   = wa_ekbe-wrbtr.  "AMOUNT IN DOCUMENT
      wa_alv-waers01 = wa_ekbe-waers.  "CURRENCY
      wa_alv-waers02 = wa_ekbe-waers.  "CURRENCY
      wa_alv-dmbtr   = wa_ekbe-dmbtr.  "AMOUNT IN LOCAL
      wa_alv-price01 = wa_ekbe-wrbtr / wa_ekbe-menge. "Precio PO en MONEDA ORIGINAL

      "TIPO DE CAMBIO
      CALL FUNCTION 'READ_EXCHANGE_RATE'
        EXPORTING
          date             = wa_alv-budat
          foreign_currency = 'USD'
          local_currency   = 'MXN'
          type_of_rate     = 'F'
        IMPORTING
          exchange_rate    = wa_alv-ukurs
        EXCEPTIONS
          no_rate_found    = 1
          no_factors_found = 2
          no_spread_found  = 3
          derived_2_times  = 4
          overflow         = 5
          zero_rate        = 6
          OTHERS           = 7.
      IF sy-subrc <> 0.
        "Implement suitable error handling here
      ENDIF.

      wa_alv-price02 = wa_alv-price01 * wa_alv-ukurs.  "Precio PO al tipo de cambio estandar
      wa_alv-hswae   = wa_ekbe-hswae.                  "Local Currency
      wa_alv-price03 = wa_ekbe-dmbtr / wa_ekbe-wrbtr.  "Tipo de Cambio Real

    ENDIF.

    wa_alv-price04 = wa_alv-price01 * wa_alv-ukurs.                       "COSTO ESTANDAR EN PESOS MXN
    wa_alv-price05 = wa_ekbe-dmbtr / wa_ekbe-wrbtr.                       "COSTO ESTANDAR EN USD{AA/AC}
    wa_alv-price06 = ( wa_alv-price03 - wa_alv-ukurs ) * wa_ekbe-wrbtr.   "VARIACIÓN POR TIPO DE CAMBIO MXN
    wa_alv-price07 = ( wa_alv-price02 - wa_alv-price04 ) * wa_alv-menge.  "VARIACION POR PRECIO DE COMPRA MXN
    wa_alv-price08 = wa_alv-price06 + wa_alv-price07.                     "PPV TOTAL DETERMINADA EN PESOS MXN
    wa_alv-price09 = wa_alv-hsl - wa_alv-price08.                         "AACOUNTING VS PPV TOTAL MXN

    "Por definir
*    wa_alv_price10 =                                                    "PPV - VARIACION EN PRECIO USD

    "Week Number
    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = wa_alv-budat
      IMPORTING
        week         = week_number
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

    wa_alv-weekly = week_number+4(2). "Semana Tabla

    CONCATENATE 'Semana' wa_alv-weekly INTO wa_alv-weekn SEPARATED BY space. "Semana Reporte

    READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_glpca-lifnr.
    IF sy-subrc EQ 0.
      wa_alv-lifnr = wa_lfa1-name1. "Vendor
    ENDIF.

    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_glpca-matnr .
    IF sy-subrc EQ 0.
      wa_alv-mtart = wa_mara-mtart. "MATERIAL TYPE
      wa_alv-meins = wa_mara-meins. "UNIDAD DE MEDIDA
    ENDIF.

    APPEND wa_alv TO it_alv.
    CLEAR: wa_alv.

  ENDLOOP.


*///////////////////////////////      ALV
  IF it_alv[] IS NOT INITIAL.

*    DELETE FROM zmm_stock_matnr.
*    COMMIT WORK AND WAIT.

    MODIFY zmm_actual_line FROM TABLE it_alv.
    COMMIT WORK AND WAIT.

    PERFORM alv_report USING it_alv[].      "LLamado al ALV

  ELSE.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.


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

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'REFDOCNR'.
  alv_git_fieldcat-reptext_ddic = 'Ref. Document'.
  alv_git_fieldcat-seltext_l    = 'Ref. Document'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'MATNR'.
  alv_git_fieldcat-reptext_ddic = 'Material'.
  alv_git_fieldcat-seltext_l    = 'Materia'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'RPRCTR'.
  alv_git_fieldcat-reptext_ddic = 'Profit Center'.
  alv_git_fieldcat-seltext_l    = 'Profit Center'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'KOSTL'.
  alv_git_fieldcat-reptext_ddic = 'Cost Center'.
  alv_git_fieldcat-seltext_l    = 'Cost Center'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'RACCT'.
  alv_git_fieldcat-reptext_ddic = 'Account Number'.
  alv_git_fieldcat-seltext_l    = 'Account Number'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'SGTXT'.
  alv_git_fieldcat-reptext_ddic = 'Acc.Text'.
  alv_git_fieldcat-seltext_l    = 'Acc.Text'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'RTCUR01'.
  alv_git_fieldcat-reptext_ddic = 'Curr. Key of CoCd Curr.'.
  alv_git_fieldcat-seltext_l    = 'Curr. Key of CoCd Curr.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'HSL'.
  alv_git_fieldcat-reptext_ddic = 'In company code currency'.
  alv_git_fieldcat-seltext_l    = 'In company code currency'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '15'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'TSL'.
  alv_git_fieldcat-reptext_ddic = 'In transaction currency'.
  alv_git_fieldcat-seltext_l    = 'In transaction currency'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'RTCUR02'.
  alv_git_fieldcat-reptext_ddic = 'Curr.Key Trans. Curr'.
  alv_git_fieldcat-seltext_l    = 'Curr.Key Trans. Curr'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'POPER'.
  alv_git_fieldcat-reptext_ddic = 'Posting period'.
  alv_git_fieldcat-seltext_l    = 'Posting period'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'BUDAT'.
  alv_git_fieldcat-reptext_ddic = 'Posting Date'.
  alv_git_fieldcat-seltext_l    = 'Posting Date'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'BLDAT'.
  alv_git_fieldcat-reptext_ddic = 'Document Date'.
  alv_git_fieldcat-seltext_l    = 'Document Date'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'EBELN'.
  alv_git_fieldcat-reptext_ddic = 'Purchasing Document'.
  alv_git_fieldcat-seltext_l    = 'Purchasing Document'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'USNAM'.
  alv_git_fieldcat-reptext_ddic = 'User name'.
  alv_git_fieldcat-seltext_l    = 'User name'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'LIFNR'.
  alv_git_fieldcat-reptext_ddic = 'Vendor'.
  alv_git_fieldcat-seltext_l    = 'Vendor'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'BWART'.
  alv_git_fieldcat-reptext_ddic = 'MM Trans. Type'.
  alv_git_fieldcat-seltext_l    = 'MM Trans. Type'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '20'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'CONCT'.
  alv_git_fieldcat-reptext_ddic = 'Concatenado'.
  alv_git_fieldcat-seltext_l    = 'Concatenado'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '15'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'MENGE'.
  alv_git_fieldcat-reptext_ddic = 'Cantidad'.
  alv_git_fieldcat-seltext_l    = 'Cantidad'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'MEINS'.
  alv_git_fieldcat-reptext_ddic = 'Unidad de Medida'.
  alv_git_fieldcat-seltext_l    = 'Unidad de Medida'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '15'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'WRBTR'.
  alv_git_fieldcat-reptext_ddic = 'Amount In Document'.
  alv_git_fieldcat-seltext_l    = 'Amount In Document'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'WAERS01'.
  alv_git_fieldcat-reptext_ddic = 'Currency'.
  alv_git_fieldcat-seltext_l    = 'Currency'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '15'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'DMBTR'.
  alv_git_fieldcat-reptext_ddic = 'Amount In Local'.
  alv_git_fieldcat-seltext_l    = 'Amount In Local'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE01'.
  alv_git_fieldcat-reptext_ddic = 'Precio Po En Moneda Original'.
  alv_git_fieldcat-seltext_l    = 'Precio Po En Moneda Original  '.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'WAERS02'.
  alv_git_fieldcat-reptext_ddic = 'Currency'.
  alv_git_fieldcat-seltext_l    = 'Currency'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'UKURS'.
  alv_git_fieldcat-reptext_ddic = 'Tc Std: Usd'.
  alv_git_fieldcat-seltext_l    = 'Tc Std: Usd'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE02'.
  alv_git_fieldcat-reptext_ddic = 'Precio Po Al Tipo De Cambio Estandar'.
  alv_git_fieldcat-seltext_l    = 'Precio Po Al Tipo De Cambio Estandar'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '6'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'HSWAE'.
  alv_git_fieldcat-reptext_ddic = 'Local Currency'.
  alv_git_fieldcat-seltext_l    = 'Local Currency'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE03'.
  alv_git_fieldcat-reptext_ddic = 'Tipo De Cambio Real'.
  alv_git_fieldcat-seltext_l    = 'Tipo De Cambio Real'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE04'.
  alv_git_fieldcat-reptext_ddic = 'Costo Estandar En Pesos Mxn'.
  alv_git_fieldcat-seltext_l    = 'Costo Estandar En Pesos Mxn'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE05'.
  alv_git_fieldcat-reptext_ddic = 'Costo Estandar En Usd'.
  alv_git_fieldcat-seltext_l    = 'Costo Estandar En Usd'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE06'.
  alv_git_fieldcat-reptext_ddic = 'Variación Por Tipo De Cambio Mxn'.
  alv_git_fieldcat-seltext_l    = 'Variación Por Tipo De Cambio Mxn'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE07'.
  alv_git_fieldcat-reptext_ddic = 'Variacion Por Precio De Compra Mxn'.
  alv_git_fieldcat-seltext_l    = 'Variacion Por Precio De Compra Mxn'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE08'.
  alv_git_fieldcat-reptext_ddic = 'Ppv Total Determinada En Pesos Mxn'.
  alv_git_fieldcat-seltext_l    = 'Ppv Total Determinada En Pesos Mxn'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE09'.
  alv_git_fieldcat-reptext_ddic = 'Aacounting Vs Ppv Total Mxn'.
  alv_git_fieldcat-seltext_l    = 'Aacounting Vs Ppv Total Mxn'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'PRICE10'.
  alv_git_fieldcat-reptext_ddic = 'Ppv - Variacion En Precio Usd'.
  alv_git_fieldcat-seltext_l    = 'Ppv - Variacion En Precio Usd'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'LIFNR'.
  alv_git_fieldcat-reptext_ddic = 'Proveedor'.
  alv_git_fieldcat-seltext_l    = 'Proveedor'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '10'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'WEEKLY'.
  alv_git_fieldcat-reptext_ddic = 'Semana'.
  alv_git_fieldcat-seltext_l    = 'Semana'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*  Por definir
*  CLEAR:alv_git_fieldcat.
*  alv_git_fieldcat-outputlen    = '12'.
*  alv_git_fieldcat-tabname      = 'IT_ALV'.
*  alv_git_fieldcat-fieldname    = 'REGLA'.
*  alv_git_fieldcat-reptext_ddic = 'Recla Coto'.
*  alv_git_fieldcat-seltext_l    = 'Recla Coto'.
*  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR:alv_git_fieldcat.
  alv_git_fieldcat-outputlen    = '12'.
  alv_git_fieldcat-tabname      = 'IT_ALV'.
  alv_git_fieldcat-fieldname    = 'MTART'.
  alv_git_fieldcat-reptext_ddic = 'Material Type'.
  alv_git_fieldcat-seltext_l    = 'Material Type'.
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
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
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
FORM pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTANDARD'.

ENDFORM.

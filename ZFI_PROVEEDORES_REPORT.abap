*&---------------------------------------------------------------------*
*& Report  ZFI_PROVEEDORES_REPORT
*&---------------------------------------------------------------------*

REPORT  zfi_proveedores_report.

*///////////////////////////////      Table
TABLES: bsak, bsik, reguh, lfa1, lfbk, adrc, bnka.

*///////////////////////////////      Global Variables
DATA: gv_fecha     LIKE sy-datum,
      gv_acree     LIKE reguh-lifnr,
      gv_refer(20) TYPE c.

*///////////////////////////////      Entry-Screen
SELECTION-SCREEN BEGIN OF BLOCK block_1 WITH FRAME TITLE text-001.
       PARAMETERS: p_bukrs LIKE bsak-bukrs OBLIGATORY.
       SELECT-OPTIONS: s_lifnr FOR bsak-lifnr.
       SELECT-OPTIONS: s_budat FOR bsak-budat.
       PARAMETERS: p_gjahr LIKE bsak-gjahr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK block_1.

*///////////////////////////////      Structures
TYPES: BEGIN OF st_bsak,
       bukrs LIKE bsak-bukrs,
       lifnr LIKE bsak-lifnr,
       gjahr LIKE bsak-gjahr,
       xzahl LIKE bsak-xzahl,
       belnr LIKE bsak-belnr,
       budat LIKE bsak-budat,
       dmbtr LIKE bsak-dmbtr,
       END OF st_bsak.

TYPES: BEGIN OF st_reguh,
       zbukr LIKE reguh-zbukr,
       vblnr LIKE reguh-vblnr,
       zaldt LIKE reguh-zaldt,
       laufd LIKE reguh-laufd,
       lifnr LIKE reguh-lifnr,
       waers LIKE reguh-waers,
       bkref LIKE reguh-bkref,
       END OF st_reguh.

TYPES: BEGIN OF st_lfa1,
       lifnr LIKE lfa1-lifnr,
       stcd1 LIKE lfa1-stcd1,
       adrnr LIKE lfa1-adrnr,
       END OF st_lfa1.

TYPES: BEGIN OF st_lfbk,
       lifnr LIKE lfbk-lifnr,
       koinh LIKE lfbk-koinh,
       bankn LIKE lfbk-bankn,
       bankl LIKE lfbk-bankl,
       banks LIKE lfbk-banks,
       bkont LIKE lfbk-bkont,
       bvtyp LIKE lfbk-bvtyp,
       END OF st_lfbk.

TYPES: BEGIN OF st_adrc,
       addrnumber LIKE adrc-addrnumber,
            city1 LIKE adrc-city1,
       END OF st_adrc.

TYPES: BEGIN OF st_bnka,
       banks LIKE bnka-banks,
       bankl LIKE bnka-bankl,
       banka LIKE bnka-banka,
       END OF st_bnka.

TYPES: BEGIN OF st_alv,
       laufd LIKE reguh-laufd,
       bkref LIKE reguh-bkref,
       stcd1 LIKE lfa1-stcd1,
       dmbtr LIKE bsak-dmbtr,
       koinh LIKE lfbk-koinh,
       city1 LIKE adrc-city1,
       bankn LIKE lfbk-bankn,
       banka LIKE bnka-banka,
       bkont LIKE lfbk-bkont,
       waers LIKE reguh-waers,
       bvtyp LIKE lfbk-bvtyp,
       END OF st_alv.

*///////////////////////////////      Internal Tables
DATA: ti_bsak  TYPE TABLE OF st_bsak.
DATA: ti_bsik  TYPE TABLE OF st_bsak.
DATA: ti_reguh TYPE TABLE OF st_reguh.
DATA: ti_lfa1  TYPE TABLE OF st_lfa1.
DATA: ti_lfbk  TYPE TABLE OF st_lfbk.
DATA: ti_adrc  TYPE TABLE OF st_adrc.
DATA: ti_bnka  TYPE TABLE OF st_bnka.
DATA: ti_alv   TYPE TABLE OF st_alv.

*///////////////////////////////      Field Symbols
FIELD-SYMBOLS: <fs_bsak>  TYPE st_bsak,
               <fs_bsik>  TYPE st_bsak,
               <fs_reguh> TYPE st_reguh,
               <fs_lfa1>  TYPE st_lfa1,
               <fs_lfbk>  TYPE st_lfbk,
               <fs_adrc>  TYPE st_adrc,
               <fs_bnka>  TYPE st_bnka,
               <fs_alv>   TYPE st_alv.

*///////////////////////////////      Queries F01
START-OF-SELECTION.
*fecha
  SELECT bukrs lifnr gjahr xzahl belnr budat dmbtr
         FROM bsak
         INTO TABLE ti_bsak
         WHERE bukrs = p_bukrs
         AND lifnr IN s_lifnr
         AND budat IN s_budat
         AND gjahr = p_gjahr
         AND xzahl = 'X'.

*APPEND ti_bsak.
  SELECT bukrs lifnr gjahr xzahl belnr budat dmbtr
         FROM bsik
         APPENDING TABLE ti_bsak
*         INTO TABLE ti_bsik
         WHERE bukrs = p_bukrs
         AND lifnr IN s_lifnr
         AND budat IN s_budat
         AND gjahr = p_gjahr
         AND xzahl = 'X'.
*APPEND ti_bsak.


  SELECT zbukr
         vblnr
         zaldt
         laufd
         lifnr
         waers                              "consulta moneda de pago
         FROM reguh
         INTO TABLE ti_reguh
         FOR ALL ENTRIES IN ti_bsak
         WHERE vblnr = ti_bsak-belnr
         AND   zbukr = ti_bsak-bukrs
         AND   zaldt = ti_bsak-budat.
*_________________________________________________

*NIT
  SELECT lifnr
         stcd1
         adrnr              "consulta cuidad
         FROM lfa1
         INTO TABLE ti_lfa1
         FOR ALL ENTRIES IN ti_bsak
         WHERE lifnr EQ ti_bsak-lifnr.
*_________________________________________________

*BENEFICIARIO
  SELECT lifnr
         koinh
         bankn
         bankl
         banks
         bkont
         bvtyp
         FROM lfbk
         INTO TABLE ti_lfbk
         FOR ALL ENTRIES IN ti_bsak
         WHERE lifnr EQ ti_bsak-lifnr.
*_________________________________________________

*CUIDAD
  SELECT addrnumber
         city1
         FROM adrc
         INTO TABLE ti_adrc
         FOR ALL ENTRIES IN ti_lfa1
         WHERE addrnumber EQ ti_lfa1-adrnr.
*_________________________________________________

*Cuenta beneficiario
  SELECT banks bankl banka
         FROM bnka
         INTO TABLE ti_bnka
         FOR ALL ENTRIES IN ti_lfbk
         WHERE bankl EQ ti_lfbk-bankl
         AND banks EQ ti_lfbk-banks.
*_________________________________________________


*&---------------------------------------------------------------------*
*                                     OPERACIONES CON TABLAS
*&---------------------------------------------------------------------*

  LOOP AT ti_reguh ASSIGNING <fs_reguh>.
*
*  ENDLOOP.

    READ TABLE ti_bsak ASSIGNING <fs_bsak> WITH KEY bukrs = <fs_reguh>-zbukr belnr = <fs_reguh>-vblnr budat = <fs_reguh>-zaldt lifnr = <fs_reguh>-lifnr.
    IF sy-subrc = 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_reguh>-lifnr
        IMPORTING
          output = <fs_reguh>-lifnr.

      gv_fecha = <fs_reguh>-laufd.

      CONCATENATE <fs_reguh>-lifnr <fs_reguh>-laufd INTO gv_refer.

      <fs_reguh>-bkref = gv_refer.

      APPEND INITIAL LINE TO ti_alv ASSIGNING <fs_alv>.
      <fs_alv>-dmbtr = <fs_bsak>-dmbtr.
      <fs_alv>-waers = <fs_reguh>-waers.
      <fs_alv>-laufd = <fs_reguh>-laufd.
      <fs_alv>-bkref = <fs_reguh>-bkref.

      READ TABLE ti_lfbk ASSIGNING <fs_lfbk> WITH KEY lifnr = <fs_bsak>-lifnr.
      IF sy-subrc = 0.
        <fs_alv>-bankn = <fs_lfbk>-bankn.
        <fs_alv>-koinh = <fs_lfbk>-koinh.
        <fs_alv>-bkont = <fs_lfbk>-bkont.
        <fs_alv>-bvtyp = <fs_lfbk>-bvtyp.

        READ TABLE ti_bnka ASSIGNING <fs_bnka> WITH KEY banks = <fs_lfbk>-banks bankl = <fs_lfbk>-bankl.
        IF sy-subrc = 0.
          <fs_alv>-banka = <fs_bnka>-banka.
        ENDIF.

      ENDIF.

      READ TABLE ti_lfa1 ASSIGNING <fs_lfa1> WITH KEY lifnr = <fs_bsak>-lifnr.
      IF sy-subrc = 0.
        READ TABLE ti_adrc ASSIGNING <fs_adrc> WITH KEY addrnumber = <fs_lfa1>-adrnr.
        IF sy-subrc = 0.
          <fs_alv>-city1 = <fs_adrc>-city1.
          <fs_alv>-stcd1 = <fs_lfa1>-stcd1.
        ENDIF.
      ENDIF.

    ENDIF.


*    APPEND INITIAL LINE TO ti_alv ASSIGNING <fs_alv>.

*    READ TABLE ti_reguh ASSIGNING <fs_reguh> WITH KEY ZBUKR = <fs_reguh>-ZBUKR vblnr = <fs_reguh>-vblnr zaldt = <fs_reguh>-zaldt.
*      IF sy-subrc = 0.
*    <fs_alv>-laufd = <fs_reguh>-laufd.
*      ENDIF.
*
*    READ TABLE ti_reguh ASSIGNING <fs_reguh> WITH KEY laufd = <fs_reguh>-laufd lifnr = <fs_reguh>-lifnr.
*      IF sy-subrc = 0.
*    <fs_alv>-bkref = <fs_reguh>-bkref.
*      ENDIF.


*    READ TABLE ti_lfa1 ASSIGNING <fs_lfa1> WITH KEY lifnr = <fs_bsak>-lifnr.
*      IF sy-subrc = 0.
*        <fs_alv>-stcd1 = <fs_lfa1>-stcd1.
*      ENDIF.

*    READ TABLE ti_bsak ASSIGNING <fs_bsak> WITH KEY bukrs = <fs_reguh>-zbukr belnr = <fs_reguh>-vblnr budat = <fs_reguh>-zaldt.
*    IF sy-subrc = 0.
*      <fs_alv>-dmbtr = <fs_bsak>-dmbtr.
*    ENDIF.

*    READ TABLE ti_lfbk ASSIGNING <fs_lfbk> WITH KEY lifnr = <fs_bsak>-lifnr.
*    IF sy-subrc = 0.
*      <fs_alv>-bankn = <fs_lfbk>-bankn.
*    ENDIF.

*    READ TABLE ti_lfbk ASSIGNING <fs_lfbk> WITH KEY lifnr = <fs_lfbk>-lifnr.
*    IF sy-subrc = 0.
*      <fs_alv>-koinh = <fs_lfbk>-koinh.
*    ENDIF.

*    READ TABLE ti_adrc ASSIGNING <fs_adrc> WITH KEY addrnumber = <fs_adrc>-addrnumber.
*    IF sy-subrc = 0.
*      <fs_alv>-city1 = <fs_adrc>-city1.
*    ENDIF.



*    READ TABLE ti_bnka ASSIGNING <fs_bnka> WITH KEY banks = <fs_bnka>-banks bankl = <fs_bnka>-bankl.
*    IF sy-subrc = 0.
*      <fs_alv>-banka = <fs_bnka>-banka.
*    ENDIF.

*    READ TABLE ti_lfbk ASSIGNING <fs_lfbk> WITH KEY lifnr = <fs_lfbk>-lifnr.
*    IF sy-subrc = 0.
*      <fs_alv>-bkont = <fs_lfbk>-bkont.
*    ENDIF.

*    READ TABLE ti_reguh ASSIGNING <fs_reguh> WITH KEY zbukr = <fs_reguh>-zbukr lifnr = <fs_reguh>-lifnr vblnr = <fs_reguh>-vblnr.
*    IF sy-subrc = 0.
*      <fs_alv>-waers = <fs_reguh>-waers.
*    ENDIF.

*    READ TABLE ti_lfbk ASSIGNING <fs_lfbk> WITH KEY lifnr = <fs_lfbk>-lifnr.
*    IF sy-subrc = 0.
*      <fs_alv>-bvtyp = <fs_lfbk>-bvtyp.
*    ENDIF.





  ENDLOOP.

*&---------------------------------------------------------------------*
*                                     ALV
*&---------------------------------------------------------------------*

END-OF-SELECTION.

  IF ti_alv[] IS NOT INITIAL.
    SORT ti_alv DESCENDING BY KOINH.
    PERFORM alv_report USING ti_alv[].      "LLamado al alv

  ELSE.
    MESSAGE i162(00) WITH 'No Existen Datos para su Selección'.
    STOP.
  ENDIF.

  TYPE-POOLS: slis.

  DATA: lf_sp_group   TYPE slis_t_sp_group_alv,       "Manejar grupos de campos
        lf_layout     TYPE slis_layout_alv,           "Manejar diseño de layout
        it_topheader  TYPE slis_t_listheader,         "Manejar cabecera del rep
        wa_top        LIKE LINE OF it_topheader.      "Línea para cabecera

*&---------------------------------------------------------------------*
*        Tablas. Catálogo de campos / manejar catálogo de parámetros
*&---------------------------------------------------------------------*

  DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

*&---------------------------------------------------------------------*
*                           Llamado al ALV
*&---------------------------------------------------------------------*




*&---------------------------------------------------------------------*
*                          PERFORMS del ALV
*&---------------------------------------------------------------------*

FORM alv_report  USING  pp_itab LIKE ti_alv[].

  PERFORM sp_group_build USING lf_sp_group[].
  PERFORM alv_ini_fieldcat.
  PERFORM layout_build USING lf_layout.
  PERFORM alv_listado USING pp_itab[].

ENDFORM.                    " ALV_REPORT

*&---------------------------------------------------------------------*
*                 FORM 1 del llamado al ALV
*&---------------------------------------------------------------------*

FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = text-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.                               " SP_GROUP_BUILD3

*&---------------------------------------------------------------------*
*                 FORM 2 del llamado al ALV
*&---------------------------------------------------------------------*

FORM alv_ini_fieldcat.

*Fecha
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'LAUFD'.
  alv_git_fieldcat-seltext_m   = 'Fecha'.
  alv_git_fieldcat-seltext_l   = 'Fecha'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Referencia
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BKREF'.
  alv_git_fieldcat-seltext_m   = 'Referencia'.
  alv_git_fieldcat-seltext_l   = 'Referencia'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*NIT
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'STCD1'.
  alv_git_fieldcat-seltext_m   = 'NIT'.
  alv_git_fieldcat-seltext_l   = 'NIT'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Monto
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'DMBTR'.
  alv_git_fieldcat-seltext_m   = 'Monto'.
  alv_git_fieldcat-seltext_l   = 'Monto'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Beneficiario
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'KOINH'.
  alv_git_fieldcat-seltext_m   = 'Beneficiario'.
  alv_git_fieldcat-seltext_l   = 'Beneficiario'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Ciudad
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'CITY1'.
  alv_git_fieldcat-seltext_m   = 'Ciudad'.
  alv_git_fieldcat-seltext_l   = 'Ciudad'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Cuenta Beneficiario
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BANKN'.
  alv_git_fieldcat-seltext_m   = 'Cuenta Beneficiario'.
  alv_git_fieldcat-seltext_l   = 'Cta. Beneficiario'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Banco Beneficiario
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BANKA'.
  alv_git_fieldcat-seltext_m   = 'Banco Beneficiario'.
  alv_git_fieldcat-seltext_l   = 'Bco. Beneficiario'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Tipo de cuenta
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BKONT'.
  alv_git_fieldcat-seltext_m   = 'Tipo de Cuenta'.
  alv_git_fieldcat-seltext_l   = 'Tipo Cuenta'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Moneda de pago
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'WEARS'.
  alv_git_fieldcat-seltext_m   = 'Moneda de Pago'.
  alv_git_fieldcat-seltext_l   = 'Mondeda Pago'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Tipo de Pago
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BVTYP'.
  alv_git_fieldcat-seltext_m   = 'Tipo de Pago'.
  alv_git_fieldcat-seltext_l   = 'Tipo Pago'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.


  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-sp_group = 'A'.
  MODIFY alv_git_fieldcat FROM alv_git_fieldcat
  TRANSPORTING sp_group WHERE fieldname = 'VBTYP'.

ENDFORM.                    " ALV_INI_FIELDCAT

*&---------------------------------------------------------------------*
*                   FORM 3 del llamado al ALV
*&---------------------------------------------------------------------*

* FORM LAYOUT_BUILD para diseñar el Layout de salida del ALV
FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.

*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.                    "LAYOUT_BUILD

*&---------------------------------------------------------------------*
*                 FORM 4 del llamado al ALV
*&---------------------------------------------------------------------*

FORM alv_listado  USING ppp_itab LIKE ti_alv[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_buffer_active          = 'X'
*            I_BACKGROUND_ID          = 'ALV_BACKGROUND'
            i_callback_top_of_page = 'TOP_OF_PAGE'
            i_callback_program       = sy-repid
*            i_callback_pf_status_set = 'PF_STATUS'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            is_layout                = lf_layout
            it_fieldcat              = alv_git_fieldcat[]
*            it_special_groups        = lf_sp_group
            i_save                   = 'X'
       TABLES
            t_outtab                 = ppp_itab.

ENDFORM.                    " ALV_LISTADO

*&---------------------------------------------------------------------*
*                   LLAMADA AL STANDARD
*&---------------------------------------------------------------------*

*FORM pf_status USING rt_extab TYPE slis_t_extab.
*
*  SET PF-STATUS 'STANDARD'.
*
*ENDFORM.                    "pf_status

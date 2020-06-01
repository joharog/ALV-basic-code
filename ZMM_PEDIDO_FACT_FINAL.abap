*&---------------------------------------------------------------------*
*&
*&Report  ZMM_PEDIDO_FACT_FINAL_3
*&
*&---------------------------------------------------------------------*


REPORT  zmm_pedido_fact_final_3.

TABLES: ekpo, eban, ekko, mseg, makt, ekbe, eket, mkpf.

DATA: g_repid TYPE sy-repid,
      canti TYPE i,
      matnr2 LIKE mara-matnr.


*&---------------------------------------------------------------------*
*&         VARIABLE DECLARATIONS
*&---------------------------------------------------------------------*
DATA : v_rep_id     TYPE sy-repid,   " report id
       v_istat      TYPE tj02t-istat, " order status
       v_cdate      TYPE sy-datum,    " current system date
       v_line_count TYPE i,      " number of lines in final internal table
       v_filename   TYPE string.   " path for download error log


*&---------------------------------------------------------------------*
*          BAPI DECLARATIONS
*&---------------------------------------------------------------------*
DATA:  lv_ponumber LIKE bapimepoheader-po_number,
       tl_poitem   TYPE TABLE OF bapimepoitem WITH HEADER LINE,
       tl_poitemx  TYPE TABLE OF bapimepoitemx WITH HEADER LINE,
       tl_return   TYPE TABLE OF bapiret2 WITH HEADER LINE.

CLEAR: tl_poitem,  tl_poitem[],
       tl_poitemx, tl_poitemx[],
       tl_return,  tl_return[].

*DATA: tl_poitem  LIKE bapimepoitem  OCCURS 0 WITH HEADER LINE,
*      tl_poitemx LIKE bapimepoitemx OCCURS 0 WITH HEADER LINE,
*      tl_return  LIKE bapiret2      OCCURS 0 WITH HEADER LINE,
*      tl_pocond  LIKE bapimepocond  OCCURS 0 WITH HEADER LINE,
*      tl_pocondx LIKE BAPIMEPOCONDX OCCURS 0 WITH HEADER LINE,
*      c_x LIKE bapita-wait.


*&---------------------------------------------------------------------*
*          STRUCTURES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF st_ekpo,
       ebeln LIKE ekpo-ebeln,
       ebelp LIKE ekpo-ebelp,
       matnr LIKE ekpo-matnr,
       aedat LIKE ekpo-aedat,
       menge LIKE ekpo-menge,
       erekz LIKE ekpo-erekz,
       bnfpo LIKE ekpo-bnfpo,
       banfn LIKE ekpo-banfn,
       loekz LIKE ekpo-loekz,
       txz01 LIKE ekpo-txz01,
       END OF st_ekpo.


TYPES: BEGIN OF st_ekko,
       ebeln LIKE ekko-ebeln,
*       ebelp like ekko-ebelp,
       bedat LIKE ekko-bedat,
       ekorg LIKE ekko-ekorg,
       bsart LIKE ekko-bsart,
       ekgrp LIKE ekko-ekgrp,
       END OF st_ekko.


TYPES: BEGIN OF st_eban,
       ekgrp LIKE eban-ekgrp,
       bnfpo LIKE eban-bnfpo,
       banfn LIKE eban-banfn,
       matnr LIKE eban-matnr,
       END OF st_eban.


TYPES: BEGIN OF st_mseg,
       matnr LIKE mseg-matnr,
       ebeln LIKE mseg-ebeln,
       ebelp LIKE mseg-ebelp,
       END OF st_mseg.


*TYPES: BEGIN OF st_makt,
*       maktx LIKE makt-maktx,
*       matnr LIKE makt-matnr,
*       END OF st_makt.


TYPES: BEGIN OF st_ekbe,
       menge LIKE ekbe-menge,
       ebeln LIKE ekbe-ebeln,
       budat LIKE ekbe-budat,
       END OF st_ekbe.


TYPES: BEGIN OF st_eket,
       wemng LIKE eket-wemng,
       ebeln LIKE eket-ebeln,
       ebelp LIKE eket-ebelp,
       END OF st_eket.


*TYPES: BEGIN OF st_mkpf,
*       budat LIKE mkpf-budat,
*       END OF st_mkpf.

* for table jest (object status details)
TYPES : BEGIN OF t_jest,
          objnr TYPE jest-objnr, " object number
        END OF t_jest.


* table which contains the selected records to be processed
TYPES:  BEGIN OF t_process,
        numer TYPE i,
        ebeln LIKE ekko-ebeln,
        ebelp LIKE ekpo-ebelp,
        bedat LIKE ekko-bedat,
        matnr LIKE mseg-matnr,
        txz01 LIKE ekpo-txz01,
        menge LIKE ekbe-menge,
        banfn LIKE ekpo-banfn,
        bnfpo LIKE ekpo-bnfpo,
        wemng LIKE eket-wemng,
        aedat LIKE ekpo-aedat,
        ekgrp LIKE eban-ekgrp,
        poren LIKE eket-wemng,
        erekz LIKE ekpo-erekz,
        END OF t_process.


DATA : it_process TYPE STANDARD TABLE OF t_process,
       wa_process TYPE t_process.

DATA : it_jest TYPE STANDARD TABLE OF t_jest,
       wa_jest TYPE t_jest.


TYPES: BEGIN OF st_alv,
       numer TYPE i,
       ebeln LIKE ekko-ebeln,
       ebelp LIKE ekpo-ebelp,
       bedat LIKE ekko-bedat,
       matnr LIKE mseg-matnr,
       txz01 LIKE ekpo-txz01,
       menge LIKE ekbe-menge,
       banfn LIKE ekpo-banfn,
       bnfpo LIKE ekpo-bnfpo,
       wemng LIKE eket-wemng,
       aedat LIKE ekpo-aedat,
       poren LIKE eket-wemng,
       flag  TYPE char1,

       ekorg LIKE ekko-ekorg,
       bsart LIKE ekko-bsart,
       ekgrp LIKE eban-ekgrp,
       erekz LIKE ekpo-erekz,
       END OF st_alv.


DATA: it_ekpo TYPE TABLE OF st_ekpo,
      it_eban TYPE TABLE OF st_eban,
      it_ekko TYPE TABLE OF st_ekko,
      it_mseg TYPE TABLE OF st_mseg,
*      it_makt TYPE TABLE OF st_makt,
      it_ekbe TYPE TABLE OF st_ekbe,
      it_eket TYPE TABLE OF st_eket,
*      it_mkpf TYPE TABLE OF st_mkpf,
      it_alv TYPE TABLE OF st_alv.


FIELD-SYMBOLS: <fs_ekpo> TYPE st_ekpo,
               <fs_ekko> TYPE st_ekko,
               <fs_eban> TYPE st_eban,
               <fs_mseg> TYPE st_mseg,
*               <fs_makt> TYPE st_makt,
               <fs_ekbe> TYPE st_ekbe,
               <fs_eket> TYPE st_eket,
*               <fs_mkpf> TYPE st_mkpf,
               <fs_alv>  TYPE st_alv.


*&---------------------------------------------------------------------*
*&          PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
DATA: lv_cont1  TYPE i,
      lv_cont2  TYPE i,
      lv_ptj    TYPE p DECIMALS 1,
      texto1(50) TYPE c,
      texto2(50) TYPE c.


*&---------------------------------------------------------------------*
*&          CONTANTS DECLARATION
*&---------------------------------------------------------------------*
CONSTANTS : c_check(1) VALUE 'X',    " value used to set X for a field
            c_langu(1) VALUE 'E',    " language used
            c_ustat(4) VALUE 'TECO'. " object status description


*&---------------------------------------------------------------------*
*&          Parmetros de Seleccion
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln OBLIGATORY,
                s_bedat FOR ekko-bedat OBLIGATORY,
                s_ekorg FOR ekko-ekorg,
                s_bsart FOR ekko-bsart,
                s_ekgrp FOR ekko-ekgrp.
SELECTION-SCREEN END OF BLOCK b01.


*&---------------------------------------------------------------------*
*&           Queries
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  g_repid = sy-repid.

  SELECT ebeln bedat ekorg bsart ekgrp
    FROM ekko
    INTO TABLE it_ekko
    WHERE ebeln IN s_ebeln
      AND bedat IN s_bedat
      AND ekorg IN s_ekorg
      AND bsart IN s_bsart
      AND ekgrp IN s_ekgrp.


  IF it_ekko IS NOT INITIAL.

    SELECT ebeln ebelp matnr aedat menge erekz bnfpo banfn loekz txz01
    FROM ekpo
    INTO TABLE it_ekpo
    FOR ALL ENTRIES IN it_ekko
    WHERE ebeln EQ it_ekko-ebeln
      AND ( erekz EQ '' OR elikz EQ '' )
      AND loekz <> 'L'.

    IF it_ekpo IS NOT INITIAL.

      SELECT menge ebeln budat
      FROM ekbe
      INTO TABLE it_ekbe
      FOR ALL ENTRIES IN it_ekko
      WHERE ebeln EQ it_ekko-ebeln.

      SELECT wemng ebeln ebelp
      FROM eket
      INTO TABLE it_eket
      FOR ALL ENTRIES IN it_ekko
      WHERE ebeln EQ it_ekko-ebeln.
*      AND ebelp EQ it_ekpo-ebelp.

      SELECT matnr ebeln ebelp
      FROM mseg
      INTO TABLE it_mseg
      FOR ALL ENTRIES IN it_ekko          "it_ekpo
      WHERE ebeln EQ it_ekko-ebeln.       "it_ekpo-ebeln.
*      AND ebelp EQ it_ekpo-ebelp.

      IF it_mseg IS NOT INITIAL.

        SELECT ekgrp bnfpo banfn matnr
        FROM eban
        INTO TABLE it_eban
        FOR ALL ENTRIES IN it_mseg
        WHERE matnr EQ it_mseg-matnr.

*          SELECT maktx matnr
*          FROM makt
*          INTO TABLE it_makt
*          FOR ALL ENTRIES IN it_mseg
*          WHERE matnr EQ it_mseg-matnr.

      ENDIF.

    ENDIF.

  ENDIF.



*&---------------------------------------------------------------------*
*          select user status from TJ02T for TXT04(status of an object)
*          into variable V_ISTAT
*&---------------------------------------------------------------------*
  SELECT SINGLE
    istat
  FROM tj02t
  INTO (v_istat)
  WHERE
    txt04 EQ c_ustat AND
    spras EQ c_langu.

*&---------------------------------------------------------------------*
*          select object number based upon the user status selected
*&---------------------------------------------------------------------*
  SELECT
    objnr
  FROM jest
  INTO TABLE it_jest
  WHERE
    stat EQ v_istat.
  IF sy-subrc <> 0.
    MESSAGE i162(00) WITH 'NUMERO SELECIONADO'.
  ENDIF.

  DATA: count TYPE sy-tabix.

  DESCRIBE TABLE it_ekko[] LINES lv_cont1.

*&---------------------------------------------------------------------*
*&          1)  Loops & Read Tables (Datos sin procesar bapi)
*&---------------------------------------------------------------------*
  LOOP AT it_ekko ASSIGNING <fs_ekko>.

    LOOP AT it_ekpo ASSIGNING <fs_ekpo> WHERE ebeln = <fs_ekko>-ebeln.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.
        <fs_alv>-ebeln = <fs_ekpo>-ebeln.
        <fs_alv>-ebelp = <fs_ekpo>-ebelp.
*        <fs_alv>-matnr = <fs_ekpo>-matnr.
        <fs_alv>-aedat = <fs_ekpo>-aedat.
        <fs_alv>-menge = <fs_ekpo>-menge.
        <fs_alv>-erekz = <fs_ekpo>-erekz.

*         <fs_alv>-ebeln = <fs_ekko>-ebeln.
        <fs_alv>-bedat = <fs_ekko>-bedat.
*         <fs_alv>-ekorg = <fs_ekko>-ekorg.
*         <fs_alv>-bsart = <fs_ekko>-bsart.
*         <fs_alv>-ekgrp = <fs_ekko>-ekgrp.

        <fs_alv>-txz01 = <fs_ekpo>-txz01.
        <fs_alv>-bnfpo = <fs_ekpo>-bnfpo.
        <fs_alv>-banfn = <fs_ekpo>-banfn.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_ekpo>-matnr
          IMPORTING
            output = matnr2.

        <fs_alv>-matnr = matnr2.


*        IF <fs_alv>-ebelp = '10'.
        canti = canti + 1.
        <fs_alv>-numer = canti.
*        ELSE.
*          <fs_alv>-numer = ' '.
*        ENDIF.


        READ TABLE it_eban ASSIGNING <fs_eban> WITH KEY matnr = <fs_ekpo>-matnr.
        IF sy-subrc = 0.
          <fs_alv>-ekgrp  = <fs_eban>-ekgrp.
        ENDIF.

        READ TABLE it_eket ASSIGNING <fs_eket> WITH KEY ebeln = <fs_ekpo>-ebeln ebelp = <fs_ekpo>-ebelp.
        IF sy-subrc = 0.
          <fs_alv>-wemng = <fs_eket>-wemng.
          <fs_alv>-poren = <fs_ekpo>-menge - <fs_eket>-wemng.
        ENDIF.

      ENDIF.

*      LOOP AT it_makt ASSIGNING <fs_makt> WHERE matnr = <fs_ekpo>-matnr.
*        IF sy-subrc = 0.
*        ENDIF.
*      ENDLOOP.

    ENDLOOP.
    lv_cont2 = lv_cont2 + 1.
    lv_ptj =  ( ( lv_cont2 * 100 ) / lv_cont1 ) .
    WRITE lv_ptj TO texto1.
    CONDENSE texto1.

    CONCATENATE texto1 '%' 'Completado' INTO texto2 SEPARATED BY space.
    CONDENSE texto2.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_ptj
        text       = texto2.

  ENDLOOP.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&         1)  ALV (Datos sin procesar bapi)
*&---------------------------------------------------------------------*

*  IF it_alv[] IS NOT INITIAL.
**    SORT ti_alv DESCENDING BY KOINH.
*    PERFORM alv_report USING it_alv[].      "LLamado al ALV
*
*  ELSE.
*    MESSAGE i162(00) WITH 'No existen datos para su selección'.
*    STOP.
*  ENDIF.

  IF it_alv[] IS NOT INITIAL .
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = '100'
        text       = 'Generando ALV ....... '.
    PERFORM alv_report USING it_alv[].      "LLamado al ALV
  ELSE.
    MESSAGE i162(00) WITH 'No existen datos para su selección'.
    STOP.
  ENDIF.


  TYPE-POOLS: slis.

  DATA: lf_sp_group   TYPE slis_t_sp_group_alv,                       "Grupos de campos
          lf_layout     TYPE slis_layout_alv.                         "Diseño de layout

*ALV Header
  DATA: lt_header       TYPE slis_t_listheader,                       "Header del rep
        ls_header       TYPE slis_listheader,                         "Linea del header
        lt_line         LIKE ls_header-info,
        lv_lines        TYPE i,
        lv_linesc(10)   TYPE c.

  DATA: p_status TYPE slis_t_extab.                                   "ALV Status Button
  DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.   "Parametros del catalogo


* FIELD CATALOG
  DATA : it_field1 TYPE slis_t_fieldcat_alv, "internal table for field catalog
         wa_field1 TYPE slis_fieldcat_alv.   "work area for field catalog

*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV
FORM alv_report  USING  pp_itab LIKE it_alv[].

  PERFORM sp_group_build USING lf_sp_group[].         " ALV PERFORM_1
  PERFORM alv_ini_fieldcat.                           " ALV PERFORM_2
  PERFORM layout_build USING lf_layout.               " ALV PERFORM_3
  PERFORM alv_listado USING pp_itab[].                " ALV PERFORM_4

ENDFORM.                    "alv_report


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV PERFORM_1
FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = text-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.                    "sp_group_build

*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV PERFORM_2
FORM alv_ini_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'NUMER'.
  alv_git_fieldcat-seltext_m   = 'Nro'.
  alv_git_fieldcat-seltext_l   = 'Nro'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '5'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'EBELN'.
  alv_git_fieldcat-seltext_m   = 'Perdido'.
  alv_git_fieldcat-seltext_l   = 'Nro. Pedido'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'EBELP'.
  alv_git_fieldcat-seltext_m   = 'Pos'.
  alv_git_fieldcat-seltext_l   = 'Posición'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '5'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BEDAT'.
  alv_git_fieldcat-seltext_m   = 'Fecha Pedido'.
  alv_git_fieldcat-seltext_l   = 'Fecha Pedido'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MATNR'.
  alv_git_fieldcat-seltext_m   = 'Material'.
  alv_git_fieldcat-seltext_l   = 'Material'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '18'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'TXZ01'.
  alv_git_fieldcat-seltext_m   = 'Descripción'.
  alv_git_fieldcat-seltext_l   = 'Descripción'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '40'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MENGE'.
  alv_git_fieldcat-seltext_m   = 'Cant. Pedido'.
  alv_git_fieldcat-seltext_l   = 'Cant. Pedido'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '13'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BANFN'.
  alv_git_fieldcat-seltext_m   = 'Sol. Pedido'.
  alv_git_fieldcat-seltext_l   = 'Sol. Pedido'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BNFPO'.
  alv_git_fieldcat-seltext_m   = 'Pos'.
  alv_git_fieldcat-seltext_l   = 'Pos. Solped'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '5'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'WEMNG'.
  alv_git_fieldcat-seltext_m   = 'Entregado'.
  alv_git_fieldcat-seltext_l   = 'Entregado'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '13'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'AEDAT'.
  alv_git_fieldcat-seltext_m   = 'Fecha Recep.'.
  alv_git_fieldcat-seltext_l   = 'Fecha Recep.'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '11'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'POREN'.
  alv_git_fieldcat-seltext_m   = 'Por Entregar'.
  alv_git_fieldcat-seltext_l   = 'Por Entregar'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '13'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'FLAG'.
  alv_git_fieldcat-seltext_m   = 'Procesar Cierre'.
  alv_git_fieldcat-seltext_l   = 'Procesar Cierre'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-checkbox    = 'X'.       " print as checkbox
  alv_git_fieldcat-edit        = 'X'.       " make field open for input
  alv_git_fieldcat-outputlen   = '13'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

ENDFORM.                    "alv_ini_fieldcat


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV PERFORM_3
FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.

*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.                    "layout_build


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV PERFORM_4
FORM alv_listado  USING ppp_itab LIKE it_alv[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_buffer_active          = 'X'
*            I_BACKGROUND_ID          = 'ALV_BACKGROUND'
            i_callback_top_of_page   = 'TOP-OF-PAGE'            "ALV Header
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'PF'                     "Status Bar
            i_callback_user_command  = 'USER_COMMAND'           "Comandos de usuario
            is_layout                = lf_layout
            it_fieldcat              = alv_git_fieldcat[]
*            it_special_groups        = lf_sp_group
            i_save                   = 'X'
       TABLES
            t_outtab                 = ppp_itab.

ENDFORM.                    "alv_listado


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV HEADER
FORM top-of-page.

  CLEAR lt_header[].                                               " Limpia la tabla y no repite el header.

* Titulo
  ls_header-typ = 'H'.
  ls_header-info = 'Cierre Masivo Ordenes de Compra'.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header.

* Fecha
  ls_header-typ = 'S'.
  ls_header-key = 'Fecha: '.
  CONCATENATE sy-datum+6(2) '.'
              sy-datum+4(2) '.'
              sy-datum(4)
              INTO ls_header-info.                                 "Fecha de hoy concatenada y separada por "."
  APPEND ls_header TO lt_header.
  CLEAR: ls_header.

*No. Registros en el Reporte
  DESCRIBE TABLE it_alv LINES lv_lines.
  lv_linesc = lv_lines.
  CONCATENATE ' ' ' '
*  CONCATENATE 'Nro. Registros Encontrados: ' lv_linesc             "Concatenamos Cant. de Registros
  INTO lt_line SEPARATED BY space.
  ls_header-typ = 'A'.
  ls_header-info = lt_line.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header, lt_line.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header
      i_logo             = 'LOGO_COMOLSA'.

ENDFORM.                    "top-of-page


*&---------------------------------------------------------------------*
*&      Form  pf
*&---------------------------------------------------------------------*
*       SUB-ROUTINE PF IS USED TO SET THE PF-STATUS OF THE SCREEN
*       ON WHICH THE ALV GRID IS DISPLAYED
*----------------------------------------------------------------------*
*       -->RT_EXTAB
*----------------------------------------------------------------------*
FORM pf USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZPF_STATUS_CB'.
ENDFORM.                    "pf



*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       SUB-ROUTINE USER_COMMAND IS USED TO HANDLE THE USER ACTION
*       AND EXECUTE THE APPROPIATE CODE
*----------------------------------------------------------------------*
*      -->LV_OKCODE   used to capture the function code
*                     of the user-defined push-buttons
*      -->L_SELFIELD   text
*----------------------------------------------------------------------*
FORM user_command USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield.

* assign the function code to variable v_okcode
  lv_okcode = sy-ucomm.

* handle the code execution based on the function code encountered
  CASE lv_okcode.

* when the function code is EXECUTE then process the selected records
    WHEN '&EXECUTE'.

* refresh it_process when user processes selected records
      REFRESH it_process.

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

*----------------------------------------------------------------------*
* sort the internal table by flag descending so that the selected
* records are appended at the beginning of internal table
*----------------------------------------------------------------------*
      SORT it_alv BY flag DESCENDING.

*----------------------------------------------------------------------*
* move the selected records from final internal table into another
* internal table so that they can be processed
*----------------------------------------------------------------------*
      LOOP AT it_alv ASSIGNING <fs_alv> WHERE flag = 'X'.
        wa_process-numer  = <fs_alv>-numer.
        wa_process-ebeln  = <fs_alv>-ebeln.
        wa_process-ebelp  = <fs_alv>-ebelp.
        wa_process-bedat  = <fs_alv>-bedat.
        wa_process-matnr  = <fs_alv>-matnr.
        wa_process-txz01  = <fs_alv>-txz01.
        wa_process-menge  = <fs_alv>-menge.
        wa_process-banfn  = <fs_alv>-banfn.
        wa_process-bnfpo  = <fs_alv>-bnfpo.
        wa_process-wemng  = <fs_alv>-wemng.
        wa_process-aedat  = <fs_alv>-aedat.
        wa_process-poren  = <fs_alv>-poren.

*&----------------------------------------------
*&  BAPI_PO_CHANGE
*&----------------------------------------------

        lv_ponumber = wa_process-ebeln.

        tl_poitem-po_item    = wa_process-ebelp.
        tl_poitem-material   = wa_process-matnr.
        tl_poitem-no_more_gr = 'X'.
        tl_poitem-final_inv  = 'X'.
        APPEND tl_poitem.

        tl_poitemx-po_item    = wa_process-ebelp.
        tl_poitemx-material   = wa_process-matnr.
        tl_poitemx-no_more_gr = 'X'.
        tl_poitemx-final_inv  = 'X'.
        APPEND tl_poitemx.

*        tl_pocond-itm_number = wa_process-ebelp.
*        tl_pocond-change_id  = 'U'.
*        tl_pocond-cond_updat = 'X'.
*        APPEND tl_pocond.
*
*        tl_pocondx-itm_number = wa_process-ebelp.
*        tl_pocondx-change_id  = 'U'.
*        tl_pocondx-cond_updat = 'X'.
*        APPEND tl_pocondx.


        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
           purchaseorder                = wa_process-ebeln
*           POHEADER                     =
*           POHEADERX                    =
*           POADDRVENDOR                 =
           testrun                      = space
*           MEMORY_UNCOMPLETE            =
*           MEMORY_COMPLETE              =
*           POEXPIMPHEADER               =
*           POEXPIMPHEADERX              =
*           VERSIONS                     =
           no_messaging                 = 'X'
           no_message_req               = 'X'
*           NO_AUTHORITY                 =
*           NO_PRICE_FROM_PO             =
*         IMPORTING
*           EXPHEADER                    =
*           EXPPOEXPIMPHEADER            =
         TABLES
           return                       = tl_return
           poitem                       = tl_poitem
           poitemx                      = tl_poitemx
*           pocond                       = tl_pocond                             "27.05.2020
*           pocondx                      = tl_pocondx
*           poaddrdelivery               =
*           poschedule                   =
*           poschedulex                  =
*           poaccount                    =
*           poaccountprofitsegment       =
*           poaccountx                   =
*           pocondheader                 =
*           pocondheaderx                =
*           pocond                       =
*           pocondx                      =
*           polimits                     =
*           pocontractlimits             =
*           poservices                   =
*           posrvaccessvalues            =
*           poservicestext               =
*           extensionin                  =
*           extensionout                 =
*           poexpimpitem                 =
*           poexpimpitemx                =
*           potextheader                 =
*           potextitem                   =
*           allversions                  =
*           popartner                    =
*           pocomponents                 =
*           pocomponentsx                =
*           poshipping                   =
*           poshippingx                  =
*           poshippingexp                =
*           pohistory                    =
*           pohistory_totals             =
*           poconfirmation               =
                  .

        READ TABLE tl_return WITH KEY type = 'E'.
        IF sy-subrc EQ 0.
*         An error was found, no update was done
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.    .


*&----------------------------------------------
        DATA: aux_erekz TYPE ekpo-erekz.
        DATA: aux_elikz TYPE ekpo-elikz.

*    DATA mod_ekpo TYPE TABLE OF ekpo.
*    FIELD-SYMBOLS <mod_ekpo> TYPE ekpo.
*
*    SELECT *
*    FROM ekpo
*    INTO TABLE mod_ekpo
*    WHERE ebeln EQ wa_process-ebeln
*      AND ebelp EQ wa_process-ebelp.
*
*    IF sy-subrc = 0.
*      LOOP AT mod_ekpo ASSIGNING <mod_ekpo>.
*        <mod_ekpo>-erekz =  'X'.
*        <mod_ekpo>-elikz =  'X'.
*      ENDLOOP.
*    ENDIF.
*
*    UPDATE ekpo FROM TABLE mod_ekpo.

        SELECT SINGLE erekz elikz
        FROM ekpo
        INTO (aux_erekz, aux_elikz)
        WHERE ebeln EQ wa_process-ebeln
          AND ebelp EQ wa_process-ebelp.

        IF aux_erekz IS NOT INITIAL.
          wa_process-erekz  = aux_erekz.
        ENDIF.

        APPEND wa_process TO it_process.

        CLEAR: aux_erekz, aux_elikz.
        CLEAR: wa_process-erekz, wa_process-ebeln, lv_ponumber.

        CLEAR: tl_poitem,  tl_poitem[],
               tl_poitemx, tl_poitemx[],
               tl_return,  tl_return[].
      ENDLOOP.

* refresh the ALV Grid output from internal table
      l_selfield-refresh = 'X'.

* now all the selected records by the user at run-time are appended into
* into a new internal table which can now be used to processed as per the
* user requirements
      DATA : line_count1 TYPE i.

      DESCRIBE TABLE it_process
      LINES line_count1.
      IF line_count1 GE 1.
        PERFORM user_action.
      ELSE.
        MESSAGE i162(00) WITH 'Debe seleccionar al menos un registro'.
      ENDIF.



    WHEN '&SEL_ALL'.
* to select all the records displayed in ALV Grid
      LOOP AT it_alv ASSIGNING <fs_alv>.
        <fs_alv>-flag = 'X'.
        MODIFY it_alv FROM <fs_alv>.
      ENDLOOP.
* refresh the ALV Grid output from internal table
      l_selfield-refresh = 'X'.


    WHEN '&DES_ALL'.
* to deselect all the records displayed in ALV Grid
      LOOP AT it_alv ASSIGNING <fs_alv>.
        <fs_alv>-flag = ' '.
        MODIFY it_alv FROM <fs_alv>.
      ENDLOOP.
* refresh the ALV Grid output from internal table
      l_selfield-refresh = 'X'.

  ENDCASE.

ENDFORM.                    "USER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  USER_ACTION
*&---------------------------------------------------------------------*
*       SUB-ROUTINE USER_ACTION CAN BE USED AS PER THE USER REQUIREMENT
*       TO PROCESS THE SELECTED RECORDS IN ALV GRID DISPLAY
*----------------------------------------------------------------------*
FORM user_action.
*       user code to process selected records in internal table
*       it_process

*&---------------------------------------------------------------------*
*          FIELD CATALOG FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
  PERFORM field_catalog1.

*&---------------------------------------------------------------------*
*          DISPLAY RECORDS IN ALV GRID FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
  PERFORM alv_display1.

ENDFORM.                    " F4_FILE_REQUEST


*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG1 FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
*       SUB-ROUTINE FIELD_CATALOG USED TO SET THE COLUMNS FOR
*       THE ALV GRID (OUTPUT FORMAT)
*       SETS THE COLUMN NAME AND THE OUTPUT LENGTH FOR THE FIELDS
*----------------------------------------------------------------------*
FORM field_catalog1 .

  REFRESH it_field1.
  CLEAR wa_field1.
  wa_field1-fieldname = 'NUMER'.
  wa_field1-seltext_l = 'Nro'.
  wa_field1-col_pos   = 1.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '5'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'EBELN'.
  wa_field1-seltext_m = 'Pedido'.
  wa_field1-seltext_l = 'Nro. Pedido'.
  wa_field1-col_pos   = 2.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '10'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'EBELP'.
  wa_field1-seltext_m = 'Pos'.
  wa_field1-seltext_l = 'Posición'.
  wa_field1-col_pos   = 3.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '5'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'BEDAT'.
  wa_field1-seltext_m = 'Fecha Pedido'.
  wa_field1-seltext_l = 'Fecha Pedido'.
  wa_field1-col_pos   = 4.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '10'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'MATNR'.
  wa_field1-seltext_m = 'Material'.
  wa_field1-seltext_l = 'Material'.
  wa_field1-col_pos   = 5.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '18'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'TXZ01'.
  wa_field1-seltext_m = 'Descripción'.
  wa_field1-seltext_l = 'Descripción'.
  wa_field1-col_pos   = 6.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '40'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'MENGE'.
  wa_field1-seltext_m = 'Cant. Pedido'.
  wa_field1-seltext_l = 'Cant. Pedido'.
  wa_field1-col_pos   = 7.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '13'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'BANFN'.
  wa_field1-seltext_m = 'Sol. Pedido'.
  wa_field1-seltext_l = 'Sol. Pedido'.
  wa_field1-col_pos   = 8.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '10'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'BNFPO'.
  wa_field1-seltext_m = 'Pos'.
  wa_field1-seltext_l = 'Pos. Solped'.
  wa_field1-col_pos   = 9.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '5'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'WEMNG'.
  wa_field1-seltext_m = 'Entregado'.
  wa_field1-seltext_l = 'Entregado'.
  wa_field1-col_pos   = 10.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '13'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'AEDAT'.
  wa_field1-seltext_m = 'Fecha Recep.'.
  wa_field1-seltext_l = 'Fecha Recep.'.
  wa_field1-col_pos   = 11.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '11'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'POREN'.
  wa_field1-seltext_m = 'Por Entregar'.
  wa_field1-seltext_l = 'Por Entregar'.
  wa_field1-col_pos   = 12.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '13'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

  CLEAR wa_field1.
  wa_field1-fieldname = 'EREKZ'.
  wa_field1-seltext_m = 'Factura Final'.
  wa_field1-seltext_l = 'Factura Final'.
  wa_field1-col_pos   = 13.
  wa_field1-sp_group  = 'A'.
  wa_field1-outputlen = '10'.
  wa_field1-tabname   = 'IT_PROCESS'.
  APPEND wa_field1 TO it_field1.

ENDFORM.                    " FIELD_CATALOG1


*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY1 FOR SELECTED RECORDS
*&---------------------------------------------------------------------*
*       SUB-ROUTINE ALV_DISPLAY IS USED TO SET THE PARAMETERS
*       FOR THE FUNCTION MODULE REUSE_ALV_GRID_DISPLAY
*       AND PASS THE INTERNAL TABLE EXISTING THE RECORDS TO BE
*       DISPLAYED IN THE GRID FORMAT
*----------------------------------------------------------------------*
FORM alv_display1.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
     i_callback_program                = v_rep_id        " report id
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
*     is_layout                         = wa_layout1      " for layout
     it_fieldcat                       = it_field1       " field catalog
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     it_sort                           = it_sort1        " sort info
*     IT_FILTER                         =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_process     " internal table
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_DISPLAY1

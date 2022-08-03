*&---------------------------------------------------------------------*
*& Report  ZMM_VALORACION_STOCK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmm_valoracion_stock.

TYPE-POOLS: slis.

TABLES: mara, makt,marc, mard, mbew,mchb.
*                ckmlhd
*                ckmlpp
*                ckmlcr


DATA: g_repid TYPE sy-repid.
DATA: ok_code LIKE sy-ucomm.
DATA: p_waers LIKE ckmlcr-waers.
DATA: ini_stock TYPE labst.

DATA: lv_fday TYPE sy-datum,
      lv_lday TYPE sy-datum,
      lv_pday TYPE sy-datum,
      lv_date LIKE sy-datum.

*///////////////////////////////      Entry-Screen
SELECTION-SCREEN BEGIN OF BLOCK block_1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:   s_matnr   FOR mara-matnr, "DEFAULT '51000000',      "Material
*                  s_matkl   FOR mara-matkl,      "Grupo de Articulos
*                  s_mtart   FOR mara-mtart,      "Tipo de Articulo
                  s_lgort   FOR mchb-lgort,      "Almacen
                  s_date    FOR sy-datum NO-DISPLAY.
*                  s_charg   FOR mchb-charg.      "Nro. Lote

PARAMETERS:       p_werks   LIKE marc-werks DEFAULT 'CN01' OBLIGATORY,      "Centro
                  p_lfgja   LIKE mbewh-lfgja OBLIGATORY, " DEFAULT '2021',   "Año
                  p_lfmon   LIKE mbewh-lfmon OBLIGATORY. " DEFAULT '8'.   "Periodo Contable
*                  p_waers   LIKE ckmlcr-waers DEFAULT 'VS2' OBLIGATORY.   "Moneda

SELECTION-SCREEN END OF BLOCK block_1.

SELECTION-SCREEN BEGIN OF BLOCK block_2 WITH FRAME TITLE text-002.
PARAMETERS: p_usd RADIOBUTTON GROUP 01 DEFAULT 'X',
            p_ves RADIOBUTTON GROUP 01.
SELECTION-SCREEN END OF BLOCK block_2.

SELECTION-SCREEN BEGIN OF BLOCK block_3 WITH FRAME TITLE text-003.
PARAMETERS: p_box1 AS CHECKBOX. "Visualizar stock de lotes
*            p_box2 AS CHECKBOX. "Sin lineas de stock en cero
SELECTION-SCREEN END OF BLOCK block_3.

*///// Structures & Interal Tables
TYPES: BEGIN OF st_alv,
         matnr   TYPE mara-matnr,   "Material
         maktx   TYPE makt-maktx,   "Texto Breve
         werks   TYPE marc-werks,   "Centro
         lgort   TYPE mard-lgort,   "Almacen
         charg   TYPE mchb-charg,   "Lote
         mtart   TYPE mara-mtart,   "Tipo Material
         matkl   TYPE mara-matkl,   "Grupo Articulo
         meins   TYPE mara-meins,   "Unidad Medida
         waers   TYPE t001-waers,   "Moneda
         labst   TYPE mard-labst,   "Unitario Stock Libre Util
         speme   TYPE mard-speme,   "Unitario Stock Bloqueo
         verpr   TYPE mbew-verpr,   "Precio estandard
         stprs   TYPE mbew-stprs,   "Precio Int. Periodico
         libre   TYPE mard-labst,   "Costo Stock Libre utilizacion
         block   TYPE mard-speme,   "Costo Stock Bloqueado
         lbkum   TYPE i,   "Stock total
         salk3   TYPE ckmlcr-salk3, "Valor Total
         pvprs   TYPE ckmlcr-pvprs,
         bwtar   TYPE mbewh-bwtar,  "Cl. Valoracion
       END   OF st_alv.


DATA: it_alv      TYPE TABLE OF st_alv, " Layout
      it_mara     TYPE TABLE OF mara  ,
      it_mkpf     TYPE TABLE OF mkpf  ,
*      it_mseg     TYPE TABLE OF mseg  ,


      it_makt     TYPE TABLE OF makt  ,
      it_mbewh    TYPE TABLE OF mbewh ,
      it_marc     TYPE TABLE OF marc  ,

      it_mard     TYPE TABLE OF mard  ,
*      it_mardh     TYPE STANDARD TABLE OF mardh ,
*      it_mardh     TYPE mardh OCCURS 0 WITH HEADER LINE,
*      it_mardh2     TYPE STANDARD TABLE OF mardh  ,
      it_t001k    TYPE TABLE OF t001k ,
      it_t001     TYPE TABLE OF t001  ,
      it_mbew     TYPE TABLE OF mbew  ,
      it_mchb     TYPE TABLE OF mchb  ,
      it_ckmlhd   TYPE TABLE OF ckmlhd,
      it_ckmlpp   TYPE TABLE OF ckmlpp,
      it_ckmlcr   TYPE TABLE OF ckmlcr.



*Types:
TYPES: BEGIN OF st_mardh,
       matnr TYPE mardh-matnr,
       werks TYPE mardh-werks,
*       lgort TYPE mardh-lgort,
       lfgja TYPE mardh-lfgja,
       lfmon TYPE mardh-lfmon,
       labst TYPE mardh-labst,
       END OF  st_mardh.

TYPES: BEGIN OF st_mseg,
*       mblrn TYPE mseg-mblrn,
       matnr TYPE mseg-matnr,
*       werks TYPE mseg-werks,
*       lgort TYPE mseg-lgort,
*       charg TYPE mseg-charg,
       shkzg TYPE mseg-shkzg,
*       waers TYPE mseg-waers,
*       bwtar TYPE mseg-bwtar,
       menge TYPE mseg-menge,
       END OF  st_mseg.

TYPES: BEGIN OF st_lotes,
       mblnr TYPE mseg-mblnr,
       matnr TYPE mseg-matnr,
       bwart TYPE mseg-bwart,
       werks TYPE mseg-werks,
       lgort TYPE mseg-lgort,
       charg TYPE mseg-charg,
       shkzg TYPE mseg-shkzg,
       bwtar TYPE mseg-bwtar,
       menge TYPE mseg-menge,
       END OF  st_lotes.

DATA: it_mardh   TYPE STANDARD TABLE OF st_mardh,
      lt_mardh   TYPE STANDARD TABLE OF st_mardh,
      wa_mardh   TYPE st_mardh,
      wa_mbewh   TYPE mbewh.

DATA: it_mseg    TYPE STANDARD TABLE OF st_mseg,
      lt_mseg    TYPE STANDARD TABLE OF st_mseg,
      wa_mseg   TYPE st_mseg.

DATA: it_lotes    TYPE STANDARD TABLE OF st_lotes,
      lt_lotes    TYPE STANDARD TABLE OF st_lotes,
      wa_lotes  TYPE st_lotes.

*DATA: wa_mardh TYPE mardh,


FIELD-SYMBOLS: <fs_alv>    TYPE st_alv,
               <fs_mara>   TYPE mara,
               <fs_makt>   TYPE makt,
               <fs_mseg>   TYPE st_mseg,
               <fs_lotes>  TYPE st_lotes,
               <fs_mbewh>  TYPE mbewh,
               <fs_marc>   TYPE marc,
               <fs_mard>   TYPE mard,
               <fs_mardh>  TYPE st_mardh,
               <fs_t001k>  TYPE t001k,
               <fs_t001>   TYPE t001,
               <fs_mbew>   TYPE mbew,
               <fs_mchb>   TYPE mchb,
               <fs_ckmlhd> TYPE ckmlhd,
               <fs_ckmlpp> TYPE ckmlpp,
               <fs_ckmlcr> TYPE ckmlcr.


*///////////////////////////////      Queries F01




START-OF-SELECTION.

  CONCATENATE p_lfgja p_lfmon '01'  INTO lv_fday.

* Obtener el ultimo dia del mes
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_fday
    IMPORTING
      last_day_of_month = lv_lday.

* Obtener el ultimo del mes anterior
  CALL FUNCTION 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
    EXPORTING
      i_date_old = lv_fday
    IMPORTING
      e_date_new = lv_pday.

  CLEAR: s_date.
  s_date-sign   = 'I'.
  s_date-option = 'BT'.
  s_date-low    = lv_fday.
  s_date-high   = lv_lday.
  APPEND s_date.

*	Moneda a mostrar
  IF p_usd EQ 'X'.
    p_waers = 'USD'.
  ELSEIF p_ves EQ 'X'.
    p_waers = 'VS2'.
  ENDIF.

*********************************************************
  SELECT *
    FROM mara
    INTO TABLE it_mara
    WHERE matnr IN s_matnr
      AND lvorm EQ ' '.

  IF it_mara IS NOT INITIAL.
*********************************************************
    SELECT *
      FROM mkpf
      INTO TABLE it_mkpf
      WHERE budat IN s_date.

*********************************************************

    IF p_box1 NE 'X'.

      SELECT *
        FROM mseg
        INTO CORRESPONDING FIELDS OF TABLE it_mseg
        FOR ALL ENTRIES IN it_mkpf
        WHERE mblnr EQ it_mkpf-mblnr
          AND matnr IN s_matnr
          AND werks EQ p_werks
          AND lgort IN s_lgort.

      LOOP AT it_mseg INTO wa_mseg.
        COLLECT wa_mseg INTO lt_mseg.
      ENDLOOP.

      SELECT *
       FROM ckmlhd
       INTO TABLE it_ckmlhd
       FOR ALL ENTRIES IN it_mseg
       WHERE matnr EQ it_mseg-matnr
         AND bwkey EQ p_werks.

      IF it_ckmlhd IS NOT INITIAL.

        SELECT *
         FROM ckmlcr
         INTO TABLE it_ckmlcr
         FOR ALL ENTRIES IN it_ckmlhd
         WHERE kalnr EQ it_ckmlhd-kalnr "Nro CC
           AND bdatj EQ p_lfgja         "Año
           AND poper EQ p_lfmon         "Periodo
           AND waers EQ p_waers.        "Moneda

      ENDIF.

    ELSEIF p_box1 EQ 'X'.

      SELECT *
        FROM mseg
        INTO CORRESPONDING FIELDS OF TABLE it_lotes
        FOR ALL ENTRIES IN it_mkpf
        WHERE mblnr EQ it_mkpf-mblnr
          AND matnr IN s_matnr
          AND werks EQ p_werks
          AND lgort IN s_lgort.

      SELECT *
       FROM ckmlhd
       INTO TABLE it_ckmlhd
       FOR ALL ENTRIES IN it_lotes
       WHERE matnr EQ it_lotes-matnr
         AND bwtar EQ it_lotes-bwtar
         AND bwkey EQ it_lotes-werks.

      IF it_ckmlhd IS NOT INITIAL.

        SELECT *
         FROM ckmlcr
         INTO TABLE it_ckmlcr
         FOR ALL ENTRIES IN it_ckmlhd
         WHERE kalnr EQ it_ckmlhd-kalnr "Nro CC
           AND bdatj EQ p_lfgja         "Año
           AND poper EQ p_lfmon         "Periodo
           AND waers EQ p_waers.        "Moneda

      ENDIF.

*        SORT it_lotes ASCENDING BY bwart MBLNR CHARG.

*      LOOP AT it_lotes INTO wa_lotes.
*        COLLECT wa_lotes INTO lt_lotes.
*      ENDLOOP.

    ENDIF.
*********************************************************
    SELECT *
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_mara
      WHERE matnr EQ it_mara-matnr
        AND spras EQ 'S'.

*********************************************************
*   Datos historicos
    SELECT *
      FROM mardh
      INTO CORRESPONDING FIELDS OF TABLE it_mardh
      FOR ALL ENTRIES IN it_mara
      WHERE matnr EQ it_mara-matnr
        AND werks EQ p_werks
        AND lgort IN s_lgort
        AND lfgja EQ lv_pday(4)
        AND lfmon EQ lv_pday+4(2).

    LOOP AT it_mardh INTO wa_mardh.
      COLLECT wa_mardh INTO lt_mardh.
    ENDLOOP.

*********************************************************
*    SELECT *
*      FROM mbewh
*      INTO TABLE it_mbewh
*      FOR ALL ENTRIES IN it_mardh
*      WHERE matnr EQ it_mardh-matnr
*        AND lfgja EQ p_lfgja
*        AND lfmon EQ p_lfmon
*        AND bwtar NE ''.
*
*      LOOP AT it_mbewh INTO wa_mbewh.
*        COLLECT wa_mardh INTO lt_mardh.
*      ENDLOOP.

*********************************************************

*    SELECT *
*      FROM ckmlhd
*      INTO TABLE it_ckmlhd
*      FOR ALL ENTRIES IN it_mardh
*      WHERE matnr EQ it_mardh-matnr
*        AND bwkey EQ p_werks.
*
*    IF it_ckmlhd IS NOT INITIAL.
*
*      SELECT *
*       FROM ckmlcr
*       INTO TABLE it_ckmlcr
*       FOR ALL ENTRIES IN it_ckmlhd
*       WHERE kalnr EQ it_ckmlhd-kalnr "Nro CC
*         AND bdatj EQ p_lfgja         "Año
*         AND poper EQ p_lfmon         "Periodo
*         AND waers EQ p_waers.        "Moneda
*
*    ENDIF.

  ENDIF.


  IF p_box1 NE 'X'.

    LOOP AT lt_mardh ASSIGNING <fs_mardh>.

      APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_mardh>-matnr
        IMPORTING
          output = <fs_alv>-matnr.            "Material

      <fs_alv>-werks = <fs_mardh>-werks.      "Centro

      READ TABLE it_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mardh>-matnr.
      IF sy-subrc = 0.
        <fs_alv>-maktx = <fs_makt>-maktx.     "Texto Breve
      ENDIF.

      READ TABLE it_mara ASSIGNING <fs_mara> WITH KEY matnr = <fs_mardh>-matnr.
      IF sy-subrc = 0.
        <fs_alv>-meins = <fs_mara>-meins.     "Unidad de Medida
        <fs_alv>-mtart = <fs_mara>-mtart.     "Tipo Material
        <fs_alv>-matkl = <fs_mara>-matkl.     "Grupo Articulo
      ENDIF.


      LOOP AT lt_mseg ASSIGNING <fs_mseg> WHERE matnr = <fs_mardh>-matnr.

        IF <fs_mseg>-shkzg EQ 'S'.
          <fs_alv>-lbkum = <fs_alv>-lbkum + <fs_mseg>-menge.
        ELSEIF <fs_mseg>-shkzg EQ 'H'.
          <fs_alv>-lbkum = <fs_mardh>-labst - <fs_mseg>-menge.
        ENDIF.

      ENDLOOP.

      LOOP AT it_ckmlhd ASSIGNING <fs_ckmlhd> WHERE matnr = <fs_mardh>-matnr AND bwkey = <fs_mardh>-werks.

        LOOP AT it_ckmlcr ASSIGNING <fs_ckmlcr> WHERE kalnr = <fs_ckmlhd>-kalnr.

          ADD <fs_ckmlcr>-stprs TO <fs_alv>-stprs.
          ADD <fs_ckmlcr>-pvprs TO <fs_alv>-pvprs.
          ADD <fs_ckmlcr>-salk3 TO <fs_alv>-salk3.

        ENDLOOP.

      ENDLOOP.

      <fs_alv>-waers = p_waers.


    ENDLOOP.


  ELSEIF p_box1 EQ 'X'.

    LOOP AT it_lotes ASSIGNING <fs_lotes>.

      APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_lotes>-matnr
        IMPORTING
          output = <fs_alv>-matnr.            "Material

      <fs_alv>-werks = <fs_lotes>-werks.      "Centro
      <fs_alv>-lgort = <fs_lotes>-lgort.
      <fs_alv>-charg = <fs_lotes>-charg.

      IF <fs_lotes>-shkzg EQ 'S'.
        <fs_alv>-lbkum = <fs_lotes>-menge.
      ELSEIF <fs_lotes>-shkzg EQ 'H'.
        <fs_alv>-lbkum = <fs_lotes>-menge * -1.
      ENDIF.


      READ TABLE it_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_lotes>-matnr.
      IF sy-subrc = 0.
        <fs_alv>-maktx = <fs_makt>-maktx.     "Texto Breve
      ENDIF.

      READ TABLE it_mara ASSIGNING <fs_mara> WITH KEY matnr = <fs_lotes>-matnr.
      IF sy-subrc = 0.
        <fs_alv>-meins = <fs_mara>-meins.     "Unidad de Medida
        <fs_alv>-mtart = <fs_mara>-mtart.     "Tipo Material
        <fs_alv>-matkl = <fs_mara>-matkl.     "Grupo Articulo
      ENDIF.


      LOOP AT it_ckmlhd ASSIGNING <fs_ckmlhd> WHERE matnr = <fs_lotes>-matnr
                                                AND bwkey = <fs_lotes>-werks
                                                AND bwtar = <fs_lotes>-bwtar.

*      READ TABLE it_ckmlhd ASSIGNING <fs_ckmlhd> WITH KEY matnr = <fs_lotes>-matnr
*                                                     bwkey = <fs_lotes>-werks
*                                                     bwtar = <fs_lotes>-bwtar.
*      IF sy-subrc EQ 0.

        LOOP AT it_ckmlcr ASSIGNING <fs_ckmlcr> WHERE kalnr = <fs_ckmlhd>-kalnr.

          ADD <fs_ckmlcr>-stprs TO <fs_alv>-stprs.
          ADD <fs_ckmlcr>-pvprs TO <fs_alv>-pvprs.
          ADD <fs_ckmlcr>-salk3 TO <fs_alv>-salk3.

        ENDLOOP.

*      ENDIF.


      ENDLOOP.

      <fs_alv>-waers = p_waers.

    ENDLOOP.


  ENDIF.


*  IF p_box1 NE 'X'.
*
*    LOOP AT it_mbewh ASSIGNING <fs_mbewh>.
*      APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = <fs_mbewh>-matnr
*        IMPORTING
*          output = <fs_alv>-matnr.          "Material
*
*      <fs_alv>-werks = <fs_mbewh>-bwkey.    "Centro
*      <fs_alv>-waers = p_waers.             "Moneda
*      <fs_alv>-bwtar = <fs_mbewh>-bwtar.    "Lote
*      <fs_alv>-lbkum = <fs_mbewh>-lbkum.    "Stock Total
*
*
*      READ TABLE it_ckmlhd ASSIGNING <fs_ckmlhd> WITH KEY bwtar = <fs_mbewh>-bwtar.
*      IF sy-subrc = 0.
*
*        READ TABLE it_ckmlcr ASSIGNING <fs_ckmlcr> WITH KEY kalnr = <fs_ckmlhd>-kalnr waers = p_waers.
*        IF sy-subrc = 0.
*          <fs_alv>-stprs = <fs_ckmlcr>-stprs.     "Precio
*          <fs_alv>-pvprs = <fs_ckmlcr>-pvprs.     "Precio Unitario
*          <fs_alv>-salk3 = <fs_ckmlcr>-salk3.     "Valor
*        ENDIF.
*
*      ENDIF.
*
*      READ TABLE it_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mbewh>-matnr.
*      IF sy-subrc = 0.
*        <fs_alv>-maktx = <fs_makt>-maktx.     "Texto Breve
*      ENDIF.
*
*      READ TABLE it_mara ASSIGNING <fs_mara> WITH KEY matnr = <fs_mbewh>-matnr.
*      IF sy-subrc = 0.
*        <fs_alv>-meins = <fs_mara>-meins.     "Unidad de Medida
*        <fs_alv>-mtart = <fs_mara>-mtart.     "Tipo Material
*        <fs_alv>-matkl = <fs_mara>-matkl.     "Grupo Articulo
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.

*  IF p_box1 EQ 'X'.
*
*    LOOP AT it_mseg ASSIGNING <fs_mseg>.
*      APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = <fs_mseg>-matnr
*        IMPORTING
*          output = <fs_alv>-matnr.          "Material
*
*      <fs_alv>-werks = <fs_mseg>-bwkey.    "Centro
*      <fs_alv>-waers = p_waers.             "Moneda
*      <fs_alv>-bwtar = <fs_mbewh>-bwtar.    "Lote
*      <fs_alv>-lbkum = <fs_mbewh>-lbkum.    "Stock Total
*
*
*    ENDLOOP.
*
*  ENDIF.

*
*  IF p_box1 NE 'X'.
*    LOOP AT it_mard ASSIGNING <fs_mard>.
*
*      APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = <fs_mard>-matnr
*        IMPORTING
*          output = <fs_alv>-matnr.           "Material
*
*      <fs_alv>-werks = <fs_mard>-werks.   "Centro
*      <fs_alv>-lgort = <fs_mard>-lgort.   "Almacen
*      <fs_alv>-labst = <fs_mard>-labst.   "Stock Libre Util
*      <fs_alv>-speme = <fs_mard>-speme.   "Stock Bloqueo
*      <fs_alv>-waers = p_waers.           "Moneda
*
*      LOOP AT it_mbew ASSIGNING <fs_mbew> WHERE matnr = <fs_mard>-matnr.
*        READ TABLE it_ckmlcr ASSIGNING <fs_ckmlcr> WITH KEY kalnr = <fs_mbew>-kaln1 BINARY SEARCH.
*        IF sy-subrc = 0.
*          <fs_alv>-salk3 = <fs_ckmlcr>-salk3 * 100. "Valor Total
*          <fs_alv>-verpr = <fs_ckmlcr>-pvprs * 100. "Precio Inter. Periodic
*          <fs_alv>-stprs = <fs_ckmlcr>-stprs * 100. "Precio Estandard
*        ENDIF.
*      ENDLOOP.
*
*
*      READ TABLE it_mchb ASSIGNING <fs_mchb> WITH KEY matnr = <fs_mard>-matnr
*                                                      werks = <fs_mard>-werks
*                                                      lgort = <fs_mard>-lgort
*                                                      clabs = <fs_mard>-labst.
*      IF sy-subrc = 0.
*        <fs_alv>-charg = <fs_mchb>-charg.   "Lote
*      ENDIF.
*
*      READ TABLE it_mara ASSIGNING <fs_mara> WITH KEY matnr = <fs_mard>-matnr.
*      IF sy-subrc = 0.
*        <fs_alv>-meins = <fs_mara>-meins.   "Unidad de Medida
*        <fs_alv>-mtart = <fs_mara>-mtart.   "Tipo Material
*        <fs_alv>-matkl = <fs_mara>-matkl.   "Grupo Articulo
*      ENDIF.
*
*
*      READ TABLE it_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mard>-matnr.
*      IF sy-subrc = 0.
*        <fs_alv>-maktx = <fs_makt>-maktx.    "Texto Breve
*      ENDIF.
*
*
*      READ TABLE it_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mard>-matnr
*                                                      bwkey = <fs_mard>-werks.
*      IF sy-subrc = 0.
*        IF <fs_mard>-labst IS NOT INITIAL.
*          <fs_alv>-lbkum = <fs_mbew>-lbkum.   "Stock total
**          <fs_alv>-verpr = <fs_mbew>-verpr * 100.
*        ENDIF.
*
**        READ TABLE it_t001k ASSIGNING <fs_t001k> WITH KEY bwkey = <fs_mbew>-bwkey.
**        IF sy-subrc = 0.
**          READ TABLE it_t001 ASSIGNING <fs_t001> WITH KEY bukrs = <fs_t001k>-bukrs.
**          IF sy-subrc = 0.
**            <fs_alv>-waers = <fs_t001>-waers.
**          ENDIF.
**
**        ENDIF.
*      ENDIF.
*
*      <fs_alv>-libre = <fs_alv>-labst * <fs_alv>-verpr.
*      <fs_alv>-block = <fs_alv>-speme * <fs_alv>-verpr.
*
*    ENDLOOP.
*  ENDIF.






*  IF p_box1 EQ 'X'.
*    LOOP AT it_mchb ASSIGNING <fs_mchb>.
*
*      APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = <fs_mchb>-matnr
*        IMPORTING
*          output = <fs_alv>-matnr.        "Material
*
*      <fs_alv>-charg = <fs_mchb>-charg.   "Lote
*      <fs_alv>-lgort = <fs_mchb>-lgort.   "Almacen
*      <fs_alv>-werks = <fs_mchb>-werks.   "Centro
*      <fs_alv>-labst = <fs_mchb>-clabs.   "Stock Libre Util
*      <fs_alv>-waers = p_waers.           "Moneda
*
*      IF p_box2 EQ 'X'.
*        IF <fs_mchb>-clabs > 0.
*          <fs_alv>-labst = <fs_mchb>-clabs.   "Stock Libre Util
*        ENDIF.
*
*      ENDIF.
*
**      <fs_alv>-labst = <fs_mchb>-clabs.   "Stock Libre Util
*      <fs_alv>-speme = <fs_mchb>-cinsm.   "Stock Bloqueo
*
*      CLEAR <fs_alv>-salk3.
*      LOOP AT it_mbew ASSIGNING <fs_mbew> WHERE bwtar = <fs_mchb>-charg.
*        READ TABLE it_ckmlcr ASSIGNING <fs_ckmlcr> WITH KEY kalnr = <fs_mbew>-kaln1 BINARY SEARCH..
*        IF sy-subrc = 0.
*          <fs_alv>-salk3 = <fs_ckmlcr>-salk3 * 100. "Valor Total
*          <fs_alv>-verpr = <fs_ckmlcr>-pvprs * 100. "Precio Inter. Periodic
*          <fs_alv>-stprs = <fs_ckmlcr>-stprs * 100. "Precio Estandard
*        ENDIF.
*      ENDLOOP.
*
*
*      READ TABLE it_mara ASSIGNING <fs_mara> WITH KEY matnr = <fs_mchb>-matnr.
*      IF sy-subrc = 0.
*        <fs_alv>-meins = <fs_mara>-meins.   "Unidad de Medida
*        <fs_alv>-mtart = <fs_mara>-mtart.   "Tipo Material
*        <fs_alv>-matkl = <fs_mara>-matkl.   "Grupo Articulo
*
*      ENDIF.
*
*
*      READ TABLE it_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mchb>-matnr.
*      IF sy-subrc = 0.
*        <fs_alv>-maktx = <fs_makt>-maktx.   "Texto Breve
*      ENDIF.
*
*
*      READ TABLE it_mard ASSIGNING <fs_mard> WITH KEY matnr = <fs_mchb>-matnr
*                                                      werks = <fs_mchb>-werks
*                                                      lgort = <fs_mchb>-lgort.
*      IF sy-subrc = 0.
**        <fs_alv>-labst = <fs_mard>-labst.        "Stock Libre Util
**        <fs_alv>-speme = <fs_mard>-speme.        "Stock Bloqueo
*      ENDIF.
*
*      SORT it_mbew[] BY matnr bwtar ASCENDING.
*      READ TABLE it_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mchb>-matnr
*                                                      bwkey = <fs_mchb>-werks.
**                                                      bwtar = <fs_mchb>-charg. "comment 22.03.2022
*      IF sy-subrc = 0.
*        <fs_alv>-lbkum = <fs_mbew>-lbkum.         "Stock total
**        <fs_alv>-verpr = <fs_mbew>-verpr * 100.   "Costo Unitario
*
*      ENDIF.
*
*      <fs_alv>-libre = <fs_alv>-labst * <fs_alv>-verpr.   "Costo Stock Libre
*      <fs_alv>-block = <fs_alv>-speme * <fs_alv>-verpr.   "Costo Stock Bloqueado
*
**      READ TABLE it_t001k ASSIGNING <fs_t001k> WITH KEY bwkey = <fs_mchb>-werks.
**      IF sy-subrc = 0.
**        READ TABLE it_t001 ASSIGNING <fs_t001> WITH KEY bukrs = <fs_t001k>-bukrs.
**        IF sy-subrc = 0.
**          <fs_alv>-waers = <fs_t001>-waers.
**        ENDIF.
**
**      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.
*
*  SORT it_alv[] BY matnr lgort charg ASCENDING.

*  IF p_box2 EQ 'X'.
**    DELETE it_alv WHERE lbkum EQ ''.
*    DELETE it_alv WHERE labst < 1 AND speme < 1.
*  ENDIF.
  IF p_box1 NE 'X'.

    SORT it_alv ASCENDING BY matnr .

  ELSEIF p_box1 EQ 'X'.

    SORT it_alv ASCENDING BY matnr lgort.

  ENDIF.


*///////////////////////////////      ALV

  IF it_alv[] IS NOT INITIAL.
    PERFORM alv_report USING it_alv[].
  ELSE.
    "Para los datos preestablecidos no hay existencias'
    MESSAGE i843(m7).
    STOP.
  ENDIF.

*  TYPE-POOLS: slis.

  DATA: lf_sp_group   TYPE slis_t_sp_group_alv,                       "Grupos de campos
          lf_layout     TYPE slis_layout_alv.                         "Diseño de layout

*ALV Header
  DATA: lt_header       TYPE slis_t_listheader,                       "Header del rep
        ls_header       TYPE slis_listheader,                         "Linea del header
        lt_line         LIKE ls_header-info,
        lv_lines        TYPE i,
        lv_linesc(10)   TYPE c.

* Eventos
  DATA: i_events            TYPE slis_t_event,
        w_events            TYPE slis_alv_event.

  DATA: p_status TYPE slis_t_extab.                                   "ALV Status Button
  DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.   "Parametros del catalogo




*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV
FORM alv_report  USING  pp_itab LIKE it_alv[].

  PERFORM sp_group_build USING lf_sp_group[].         " ALV PERFORM_1
  PERFORM alv_ini_fieldcat.                           " ALV PERFORM_2
  PERFORM layout_build USING lf_layout.               " ALV PERFORM_3
  PERFORM do_events.
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


*///////////////////////////////      ALV PERFORM_2
FORM alv_ini_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MATNR'.
  alv_git_fieldcat-seltext_m   = 'Material'.
  alv_git_fieldcat-seltext_l   = 'Material'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MAKTX'.
  alv_git_fieldcat-seltext_m   = 'Texto breve del material'.
  alv_git_fieldcat-seltext_l   = 'Texto breve del material'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '40'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'WERKS'.
  alv_git_fieldcat-seltext_m   = 'Centro'.
  alv_git_fieldcat-seltext_l   = 'Centro'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '6'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  IF p_box1 EQ 'X'.

    CLEAR alv_git_fieldcat.
    alv_git_fieldcat-fieldname   = 'LGORT'.
    alv_git_fieldcat-seltext_m   = 'Almacén'.
    alv_git_fieldcat-seltext_l   = 'Almacén'.
    alv_git_fieldcat-col_pos     = 0.
    alv_git_fieldcat-sp_group    = 'A'.
    alv_git_fieldcat-outputlen   = '7'.
    APPEND alv_git_fieldcat TO alv_git_fieldcat.

    CLEAR alv_git_fieldcat.
    alv_git_fieldcat-fieldname   = 'CHARG'.
    alv_git_fieldcat-seltext_m   = 'Lote'.
    alv_git_fieldcat-seltext_l   = 'Lote'.
    alv_git_fieldcat-col_pos     = 0.
    alv_git_fieldcat-sp_group    = 'A'.
    alv_git_fieldcat-outputlen   = '10'.
    APPEND alv_git_fieldcat TO alv_git_fieldcat.

  ENDIF.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MTART'.
  alv_git_fieldcat-seltext_m   = 'Tipo Material'.
  alv_git_fieldcat-seltext_l   = 'Tipo Material'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '11'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MATKL'.
  alv_git_fieldcat-seltext_m   = 'Grupo Artc.'.
  alv_git_fieldcat-seltext_l   = 'Grupo Articulo'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MEINS'.
  alv_git_fieldcat-seltext_m   = 'Unidad'.
  alv_git_fieldcat-seltext_l   = 'Unidad de Medida'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '6'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'LBKUM'.
  alv_git_fieldcat-seltext_m   = 'Stock Total'.
  alv_git_fieldcat-seltext_l   = 'Stock Total'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '12'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'STPRS'.
  alv_git_fieldcat-seltext_m   = 'Precio'.
  alv_git_fieldcat-seltext_l   = 'Precio'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '17'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'PVPRS'.
  alv_git_fieldcat-seltext_m   = 'Precio Unitario'.
  alv_git_fieldcat-seltext_l   = 'Precio Unitario'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '17'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'SALK3'.
  alv_git_fieldcat-seltext_m   = 'Valor'.
  alv_git_fieldcat-seltext_l   = 'Valor'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '17'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'WAERS'.
  alv_git_fieldcat-seltext_m   = 'Mod.'.
  alv_git_fieldcat-seltext_l   = 'Moneda'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '6'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*
*  CLEAR alv_git_fieldcat.
*  alv_git_fieldcat-fieldname   = 'LABST'.
*  alv_git_fieldcat-seltext_s   = 'Stock Libre'.
*  alv_git_fieldcat-seltext_l   = 'Stock Libre'.
*  alv_git_fieldcat-col_pos     = 0.
*  alv_git_fieldcat-sp_group    = 'A'.
*  alv_git_fieldcat-outputlen   = '13'.
**  alv_git_fieldcat-no_out      = 'X'.
*  APPEND alv_git_fieldcat TO alv_git_fieldcat.
*
*  CLEAR alv_git_fieldcat.
*  alv_git_fieldcat-fieldname   = 'SPEME'.
*  alv_git_fieldcat-seltext_m   = 'Stock Bloq.'.
*  alv_git_fieldcat-seltext_l   = 'Stock Bloq.'.
*  alv_git_fieldcat-col_pos     = 0.
*  alv_git_fieldcat-sp_group    = 'A'.
*  alv_git_fieldcat-outputlen   = '13'.
**  alv_git_fieldcat-no_out      = 'X'.
*  APPEND alv_git_fieldcat TO alv_git_fieldcat.
*
*  CLEAR alv_git_fieldcat.
*  alv_git_fieldcat-fieldname   = 'LIBRE'.
*  alv_git_fieldcat-seltext_m   = 'Costo Stock Libre'.
*  alv_git_fieldcat-seltext_l   = 'Costo Stock Libre Utilizacion'.
*  alv_git_fieldcat-col_pos     = 0.
*  alv_git_fieldcat-sp_group    = 'A'.
*  alv_git_fieldcat-outputlen   = '17'.
*  APPEND alv_git_fieldcat TO alv_git_fieldcat.
*
*  CLEAR alv_git_fieldcat.
*  alv_git_fieldcat-fieldname   = 'BLOCK'.
*  alv_git_fieldcat-seltext_m   = 'Costo Stock Bloq.'.
*  alv_git_fieldcat-seltext_l   = 'Costo Stock Bloqueado'.
*  alv_git_fieldcat-col_pos     = 0.
*  alv_git_fieldcat-sp_group    = 'A'.
*  alv_git_fieldcat-outputlen   = '17'.
*  APPEND alv_git_fieldcat TO alv_git_fieldcat.



  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-sp_group = 'A'.
*  MODIFY alv_git_fieldcat FROM alv_git_fieldcat
*  TRANSPORTING sp_group WHERE fieldname = 'VBTYP'.

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

  IF p_box1 NE 'X'.
    DELETE alv_git_fieldcat WHERE fieldname EQ 'CHARG'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program        = sy-repid
            i_buffer_active           = 'X'
            i_callback_top_of_page    = 'TOP_OF_PAGE'                   "ALV Header
*            i_callback_pf_status_set  = 'STATUS_9001'                  "Status Bar
*            i_callback_user_command   = 'USER_COMMAND_9001'            "Comandos de usuario
            is_layout                 = lf_layout
            it_fieldcat               = alv_git_fieldcat[]
            i_save                    = 'X'
            it_events                 = i_events
       TABLES
            t_outtab                  = ppp_itab.




ENDFORM.                    "alv_listado



*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV HEADER
FORM top_of_page.

  CLEAR lt_header[].                                               " Limpia la tabla y no repite el header.

* Titulo
  ls_header-typ = 'H'.
  ls_header-info = 'Reporte de Valoracion Stock'.
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
  CONCATENATE 'No. Registros: ' lv_linesc        "Concatenamos Cant. de Registros
  INTO lt_line SEPARATED BY space.
  ls_header-typ = 'A'.
  ls_header-info = lt_line.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header, lt_line.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      i_logo             = 'LOGO_MOLANCA'
      it_list_commentary = lt_header.

ENDFORM.                    "top-of-page



*&---------------------------------------------------------------------*
*&      Form  do_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM do_events.

  REFRESH i_events.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = i_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

* Leer los eventos que me interesan
  CLEAR w_events.

  READ TABLE i_events INTO w_events
  WITH KEY name = slis_ev_top_of_page.

  IF sy-subrc = 0.
    MOVE 'TOP_OF_PAGE' TO w_events-form.
    MODIFY i_events FROM w_events INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " do_events                " EXTRAERDAT



*----------------------------------------------------------------------*
*  MODULE status_9001 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZPF_STATUS'.

ENDMODULE.                 " STATUS_9001  OUTPUT

*----------------------------------------------------------------------*
*  MODULE user_command_9001 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'SALIR' OR 'CANCELAR'.
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                    "user_command_9001 INPUT

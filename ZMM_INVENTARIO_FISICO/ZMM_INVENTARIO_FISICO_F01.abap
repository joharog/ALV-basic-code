*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTARIO_FISICO_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form data_val
*&---------------------------------------------------------------------*
FORM data_val.

* REFRESH: lt_inv.
* CLEAR: ls_inv.

  CLEAR: gv_msg, gv_ok.

  SELECT * FROM t001 WHERE bukrs EQ p_bukrs. ENDSELECT.
  IF sy-subrc EQ 0.
    SELECT * FROM t001w WHERE werks EQ p_werks. ENDSELECT.
    IF sy-subrc EQ 0.
      SELECT * FROM t001l WHERE lgort EQ p_lgort AND werks EQ p_werks. ENDSELECT.
      IF sy-subrc EQ 0.
        SELECT * FROM mara WHERE matnr EQ p_matnr AND mtart EQ 'ZGAS'. ENDSELECT.
        IF sy-subrc EQ 0.
          gv_ok = abap_true.
        ELSE.
          DATA(lv_matnr) = |{ p_matnr ALPHA = OUT }|.
          gv_msg = |Material | & |{ lv_matnr }| & | no asignado al tipo ZGAS|.
          MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        gv_msg = |Almacén | & |{ p_lgort }| & | no existe|.
        MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      gv_msg = |Centro | & |{ p_werks }| & | no existe|.
      MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    gv_msg = |Sociedad | & |{ p_bukrs }| & | no existe|.
    MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form save_mat
*&---------------------------------------------------------------------*
FORM save_mat.

  DATA: ls_inv TYPE ztmm_inv_fis_gas,
        lt_inv TYPE TABLE OF ztmm_inv_fis_gas.

  CLEAR: ls_inv.
  REFRESH lt_inv.

  IF gv_ok IS NOT INITIAL.

    GET TIME STAMP FIELD ls_inv-stamp.
    ls_inv-bukrs = p_bukrs.
    ls_inv-monat = p_ersda+4(2).
    ls_inv-werks = p_werks.
    ls_inv-lgort = p_lgort.
    ls_inv-matnr = p_matnr.
    ls_inv-menge = p_menge.
*  ls_inv-meins = p_meins.
    ls_inv-ersda = p_ersda.
    APPEND ls_inv TO lt_inv.

    INSERT ztmm_inv_fis_gas FROM TABLE lt_inv.
    COMMIT WORK AND WAIT.

    MESSAGE 'Material registrado en el invetario.' TYPE 'I'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form show_data
*&---------------------------------------------------------------------*
FORM show_data .

  DATA: lt_collect TYPE TABLE OF ztmm_inv_fis_gas,
        ls_collect TYPE ztmm_inv_fis_gas.

  REFRESH: lt_collect, gt_inv.

* Mostrar Cabecera
  WRITE:   / sy-uline(62),
           / sy-vline, 3  'Centro',
          10 sy-vline, 13 'Almacen',
          22 sy-vline, 26 'Material',
          37 sy-vline, 45 'Cantidad',
          62 sy-vline,
           / sy-uline(62).

* Obtener/Ordenar datos de tabla
  SELECT * FROM ztmm_inv_fis_gas INTO TABLE @DATA(lt_centros).
  IF sy-subrc EQ 0.

    SORT lt_collect BY werks.

    LOOP AT lt_centros INTO DATA(ls_centros).
      CLEAR: ls_centros-stamp, ls_centros-bukrs, ls_centros-monat, ls_centros-lgort,
             ls_centros-matnr, ls_centros-meins, ls_centros-ersda.
      COLLECT ls_centros INTO lt_collect.
    ENDLOOP.

    LOOP AT lt_collect INTO ls_collect.
      DATA(fix_centros) = lt_centros.
      DELETE fix_centros WHERE werks NE ls_collect-werks.
      MOVE-CORRESPONDING fix_centros TO gs_inv-datos_c.
      gs_inv-centros = ls_collect-werks.
      gs_inv-total = ls_collect-menge.
      APPEND gs_inv TO gt_inv.
    ENDLOOP.

    CLEAR: ls_centros, ls_collect, gs_inv.


* Mostrar Posiciones
    LOOP AT gt_inv INTO gs_inv.
      gv_total += gs_inv-total.
      LOOP AT gs_inv-datos_c INTO DATA(ls_datos_c) WHERE werks EQ gs_inv-centros.
        WRITE: / sy-vline,   3 ls_datos_c-werks,
               10 sy-vline, 13 ls_datos_c-lgort,
               22 sy-vline, 26 ls_datos_c-matnr,
               37 sy-vline, 45 ls_datos_c-menge,
               62 sy-vline.
      ENDLOOP.
      WRITE:    / sy-uline(62),
                / sy-vline, 3 gs_inv-centros,
               45 gs_inv-total,
               62 sy-vline,
                / sy-uline(62).
    ENDLOOP.

* Mostrar Total todo los Centros
    WRITE: / sy-uline(62),
           /   ' TOTAL', 45 gv_total.

  ENDIF.
ENDFORM.

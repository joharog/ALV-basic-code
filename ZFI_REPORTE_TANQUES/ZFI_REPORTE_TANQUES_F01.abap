*&---------------------------------------------------------------------*
*& Include          ZFI_REPORTE_TANQUES_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM get_data.

  PERFORM b_header.
  PERFORM b_data.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form b_header
*&---------------------------------------------------------------------*
FORM b_header.
  SELECT SINGLE * FROM t001 INTO @DATA(ls_t001)
    WHERE bukrs IN @p_bukrs.

*  SELECT SINGLE * FROM eqbs INTO @DATA(ls_eqbs)
*    WHERE b_werk EQ @ls_t001-bwkey.

  SELECT SINGLE * FROM adrc INTO @DATA(ls_adrc)
    WHERE addrnumber EQ @ls_t001-adrnr.

  gs_header-s_code = ls_t001-bukrs.
  gs_header-s_name = |{ ls_adrc-name1 }| & |{ ls_adrc-name2 }|.
  gs_header-r_date = sy-datum.
  gs_header-r_name = TEXT-002.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form b_data
*&---------------------------------------------------------------------*
FORM b_data.

  SELECT * FROM t001k INTO TABLE @DATA(lt_t001k)
      WHERE bukrs IN @p_bukrs.

  IF sy-subrc EQ 0.
    SELECT * FROM eqbs INTO TABLE @DATA(lt_eqbs)
      FOR ALL ENTRIES IN @lt_t001k
        WHERE kunnr  IN @p_kunnr
          AND b_werk EQ @lt_t001k-bwkey
          AND lbbsa  EQ '01'  "Libre Utilización
          AND sobkz  EQ 'W'.  "Consignación Cliente

    IF sy-subrc EQ 0.
      SELECT * FROM equi INTO TABLE @DATA(lt_equi)
        FOR ALL ENTRIES IN @lt_eqbs
        WHERE equnr EQ @lt_eqbs-equnr
          AND sernr IN @p_sernr.

      IF sy-subrc EQ 0.

*       Obtener listado de ZTFI_017_TANQ
        SELECT * FROM ztfi_017_tanq INTO TABLE @DATA(lt_tanq).
        IF sy-subrc EQ 0.
          LOOP AT lt_tanq INTO DATA(ls_tanq).
            APPEND VALUE #( sign = 'I'
                            option = 'EQ'
                            low = ls_tanq-mtart )
                            TO rg_mtart.
          ENDLOOP.
        ENDIF.

        SELECT * FROM mara INTO TABLE @DATA(lt_mara)
          FOR ALL ENTRIES IN @lt_equi
          WHERE matnr EQ @lt_equi-matnr
            AND mtart IN @rg_mtart.

        IF sy-subrc EQ 0.
          SELECT * FROM anla INTO TABLE @DATA(lt_anla)
            FOR ALL ENTRIES IN @lt_equi
            WHERE sernr EQ @lt_equi-sernr
              AND anln1 IN @p_anln1.

        ENDIF.
      ENDIF.

      SELECT * FROM kna1 INTO TABLE @DATA(lt_kna1)
        FOR ALL ENTRIES IN @lt_eqbs
        WHERE kunnr EQ @lt_eqbs-kunnr.

      IF sy-subrc EQ 0.
        SELECT * FROM adrc INTO TABLE @DATA(lt_adrc)
          FOR ALL ENTRIES IN @lt_kna1
          WHERE addrnumber EQ @lt_kna1-adrnr.

        IF sy-subrc EQ 0.
          SELECT * FROM t005u INTO TABLE @DATA(lt_t005u)
            FOR ALL ENTRIES IN @lt_adrc
            WHERE bland EQ @lt_adrc-region
              AND land1 EQ @lt_adrc-country
              AND spras EQ 'S'.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT lt_anla INTO DATA(ls_anla).

    gs_data-invnr = ls_anla-invnr.
*    gs_data-ord43 = ls_anla-ord43. "(NO APLICA, VALIDAR CON FUNCIONAL)
    gs_data-anln1 = ls_anla-anln1.
    gs_data-dactv = |{ ls_anla-txt50 }{ c_slash } { ls_anla-txa50 }|.

    READ TABLE lt_equi INTO DATA(ls_equi) WITH KEY equnr = ls_anla-sernr.
    IF sy-subrc EQ 0.

      gs_data-sernr  = ls_equi-sernr.
      gs_data-matnr  = ls_equi-matnr.
      gs_data-datlwb = ls_equi-datlwb.
      gs_data-iq03   = |{ ls_equi-matnr } - { ls_equi-sernr }|.

      READ TABLE lt_eqbs INTO DATA(ls_eqbs) WITH KEY equnr = ls_equi-equnr.
      IF sy-subrc EQ 0.
        gs_data-kunnr = ls_eqbs-kunnr.

        READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_eqbs-kunnr.
        IF sy-subrc EQ 0.
          gs_data-names = |{ ls_kna1-name1 }| & |{ ls_kna1-name2 }|.

          READ TABLE lt_adrc INTO DATA(ls_adrc) WITH KEY addrnumber = ls_kna1-adrnr.
          IF sy-subrc EQ 0.

            gs_data-department = ls_adrc-city1.
            gs_data-zone       = ls_adrc-city2.
            gs_data-plant      = ls_adrc-str_suppl3.

            READ TABLE lt_t005u INTO DATA(ls_t005u) WITH KEY land1 = ls_adrc-region.
            IF sy-subrc EQ 0.
              gs_data-address = |{ ls_adrc-street }| & |{ ls_adrc-str_suppl1 }| & |{ ls_t005u-bezei }|.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND gs_data TO gt_data.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form alv_report
*&---------------------------------------------------------------------*
FORM alv_report.
  PERFORM alv_group.
  PERFORM alv_fieldcat.
  PERFORM alv_layout.
  PERFORM alv_display.
ENDFORM.

FORM alv_group.
  APPEND VALUE #( sp_group = 'A'
                  text     = TEXT-001 )
                  TO gt_slis_group.
ENDFORM.



FORM alv_fieldcat.
  REFRESH gt_slis_fieldcat.

  APPEND VALUE #( outputlen = 15
                  fieldname = 'KUNNR'
                  seltext_s = 'Cód.Cliente'
                  seltext_l = 'Código Cliente')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 10
                  fieldname = 'NAMES'
                  seltext_s = 'Razón.Soc'
                  seltext_l = 'Razón Social')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 16
                  fieldname = 'SERNR'
                  seltext_s = 'Ser.Tanq'
                  seltext_l = 'Serie Tanque')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                  fieldname = 'INVNR'
                  seltext_s = 'Núm.Economico'
                  seltext_l = 'Número Económico')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                  fieldname = 'DATLWB'
                  seltext_s = 'Fec.Asig'
                  seltext_l = 'Fecha de Asignación')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 15
                  fieldname = 'ANLN1'
                  seltext_s = 'Cód.Activo'
                  seltext_l = 'Código Activo SAP')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 30
                  fieldname = 'DACTV'
                  seltext_s = 'Desc.Activo'
                  seltext_l = 'Descripción del Activo')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 15
                  fieldname = 'ADDRESS'
                  seltext_s = 'Dirección Dest.'
                  seltext_l = 'Dirección Destinario')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 23
                  fieldname = 'DEPARTMENT'
                  seltext_s = 'Departamento Dest.'
                  seltext_l = 'Departamento Destinario')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                  fieldname = 'ZONE'
                  seltext_s = 'Zona Dest.'
                  seltext_l = 'Zona Destinario')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                  fieldname = 'PLANT'
                  seltext_s = 'Planta Dest.'
                  seltext_l = 'Planta Destinario')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( outputlen = 20
                 fieldname = 'IQ03'
                 seltext_s = 'Ref.Doc.Comercial'
                 seltext_l = 'Ref.Doc.Comercial')
                 TO gt_slis_fieldcat.
ENDFORM.

FORM alv_layout.
  CLEAR gs_slis_layout.
*  gs_slis_layout-colwidth_optimize   = 'X'.
  gs_slis_layout-zebra               = 'X'.
*  gs_slis_layout-box_fieldname       = 'CHECK'.
*  gs_slis_layout-get_selinfos        = 'X'.
*  gs_slis_layout-f2code              = 'BEAN' .
*  gs_slis_layout-confirmation_prompt = 'X'.
*  gs_slis_layout-key_hotspot         = 'X'.
*  gs_slis_layout-info_fieldname      = 'COL'.
ENDFORM.

FORM alv_display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_top_of_page   = 'TOP_OF_PAGE'
*     i_callback_pf_status_set = 'PF_STATUS'
*     i_callback_user_command  = 'USER_COMMAND'
      is_layout          = gs_slis_layout
      it_fieldcat        = gt_slis_fieldcat
      it_special_groups  = gt_slis_group
      i_save             = 'X'
    TABLES
      t_outtab           = gt_data.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*FORM pf_status USING extab TYPE slis_t_extab.
*  SET PF-STATUS 'STANDARD'.
*ENDFORM.

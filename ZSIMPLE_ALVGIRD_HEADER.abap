*&---------------------------------------------------------------------*
*& Report  ZSIMPLE_REPORT
*&
*&---------------------------------------------------------------------*


REPORT  zsimple_report.

TABLES: mara, makt.
DATA: g_repid TYPE sy-repid.

*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     Parmetros de Seleccion
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_ersda FOR mara-ersda.
SELECTION-SCREEN END OF BLOCK b01.


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     Declarcion de estructuras
TYPES: BEGIN OF st_mara,
       matnr LIKE mara-matnr,
       ersda LIKE mara-ersda,
       ernam LIKE mara-ernam,
       laeda LIKE mara-laeda,
       aenam LIKE mara-aenam,
       vpsta LIKE mara-vpsta,
       pstat LIKE mara-pstat,
       lvorm LIKE mara-lvorm,
       mtart LIKE mara-mtart,
       mbrsh LIKE mara-mbrsh,
       matkl LIKE mara-matkl,
       bismt LIKE mara-bismt,
       meins LIKE mara-meins,
       END OF st_mara.

TYPES: BEGIN OF st_makt,
       matnr LIKE makt-matnr,
       spras LIKE makt-spras,
       maktx LIKE makt-maktx,
       maktg LIKE makt-maktg,
       END OF st_makt.

TYPES: BEGIN OF st_alv,
       matnr LIKE mara-matnr,
       maktx LIKE makt-maktx,
       END OF st_alv.


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     Declaracion de tablas internas
DATA: it_mara TYPE TABLE OF st_mara,
      it_makt TYPE TABLE OF st_makt,
      it_alv  TYPE TABLE OF st_alv.

***** Work Areas *****
*DATA: wa_mara LIKE LINE OF it_mara,
*      wa_makt LIKE LINE OF it_makt,
*      wa_alv  LIKE LINE OF it_alv.

*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     Declaracion de Field Symbols
FIELD-SYMBOLS: <fs_mara> TYPE st_mara,
               <fs_makt> TYPE st_makt,
               <fs_alv>  TYPE st_alv.


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     Queries
START-OF-SELECTION.

  g_repid = sy-repid.

  SELECT matnr ersda ernam laeda aenam vpsta pstat lvorm mtart mbrsh matkl bismt meins
    FROM mara
    INTO TABLE it_mara
    WHERE matnr IN s_matnr
      AND ersda IN s_ersda.

  SELECT matnr spras maktx maktg
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_mara
    WHERE matnr = it_mara-matnr.


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     Loops & Read Tables
  LOOP AT it_mara ASSIGNING <fs_mara>.

    READ TABLE it_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara>-matnr.
    IF sy-subrc = 0.

      APPEND INITIAL LINE TO it_alv ASSIGNING <fs_alv>.
      <fs_alv>-matnr = <fs_mara>-matnr.
      <fs_alv>-maktx = <fs_makt>-maktx.
    ENDIF.

  ENDLOOP.

END-OF-SELECTION.


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV
IF it_alv[] IS NOT INITIAL.
  PERFORM alv_report USING it_alv[].
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
*Fecha
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MATNR'.
  alv_git_fieldcat-seltext_m   = 'Material'.
  alv_git_fieldcat-seltext_l   = 'Material'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '18'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

*Referencia
  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'MAKTX'.
  alv_git_fieldcat-seltext_m   = 'Descripcion'.
  alv_git_fieldcat-seltext_l   = 'Descripcion'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '40'.
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
            i_callback_pf_status_set = 'SET_PF_STATUS'          "Status Bar
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'           "Comandos de usuario
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
  ls_header-info = 'Simple ALV Report'.
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
  CONCATENATE 'Total No. Registros Encontrados: ' lv_linesc        "Concatenamos Cant. de Registros
  INTO lt_line SEPARATED BY space.
  ls_header-typ = 'A'.
  ls_header-info = lt_line.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header, lt_line.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.

ENDFORM.                    "top-of-page


*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     STATUS BUTTON
FORM set_pf_status USING p_status TYPE slis_t_extab.
  SET PF-STATUS 'ZPF_STATUS'.
ENDFORM.                    "SET_PF_STATUS

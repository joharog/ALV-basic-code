REPORT ALV_CHECKBOX_SAMPLE.

DATA: BEGIN OF t_ekko,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        flag  TYPE c,
      END OF t_ekko.

DATA: gd_repid LIKE sy-repid, "Exists
      ref_grid TYPE REF TO cl_gui_alv_grid. "new</b>

DATA: BEGIN OF it_ekko OCCURS 0.
        INCLUDE STRUCTURE t_ekko.
DATA: END OF it_ekko.

DATA: BEGIN OF it_backup OCCURS 0.
        INCLUDE STRUCTURE t_ekko.
DATA: END OF it_backup.
*ALV data declarations
TYPE-POOLS: slis.                                 "ALV Declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_layout    TYPE slis_layout_alv.


************************************************************************
*Start-of-selection.
START-OF-SELECTION.

  PERFORM data_retrieval.
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  it_backup[] = it_ekko[].
  PERFORM display_alv_report.


*&--------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM build_fieldcatalog.
  REFRESH fieldcatalog.
  CLEAR fieldcatalog.
*
  fieldcatalog-fieldname   = 'FLAG'.
  fieldcatalog-seltext_m   = 'Check'.
  fieldcatalog-input     = 'X'.
  fieldcatalog-edit     = 'X'.
  fieldcatalog-checkbox = 'X'.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'EBELN'.
  fieldcatalog-seltext_m   = 'Purchase Order'.
  fieldcatalog-input     = 'X'.
  fieldcatalog-edit     = 'X'.
  fieldcatalog-col_pos     = 2.
  APPEND fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'EBELP'.
  fieldcatalog-seltext_m   = 'PO Item'.
  fieldcatalog-col_pos     = 3.
  APPEND fieldcatalog.
  CLEAR  fieldcatalog.

ENDFORM.                    " BUILD_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM build_layout.
  "Permet d'ajuster les colonnes au text
*  gd_layout-colwidth_optimize = 'X'.
*  GD_LAYOUT-TOTALS_TEXT       = 'Totals'(201).

*  gd_layout-box_fieldname = 'SELECT'.
*  gd_layout-box_tabname   = 'IT_EKKO'.

ENDFORM.                    " BUILD_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       Display report using ALV grid
*----------------------------------------------------------------------*
FORM display_alv_report .
  gd_repid = sy-repid.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gd_repid
*     i_callback_top_of_page   = 'TOP-OF-PAGE'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
*     i_grid_title             = 'My Title'
      is_layout                = gd_layout
      it_fieldcat              = fieldcatalog[]
    TABLES
      t_outtab                 = it_ekko
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    WRITE:/ sy-subrc.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV_REPORT


*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       Retrieve data form EKPO table and populate itab it_ekko
*----------------------------------------------------------------------*
FORM data_retrieval.
  SELECT ebeln ebelp
   UP TO 10 ROWS
    FROM ekpo
    INTO CORRESPONDING FIELDS OF TABLE  it_ekko.
ENDFORM.                    " DATA_RETRIEVAL


*----------------------------------------------------------------------*
*                      FORM SET_PF_STATUS                              *
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab   TYPE  slis_t_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.                    "set_pf_status


*&--------------------------------------------------------------------*
*&      Form  user_command
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->R_UCOMM    text
*      -->RS_SELFIELDtext
*---------------------------------------------------------------------*
FORM user_command  USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.




*then insert the following code in your USER_COMMAND routine...


  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.

  IF NOT ref_grid IS INITIAL.
    CALL METHOD ref_grid->check_changed_data.
  ENDIF.

*modify

  CASE r_ucomm.
    WHEN '&IC1'.
      CHECK rs_selfield-tabindex > 0.
      IF rs_selfield-value EQ '6000000001'.
        CALL TRANSACTION 'ZDF2'.
      ENDIF.
    WHEN 'REFRESH'.

      READ TABLE it_ekko INDEX  rs_selfield-tabindex.
      IF sy-subrc = 0.
        READ TABLE it_backup INDEX rs_selfield-tabindex.
        IF sy-subrc = 0.
          IF it_ekko <> it_backup.
*  then do your check
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM data_retrieval.
      rs_selfield-refresh = 'X'.

  ENDCASE.
ENDFORM.                    "user_command

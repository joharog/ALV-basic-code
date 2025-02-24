*&---------------------------------------------------------------------*
*& Report ZFI_REPORTE_TANQUES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_reporte_tanques.

INCLUDE zfi_reporte_tanques_top.
INCLUDE zfi_reporte_tanques_f01.


SELECTION-SCREEN FUNCTION KEY 1.
*----------------------------------------------------------------------*
*                  A T  S E L E C T I O N - S C R E E N                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      zcl_abap_tools=>show_updateview( tabname = v_mat ).
  ENDCASE.

*----------------------------------------------------------------------*
*                     I N I T I A L I Z A T I O N                      *
*----------------------------------------------------------------------*
INITIALIZATION.
  CLEAR smp_dyntxt.
  smp_dyntxt-text       = 'Materiales'.
  smp_dyntxt-icon_id    = i_mat.
  smp_dyntxt-icon_text  = 'Materiales'.
  smp_dyntxt-quickinfo  = 'Materiales por tipo'.
  sscrfields-functxt_01 = smp_dyntxt.

*---------------------------------------------------------------------*
*           S T A R T  -  O F  -  S E L E C T I O N                   *
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_data.

  IF gt_data[] IS NOT INITIAL.
     PERFORM alv_report.      "LLamado al ALV
  else.
     MESSAGE 'No se encontraron datos para la selección' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

END-OF-SELECTION.

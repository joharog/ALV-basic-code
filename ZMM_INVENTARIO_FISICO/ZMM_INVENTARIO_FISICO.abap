*&---------------------------------------------------------------------*
*& Report ZMM_INVENTARIO_FISICO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_inventario_fisico.

INCLUDE zmm_inventario_fisico_top.
INCLUDE zmm_inventario_fisico_cls.
INCLUDE zmm_inventario_fisico_f01.


*----------------------------------------------------------------------*
*                  A T  S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
* Excluir botones por defecto
AT SELECTION-SCREEN OUTPUT.
  APPEND: 'SPOS' TO gt_exclude,
          'NONE' TO gt_exclude,
          'CRET' TO gt_exclude.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_exclude.


* Validacion y visualizacion de datos
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM data_val.
      PERFORM save_mat.
    WHEN 'FC02'.
      gv_show = abap_true.
  ENDCASE.


*----------------------------------------------------------------------*
*              S T A R T  -  O F  -  S E L E C T I O N                 *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  start=>main( ).

  IF gv_show IS NOT INITIAL.
    PERFORM show_data.
  ENDIF.

END-OF-SELECTION.

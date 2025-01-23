*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTARIO_FISICO_CLS
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                  C L A S S  D E F I N I C I O N                      *
*----------------------------------------------------------------------*
CLASS start DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

*----------------------------------------------------------------------*
*               C L A S S  I M P L E M E N T A T I O N                 *
*----------------------------------------------------------------------*
CLASS start IMPLEMENTATION.
  METHOD main.

    CLEAR smp_dyntxt.
    smp_dyntxt-icon_id    = i_check.
    smp_dyntxt-icon_text  = 'AGREGAR'.
    smp_dyntxt-quickinfo  = 'Guardar material'.
    sscrfields-functxt_01 = smp_dyntxt.

    CLEAR smp_dyntxt.
    smp_dyntxt-icon_id    = i_display.
    smp_dyntxt-icon_text  = 'VISUALIZAR'.
    smp_dyntxt-quickinfo  = 'Ver listado de materiales'.
    sscrfields-functxt_02 = smp_dyntxt.

* No esta tomando mas de 2 botones
*    CLEAR smp_dyntxt.
*    smp_dyntxt-icon_id    = i_exit.
*    smp_dyntxt-icon_text  = 'CANCELAR'.
*    smp_dyntxt-quickinfo  = 'Salir'.
*    sscrfields-functxt_03 = smp_dyntxt.

    CALL SELECTION-SCREEN 1100 STARTING AT 10 10.

  ENDMETHOD.
ENDCLASS.

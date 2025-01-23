*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTARIO_FISICO_TOP
*&---------------------------------------------------------------------*

TABLES: t001, t001w, t001l, mara, ekpo, sscrfields.


*----------------------------------------------------------------------*
*                     G L O B A L  D A T A
*----------------------------------------------------------------------*
DATA: gt_exclude TYPE TABLE OF sy-ucomm.

DATA: gt_inv TYPE TABLE OF zstr_inv_fis WITH HEADER LINE,
      gs_inv TYPE zstr_inv_fis.

DATA: gv_msg   TYPE char100,
      gv_ok    TYPE char1,
      gv_show  TYPE char1,
      gv_total TYPE menge_d,
      tsl      TYPE timestamp.


*----------------------------------------------------------------------*
*                         C O N S T A N T S
*----------------------------------------------------------------------*
CONSTANTS: i_check   TYPE char4 VALUE '@01@',
           i_display TYPE char4 VALUE '@10@',
           i_exit    TYPE char4 VALUE '@02@'.

DATA: smp_dyntxt TYPE smp_dyntxt.


*----------------------------------------------------------------------*
*                  S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 1100.
  PARAMETERS: p_bukrs LIKE t001-bukrs,
              p_werks LIKE t001w-werks,
              p_lgort LIKE t001l-lgort,
              p_matnr LIKE mara-matnr,
              p_menge LIKE ekpo-menge,
              p_ersda LIKE mara-ersda.
  SELECTION-SCREEN: FUNCTION KEY 1,
  FUNCTION KEY 2.
SELECTION-SCREEN END OF SCREEN 1100.

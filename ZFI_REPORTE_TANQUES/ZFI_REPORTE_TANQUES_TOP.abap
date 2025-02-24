*&---------------------------------------------------------------------*
*& Include          ZFI_REPORTE_TANQUES_TOP
*&---------------------------------------------------------------------*

TABLES: t001, adrc, t001k, mara, equi, anla, kna1, eqbs, vbak, sscrfields.

TYPE-POOLS: slis.

*----------------------------------------------------------------------*
*                   T Y P E S   D E F I N I T I O N                    *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_header,
         s_code TYPE bukrs,
         s_name TYPE char100,
         r_date TYPE datum,
         r_name TYPE char100,
       END OF ty_header,

       BEGIN OF ty_data,
         kunnr      TYPE eqbs-kunnr,        " Código Cliente (con Activo Asignado)
         names      TYPE char100,           " Razón Social
         sernr      TYPE equi-sernr,        " Serie Tanque
         invnr      TYPE anla-invnr,        " Número Económico
         ord43      TYPE anla-ord43,        " Tanque Propio
         datlwb     TYPE equi-datlwb,       " Fecha de Asignación
         anln1      TYPE anla-anln1,        " Código Activo SAP
         dactv      TYPE char200,           " Descripción del Activo
         address    TYPE char200,           " Ubicación destinatario (Dirección)
         department TYPE adrc-city1,        " Ubicación destinatario (Departamento)
         zone       TYPE adrc-city2,        " Ubicación destinatario (Zona)
         plant      TYPE adrc-str_suppl3,   " Ubicación destinatario (Planta)
         iq03       TYPE char30,                 " Hotspot TCODE: IQ03
         matnr      TYPE mara-matnr,             " Material Equipo
       END OF ty_data.


*----------------------------------------------------------------------*
*                     G L O B A L  D A T A
*----------------------------------------------------------------------*

DATA: gs_header TYPE ty_header,
      gs_data   TYPE ty_data.

DATA: gt_data TYPE TABLE OF ty_data.

DATA: smp_dyntxt TYPE smp_dyntxt.

DATA: rg_mtart TYPE RANGE OF mara-mtart,
      rs_mtart LIKE LINE OF rg_mtart.

*----------------------------------------------------------------------*
*                         C O N S T A N T S
*----------------------------------------------------------------------*
CONSTANTS: c_slash TYPE char1 VALUE '/',
           i_mat   TYPE char4 VALUE '@A6@',
           v_mat   TYPE tabname VALUE 'ZTFI_017_TANQ'.


*&---------------------------------------------------------------------*
*&           A L V  -  D E F I N I T I O N
*&---------------------------------------------------------------------*
DATA:
  gt_slis_group    TYPE slis_t_sp_group_alv,
  gt_slis_fieldcat TYPE slis_t_fieldcat_alv,
  gs_slis_layout   TYPE slis_layout_alv,
  gt_slis_header   TYPE slis_t_listheader,

  ls_refalv        TYPE REF TO cl_gui_alv_grid.


*----------------------------------------------------------------------*
*                  S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR t001-bukrs OBLIGATORY NO INTERVALS NO-EXTENSION,
                  p_kunnr FOR kna1-kunnr NO INTERVALS NO-EXTENSION,
                  p_sernr FOR anla-sernr NO INTERVALS NO-EXTENSION,
                  p_anln1 FOR anla-anln1 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b01.

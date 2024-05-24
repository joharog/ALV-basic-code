*&---------------------------------------------------------------------*
*& Report ZMM_MASS_UPLOAD_OC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_mass_upload_oc.

TYPE-POOLS: truxs.

DATA: ref_grid TYPE REF TO cl_gui_alv_grid.
DATA: lv_answer TYPE char1.
DATA: message_e TYPE char255.

DATA: t_return TYPE TABLE OF bapiret2.
*DATA: BEGIN OF t_return OCCURS 10.
*        INCLUDE STRUCTURE bapiret2.
*DATA  END OF t_return.

DATA: w_return       LIKE LINE OF t_return.
DATA: any_ocgen TYPE char1.

TYPES: BEGIN OF ty_data,
         check                      TYPE char1,       "Check Box
         orden_compra               TYPE ebeln,
*        POHEADER
         comp_code                  TYPE bukrs,
         doc_type                   TYPE esart,
         vendor                     TYPE elifn,
         purch_org                  TYPE ekorg,
         pur_group                  TYPE bkgrp,
         currency                   TYPE waers,

*        POITEM
         po_item                    TYPE ebelp,      "Número de posición del documento de compras
         acctasscat                 TYPE knttp,      "Tipo de imputación
         epstp                      TYPE pstyp,      "Tipo de posición del documento de compras
         short_text                 TYPE txz01,      "Texto breve
         material                   TYPE matnr18,    "Número de material (18 caracteres)
         plant                      TYPE ewerk,      "Centro
         stge_loc                   TYPE lgort_d,    "Almacén
         matl_group                 TYPE matkl,      "Grupo de artículos
         quantity_item              TYPE bstmg,      "Cantidad de pedido
         po_unit                    TYPE bstme_iso,  "Unidad de medida de pedido
         net_price                  TYPE bapicurext, "Importe de moneda para BAPIs (con 9 decimales)
         tax_code                   TYPE mwskz,      "Indicador IVA

*        POSCHEDULE
         delivery_date              TYPE eeind,
         quantity_sche              TYPE etmen,

*        POACCOUNT
         gl_account                 TYPE saknr, "Número de la cuenta de mayor
         costcenter                 TYPE kostl, "Centro de coste
         orderid                    TYPE aufnr, "Número de orden

*        POSERVICES
         ext_line                   TYPE extrow,      "Número de línea
         service                    TYPE asnum,       "Número de servicio
         ktext1                     TYPE sh_text1,    "Texto breve
         quantity_serv              TYPE mengev,      "Cantidad
         base_uom                   TYPE meins,       "Unidad de medida base
         gr_price                   TYPE bapigrprice, "Precio bruto
         waers_serv                 TYPE string,      "Moneda servicio

*       ZFIELDS EKPO FOR BAPI EXTENSION
         zz1_sistema_pdi            TYPE ekpo-zz1_sistema_pdi,
         zz1_tipodeservicio1_pdi    TYPE ekpo-zz1_tipodeservicio1_pdi,
         zz1_fuerzadeventa_pdi      TYPE ekpo-zz1_fuerzadeventa_pdi,
         zz1_placa1_pdi             TYPE ekpo-zz1_placa1_pdi,
         zz1_ordendetrabajo1_pdi    TYPE ekpo-zz1_ordendetrabajo1_pdi,
         zz1_tipodefecha_pdi        TYPE ekpo-zz1_tipodefecha_pdi,
         zz1_kilometraje1_pdi       TYPE ekpo-zz1_kilometraje1_pdi,

*       ZFIELDS EKKO FOR BAPI EXTENSION
         zz1_agrupadordocumento_pdh TYPE ekko-zz1_agrupadordocumento_pdh,
         zz1_piloto_pdh             TYPE ekko-zz1_piloto_pdh,
         zz1_fechapicking1_pdh      TYPE ekko-zz1_fechapicking1_pdh,
         zz1_placast_pdh            TYPE ekko-zz1_placast_pdh,
         zz1_motivostraslados_pdh   TYPE ekko-zz1_motivostraslados_pdh,
         zz1_departamento1_pdh      TYPE ekko-zz1_departamento1_pdh,
       END OF ty_data.

DATA: lt_excel TYPE TABLE OF alsmex_tabline,
      ls_excel TYPE alsmex_tabline,
      lt_data  TYPE TABLE OF ty_data,
      ls_data  TYPE ty_data.

DATA: lt_auxoc TYPE TABLE OF ty_data,
      ls_auxoc TYPE ty_data.


TYPES: BEGIN OF t_datatab,
         col1(30) TYPE c,
         col2(30) TYPE c,
         col3(30) TYPE c,
       END OF t_datatab.
DATA: it_datatab TYPE STANDARD TABLE OF t_datatab,
      wa_datatab TYPE t_datatab.
DATA: it_raw TYPE truxs_t_text_data.


CONSTANTS: c_x        VALUE 'X',
           gc_refresh TYPE syucomm VALUE '&REFRESH'.

TYPE-POOLS: slis.

DATA: lf_sp_group   TYPE slis_t_sp_group_alv,  "MANEJAR GRUPOS DE CAMPOS
      lf_layout     TYPE slis_layout_alv,      "MANEJAR DISEÑO DE LAYOUT
      it_topheader  TYPE slis_t_listheader,    "MANEJAR CABECERA DEL REP
      wa_top        LIKE LINE OF it_topheader, "LÍNEA PARA CABECERA
      lt_event_exit TYPE slis_t_event_exit,    "Event
      ls_event_exit TYPE slis_event_exit.      "Event

DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.     "Parametros del catalogo


SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename DEFAULT 'C:\'. "Users\Ung\Desktop\NUS Consulting\Arium\layoutpropio.xlsx'.
SELECTION-SCREEN: END OF BLOCK b1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'p_file'
    IMPORTING
      file_name  = p_file.

START-OF-SELECTION.
*Upload data from Excel sheet to internal table.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 11
      i_end_col               = 42
      i_end_row               = 9999
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*Populate data to internal tables and structures
  SORT lt_excel BY row col.

  LOOP AT lt_excel INTO ls_excel.

    CASE ls_excel-col.
      WHEN 1.
        ls_data-doc_type = ls_excel-value.
      WHEN 2.
        ls_data-vendor = ls_excel-value.
      WHEN 3.
        ls_data-currency = ls_excel-value.
      WHEN 4.
        ls_data-purch_org = ls_excel-value.
      WHEN 5.
        ls_data-pur_group = ls_excel-value.
      WHEN 6.
        ls_data-comp_code = ls_excel-value.
      WHEN 7.
        ls_data-po_item = ls_excel-value.
      WHEN 8.
        ls_data-acctasscat = ls_excel-value.
      WHEN 9.
        ls_data-epstp = ls_excel-value.   "Sin identificar
      WHEN 10.
        ls_data-material = ls_excel-value.
      WHEN 11.
        ls_data-short_text = ls_excel-value.
      WHEN 12.
        ls_data-quantity_item = ls_excel-value.
      WHEN 13.
        ls_data-po_unit = ls_excel-value.
      WHEN 14.
        ls_data-delivery_date = ls_excel-value.
      WHEN 15.
        ls_data-net_price = ls_excel-value.
      WHEN 16.
        ls_data-plant = ls_excel-value.
      WHEN 17.
        ls_data-matl_group = ls_excel-value.
      WHEN 18.
        ls_data-tax_code = ls_excel-value.
      WHEN 19.
        ls_data-stge_loc = ls_excel-value.
      WHEN 20.
        ls_data-gl_account = ls_excel-value.
      WHEN 21.
        ls_data-costcenter = ls_excel-value.
      WHEN 22.
        ls_data-orderid = ls_excel-value.
      WHEN 23.
        ls_data-ext_line = ls_excel-value.
      WHEN 24.
        ls_data-service = ls_excel-value.
      WHEN 25.
        ls_data-ktext1 = ls_excel-value.
      WHEN 26.
        ls_data-quantity_serv = ls_excel-value.
      WHEN 27.
        ls_data-base_uom = ls_excel-value.
      WHEN 28.
        ls_data-base_uom = ls_excel-value.
      WHEN 29.
        ls_data-waers_serv = ls_excel-value.


      WHEN 30.
        ls_data-zz1_sistema_pdi         = ls_excel-value.
      WHEN 31.
        ls_data-zz1_tipodeservicio1_pdi = ls_excel-value.
      WHEN 32.
        ls_data-zz1_fuerzadeventa_pdi   = ls_excel-value.
      WHEN 33.
        ls_data-zz1_placa1_pdi          = ls_excel-value.
      WHEN 34.
        ls_data-zz1_ordendetrabajo1_pdi = ls_excel-value.
      WHEN 35.
        ls_data-zz1_tipodefecha_pdi     = ls_excel-value.
      WHEN 36.
        ls_data-zz1_kilometraje1_pdi    = ls_excel-value.

      WHEN 37.
        ls_data-zz1_agrupadordocumento_pdh = ls_excel-value.
      WHEN 38.
        ls_data-zz1_piloto_pdh             = ls_excel-value.
      WHEN 39.
        CONCATENATE ls_excel-value+6(4) ls_excel-value+3(2) ls_excel-value(2)
        INTO ls_data-zz1_fechapicking1_pdh.
      WHEN 40.
        ls_data-zz1_placast_pdh            = ls_excel-value.
      WHEN 41.
        ls_data-zz1_motivostraslados_pdh   = ls_excel-value.
      WHEN 42.
        ls_data-zz1_departamento1_pdh      = ls_excel-value.

    ENDCASE.

    AT END OF row.
      APPEND ls_data TO lt_data.
    ENDAT.

  ENDLOOP.

END-OF-SELECTION.

  PERFORM call_alv.

FORM call_alv.

  IF lt_data[] IS NOT INITIAL.
    PERFORM alv_report USING lt_data[].
  ELSE.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.


*///////////////////////////////      ALV PERFORMS
FORM alv_report  USING  pp_itab LIKE lt_data[].

  PERFORM sp_group_build USING lf_sp_group[].         "1
  PERFORM alv_ini_fieldcat.                           "2
  PERFORM layout_build USING lf_layout.               "3
  PERFORM alv_listado USING pp_itab[].                "4

ENDFORM.

*///////////////////////////////      ALV PERFORM_1
FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = TEXT-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.


*///////////////////////////////      ALV PERFORM_2
FORM alv_ini_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '5'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'CHECK'.
  alv_git_fieldcat-seltext_l = 'Sel.'.
  alv_git_fieldcat-checkbox  = 'X'.
  alv_git_fieldcat-edit      = 'X'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '13'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'ORDEN_COMPRA'.
  alv_git_fieldcat-seltext_l = 'OC Generada'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'DOC_TYPE'.
  alv_git_fieldcat-seltext_l = 'Clase. OC'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'VENDOR'.
  alv_git_fieldcat-seltext_l = 'Proveedor'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'CURRENCY'.
  alv_git_fieldcat-seltext_l = 'Clave. Moneda'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'PURCH_ORG'.
  alv_git_fieldcat-seltext_l = 'Org. Compras'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'PUR_GROUP'.
  alv_git_fieldcat-seltext_l = 'Grp. Compras'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'COMP_CODE'.
  alv_git_fieldcat-seltext_l = 'Sociedad'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'PO_ITEM'.
  alv_git_fieldcat-seltext_l = 'Pos. Doc.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'ACCTASSCAT'.
  alv_git_fieldcat-seltext_l = 'Tipo Imp.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'EPSTP'.
  alv_git_fieldcat-seltext_l = 'Tipo Pos.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'MATERIAL'.
  alv_git_fieldcat-seltext_l = 'Material'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '20'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'SHORT_TEXT'.
  alv_git_fieldcat-seltext_l = 'Descripcion'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'QUANTITY_ITEM'.
  alv_git_fieldcat-seltext_l = 'Cant.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'PO_UNIT'.
  alv_git_fieldcat-seltext_l = 'UM Material'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'DELIVERY_DATE'.
  alv_git_fieldcat-seltext_l = 'Fecha Entrega'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'NET_PRICE'.
  alv_git_fieldcat-seltext_l = 'Precio Neto'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'PLANT'.
  alv_git_fieldcat-seltext_l = 'Centro'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'MATL_GROUP'.
  alv_git_fieldcat-seltext_l = 'Gpo. Articulos'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'TAX_CODE'.
  alv_git_fieldcat-seltext_l = 'Ind. Imp.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'STGE_LOC'.
  alv_git_fieldcat-seltext_l = 'Almacen'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'GL_ACCOUNT'.
  alv_git_fieldcat-seltext_l = 'Cta. Mayor'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'COSTCENTER'.
  alv_git_fieldcat-seltext_l = 'Centro Costos'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'ORDERID'.
  alv_git_fieldcat-seltext_l = 'Orden'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'EXT_LINE'.
  alv_git_fieldcat-seltext_l = 'Linea'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'SRVPOS'.
  alv_git_fieldcat-seltext_l = 'Nro. Servicio'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'KTEXT1'.
  alv_git_fieldcat-seltext_l = 'Nombre Servicio'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '12'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'QUANTITY_SERV'.
  alv_git_fieldcat-seltext_l = 'Cant. Servicio'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'BASE_UOM'.
  alv_git_fieldcat-seltext_l = 'UM. Servicio'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'PRICE_UNIT'.
  alv_git_fieldcat-seltext_l = 'Precio Srv. Unit'. "No sale
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'WAERS'.
  alv_git_fieldcat-seltext_l = 'Moneda'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_SISTEMA_PDI'.
  alv_git_fieldcat-seltext_l = 'SISTEMA PDI'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_TIPODESERVICIO1_PDI'.
  alv_git_fieldcat-seltext_l = 'TIPODESERVICIO PDI'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_FUERZADEVENTA_PDI'.
  alv_git_fieldcat-seltext_l = 'FUERZA DE VENTA PDI'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_PLACA1_PDI'.
  alv_git_fieldcat-seltext_l = 'PLACA1 PDI'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_ORDENDETRABAJO1_PDI'.
  alv_git_fieldcat-seltext_l = 'ORDEN DE TRABAJO1 PDI'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_TIPODEFECHA_PDI'.
  alv_git_fieldcat-seltext_l = 'TIPO DE FECHA PDI'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_KM_VTA_PDI'.
  alv_git_fieldcat-seltext_l = 'KM VTA PDI'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_AGRUPADORDOCUMENTO_PDH'.
  alv_git_fieldcat-seltext_l = 'AGRUPADOR DOCUMENTO PDH'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_PILOTO_PDH'.
  alv_git_fieldcat-seltext_l = 'PILOTO PDH'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_FECHAPICKING1_PDH'.
  alv_git_fieldcat-seltext_l = 'FECHA PICKING PDH'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_PLACAST_PDH'.
  alv_git_fieldcat-seltext_l = 'PLACAST PDH'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_MOTIVOSTRASLADOS_PDH'.
  alv_git_fieldcat-seltext_l = 'MOTIVOS TRASLADOS PDH'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_DEPARTAMENTO1_PDH'.
  alv_git_fieldcat-seltext_l = 'DEPARTAMENTO PDH'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.


  CLEAR ls_event_exit.
  ls_event_exit-ucomm        = gc_refresh.    " Refresh
  ls_event_exit-after        = c_x.
  APPEND ls_event_exit TO lt_event_exit.

ENDFORM.

*///////////////////////////////      ALV PERFORM_3
FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.
  u_lf_layout-colwidth_optimize   = 'X'.
*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.

*///////////////////////////////      ALV PERFORM_4
FORM alv_listado  USING ppp_itab LIKE lt_data[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = lf_layout
      it_fieldcat              = alv_git_fieldcat[]
      it_event_exit            = lt_event_exit
*     it_special_groups        = lf_sp_group
      i_save                   = 'X'
    TABLES
      t_outtab                 = ppp_itab.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1
               sy-msgv2
               sy-msgv3
               sy-msgv4.
  ENDIF.

ENDFORM.


FORM pf_status_set USING extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.


FORM popup_oc.

  CLEAR: lv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmación '
      text_question         = '¿Quieres generar las ordenes de compra selecionadas?'
      text_button_1         = 'Si'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_CANCEL'
      display_cancel_button = ' '
      popup_type            = 'ICON_MESSAGE_ERROR'
    IMPORTING
      answer                = lv_answer.
  IF lv_answer NE 1.
    LEAVE SCREEN.
  ELSE.
    PERFORM generate_oc.
  ENDIF.

ENDFORM.

FORM generate_oc.

  REFRESH: t_return.
  CLEAR: w_return.

* Declaraciones BAPI_PO_CREATE1
  DATA: BEGIN OF t_return_log OCCURS 10.
          INCLUDE STRUCTURE bapiret2.
DATA  END OF t_return_log.
  DATA: w_return_log       LIKE LINE OF t_return_log.

  TYPES: BEGIN OF ty_pritem.
           INCLUDE STRUCTURE bapimepoitem.
  TYPES: END OF ty_pritem.

  TYPES: BEGIN OF ty_pritemx.
           INCLUDE STRUCTURE bapimepoitemx.
  TYPES: END OF ty_pritemx.

  TYPES: BEGIN OF ty_poaccount.
           INCLUDE STRUCTURE bapimepoaccount.
  TYPES: END OF ty_poaccount.

  TYPES: BEGIN OF ty_poaccountx.
           INCLUDE STRUCTURE bapimepoaccountx.
  TYPES: END OF ty_poaccountx.

  DATA: t_poheader     LIKE bapimepoheader,
        t_poheaderx    LIKE bapimepoheaderx,

        v_number       LIKE bapimereqheader-preq_no,
        t_poitem       TYPE TABLE OF ty_pritem,
        w_poitem       LIKE LINE OF t_poitem,
        t_poitemx      TYPE TABLE OF ty_pritemx,
        w_poitemx      LIKE LINE OF t_poitemx,
        t_poschedule   TYPE TABLE OF bapimeposchedule,
        w_poschedule   LIKE LINE OF t_poschedule,
        t_poschedulex  TYPE TABLE OF bapimeposchedulx,
        w_poschedulex  LIKE LINE OF t_poschedulex,
        t_poaccount    TYPE TABLE OF ty_poaccount,
        w_poaccount    LIKE LINE OF t_poaccount,
        t_poaccountx   TYPE TABLE OF ty_poaccountx,
        w_poaccountx   LIKE LINE OF t_poaccountx,
        t_poservices   TYPE TABLE OF bapiesllc,
        w_poservices   LIKE LINE OF t_poservices,

        t_potextitem   TYPE TABLE OF bapimepotext,
        t_poheadertext TYPE TABLE OF  bapimepotextheader,
        w_poheadertext TYPE bapimepotextheader,

        t_pocond       TYPE TABLE OF  bapimepocond,
        t_pocondx      TYPE TABLE OF   bapimepocondx,

        t_possched     TYPE TABLE OF bapimeposchedule,
        t_posschedx    TYPE TABLE OF bapimeposchedulx,
        w_possched     TYPE bapimeposchedule,
        w_posschedx    TYPE bapimeposchedulx,

        wa_pocond      TYPE bapimepocond,
        wa_pocondx     TYPE bapimepocondx,
        v_tabix        TYPE sy-tabix,
        lv_tabix       TYPE sy-tabix,
        v_tabixc       TYPE c LENGTH 10,
        v_req.

  DATA: bapi_te_mepoheader  TYPE bapi_te_mepoheader,
        bapi_te_mepoheaderx TYPE bapi_te_mepoheaderx,
        bapi_te_mepoitem    TYPE bapi_te_mepoitem,
        bapi_te_mepoitemx   TYPE bapi_te_mepoitemx,
        lt_extension        TYPE TABLE OF bapiparex,
        ls_extension        TYPE bapiparex.


  DATA: t_item_schedules LIKE bapimeposchedule OCCURS 0 WITH HEADER LINE.

  DATA ls_ref1 TYPE REF TO cl_gui_alv_grid .

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ls_ref1.

  CALL METHOD ls_ref1->check_changed_data.

*
  "Temporal para OC
  REFRESH lt_auxoc.
  CLEAR: ls_auxoc.
*  lt_auxoc[] = lt_data[].

* Eliminar registros que no esten selecionados
*  DELETE lt_auxoc WHERE check EQ ' '.


  IF lt_data[] IS INITIAL. "change

    MESSAGE 'Debe selecionar al menos un registro' TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.

    CLEAR: w_poitem-po_item,
           w_poitemx-po_item,
           w_poaccount-po_item,
           w_poaccountx-po_item,
           wa_pocond-itm_number.

    REFRESH: t_potextitem.


    IF lt_data[] IS INITIAL. "change

      MESSAGE 'Purchase requisitions already generated a purchase order.' TYPE 'S' DISPLAY LIKE 'E'.

    ELSE.

      LOOP AT lt_data INTO ls_auxoc WHERE check EQ 'X'. "change

        CLEAR: t_poheader,
               t_poheaderx,
               w_return_log.

        REFRESH: t_return_log,
                 t_poitem ,
                 t_poitemx,
                 t_poheadertext,
                 t_potextitem,
                 t_poaccount,
                 t_poaccountx,
                 t_pocond,
                 t_pocondx,
                 t_item_schedules,
                 t_posschedx.

*        LOOP AT lt_auxoc INTO DATA(ls_auxoc) WHERE banfn EQ ls_auxoc-banfn.
*
        v_tabix = sy-tabix.

*&---------------------------------------------------------------------*
*&      POHEADER
*&---------------------------------------------------------------------*
        t_poheader-comp_code = ls_auxoc-comp_code.
        t_poheader-doc_type  = ls_auxoc-doc_type.
        t_poheader-vendor    = |{ ls_auxoc-vendor ALPHA = IN }|.
        t_poheader-purch_org = ls_auxoc-purch_org.
        t_poheader-pur_group = ls_auxoc-pur_group.
        t_poheader-currency  = ls_auxoc-currency.
        t_poheaderx-comp_code = 'X'.
        t_poheaderx-doc_type  = 'X'.
        t_poheaderx-vendor    = 'X'.
        t_poheaderx-purch_org = 'X'.
        t_poheaderx-pur_group = 'X'.
        t_poheaderx-currency  = 'X'.

*&---------------------------------------------------------------------*
*&      POITEM
*&---------------------------------------------------------------------*
        w_poitem-po_item     = |{ ls_auxoc-po_item ALPHA = IN }|.
        w_poitem-material    = |{ ls_auxoc-material ALPHA = IN }|.
        w_poitem-plant       = ls_auxoc-plant.
        w_poitem-quantity    = ls_auxoc-quantity_item.
        SELECT SINGLE meins INTO w_poitem-po_unit FROM mara WHERE matnr EQ w_poitem-material.
*        w_poitem-po_unit     = ls_auxoc-po_unit.
        w_poitem-net_price   = ls_auxoc-net_price.
        w_poitem-tax_code    = ls_auxoc-tax_code.
        w_poitem-short_text  = ls_auxoc-short_text.
        w_poitem-item_cat    = ls_auxoc-epstp.
        w_poitem-acctasscat  = ls_auxoc-acctasscat.
        w_poitem-stge_loc    = ls_auxoc-stge_loc.

*        w_poitem-gr_ind      = 'X'.
*        w_poitem-ir_ind      = 'X'.
*        w_poitem-gr_basediv  = ' '.
*        w_poitem-no_rounding = 'X'.
*        w_poitem-price_unit   = 1.
*        w_poitem-conv_num1 = '1'.
*        w_poitem-conv_den1 = '1'.

        w_poitemx-po_item    = |{ ls_auxoc-po_item ALPHA = IN }|.
        w_poitemx-material   = 'X'.
        w_poitemx-plant      = 'X'.
        w_poitemx-quantity   = 'X'.
        w_poitemx-po_unit    = 'X'.
        w_poitemx-net_price  = 'X'.
        w_poitemx-tax_code   = 'X'.
        w_poitemx-short_text = 'X'.
        w_poitemx-item_cat   = 'X'.
        w_poitemx-acctasscat = 'X'.
        w_poitemx-stge_loc   = 'X'.

*        w_poitemx-ir_ind      = 'X'.
*        w_poitemx-gr_basediv  = 'X'.
*        w_poitemx-no_rounding = 'X'.
*        w_poitemx-conv_num1 = 'X'.
*        w_poitemx-conv_den1 = 'X'.

        APPEND w_poitem TO t_poitem.
        APPEND w_poitemx TO t_poitemx.

*&---------------------------------------------------------------------*
*&      POSCHEDULE
*&---------------------------------------------------------------------*
        REPLACE ALL OCCURRENCES OF '.' IN ls_auxoc-delivery_date WITH ''.
        CONCATENATE ls_auxoc-delivery_date+4(4)
                    ls_auxoc-delivery_date+2(2)
                    ls_auxoc-delivery_date(2)
                    INTO w_poschedule-delivery_date.
        w_poschedule-po_item        = |{ ls_auxoc-po_item ALPHA = IN }|.
        w_poschedule-quantity       = ls_auxoc-quantity_item.
        w_poschedulex-po_item       = |{ ls_auxoc-po_item ALPHA = IN }|.
        w_poschedulex-delivery_date = 'X'.
        w_poschedulex-quantity      = 'X'.
        APPEND w_poschedule TO t_poschedule.
        APPEND w_poschedulex TO t_poschedulex.

*&---------------------------------------------------------------------*
*&      POACCOUNT
*&---------------------------------------------------------------------*
        w_poaccount-po_item    = |{ ls_auxoc-po_item ALPHA = IN }|.
        w_poaccount-quantity   = ls_auxoc-quantity_item.
        w_poaccount-gl_account = ls_auxoc-gl_account.
        w_poaccount-costcenter = |{ ls_auxoc-costcenter ALPHA = IN }|.
        w_poaccount-orderid    = ls_auxoc-orderid.

        w_poaccountx-po_item    = |{ ls_auxoc-po_item ALPHA = IN }|.
        w_poaccountx-quantity   = 'X'.
        w_poaccountx-gl_account = 'X'.
        w_poaccountx-costcenter = 'X'.
        w_poaccountx-orderid    = 'X'.

        APPEND w_poaccount TO t_poaccount.
        APPEND w_poaccountx TO t_poaccountx.

*&---------------------------------------------------------------------*
*&      POSERVICES
*&---------------------------------------------------------------------*
        w_poservices-ext_line   = ls_auxoc-ext_line.      "EXTROW       Número de linea del servicios
        w_poservices-service    = ls_auxoc-service.       "ASNUM        Número de servicio
        w_poservices-short_text = ls_auxoc-ktext1.        "SH_TEXT1     Texto breve
        w_poservices-quantity   = ls_auxoc-quantity_serv. "MENGEV       Cantidad
        w_poservices-base_uom   = ls_auxoc-base_uom.      "MEINS        Unidad de medida base
        w_poservices-gr_price   = ls_auxoc-gr_price.      "BAPIGRPRICE  Precio bruto

        APPEND w_poservices TO t_poservices.

*&---------------------------------------------------------------------*
*&      BAPI EXTENSION_IN
*&---------------------------------------------------------------------*
        REFRESH: lt_extension.

        "EXTENSION FOR EKKO
        bapi_te_mepoheader-po_number                  = ''.
        bapi_te_mepoheader-zz1_agrupadordocumento_pdh = ls_auxoc-zz1_agrupadordocumento_pdh.
        bapi_te_mepoheader-zz1_piloto_pdh             = ls_auxoc-zz1_piloto_pdh.
        bapi_te_mepoheader-zz1_fechapicking1_pdh      = ls_auxoc-zz1_fechapicking1_pdh.
        bapi_te_mepoheader-zz1_placast_pdh            = ls_auxoc-zz1_placast_pdh.
        bapi_te_mepoheader-zz1_motivostraslados_pdh   = ls_auxoc-zz1_motivostraslados_pdh.
        bapi_te_mepoheader-zz1_departamento1_pdh      = ls_auxoc-zz1_departamento1_pdh.

        ls_extension-structure = 'BAPI_TE_MEPOHEADER'.
*        ls_extension-valuepart1 = bapi_te_mepoheader.

        CALL FUNCTION 'PI_BP_MOVE_UNICODE'
          EXPORTING
            iv_move_from = bapi_te_mepoheader
          CHANGING
            cv_move_to   = ls_extension-valuepart1.

        APPEND ls_extension TO lt_extension.
        CLEAR: ls_extension.

        bapi_te_mepoheaderx-po_number                  = ''.
        bapi_te_mepoheaderx-zz1_agrupadordocumento_pdh = 'X'.
        bapi_te_mepoheaderx-zz1_piloto_pdh             = 'X'.
        bapi_te_mepoheaderx-zz1_fechapicking1_pdh      = 'X'.
        bapi_te_mepoheaderx-zz1_placast_pdh            = 'X'.
        bapi_te_mepoheaderx-zz1_motivostraslados_pdh   = 'X'.
        bapi_te_mepoheaderx-zz1_departamento1_pdh      = 'X'.

        ls_extension-structure = 'BAPI_TE_MEPOHEADERX'.
*        ls_extension-valuepart1 = bapi_te_mepoheaderx.

        CALL FUNCTION 'PI_BP_MOVE_UNICODE'
          EXPORTING
            iv_move_from = bapi_te_mepoheaderx
          CHANGING
            cv_move_to   = ls_extension-valuepart1.

        APPEND ls_extension TO lt_extension.
        CLEAR: ls_extension.


        "EXTENSION FOR EKPO
        bapi_te_mepoitem-po_item                 = |{ ls_auxoc-po_item ALPHA = IN }|.
        bapi_te_mepoitem-zz1_sistema_pdi         = ls_auxoc-zz1_sistema_pdi.
        bapi_te_mepoitem-zz1_tipodeservicio1_pdi = ls_auxoc-zz1_tipodeservicio1_pdi.
        bapi_te_mepoitem-zz1_fuerzadeventa_pdi   = ls_auxoc-zz1_fuerzadeventa_pdi.
        bapi_te_mepoitem-zz1_placa1_pdi          = ls_auxoc-zz1_placa1_pdi.
        bapi_te_mepoitem-zz1_ordendetrabajo1_pdi = ls_auxoc-zz1_ordendetrabajo1_pdi.
        bapi_te_mepoitem-zz1_tipodefecha_pdi     = ls_auxoc-zz1_tipodefecha_pdi.
        bapi_te_mepoitem-zz1_kilometraje1_pdi    = ls_auxoc-zz1_kilometraje1_pdi.

        ls_extension-structure = 'BAPI_TE_MEPOITEM'.
*        ls_extension-valuepart1 = bapi_te_mepoitem.

        CALL FUNCTION 'PI_BP_MOVE_UNICODE'
          EXPORTING
            iv_move_from = bapi_te_mepoitem
          CHANGING
            cv_move_to   = ls_extension-valuepart1.

        APPEND ls_extension TO lt_extension.
        CLEAR: ls_extension.

        bapi_te_mepoitemx-po_item                 = |{ ls_auxoc-po_item ALPHA = IN }|.
        bapi_te_mepoitemx-zz1_sistema_pdi         = 'X'.
        bapi_te_mepoitemx-zz1_tipodeservicio1_pdi = 'X'.
        bapi_te_mepoitemx-zz1_fuerzadeventa_pdi   = 'X'.
        bapi_te_mepoitemx-zz1_placa1_pdi          = 'X'.
        bapi_te_mepoitemx-zz1_ordendetrabajo1_pdi = 'X'.
        bapi_te_mepoitemx-zz1_tipodefecha_pdi     = 'X'.
        bapi_te_mepoitemx-zz1_kilometraje1_pdi    = 'X'.

        ls_extension-structure = 'BAPI_TE_MEPOITEMX'.
*        ls_extension-valuepart1 = bapi_te_mepoitemx.

        CALL FUNCTION 'PI_BP_MOVE_UNICODE'
          EXPORTING
            iv_move_from = bapi_te_mepoitemx
          CHANGING
            cv_move_to   = ls_extension-valuepart1.

        APPEND ls_extension TO lt_extension.
        CLEAR: ls_extension.


*&---------------------------------------------------------------------*
        BREAK-POINT.
        CALL FUNCTION 'BAPI_PO_CREATE1'
          EXPORTING
            poheader         = t_poheader
            poheaderx        = t_poheaderx
            no_price_from_po = 'X'
          TABLES
            return           = t_return_log
            extensionin      = lt_extension
            poitem           = t_poitem
            poitemx          = t_poitemx
            poaccount        = t_poaccount
            poaccountx       = t_poaccountx
            poschedule       = t_poschedule
            poschedulex      = t_poschedulex
            poservices       = t_poservices.


        READ TABLE t_return_log INTO w_return_log  WITH KEY type = 'S' id = '06' number = '017'.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = t_return_log.

          ls_auxoc-orden_compra = w_return_log-message_v2.
          MODIFY lt_data FROM ls_auxoc INDEX sy-tabix.

          any_ocgen  = 'X'.

          CLEAR: t_poheader, t_poheaderx.
          REFRESH: t_return_log, t_poitem, t_poitemx, t_poschedule, t_poschedulex.

        ELSE.

          MESSAGE 'Han ocurriendo errores en la generacion de las ordenes de compras.' TYPE 'S' DISPLAY LIKE 'E'.

          APPEND LINES OF t_return_log TO t_return.

        ENDIF.

      ENDLOOP.

      IF any_ocgen EQ 'X'.
        MESSAGE 'Se han creado ordenes de compras. Actualizar el reporte mediante el boton "Actualizar Lista"' TYPE 'S'." DISPLAY LIKE 'I'.
        CLEAR: any_ocgen.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM user_command USING p_ucomm TYPE sy-ucomm
                         p_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN '&GEN_OC'.
      PERFORM popup_oc.
    WHEN '&GEN_LOG'.
      IF t_return IS NOT INITIAL.
*        LEAVE TO LIST-PROCESSING.
        PERFORM show_log.
      ELSE.
        MESSAGE 'Log de errores vacio' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN '&REFRESH'.
*      PERFORM call_alv.
      p_selfield-refresh    = c_x.
      p_selfield-col_stable = c_x.
      p_selfield-row_stable = c_x.
  ENDCASE.
ENDFORM.

FORM show_log.

  DATA: lt_bal_msg TYPE /scmtms/t_bal_s_msg,
        ls_bal_msg TYPE bal_s_msg.

  DATA: text TYPE string.

  REFRESH: lt_bal_msg.


  LOOP AT t_return INTO w_return WHERE type EQ 'E'.

    ls_bal_msg-msgty = w_return-type.
    ls_bal_msg-msgid = 'YMQAF01_WSEXT'.
    ls_bal_msg-msgno = '400'.
    ls_bal_msg-msgv1 = w_return-message(50).
    ls_bal_msg-msgv2 = w_return-message+50(50).
    ls_bal_msg-msgv3 = w_return-message+100(50).
*    ls_bal_msg-msgv4
    APPEND ls_bal_msg TO lt_bal_msg.

    CLEAR: ls_bal_msg.
  ENDLOOP.

  CALL METHOD /scmtms/cl_batch_helper_80=>show_application_log_in_popup
    EXPORTING
      it_bal_msg = lt_bal_msg.

ENDFORM.

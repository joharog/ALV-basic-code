*&---------------------------------------------------------------------*
*& Report ZCARGAEXEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_mass_upload_oc2.


DATA: lv_filename TYPE string,
      lv_filesize TYPE w3param-cont_len,
      lv_filetype TYPE w3param-cont_type,
      lt_bin_data TYPE w3mimetabtype.

DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

DATA: lt_worksheets TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.

FIELD-SYMBOLS: <gt_data>   TYPE STANDARD TABLE,
               <gv_fldval> TYPE any.

TYPES: BEGIN OF ty_data,
         exclude                    TYPE char1,
         check                      TYPE char1,       "Check Box
         orden_compra               TYPE ebeln,
*        POHEADER
         comp_code                  TYPE bukrs,
         doc_type                   TYPE esart,
         vendor                     TYPE elifn,
         purch_org                  TYPE ekorg,
         pur_group                  TYPE bkgrp,
         currency                   TYPE waers,
         suppl_plnt                 TYPE reswk,

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
         agreement                  TYPE konnr,      "Número del contrato superior
         agmt_item                  TYPE ktpnr,      "Número de posición del contrato superior
         suppl_stloc                TYPE reslo,      "Almacén emisor para pedido de traslado

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
         zz1_canal1_pdi             TYPE ekpo-zz1_canal1_pdi,

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

FIELD-SYMBOLS: <fs_data> TYPE ty_data.

TYPE-POOLS: slis.

DATA: lf_sp_group      TYPE slis_t_sp_group_alv,  "MANEJAR GRUPOS DE CAMPOS
      lf_layout        TYPE slis_layout_alv,      "MANEJAR DISEÑO DE LAYOUT
      it_topheader     TYPE slis_t_listheader,    "MANEJAR CABECERA DEL REP
      wa_top           LIKE LINE OF it_topheader, "LÍNEA PARA CABECERA
      lt_event_exit    TYPE slis_t_event_exit,    "Event
      ls_event_exit    TYPE slis_event_exit,      "Event
      alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.     "Parametros del catalogo

CONSTANTS: c_x        VALUE 'X',
           gc_refresh TYPE syucomm VALUE '&REFRESH'.

DATA: true_oc   TYPE char1,
      false_oc  TYPE char1,
      lv_answer TYPE char1,
      lv_date   TYPE string.

DATA: t_return TYPE TABLE OF bapiret2,
      w_return LIKE LINE OF t_return.

SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename DEFAULT 'C:\'.
SELECTION-SCREEN: END OF BLOCK b1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'p_file'
    IMPORTING
      file_name  = p_file.

START-OF-SELECTION.

  TRY.

      IF p_file IS NOT INITIAL.

        lv_filename =  p_file.

        cl_gui_frontend_services=>gui_upload( EXPORTING filename   = lv_filename
                                                        filetype   = 'BIN'
                                              IMPORTING filelength = lv_filesize
                                              CHANGING  data_tab   = lt_bin_data ).

        IF lv_filename IS NOT INITIAL.
          SEARCH lv_filename FOR '.xlsx'.
          IF sy-subrc EQ 4.
            SEARCH lv_filename FOR '.xls'.
            IF sy-subrc EQ 4.
              MESSAGE 'Cargue un archivo excel en el formato correcto' TYPE 'S' DISPLAY LIKE 'E'.
              STOP.
            ENDIF.
          ENDIF.
        ENDIF.

*--     Convert solix to string
        DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin_data ).

        TRY.
            lo_excel_ref = NEW cl_fdt_xl_spreadsheet( document_name = lv_filename
                                                      xdocument     = lv_bin_data ).
          CATCH cx_fdt_excel_core.
        ENDTRY.

        IF lo_excel_ref IS BOUND.

*--       Get List of Worksheets
          lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
            IMPORTING
              worksheet_names = lt_worksheets ).

          IF NOT lt_worksheets IS INITIAL.

            READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

            DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_woksheetname ).

*--         Excel work sheet data in dyanmic internal table
            ASSIGN lo_data_ref->* TO <gt_data>.

            LOOP AT <gt_data> ASSIGNING FIELD-SYMBOL(<fs_str>).

              DATA(lv_tabix) = sy-tabix.

              IF lv_tabix => 11.

                ASSIGN COMPONENT 'A' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-doc_type.

                ASSIGN COMPONENT 'B' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-vendor.

                ASSIGN COMPONENT 'C' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-suppl_plnt.

                ASSIGN COMPONENT 'D' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-currency.

                ASSIGN COMPONENT 'E' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-purch_org.

                ASSIGN COMPONENT 'F' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-pur_group.

                ASSIGN COMPONENT 'G' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-comp_code.

                ASSIGN COMPONENT 'H' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-po_item.

                ASSIGN COMPONENT 'I' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-acctasscat.

                ASSIGN COMPONENT 'J' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-epstp.

                ASSIGN COMPONENT 'K' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-material.

                ASSIGN COMPONENT 'L' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-short_text.

                ASSIGN COMPONENT 'M' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-quantity_item.

                ASSIGN COMPONENT 'N' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-po_unit.

                ASSIGN COMPONENT 'O' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-delivery_date.

                ASSIGN COMPONENT 'P' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-net_price.

                ASSIGN COMPONENT 'Q' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-plant.

                ASSIGN COMPONENT 'R' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-matl_group.

                ASSIGN COMPONENT 'S' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-tax_code.

                ASSIGN COMPONENT 'T' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-stge_loc.

                ASSIGN COMPONENT 'U' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-suppl_stloc.

                ASSIGN COMPONENT 'V' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-gl_account.

                ASSIGN COMPONENT 'W' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-costcenter.

                ASSIGN COMPONENT 'X' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-orderid.

                ASSIGN COMPONENT 'Y' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-ext_line.

                ASSIGN COMPONENT 'Z' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-service.

                ASSIGN COMPONENT 'AA' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-ktext1.

                ASSIGN COMPONENT 'AB' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-quantity_serv.

                ASSIGN COMPONENT 'AB' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-base_uom.

                ASSIGN COMPONENT 'AD' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-gr_price.

                ASSIGN COMPONENT 'AE' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-waers_serv.

                ASSIGN COMPONENT 'AF' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_sistema_pdi.

                ASSIGN COMPONENT 'AG' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_tipodeservicio1_pdi.

                ASSIGN COMPONENT 'AH' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_fuerzadeventa_pdi.

                ASSIGN COMPONENT 'AI' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_placa1_pdi.

                ASSIGN COMPONENT 'AJ' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_ordendetrabajo1_pdi.

                ASSIGN COMPONENT 'AK' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_tipodefecha_pdi.

                ASSIGN COMPONENT 'AL' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_kilometraje1_pdi.

                ASSIGN COMPONENT 'AM' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_agrupadordocumento_pdh.

                ASSIGN COMPONENT 'AN' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_piloto_pdh.

                ASSIGN COMPONENT 'AO' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO lv_date.

                IF lv_date IS NOT INITIAL.
                  ls_data-zz1_fechapicking1_pdh = |{ lv_date+6(4) }{ lv_date+3(2) }{ lv_date(2) }|.
                ENDIF.

                ASSIGN COMPONENT 'AP' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_placast_pdh.

                ASSIGN COMPONENT 'AQ' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_motivostraslados_pdh.

                ASSIGN COMPONENT 'AR' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_departamento1_pdh.

                ASSIGN COMPONENT 'AS' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-agmt_item.

                ASSIGN COMPONENT 'AT' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-agreement.

                ASSIGN COMPONENT 'AU' OF STRUCTURE <fs_str> TO <gv_fldval>.
                MOVE <gv_fldval> TO ls_data-zz1_canal1_pdi.

                APPEND ls_data TO lt_data.
                CLEAR: ls_data.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDIF.

    CATCH cx_root INTO DATA(e_text).
      MESSAGE e_text->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

END-OF-SELECTION.

  PERFORM call_alv.

FORM call_alv.

  IF lt_data[] IS NOT INITIAL.
    DELETE lt_data WHERE doc_type EQ '' AND vendor EQ ''.
    PERFORM alv_report USING lt_data[].
  ELSE.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*--  ALV PERFORMS
FORM alv_report  USING  pp_itab LIKE lt_data[].

  PERFORM sp_group_build USING lf_sp_group[].         "1
  PERFORM alv_ini_fieldcat.                           "2
  PERFORM layout_build USING lf_layout.               "3
  PERFORM alv_listado USING pp_itab[].                "4

ENDFORM.

*--  ALV PERFORM_1
FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR: ls_sp_group.

  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = TEXT-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.


*--  ALV PERFORM_2
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
  alv_git_fieldcat-outputlen = '17'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'SUPPL_PLNT'.
  alv_git_fieldcat-seltext_l = 'Centro Suministrador'.
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
  alv_git_fieldcat-outputlen = '40'.
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
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'SUPPL_STLOC'.
  alv_git_fieldcat-seltext_l = 'Almacen Procedencia'.
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
  alv_git_fieldcat-seltext_l = 'Sistema'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_TIPODESERVICIO1_PDI'.
  alv_git_fieldcat-seltext_l = 'Tipo de Servicio'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_FUERZADEVENTA_PDI'.
  alv_git_fieldcat-seltext_l = 'Fuerza de Venta'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_PLACA1_PDI'.
  alv_git_fieldcat-seltext_l = 'Placa'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_ORDENDETRABAJO1_PDI'.
  alv_git_fieldcat-seltext_l = 'Orden de Trabajo'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_TIPODEFECHA_PDI'.
  alv_git_fieldcat-seltext_l = 'Tipo de Fecha'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_KILOMETRAJE1_PDI'.
  alv_git_fieldcat-seltext_l = 'Kilometraje'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_AGRUPADORDOCUMENTO_PDH'.
  alv_git_fieldcat-seltext_l = 'Camion'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_PILOTO_PDH'.
  alv_git_fieldcat-seltext_l = 'Piloto'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_FECHAPICKING1_PDH'.
  alv_git_fieldcat-seltext_l = 'Fecha Picking'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_PLACAST_PDH'.
  alv_git_fieldcat-seltext_l = 'Placa ST'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_MOTIVOSTRASLADOS_PDH'.
  alv_git_fieldcat-seltext_l = 'Motivos Trasl.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_DEPARTAMENTO1_PDH'.
  alv_git_fieldcat-seltext_l = 'Departamento'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'AGMT_ITEM'.
  alv_git_fieldcat-seltext_l = 'Pos.Contrato'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'AGREEMENT'.
  alv_git_fieldcat-seltext_l = 'Contrato'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-tabname   = 'LT_ALV'.
  alv_git_fieldcat-fieldname = 'ZZ1_CANAL1_PDI'.
  alv_git_fieldcat-seltext_l = 'Objeto PA'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR ls_event_exit.
  ls_event_exit-ucomm        = gc_refresh.
  ls_event_exit-after        = c_x.
  APPEND ls_event_exit TO lt_event_exit.

ENDFORM.

*--  ALV PERFORM_3
FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.

*  u_lf_layout-colwidth_optimize   = 'X'.
*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.

*--  ALV PERFORM_4
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

  "Temporal para OC
  REFRESH lt_auxoc.
  CLEAR: ls_auxoc.

  IF lt_data[] IS INITIAL. "change

    MESSAGE 'Debe selecionar al menos un registro' TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.


    DATA(itab_count) = lt_data.
    DELETE itab_count WHERE check EQ ''.
    DESCRIBE TABLE itab_count LINES DATA(lv_count).

    CLEAR: lv_tabix.

    DATA: lv_pos TYPE posnr_va,
          flag_p TYPE char1.


    CLEAR: lv_pos, flag_p.


    LOOP AT lt_data INTO ls_auxoc WHERE check EQ 'X' AND exclude NE 'X'."change

      lv_tabix = sy-tabix.
      v_tabix  = sy-tabix.


      "Validar que posicion inicie con 10.
      IF ls_auxoc-po_item EQ 10 AND lv_pos IS INITIAL.

        lv_pos = ls_auxoc-po_item. "Siempre se llena con 10.
        flag_p = abap_true.

*&---------------------------------------------------------------------*
*&      P O H E A D E R
*&---------------------------------------------------------------------*
        IF ls_auxoc-comp_code IS NOT INITIAL.
          t_poheader-comp_code  = ls_auxoc-comp_code.
          t_poheaderx-comp_code = 'X'.
        ENDIF.

        IF ls_auxoc-doc_type IS NOT INITIAL.
          t_poheader-doc_type  = ls_auxoc-doc_type.
          t_poheaderx-doc_type = 'X'.
          IF ls_auxoc-doc_type EQ 'ZUB'.
            t_poheader-suppl_plnt  = ls_auxoc-suppl_plnt.
            t_poheaderx-suppl_plnt = 'X'.
          ENDIF.
        ENDIF.

        IF ls_auxoc-vendor IS NOT INITIAL.
          t_poheader-vendor     = |{ ls_auxoc-vendor ALPHA = IN }|.
          t_poheaderx-vendor    = 'X'.
        ENDIF.

        IF ls_auxoc-purch_org IS NOT INITIAL.
          t_poheader-purch_org  = ls_auxoc-purch_org.
          t_poheaderx-purch_org = 'X'.
        ENDIF.

        IF ls_auxoc-pur_group IS NOT INITIAL.
          t_poheader-pur_group  = ls_auxoc-pur_group.
          t_poheaderx-pur_group = 'X'.
        ENDIF.

        IF ls_auxoc-currency IS NOT INITIAL.
          t_poheader-currency   = ls_auxoc-currency.
          t_poheaderx-currency  = 'X'.
        ENDIF.

      ENDIF.


      "LLenamos las tablas.
      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_auxoc>) WHERE check EQ 'X' AND exclude NE 'X'.

        DATA(tabix_in) = sy-tabix.

        IF flag_p EQ abap_true OR lv_pos NE <fs_auxoc>-po_item.

          "Borrar en la primera entrada y aplicamos exclusion de las procesadas
          <fs_auxoc>-exclude = abap_true.
          CLEAR: flag_p.

*&---------------------------------------------------------------------*
*&      P O I T E M
*&---------------------------------------------------------------------*
          IF <fs_auxoc>-po_item IS NOT INITIAL.
            w_poitem-po_item  = |{ <fs_auxoc>-po_item ALPHA = IN }|.
            w_poitemx-po_item = |{ <fs_auxoc>-po_item ALPHA = IN }|.
          ENDIF.

          IF <fs_auxoc>-material IS NOT INITIAL.
            w_poitem-material  = |{ <fs_auxoc>-material ALPHA = IN }|.
            w_poitemx-material = 'X'.
            SELECT SINGLE meins INTO w_poitem-po_unit FROM mara WHERE matnr EQ w_poitem-material.
            w_poitemx-po_unit  = 'X'.
          ENDIF.

          IF <fs_auxoc>-plant IS NOT INITIAL.
            w_poitem-plant  = <fs_auxoc>-plant.
            w_poitemx-plant = 'X'.
          ENDIF.

          IF <fs_auxoc>-quantity_item IS NOT INITIAL.
            w_poitem-quantity  = <fs_auxoc>-quantity_item.
            w_poitemx-quantity = 'X'.
          ENDIF.

          IF <fs_auxoc>-net_price < 1.
            SELECT SINGLE kbetr FROM konp INTO w_poitem-net_price
               WHERE kschl EQ 'PB00' AND knumh EQ ( SELECT knumh FROM a017
                                                      WHERE lifnr EQ t_poheader-vendor AND matnr EQ w_poitem-material ).
            w_poitemx-net_price = 'X'.
          ELSE.
            w_poitem-net_price  = <fs_auxoc>-net_price.
            w_poitemx-net_price = 'X'.
          ENDIF.

          IF <fs_auxoc>-tax_code IS NOT INITIAL.
            w_poitem-tax_code  = <fs_auxoc>-tax_code.
            w_poitemx-tax_code = 'X'.
          ENDIF.

          IF <fs_auxoc>-short_text IS NOT INITIAL.
            w_poitem-short_text  = <fs_auxoc>-short_text.
            w_poitemx-short_text = 'X'.
          ENDIF.

          IF <fs_auxoc>-epstp IS NOT INITIAL.
            w_poitem-item_cat  = <fs_auxoc>-epstp.
            w_poitemx-item_cat = 'X'.
          ENDIF.

          IF <fs_auxoc>-acctasscat IS NOT INITIAL.
            w_poitem-acctasscat  = <fs_auxoc>-acctasscat.
            w_poitemx-acctasscat = 'X'.
          ENDIF.

          IF <fs_auxoc>-stge_loc IS NOT INITIAL.
            w_poitem-stge_loc  = <fs_auxoc>-stge_loc.
            w_poitemx-stge_loc = 'X'.
          ENDIF.

          IF <fs_auxoc>-agmt_item IS NOT INITIAL.
            w_poitem-agmt_item  = <fs_auxoc>-agmt_item.
            w_poitemx-agmt_item = 'X'.
          ENDIF.

          IF <fs_auxoc>-agreement IS NOT INITIAL.
            w_poitem-agreement  = <fs_auxoc>-agreement.
            w_poitemx-agreement = 'X'.
          ENDIF.

          IF <fs_auxoc>-suppl_stloc IS NOT INITIAL.
            w_poitem-suppl_stloc  = <fs_auxoc>-suppl_stloc.
            w_poitemx-suppl_stloc = 'X'.
          ENDIF.

          APPEND w_poitem TO t_poitem.
          APPEND w_poitemx TO t_poitemx.


*&---------------------------------------------------------------------*
*&      P O S C H E D U L E
*&---------------------------------------------------------------------*
          IF <fs_auxoc>-delivery_date IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF '.' IN <fs_auxoc>-delivery_date WITH ''.
            CONCATENATE <fs_auxoc>-delivery_date+4(4)
                        <fs_auxoc>-delivery_date+2(2)
                        <fs_auxoc>-delivery_date(2)
                        INTO w_poschedule-delivery_date.
            w_poschedulex-delivery_date = 'X'.
          ENDIF.

          IF <fs_auxoc>-po_item IS NOT INITIAL.
            w_poschedule-po_item  = |{ <fs_auxoc>-po_item ALPHA = IN }|.
            w_poschedulex-po_item = |{ <fs_auxoc>-po_item ALPHA = IN }|.
          ENDIF.

          IF <fs_auxoc>-quantity_item IS NOT INITIAL.
            w_poschedule-quantity  = <fs_auxoc>-quantity_item.
            w_poschedulex-quantity = 'X'.
          ENDIF.

          APPEND w_poschedule TO t_poschedule.
          APPEND w_poschedulex TO t_poschedulex.


*&---------------------------------------------------------------------*
*&      P O A C C O U N T
*&---------------------------------------------------------------------*
          IF <fs_auxoc>-po_item IS NOT INITIAL.
            w_poaccount-po_item    = |{ <fs_auxoc>-po_item ALPHA = IN }|.
            w_poaccountx-po_item    = |{ <fs_auxoc>-po_item ALPHA = IN }|.
          ENDIF.

          IF <fs_auxoc>-quantity_item IS NOT INITIAL.
            w_poaccount-quantity   = <fs_auxoc>-quantity_item.
            w_poaccountx-quantity   = 'X'.
          ENDIF.

          IF <fs_auxoc>-gl_account IS NOT INITIAL.
            w_poaccount-gl_account = <fs_auxoc>-gl_account.
            w_poaccountx-gl_account = 'X'.
          ENDIF.

          IF <fs_auxoc>-costcenter IS NOT INITIAL.
            w_poaccount-costcenter = |{ <fs_auxoc>-costcenter ALPHA = IN }|.
            w_poaccountx-costcenter = 'X'.
          ENDIF.

          IF <fs_auxoc>-orderid IS NOT INITIAL.
            w_poaccount-orderid    = <fs_auxoc>-orderid.
            w_poaccountx-orderid    = 'X'.
          ENDIF.

          APPEND w_poaccount TO t_poaccount.
          APPEND w_poaccountx TO t_poaccountx.


*&---------------------------------------------------------------------*
*&      P O S E R V I C E S
*&---------------------------------------------------------------------*
          w_poservices-service    = <fs_auxoc>-service.       "ASNUM        Número de servicio
          w_poservices-short_text = <fs_auxoc>-ktext1.        "SH_TEXT1     Texto breve
          w_poservices-quantity   = <fs_auxoc>-quantity_serv. "MENGEV       Cantidad
          w_poservices-base_uom   = <fs_auxoc>-base_uom.      "MEINS        Unidad de medida base
          w_poservices-gr_price   = <fs_auxoc>-gr_price.      "BAPIGRPRICE  Precio bruto

          APPEND w_poservices TO t_poservices.


*&---------------------------------------------------------------------*
*&      BAPI EXTENSION_IN
*&---------------------------------------------------------------------*
          REFRESH: lt_extension.

          "EXTENSION FOR EKKO
          bapi_te_mepoheader-po_number                  = ''.
          bapi_te_mepoheaderx-po_number                  = ''.

          IF <fs_auxoc>-zz1_agrupadordocumento_pdh IS NOT INITIAL.
            bapi_te_mepoheader-zz1_agrupadordocumento_pdh = <fs_auxoc>-zz1_agrupadordocumento_pdh.
            bapi_te_mepoheaderx-zz1_agrupadordocumento_pdh = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_piloto_pdh IS NOT INITIAL.
            bapi_te_mepoheader-zz1_piloto_pdh             = <fs_auxoc>-zz1_piloto_pdh.
            bapi_te_mepoheaderx-zz1_piloto_pdh             = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_piloto_pdh IS NOT INITIAL.
            bapi_te_mepoheader-zz1_piloto_pdh             = <fs_auxoc>-zz1_piloto_pdh.
          ENDIF.

          IF <fs_auxoc>-zz1_fechapicking1_pdh IS NOT INITIAL.
            bapi_te_mepoheader-zz1_fechapicking1_pdh      = <fs_auxoc>-zz1_fechapicking1_pdh.
            bapi_te_mepoheaderx-zz1_fechapicking1_pdh      = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_placast_pdh IS NOT INITIAL.
            bapi_te_mepoheader-zz1_placast_pdh            = <fs_auxoc>-zz1_placast_pdh.
            bapi_te_mepoheaderx-zz1_placast_pdh            = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_motivostraslados_pdh IS NOT INITIAL.
            bapi_te_mepoheader-zz1_motivostraslados_pdh   = <fs_auxoc>-zz1_motivostraslados_pdh.
            bapi_te_mepoheaderx-zz1_motivostraslados_pdh   = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_departamento1_pdh IS NOT INITIAL.
            bapi_te_mepoheader-zz1_departamento1_pdh      = <fs_auxoc>-zz1_departamento1_pdh.
            bapi_te_mepoheaderx-zz1_departamento1_pdh      = 'X'.
          ENDIF.

          ls_extension-structure = 'BAPI_TE_MEPOHEADER'.
*        ls_extension-valuepart1 = bapi_te_mepoheader.

          CALL FUNCTION 'PI_BP_MOVE_UNICODE'
            EXPORTING
              iv_move_from = bapi_te_mepoheader
            CHANGING
              cv_move_to   = ls_extension-valuepart1.

          APPEND ls_extension TO lt_extension.
          CLEAR: ls_extension.

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
          bapi_te_mepoitem-po_item  = |{ <fs_auxoc>-po_item ALPHA = IN }|.
          bapi_te_mepoitemx-po_item = |{ <fs_auxoc>-po_item ALPHA = IN }|.

          IF <fs_auxoc>-zz1_sistema_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_sistema_pdi  = <fs_auxoc>-zz1_sistema_pdi.
            bapi_te_mepoitemx-zz1_sistema_pdi = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_tipodeservicio1_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_tipodeservicio1_pdi  = <fs_auxoc>-zz1_tipodeservicio1_pdi.
            bapi_te_mepoitemx-zz1_tipodeservicio1_pdi = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_fuerzadeventa_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_fuerzadeventa_pdi  = <fs_auxoc>-zz1_fuerzadeventa_pdi.
            bapi_te_mepoitemx-zz1_fuerzadeventa_pdi = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_placa1_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_placa1_pdi  = <fs_auxoc>-zz1_placa1_pdi.
            bapi_te_mepoitemx-zz1_placa1_pdi = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_ordendetrabajo1_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_ordendetrabajo1_pdi  = <fs_auxoc>-zz1_ordendetrabajo1_pdi.
            bapi_te_mepoitemx-zz1_ordendetrabajo1_pdi = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_tipodefecha_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_tipodefecha_pdi  = <fs_auxoc>-zz1_tipodefecha_pdi.
            bapi_te_mepoitemx-zz1_tipodefecha_pdi = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_kilometraje1_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_kilometraje1_pdi  = <fs_auxoc>-zz1_kilometraje1_pdi.
            bapi_te_mepoitemx-zz1_kilometraje1_pdi = 'X'.
          ENDIF.

          IF <fs_auxoc>-zz1_canal1_pdi IS NOT INITIAL.
            bapi_te_mepoitem-zz1_canal1_pdi  = <fs_auxoc>-zz1_canal1_pdi.
            bapi_te_mepoitemx-zz1_canal1_pdi = 'X'.
          ENDIF.

          ls_extension-structure = 'BAPI_TE_MEPOITEM'.
*        ls_extension-valuepart1 = bapi_te_mepoitem.

          CALL FUNCTION 'PI_BP_MOVE_UNICODE'
            EXPORTING
              iv_move_from = bapi_te_mepoitem
            CHANGING
              cv_move_to   = ls_extension-valuepart1.

          APPEND ls_extension TO lt_extension.
          CLEAR: ls_extension.

          ls_extension-structure = 'BAPI_TE_MEPOITEMX'.
*        ls_extension-valuepart1 = bapi_te_mepoitemx.

          CALL FUNCTION 'PI_BP_MOVE_UNICODE'
            EXPORTING
              iv_move_from = bapi_te_mepoitemx
            CHANGING
              cv_move_to   = ls_extension-valuepart1.

          APPEND ls_extension TO lt_extension.
          CLEAR: ls_extension.

        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

*&---------------------------------------------------------------------*
*&    EXCUTE BAPI PO
*&---------------------------------------------------------------------*
*      IF lv_count = 1.

*        DATA: t_poshippingexp TYPE TABLE OF bapimeposhippexp.

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
*            poshippingexp    = t_poshippingexp.


      READ TABLE t_return_log INTO w_return_log  WITH KEY type = 'S' id = '06' number = '017'.
      IF sy-subrc EQ 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = t_return_log.

        LOOP AT lt_data ASSIGNING <fs_auxoc> WHERE exclude EQ 'X' AND orden_compra EQ ''.
          <fs_auxoc>-orden_compra = w_return_log-message_v2.
*          MODIFY lt_data FROM ls_auxoc INDEX lv_tabix.
        ENDLOOP.

        PERFORM wipeout_all.

        true_oc  = 'X'.

        CLEAR:
               t_poheader,
               t_poheaderx,
               w_poitem,
               w_poitemx,
               w_poschedule,
               w_poschedulex,
               w_poaccount,
               w_poaccountx,
               w_poservices,
               bapi_te_mepoheader,
               bapi_te_mepoheaderx,
               bapi_te_mepoitem,
               bapi_te_mepoitemx,
               ls_extension,
               LV_POS.

        REFRESH:
                 t_poitem,
                 t_poitemx,
                 t_poschedule,
                 t_poschedulex,
                 t_poaccount,
                 t_poaccountx,
                 t_poservices,
                 lt_extension.

*        CLEAR: t_poheader, t_poheaderx, lv_pos.
*        REFRESH: t_return_log, t_poitem, t_poitemx, t_poschedule, t_poschedulex.

      ELSE.

        false_oc = 'X'.

        IF t_poheader IS INITIAL.

        ELSE.
          APPEND LINES OF t_return_log TO t_return.
        ENDIF.

        LOOP AT lt_data ASSIGNING <fs_auxoc> WHERE exclude EQ 'X' AND orden_compra EQ ''.
          CLEAR: <fs_auxoc>-exclude.
        ENDLOOP.

      CLEAR:
             t_poheader,
             t_poheaderx,
             w_poitem,
             w_poitemx,
             w_poschedule,
             w_poschedulex,
             w_poaccount,
             w_poaccountx,
             w_poservices,
             bapi_te_mepoheader,
             bapi_te_mepoheaderx,
             bapi_te_mepoitem,
             bapi_te_mepoitemx,
             ls_extension,
             LV_POS.

      REFRESH:
               t_poitem,
               t_poitemx,
               t_poschedule,
               t_poschedulex,
               t_poaccount,
               t_poaccountx,
               t_poservices,
               lt_extension.



      ENDIF.



*      CLEAR: t_poheader, t_poheaderx, w_return_log.
*
*      REFRESH: t_return_log, lt_extension,
*               t_poitemx, t_poitem,
*               t_poaccount, t_poaccountx,
*               t_poschedule, t_poschedulex,
*               t_poservices.

*      ENDIF.

    ENDLOOP.

    LOOP AT lt_data ASSIGNING <fs_auxoc> WHERE exclude EQ 'X' AND orden_compra NE ''.
      CLEAR: <fs_auxoc>-exclude.
    ENDLOOP.


*    IF lv_count > 1.

*      CALL FUNCTION 'BAPI_PO_CREATE1'
*        EXPORTING
*          poheader         = t_poheader
*          poheaderx        = t_poheaderx
*          no_price_from_po = 'X'
*        TABLES
*          return           = t_return_log
*          extensionin      = lt_extension
*          poitem           = t_poitem
*          poitemx          = t_poitemx
*          poaccount        = t_poaccount
*          poaccountx       = t_poaccountx
*          poschedule       = t_poschedule
*          poschedulex      = t_poschedulex
*          poservices       = t_poservices.
*
*
*      READ TABLE t_return_log INTO w_return_log  WITH KEY type = 'S' id = '06' number = '017'.
*      IF sy-subrc EQ 0.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait   = 'X'
*          IMPORTING
*            return = t_return_log.

*        LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_auxoc>) WHERE check EQ 'X'.
*          <fs_auxoc>-orden_compra = w_return_log-message_v2.


**          Insercion de Datos Para Objeto PA.
***          Obtener Imputación doc. compras
*          SELECT SINGLE * FROM ekkn INTO @DATA(ls_ekkn) WHERE ebeln EQ @<fs_auxoc>-orden_compra.
*          IF sy-subrc EQ 0.
*
***          Obtener CeBE
*            SELECT SINGLE * FROM marc INTO @DATA(ls_marc) WHERE matnr EQ @<fs_auxoc>-material
*                                                            AND werks EQ @<fs_auxoc>-plant.
*
***          Obtener Ultimo Rango Generado Objeto PA
*            SELECT * FROM ce4ar01_acct ORDER BY paobjnr INTO TABLE @DATA(lt_ce4ar01) UP TO 5 ROWS.
*              IF sy-subrc eQ 0.
*
*                READ TABLE lt_ce4ar01 INTO DATA(ls_ce4ar01) INDEX 1.
*                IF sy-subrc EQ 0.
*                  DATA(lv_objetopa) = ls_ce4ar01-PAOBJNR + 1.
*                ENDIF.
*
*              ENDIF.
*
*            DATA: ls_ce4ar01 type ce4ar01_acct.
*
*
*          ENDIF.

****          Obtener Segmento
**          SELECT SINGLE * FROM cepc INTO @DATA(ls_cepc) WHERE prctr EQ @ls_marc-prctr
**                                                          AND KOKRS EQ @ls_ekkn-KOKRS.

*        ENDLOOP.

*        true_oc  = 'X'.
*        CLEAR: t_poheader, t_poheaderx.
*        REFRESH: t_return_log, t_poitem, t_poitemx, t_poschedule, t_poschedulex.

*      ELSE.

*        false_oc = 'X'.
*        APPEND LINES OF t_return_log TO t_return.
*
*        CLEAR: t_poheader, t_poheaderx, w_return_log.
*
*        REFRESH: t_return_log, lt_extension,
*                 t_poitemx, t_poitem,
*                 t_poaccount, t_poaccountx,
*                 t_poschedule, t_poschedulex,
*                 t_poservices.

*      ENDIF.
*    ENDIF.


    IF true_oc  EQ 'X' AND false_oc IS INITIAL.
      MESSAGE 'Se han creado Ordenes de Compras' TYPE 'S'.
    ELSEIF false_oc  EQ 'X' AND true_oc IS INITIAL.
      MESSAGE 'Han ocurriendo errores en la generacion de las Ordenes de Compras. Revisar Log de Errores' TYPE 'S' DISPLAY LIKE 'E'.
    ELSEIF false_oc EQ 'X' AND true_oc EQ 'X'.
      MESSAGE 'Algunos registros no pudieron generar Ordenes de Compras. Revisar Log de Errores' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    CLEAR: true_oc, false_oc.

  ENDIF.

ENDFORM.

FORM user_command USING p_ucomm TYPE sy-ucomm
                         p_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN '&GEN_OC'.
      PERFORM popup_oc.
      p_selfield-refresh    = c_x.
      p_selfield-col_stable = c_x.
      p_selfield-row_stable = c_x.
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
    WHEN '&SEL_ALL'.
      LOOP AT lt_data ASSIGNING <fs_data>.
        <fs_data>-check = 'X'.
        MODIFY lt_data FROM <fs_data>.
      ENDLOOP.
      p_selfield-refresh    = c_x.
    WHEN '&DES_ALL'.
      LOOP AT lt_data ASSIGNING <fs_data>.
        <fs_data>-check = ''.
        MODIFY lt_data FROM <fs_data>.
      ENDLOOP.
      p_selfield-refresh    = c_x.
  ENDCASE.
ENDFORM.

FORM show_log.

  DATA: lt_bal_msg TYPE /scmtms/t_bal_s_msg,
        ls_bal_msg TYPE bal_s_msg.

  DATA: text TYPE string.

  REFRESH: lt_bal_msg.


  LOOP AT t_return INTO w_return WHERE type NE 'S'.

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
*&---------------------------------------------------------------------*
*& Form wipeout_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM wipeout_all .

*  CLEAR:
*         t_poheader,
*         t_poheaderx,
*         w_poitem,
*         w_poitemx,
*         w_poschedule,
*         w_poschedulex,
*         w_poaccount,
*         w_poaccountx,
*         w_poservices,
*         bapi_te_mepoheader,
*         bapi_te_mepoheaderx,
*         bapi_te_mepoitem,
*         bapi_te_mepoitemx,
*         ls_extension.
*
*  REFRESH:
*          t_poitem,
*          t_poitemx,
*          t_poschedule,
*          t_poschedulex,
*          t_poaccount,
*          t_poaccountx,
*          t_poservices,
*          lt_extension.

ENDFORM.

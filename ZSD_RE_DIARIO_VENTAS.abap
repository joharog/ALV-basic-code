*&---------------------------------------------------------------------*
*& Report  ZSD_RE_DIARIO_VENTAS
*&
*&---------------------------------------------------------------------*
* Optimizacion para COMOLSA Colombia - Junio 2022                      *
* Consultora     : C1C Consulting                                      *
* ID Autor       : JARV                                                *
* Autor Opt.     : Johan Rodriguez                                     *
*----------------------------------------------------------------------*
REPORT zsddiarioventas NO STANDARD PAGE HEADING.

TYPE-POOLS: slis.

TABLES: vbrk, vbrp, vbak, t151t, t171t, vbap, kna1, mara, bkpf, vbfa,
        t052, tvzbt, vbpa, pa0002, pa0001, tvgrt, p0000, likp, bsec.


TYPES: g_ty_s_sflight TYPE alv_t_t2.

TYPES: BEGIN OF g_ty_s_test,
         select_amount            TYPE i,
         selected_recs            TYPE i,
         no_info_popup            TYPE char1,
         info_popup_once          TYPE char1,
         events                   TYPE lvc_fname OCCURS 0,
         events_exit              TYPE slis_t_event_exit,
         events_info_popup        TYPE lvc_fname OCCURS 0,
         list_append              TYPE char1,
         list_amount              TYPE i,
         list_append_status       TYPE i,
         html_top_height          TYPE i,
         html_end_height          TYPE i,
         sett_minimize_tol        TYPE char1,
         sett_mimimize_eol        TYPE char1,
         sett_top_only_print      TYPE char1,
         sett_eol_only_print      TYPE char1,
         sett_no_colopt_print     TYPE char1,
         bypassing_buffer         TYPE char1,
         buffer_active            TYPE char1,
       END OF g_ty_s_test,

       BEGIN OF g_ty_s_evt_exit.
        INCLUDE TYPE slis_event_exit.

TYPES: text TYPE string,
       END OF g_ty_s_evt_exit,
       g_ty_t_evt_exit TYPE STANDARD TABLE OF g_ty_s_evt_exit,

       BEGIN OF g_ty_s_outtab.
        INCLUDE TYPE g_ty_s_sflight.
TYPES:   box                  TYPE char1,
         lights               TYPE char1,
       END   OF g_ty_s_outtab,
       g_ty_t_outtab TYPE TABLE OF g_ty_s_outtab.


DATA: w_disvariant      TYPE disvariant,    "Variant information
      w_es_variant      LIKE disvariant,    "Manejo de variantes
      w_variant_exit(1) TYPE c.             "Manejo de variantes

DATA: alv_field  TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      alv_layout TYPE slis_layout_alv,
      repid LIKE sy-repid,
      gs_test TYPE g_ty_s_test,
      g_start_listinfo TYPE slis_lineinfo,
      gt_outtab TYPE g_ty_t_outtab WITH HEADER LINE,
      gs_outtab TYPE g_ty_s_outtab.

DATA: ivbak LIKE vbak OCCURS 0 WITH HEADER LINE,
      ivbrk LIKE vbrk OCCURS 0 WITH HEADER LINE,
      ikna1 LIKE kna1 OCCURS 0 WITH HEADER LINE,
      ivbrp LIKE vbrp OCCURS 0 WITH HEADER LINE,
      ivbkd LIKE vbkd OCCURS 0 WITH HEADER LINE," L/N JGP 180422 T_11772
      imakt LIKE makt OCCURS 0 WITH HEADER LINE,
      it151t LIKE t151t OCCURS 0 WITH HEADER LINE,
      it171t LIKE t171t OCCURS 0 WITH HEADER LINE,
      ti_evento TYPE slis_t_event,
      ti_cabecera TYPE slis_t_listheader,
      ti_endoflist TYPE slis_t_listheader.

DATA: belnr   LIKE bseg-augbl,
      dias    LIKE t052-ztag1,
      wotnr   TYPE p,
      monto   LIKE bseg-wrbtr,
      kumne   TYPE ekpo-bpumn,
      kumza   TYPE ekpo-bpumz,
      mein_kg TYPE vbrp-meins VALUE 'KG',
      anulada TYPE vbfa-vbtyp_n VALUE 'N',
      cmont(20).

*&-- Tabla interna para totales
DATA: BEGIN OF itab OCCURS 0,
      kbetr1 LIKE konv-kbetr,  "Importe RU 11122017
      waerk LIKE vbrk-waerk,   "Moneda
      fkimg LIKE vbrp-fkimg,   "Cantidad
      preun LIKE vbrp-netwr,   "Precio Unitario
      netwr LIKE vbrp-netwr,   "Monto Neto
      mwsbp LIKE vbrp-mwsbp,   "IVA
      impfi LIKE vbrp-netwr,   "Importe Final
      wrbtr LIKE bseg-wrbtr,   "Monto de Anticipo
END OF itab.

*PARA LA FECHA
DATA gv_string     TYPE t247-ltx.
*
*&-- Tabla interna para ALV
DATA: BEGIN OF ti_formato OCCURS 0,
      kbetr1   LIKE konv-kbetr,    "Importe RU 11122017
      bukrs    LIKE bseg-bukrs,
      kunnr    LIKE vbak-kunnr,    "Codigo Cliente
      name1    LIKE bsec-name1,    "Razón Social
      audat    TYPE string,        "MES DE DOCUMENTO
      stcd1    LIKE bsec-stcd1,    "Número de Identificación Tributaria.
      kdgrp    LIKE vbrk-kdgrp,    "Cod.Grupo de Cliente
      ktext    LIKE t151t-ktext,   "Texto Grupo de Cliente
      fkart    LIKE vbrk-fkart,    "Clase Doc
      bstkd    LIKE vbkd-bstkd,    "Número de pedido del cliente
      charg    LIKE vbrp-charg,    "Número de lote
      abgru    LIKE vbap-abgru,    "Motivo de rechazo de ofertas/pedido
      ernam    LIKE vbrk-ernam,    "Nombre responsable que añadio obj.
      vbeln    LIKE vbrk-vbeln,    "Num.Factura
      aubel    LIKE vbrp-aubel,    "Nro Pedido JGP18042022 T_11772
      hkont    LIKE bseg-hkont,    "njgm-20-02-2012  Cuenta de mayor
      spart    LIKE tspat-vtext,   "Sector
      vtweg    LIKE tvtwt-vtext,   "Canal
      spart_t  LIKE tspat-vtext,   "Sector Descripción
      vtweg_t  LIKE tvtwt-vtext,   "Canal Descripción
      fkdat    LIKE vbrk-fkdat,    "Fecha de la Factura
      bzirk    LIKE vbrk-bzirk,    "Cod. Zona de Venta
      bztxt    LIKE t171t-bztxt,   "Texto Zona de Venta
      matnr    LIKE vbap-matnr,    "Cod.Material
      maktx    LIKE makt-maktx,    "Denominacion Material
***///
      matkl    LIKE vbrp-matkl,    "Grupo de Artículo
      atwrt_r  LIKE ausp-atwrt,    "Referencia
      atwrt_c  LIKE ausp-atwrt,    "Color
      wgbez    LIKE t023t-wgbez,   "Desc. Grupo de Artículo
      ktgrm    LIKE vbrp-ktgrm,    "Grp. Imputación
      vtext_gi LIKE tvkmt-vtext,   "Denominación Grp. Imputación
***///
      fkimg    LIKE vbrp-fkimg,    "Cantidad
      vrkme    LIKE vbrp-vrkme,    "Unidad de Medida Venta
      preun    TYPE p DECIMALS 2,  "Precio Unitario (calcular
      preur    LIKE vbrp-netwr,    "Precio Unitario Real (calcular)
      netwr1   TYPE p DECIMALS 2,
      mwsbp    LIKE vbrp-mwsbp,    "IVA
      impfi    TYPE p DECIMALS 2,  "vbrp-netwr,
  "Importe Final (calcular)
      kbetr    LIKE konv-kbetr,
      kwert    LIKE konv-kwert,
      vbtyp    LIKE vbrk-vbtyp,    "Tipo de documento comercial
      posnr    LIKE vbrp-posnr,    "Posicion
      zterm    LIKE vbrk-zterm,    "Condicion de Pago
      vtext    LIKE tvzbt-vtext,   "Denominacion de Cond.
      fvenc    LIKE vbrk-fkdat,    "Fecha de Vencimiento
      fven_txt TYPE string,        "MES DE VENCIMIENTO
      moros    TYPE i,             "Morosidad (Dias)
      wrbtr    LIKE bseg-wrbtr,    "Monto de Anticipo
      waerk    LIKE vbrk-waerk,    "Moneda del documento
      sttco(20),                   "Status de Contabilizacion
      stblg    LIKE bkpf-stblg,    "Documento de Anulacion
      prctr    LIKE bseg-prctr,    "Centro de Beneficio
*/////BM   frantillo
      bezei    LIKE v_tvkgr-bezei, "Denom Grp Vend
      vkgrp    LIKE vbrp-vkgrp,    "Grupo de Vendedores
      pernr    LIKE pa0001-pernr,
      ename    LIKE pa0001-ename,  "Nombre del Vendedor
      bktxt    LIKE bkpf-bktxt,    "Cabecera documento "njgm-29-02-2012
****///
      btgew    LIKE likp-btgew,   "Cantidad total Entrega
      gewei    LIKE likp-gewei,   "Unidad cantidad total entrega
      fklmg    LIKE vbrp-fklmg,   "Cant. en unid. de medida de entrega
      cant     TYPE ru_lmnga,     "Real Entregado
      lfimg    TYPE lips-lfimg,   "18.11.2010 JADM
      meins    LIKE vbrp-meins,   "Unid. de medida de entrega
      vgbel    LIKE vbrp-vgbel,   "Núm. documento del documento modelo
      ort01    LIKE kna1-ort01,   "poblacion
      bezei_r  LIKE t005u-bezei,  "region
      netwr_cop LIKE vbrp-netwr,  "Monto Neto en COP "NJGM-17.06.2013
      waerk_cop LIKE vbrk-cmwae,  "Moneda Local      "NJGM-17.06.2013
      kurrf LIKE vbrk-kurrf,
      line_color(4),              "NLC210807.n
      "BOM BDBC 24.02.2021 {
      kunnr_2 LIKE likp-kunnr,  "Destinario
      stras   LIKE kna1-stras,  "Direccion Destinario
      bezei_2 LIKE t005u-bezei, "Departamento Destinatario
      ort01_2 LIKE kna1-ort01,  "Ciudad Destinatario
      lifnr   LIKE vbpa-lifnr,  "Código Empresa Transportadora
      name1a  LIKE lfa1-name1,  "Nombre Empresa transportadora
      sortl   LIKE lfa1-sortl,  "Placa del Vehículo
      name1b  LIKE lfa1-name1,  "Nombre del Conductor
      stcd1_2 LIKE lfa1-stcd1,  "Doc. Identidad del Conductor
      "} EOM BDBC 24.02.2021
      werks LIKE vbrp-werks,  "Centro        "JARV T_10957 13.10.2021
      belnr LIKE bkpf-belnr,  "Doc. Contable "JARV T_12008 26.05.2022
END OF ti_formato.

*                                                          "NLC210807.sn
DATA: BEGIN OF ti_totales_aux OCCURS 0,
      kbetr1   LIKE konv-kbetr,    "Importe RU 11122017
      kunnr    LIKE vbak-kunnr,    "Codigo Cliente
      name1    LIKE bsec-name1,    "Razón Social
      stcd1    LIKE bsec-stcd1,
      kdgrp    LIKE vbrk-kdgrp,    "Cod.Grupo de Cliente
      ktext    LIKE t151t-ktext,   "Texto Grupo de Cliente
      fkart    LIKE vbrk-fkart,    "Clase Doc
      bstkd    LIKE vbkd-bstkd,    "Número ORD JGP20042022 T_11772
      vbeln    LIKE vbrk-vbeln,    "Num.Factura
      aubel    LIKE vbrp-aubel,    "Nro Pedido JGP20042022 T_11772
      hkont    LIKE bseg-hkont,    "njgm-20-02-2012  Cuenta de mayor
      spart    LIKE tspat-vtext,   "Sector
      vtweg    LIKE tvtwt-vtext,   "Canal
      spart_t  LIKE tspat-vtext,   "Sector Descripción
      vtweg_t  LIKE tvtwt-vtext,   "Canal Descripción
      fkdat    LIKE vbrk-fkdat,    "Fecha de la Factura
      bzirk    LIKE vbrk-bzirk,    "Cod. Zona de Venta
      bztxt    LIKE t171t-bztxt,   "Texto Zona de Venta
      matnr    LIKE vbap-matnr,    "Cod.Material
      maktx    LIKE makt-maktx,    "Denominacion Material
      matkl    LIKE vbrp-matkl,    "Grupo de Artículo
      wgbez    LIKE t023t-wgbez,   "Desc. Grupo de Artículo
      ktgrm    LIKE vbrp-ktgrm,    "Grp. Imputación
      vtext_gi LIKE tvkmt-vtext,   "Denominación Grp. Imputación
      fkimg    LIKE vbrp-fkimg,    "Cantidad
      vrkme    LIKE vbrp-vrkme,    "Unidad de Medida Venta
      preun    LIKE vbrp-netwr,    "Precio Unitario (calcular)
      preur    LIKE vbrp-netwr,    "Precio Unitario Real (calcular)
      netwr    LIKE vbrp-netwr,    "Monto Neto
      mwsbp    LIKE vbrp-mwsbp,    "IVA
      impfi    LIKE vbrp-netwr,    "Importe Final (calcular)
      vbtyp    LIKE vbrk-vbtyp,    "Tipo de documento comercial
      posnr    LIKE vbrp-posnr,    "Posicion
      zterm    LIKE vbrk-zterm,    "Condicion de Pago
      vtext    LIKE tvzbt-vtext,   "Denominacion de Cond.
      fvenc    LIKE vbrk-fkdat,    "Fecha de Vencimiento
      moros    TYPE i,             "Morosidad (Dias)
      wrbtr    LIKE bseg-wrbtr,    "Monto de Anticipo
      waerk    LIKE vbrk-waerk,    "Moneda del documento
      sttco(20),                   "Status de Contabilizacion
      stblg    LIKE bkpf-stblg,    "Documento de Anulacion
      prctr    LIKE bseg-prctr,    "Centro de Beneficio
      bezei    LIKE v_tvkgr-bezei, "Denom Grp Vend
      vkgrp    LIKE vbrp-vkgrp,    "Grupo de Vendedores
      pernr    LIKE pa0001-pernr,  "Num. Personal
      ename    LIKE pa0001-ename,  "Nombre del Vendedor
      bktxt    LIKE bkpf-bktxt,    "Cabecera documento "njgm-29-02-2012
*      xblnr    LIKE bkpf-xblnr,   "Referencia    "njgm-29-02-2012
      btgew    LIKE likp-btgew,    "Cantidad total Entrega
      gewei    LIKE likp-gewei,    "Unidad cantidad total entrega
      fklmg    LIKE vbrp-fklmg,    "Cant. en unid. de medida de entrega
      cant     TYPE ru_lmnga,      "Real Entregado
      meins    LIKE vbrp-meins,    "Unid. de medida de entrega
      vgbel    LIKE vbrp-vgbel,    "Núm. documento del documento modelo
      ort01    LIKE kna1-ort01,    "poblacion
      werks    LIKE vbrp-werks,    "Centro "JARV T_10957 10.11.2021
      ort01_2  LIKE kna1-ort01,    "Ciudad Dest. JGP20042022 T_11772
      bezei_r  LIKE t005u-bezei,   "Dep.Desp. JGP28042022 T_11772
      bezei_2  LIKE t005u-bezei,   "Deo Dest. JGP28042022 T_11772
      belnr    LIKE bkpf-belnr,    "Doc.Contable JARV T_12008 26.05.2022
END OF ti_totales_aux.

DATA: BEGIN OF ti_totales OCCURS 0,
      kbetr1   LIKE konv-kbetr,  "Importe RU 11122017
      kunnr    LIKE vbak-kunnr,  "Codigo Cliente
      name1    LIKE bsec-name1,  "Razón Social
      stcd1    LIKE bsec-stcd1,
      preun    LIKE vbrp-netwr,  "Precio Unitario (calcular)
      netwr    LIKE vbrp-netwr,  "Monto Neto
      mwsbp    LIKE vbrp-mwsbp,  "IVA
      impfi    LIKE vbrp-netwr,  "Importe Final (calcular)
      btgew    LIKE likp-btgew,  "Cantidad total Entrega
      fklmg    LIKE vbrp-fklmg,  "Cant. en unid. de medida de entrega
      cant     TYPE ru_lmnga,
      werks    LIKE vbrp-werks,  "Centro "JARV T_10957 10.11.2021
      belnr    LIKE bkpf-belnr,  "Doc. Contable JARV T_12008 26.05.2022
END OF ti_totales.

DATA: BEGIN OF wa_totales OCCURS 0,
      kbetr1   LIKE konv-kbetr,  "Importe RU 11122017
      kunnr    LIKE vbak-kunnr,  "Codigo Cliente
      name1    LIKE bsec-name1,  "Razón Social
      stcd1    LIKE bsec-stcd1,
      preun    LIKE vbrp-netwr,  "Precio Unitario (calcular)
      netwr    LIKE vbrp-netwr,  "Monto Neto
      mwsbp    LIKE vbrp-mwsbp,  "IVA
      impfi    LIKE vbrp-netwr,  "Importe Final (calcular)
      btgew    LIKE likp-btgew,  "Cantidad total Entrega
      fklmg    LIKE vbrp-fklmg,  "Cant. en unid. de medida de entrega
      cant     TYPE ru_lmnga,
      werks    LIKE vbrp-werks,  "Centro "JARV T_10957 10.11.2021
      belnr    LIKE bkpf-belnr,  "Doc. Contable JARV T_12008 26.05.2022
END OF wa_totales.
*                                                          "NLC210807.en

DATA: wa_formato LIKE ti_formato.
DATA: ti_formato2 LIKE ti_formato OCCURS 0 WITH HEADER LINE.
DATA: p_kunnr TYPE vbpa-kunnr.
DATA: kurrf TYPE p DECIMALS 6. "njgm-17.10.2013
DATA: val TYPE p DECIMALS 4.
DATA: val1 TYPE p DECIMALS 2.

*JARV T_12057 - 16.06.2022
DATA: BEGIN OF ti_entregas OCCURS 0,
vbeln TYPE vbeln,
vgbel TYPE vgbel,
END OF ti_entregas.

DATA: BEGIN OF ti_entregas1 OCCURS 0,
vbeln LIKE lips-vbeln,
vgbel LIKE vbrp-vgbel,
END OF ti_entregas1.

TYPES: BEGIN OF st_lips,
vbeln LIKE lips-vbeln,
lfimg LIKE lips-lfimg,
END OF st_lips.

DATA: ti_lips TYPE TABLE OF st_lips,
      ti_lipsf TYPE TABLE OF st_lips,
      wa_lips TYPE st_lips,
      wa_lipsf TYPE st_lips.

DATA: it_tvtwt TYPE TABLE OF tvtwt,
      it_t151t TYPE TABLE OF t151t,
      it_t171t TYPE TABLE OF t171t,
      it_tspat TYPE TABLE OF tspat,
      wa_tvtwt TYPE tvtwt,
      wa_t151t TYPE t151t,
      wa_t171t TYPE t171t,
      wa_tspat TYPE tspat.

DATA: it_t023t  TYPE TABLE OF t023t,
      it_tvkmt  TYPE TABLE OF tvkmt,
*      it_bkpf   TYPE TABLE OF bkpf,
      it_tvgrt  TYPE TABLE OF tvgrt,
      it_pa0002 TYPE TABLE OF pa0002,
      it_kna1   TYPE TABLE OF kna1,
      it_vbfa   TYPE TABLE OF vbfa,
      it_tvzbt  TYPE TABLE OF tvzbt,
      it_likp   TYPE TABLE OF likp,
      it_t052   TYPE TABLE OF t052,
      wa_t023t  TYPE t023t,
      wa_tvkmt  TYPE tvkmt,
*      wa_bkpf   TYPE bkpf,
      wa_tvgrt  TYPE tvgrt,
      wa_pa0002 TYPE pa0002,
      wa_kna1   TYPE kna1,
      wa_vbfa   TYPE vbfa,
      wa_tvzbt  TYPE tvzbt,
      wa_likp   TYPE likp,
      wa_entregas LIKE ti_entregas,
      wa_t052   TYPE t052.

TYPES: BEGIN OF st_bkpf,
        belnr TYPE bkpf-belnr,
        bktxt TYPE bkpf-bktxt,
        awkey TYPE char10,
       END OF st_bkpf.

DATA: it_rv_vbfa TYPE TABLE OF vbfa,
      wa_rv_vbfa LIKE vbfa,
      st_comwa TYPE vbco6,
      lv_zbd1t TYPE dzbd1t,
      lv_zbd2t TYPE dzbd2t.
*JARV T_12057 - 16.06.2022

DATA: wa_btgew LIKE likp-btgew,
      wa_gewei LIKE likp-gewei,
      wa_lfimg LIKE lips-lfimg, "18.11.2010 JADM
      lfimg    TYPE lfimg.

*&-----------------------------------------------
* 28.09.2010 JADM
DATA: BEGIN OF i_match OCCURS 0,
        shlpname  LIKE ddshretval-shlpname,
        fieldname LIKE ddshretval-fieldname,
        recordpos LIKE ddshretval-recordpos,
        fieldval  LIKE ddshretval-fieldval,
        retfield  LIKE ddshretval-retfield,
      END OF i_match.

DATA: BEGIN OF it_values OCCURS 0,
        pernr TYPE persno,  "Num. Personal
        sname TYPE smnam,   "Nombre del Vendedor
      END OF it_values.

******STAR FAA 12/11/2014
DATA: it_vbpa  TYPE TABLE OF vbpa,
      it_t005u TYPE TABLE OF t005u,
      wa_vbpa  LIKE LINE OF  it_vbpa,
      wa_t005u LIKE LINE OF  it_t005u.

DATA: lv_monto  LIKE bapicurr-bapicurr,
      monto2    TYPE p DECIMALS 5,
      monto_usd TYPE p DECIMALS 6.
***** EOB FAA 12/11/2014

DATA: monto18 TYPE p DECIMALS 2. "BDBC 22.07.2022 T_12304


DATA: gt_vbpa    TYPE TABLE OF vbpa,
      gt_likp    TYPE TABLE OF likp,
      gt_kna1    TYPE TABLE OF kna1,
      gt_t005u   TYPE TABLE OF t005u,
      gt_lfa1    TYPE TABLE OF lfa1,
      ti_vbpa2   TYPE TABLE OF vbpa, "L/N JGP 28042022 T_11772
      wa_vbpa1   TYPE vbpa,
      wa_vbpa2   TYPE vbpa,          "L/N JGP 28042022 T_11772
      wa_kna1_2  TYPE kna1,
      wa_t005u_2 TYPE t005u,
      wa_lfa1    TYPE lfa1.

******************************************** YYMS 02.02.15 - start
DATA: ti_vbpa TYPE STANDARD TABLE OF vbpa WITH HEADER LINE.
DATA: ti_vbap TYPE STANDARD TABLE OF vbap WITH HEADER LINE.
DATA: gt_ausp TYPE STANDARD TABLE OF ausp WITH HEADER LINE.
DATA: ti_konv TYPE TABLE OF konv WITH HEADER LINE.
DATA: ti_konv2 TYPE TABLE OF konv WITH HEADER LINE.
*  DATA: TI_KONV TYPE SORTED TABLE OF KONV."WITH HEADER LINE.
DATA: v_knumv TYPE knumv.

DATA: BEGIN OF gt_caract OCCURS 0,
  objek LIKE ausp-objek,
  atinn LIKE ausp-atinn,
  atwrt LIKE ausp-atwrt,
  atnam LIKE cabn-atnam,
END OF gt_caract.

*&-----------------------------------------------
CONSTANTS: formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE',
           formname_end_of_list TYPE slis_formname VALUE 'END_OF_LIST'.
*&-----------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK param WITH FRAME TITLE text-001.

SELECT-OPTIONS: fecha  FOR vbrk-fkdat OBLIGATORY,
                soc    FOR vbrk-bukrs DEFAULT 'CO11'
                                      NO INTERVALS NO-EXTENSION,
                org    FOR vbrk-vkorg DEFAULT 'OV01'
                                      NO INTERVALS NO-EXTENSION,
                canal  FOR vbrk-vtweg,
                sector FOR vbrk-spart,
                vbeln  FOR vbrk-vbeln,
                kunnr  FOR kna1-kunnr,
                matnr  FOR mara-matnr,
                matkl  FOR vbrp-matkl,
                ofic   FOR vbak-vkbur,
                gr_cli FOR vbrk-kdgrp,
                zone   FOR vbrk-bzirk,
                vkgrp  FOR vbrp-vkgrp,
                pernr  FOR pa0002-pernr."MATCHCODE OBJECT zpremk.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK prn_typ WITH FRAME TITLE text-005.

PARAMETERS:     normal  RADIOBUTTON GROUP rbg1,
                resume  RADIOBUTTON GROUP rbg1,
                tot_cli RADIOBUTTON GROUP rbg1,   "NLC210807.n
                amplia  RADIOBUTTON GROUP rbg1.   "JGP 17032021

SELECTION-SCREEN END OF BLOCK prn_typ.

SELECTION-SCREEN END OF BLOCK param.
**** STAR FAA 12/11/2014
SELECTION-SCREEN BEGIN OF BLOCK mon_vis WITH FRAME TITLE text-003.
PARAMETER p_waerk LIKE vbrk-waerk OBLIGATORY.
SELECTION-SCREEN END OF BLOCK mon_vis.
**** EOB FAA 12/11/2014

SELECTION-SCREEN BEGIN OF BLOCK variante WITH FRAME TITLE text-002.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK variante.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM variantes.


INITIALIZATION.
  repid = sy-repid.
  w_disvariant-report = repid.
*&-----------------------------------------------------
*& 28.09.2010 JADM

  SELECT *
    FROM pa0001
     INTO  CORRESPONDING FIELDS OF TABLE it_values.
*      WHERE pernr EQ pa0001-pernr.
*&-----------------------------------------------------

*&-------------------------------------------------------
* 28.09.2010 JADM
*&-------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pernr-low.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PERNR'
      window_title    = 'Nombre del Vendedor'
      value_org       = 'S'
    TABLES
      value_tab       = it_values
      return_tab      = i_match
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc EQ 0.
    READ TABLE i_match INDEX 1.
    MOVE i_match-fieldval TO pernr-low.
  ENDIF.
*&-------------------------------------------------------
* 28.09.2010 JADM
*&-------------------------------------------------------

  SELECT *
    FROM pa0001
     INTO  CORRESPONDING FIELDS OF TABLE it_values.
*      WHERE pernr EQ vbpa-pernr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pernr-high.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PERNR'
      window_title    = 'Nonbre del Vendedor'
      value_org       = 'S'
    TABLES
      value_tab       = it_values
      return_tab      = i_match
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc EQ 0.
    READ TABLE i_match INDEX 1.
    MOVE i_match-fieldval TO pernr-high.
  ENDIF.
*&-------------------------------------------------------
* 28.09.2010 JADM
*&-------------------------------------------------------
START-OF-SELECTION.
  PERFORM cargar_datos.

END-OF-SELECTION.
  IF ti_formato[] IS INITIAL.
    MESSAGE i208(00)
    WITH 'No existe información para los valores solicitados'.
    STOP.
  ENDIF.

  PERFORM eventos USING ti_evento[].
  PERFORM hacer_cabecera.
  PERFORM cabecera2.
  PERFORM catalogo.
  PERFORM layout.
  PERFORM mostrar_dventas.

*&---------------------------------------------------------------------*
*&      Form  cargar_datos
*&---------------------------------------------------------------------*
FORM cargar_datos .

*&-- Busca facturas que correspondan con la seleccion
  SELECT * INTO TABLE ivbrk FROM vbrk
  WHERE vbeln IN vbeln
    AND bukrs IN soc
    AND vkorg IN org
    AND vtweg IN canal
    AND spart IN sector
    AND fkdat IN fecha
    AND kdgrp IN gr_cli
    AND bzirk IN zone
    AND kunag IN kunnr.

  IF ivbrk[] IS NOT INITIAL. "ConsuLta para llenado de IVBRK JGP

    SELECT *
      FROM t052
      INTO TABLE it_t052
      FOR ALL ENTRIES IN ivbrk
      WHERE zterm EQ ivbrk-zterm.

*&-- Busca formula para calculo de vencimiento
    SELECT *
      FROM tvzbt
      INTO TABLE it_tvzbt
      FOR ALL ENTRIES IN ivbrk
      WHERE zterm EQ ivbrk-zterm
        AND spras EQ sy-langu.

*    SELECT *
*      INTO CORRESPONDING FIELDS OF TABLE it_bkpf
*      FROM bkpf
*      FOR ALL ENTRIES IN ivbrk
*      WHERE awkey EQ ivbrk-vbeln
*        AND bukrs IN soc.

*   Documento Anulado
    SELECT *
      FROM vbfa
      INTO TABLE it_vbfa
      FOR ALL ENTRIES IN ivbrk
      WHERE vbelv EQ ivbrk-vbeln
        AND vbtyp_n EQ anulada.

*   Busca texto de Canal de Distrib.
    SELECT *
      FROM tvtwt
      INTO TABLE it_tvtwt
      FOR ALL ENTRIES IN ivbrk
      WHERE vtweg EQ ivbrk-vtweg
        AND spras EQ sy-langu.

*   Busca textos de Grupos de clientes
    SELECT *
      FROM t151t
      INTO TABLE it_t151t
      FOR ALL ENTRIES IN ivbrk
      WHERE kdgrp EQ ivbrk-kdgrp
        AND spras EQ sy-langu.

*   Busca textos de Zonas de Ventas
    SELECT *
      FROM t171t
      INTO TABLE it_t171t
      FOR ALL ENTRIES IN ivbrk
      WHERE bzirk EQ ivbrk-bzirk
        AND spras EQ sy-langu.

*   Busca nombre del cliente
    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN ivbrk
      WHERE kunnr EQ ivbrk-kunag
        AND ktokd NE 'Z015'.


*$---------------------------------------------------------------------
*&-- Busca textos de Grupos de clientes
    SELECT * INTO TABLE it151t FROM t151t.

*&--y Zonas de ventas
    SELECT * INTO TABLE it171t FROM t171t.

    SELECT a~objek a~atinn a~atwrt b~atnam
      INTO TABLE gt_caract
      FROM ausp AS a INNER JOIN cabn AS b
      ON a~atinn EQ b~atinn
      WHERE a~klart EQ '001'.
*$---------------------------------------------------------------------

    SELECT knumv kwert kbetr kposn stunr kpein kschl
      FROM konv
      INTO CORRESPONDING FIELDS OF TABLE ti_konv
      FOR ALL ENTRIES IN ivbrk
      WHERE knumv EQ ivbrk-knumv
        AND kschl IN ('ZPR0','ZPR1','PR00','ZPMP','ZPVA','ZPS0','ZPR2',
                      'VPRS')
       AND kawrt NE '0.00'.

    SELECT knumv kwert kbetr kposn stunr kpein kschl
      FROM konv
      INTO CORRESPONDING FIELDS OF TABLE ti_konv2
      FOR ALL ENTRIES IN ivbrk
      WHERE knumv EQ ivbrk-knumv
        AND kschl IN ('VPRS').

    SELECT *
      FROM vbrp
      INTO TABLE ivbrp
      FOR ALL ENTRIES IN ivbrk
      WHERE vbeln EQ ivbrk-vbeln
      AND matnr IN matnr
      AND vkgrp IN vkgrp
      AND fkimg <> 0.

    IF ivbrp[] IS NOT INITIAL.  "ConsuLta para llenado de IVBRP
*   Descripcion Grp Vendedores
      SELECT *
        FROM tvgrt
        INTO TABLE it_tvgrt
        FOR ALL ENTRIES IN ivbrp
        WHERE vkgrp EQ ivbrp-vkgrp
          AND spras EQ 'S'.

*   Busca descripcion del Sector
      SELECT *
        FROM tspat
        INTO TABLE it_tspat
        FOR ALL ENTRIES IN ivbrp
        WHERE spart EQ ivbrp-spart
          AND spras EQ sy-langu.

*   Descripción Grupo de Artículos
      SELECT *
        INTO TABLE it_t023t
        FROM t023t
        FOR ALL ENTRIES IN ivbrp
        WHERE spras EQ sy-langu
         AND  matkl EQ ivbrp-matkl.

*   Descripción Grupo de imputación
      SELECT *
        INTO TABLE it_tvkmt
        FROM tvkmt
        FOR ALL ENTRIES IN ivbrp
        WHERE spras EQ sy-langu
        AND ktgrm EQ ivbrp-ktgrm.

*   JARV T_12057 14.06.2022
      SELECT vbeln vgbel FROM vbrp
        INTO TABLE ti_entregas1
        FOR ALL ENTRIES IN ivbrk
        WHERE vgtyp EQ 'J'
          AND vbeln EQ ivbrk-vbeln.

      SELECT vbeln lfimg
       FROM lips
       INTO TABLE ti_lips
       FOR ALL ENTRIES IN ti_entregas1
       WHERE vbeln EQ ti_entregas1-vgbel.

      LOOP AT ti_lips INTO wa_lips.
        COLLECT wa_lips INTO ti_lipsf.
      ENDLOOP.
*   JARV T_12057 14.06.2022

      SELECT vbeln vgbel FROM vbrp
        INTO TABLE ti_entregas
        FOR ALL ENTRIES IN ivbrk
        WHERE vbeln EQ ivbrk-vbeln AND
              matnr IN matnr AND
              vkgrp IN vkgrp.

      IF ti_entregas[] IS NOT INITIAL.
        SORT ti_entregas BY vbeln vgbel.

        DELETE ADJACENT DUPLICATES FROM ti_entregas
        COMPARING vbeln vgbel.

        SELECT *
          FROM likp
          INTO TABLE it_likp
          FOR ALL ENTRIES IN ti_entregas
          WHERE vbeln EQ ti_entregas-vgbel.
      ENDIF.

    ENDIF.

  ENDIF. "ConsuLta para llenado de IVBRK JGP 09062021


  IF ivbrp[] IS NOT INITIAL.
    SELECT kunnr vbeln pernr parvw
      FROM vbpa
      INTO CORRESPONDING FIELDS OF TABLE ti_vbpa
      FOR ALL ENTRIES IN ivbrp
      WHERE vbeln EQ ivbrp-vbeln AND
            parvw IN ('AG','VE') AND
            kunnr IN kunnr.

    IF ti_vbpa[] IS NOT INITIAL.
      SELECT *
       FROM pa0002
       INTO TABLE it_pa0002
       FOR ALL ENTRIES IN ti_vbpa
       WHERE pernr EQ ti_vbpa-pernr.

      SELECT vbeln abgru
        FROM vbap
        INTO CORRESPONDING FIELDS OF TABLE ti_vbap
        FOR ALL ENTRIES IN ivbrp
        WHERE vbeln EQ ivbrp-vgbel.
    ENDIF.

  ENDIF.


  IF ivbrp[] IS NOT INITIAL.
    SELECT *
      FROM vbpa
      INTO TABLE gt_vbpa
      FOR ALL ENTRIES IN ivbrp
      WHERE vbeln EQ ivbrp-vgbel
        AND parvw IN ('SP', 'ZC').

    IF gt_vbpa[] IS NOT INITIAL. "L/N JGP 09062021
      SELECT *
        FROM lfa1
        INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_vbpa
        WHERE lifnr EQ gt_vbpa-lifnr.
    ENDIF. " gt_vbpa LN  JGP 09062021

    SELECT *
      FROM vbpa
      INTO TABLE ti_vbpa2
      FOR ALL ENTRIES IN ivbrp
      WHERE vbeln EQ ivbrp-vgbel
      AND parvw EQ 'WE'.

    IF ti_vbpa2[] IS NOT INITIAL. "L/N JGP 09062021
      SELECT *
        FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN ti_vbpa2
        WHERE kunnr EQ ti_vbpa2-kunnr.

      IF gt_kna1[] IS NOT INITIAL. "L/N JGP 09062021
        SELECT *
          FROM t005u
          INTO TABLE gt_t005u
          FOR ALL ENTRIES IN gt_kna1
          WHERE bland EQ gt_kna1-regio
            AND spras EQ 'S' .
      ENDIF.

*     JGP 18042022 T_11772
      SELECT *
        FROM vbkd
        INTO TABLE ivbkd
        FOR ALL ENTRIES IN ivbrp
        WHERE vbeln EQ ivbrp-aubel.

    ENDIF. "L/N JGP 09062021

  ENDIF.


*  JARV T_12057 14.05.2022
  LOOP AT ivbrk.

*&-- Datos de cabecera de la factura
    MOVE ivbrk-vbeln TO ti_formato-vbeln.
    MOVE ivbrk-ernam TO ti_formato-ernam.
    MOVE ivbrk-bukrs TO ti_formato-bukrs.
    MOVE ivbrk-fkart TO ti_formato-fkart.
    MOVE ivbrk-kdgrp TO ti_formato-kdgrp.
    MOVE ivbrk-bzirk TO ti_formato-bzirk.
    MOVE ivbrk-vtweg TO ti_formato-vtweg.
    MOVE ivbrk-fkart TO ti_formato-fkart.
    MOVE ivbrk-fkdat TO ti_formato-fkdat.
    MOVE ivbrk-vbtyp TO ti_formato-vbtyp.
    MOVE ivbrk-zterm TO ti_formato-zterm.
    ivbrk-kurrf = ivbrk-kurrf * 1000. "njgm-19.08.2013
    MOVE ivbrk-kurrf TO ti_formato-kurrf.

    READ TABLE ti_entregas WITH KEY vbeln = ivbrk-vbeln .


*    CLEAR ti_formato-bktxt.
*    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = ivbrk-vbeln.
*    IF sy-subrc EQ 0.
*      MOVE wa_bkpf-bktxt TO ti_formato-bktxt.
*    ENDIF.



    LOOP AT ivbrp WHERE vkbur IN ofic AND vbeln = ivbrk-vbeln.

*&-- Datos de posicion de la factura
      MOVE ivbrp-posnr TO ti_formato-posnr.
      MOVE ivbrp-fkimg TO ti_formato-fkimg.
      MOVE ivbrp-spart TO ti_formato-spart.
      MOVE ivbrp-matkl TO ti_formato-matkl.
      MOVE ivbrp-ktgrm TO ti_formato-ktgrm.
      MOVE ivbrp-charg TO ti_formato-charg.
      MOVE ivbrp-aubel TO ti_formato-aubel.
      MOVE ivbrp-werks TO ti_formato-werks. "JARV T_10957 13.10.2021
      MOVE ivbrp-fklmg TO ti_formato-fklmg.
      MOVE ivbrp-meins TO ti_formato-meins.
      MOVE ivbrp-vgbel TO ti_formato-vgbel.
      MOVE ivbrp-vkgrp TO ti_formato-vkgrp.
      MOVE ivbrp-matnr TO ti_formato-matnr.
      MOVE ivbrp-arktx TO ti_formato-maktx.
      MOVE ivbrp-vrkme TO ti_formato-vrkme.
      MOVE ivbrp-prctr TO ti_formato-prctr.

*     JARV T_12057 - 21.06.2022
*     Obtener Doc. Contable
      FREE: it_rv_vbfa[].
      CLEAR: st_comwa,wa_rv_vbfa,it_rv_vbfa.

      st_comwa-vbeln = ivbrk-vbeln.
      st_comwa-posnr = ''.
      st_comwa-etenr = ''.


      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          aufbereitung = '2'
          belegtyp     = ' '
          comwa        = st_comwa
          nachfolger   = 'X'
          n_stufen     = '50'
          vorgaenger   = 'X'
          v_stufen     = '50'
          no_acc_doc   = ' '
        TABLES
          vbfa_tab     = it_rv_vbfa.

      IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      READ TABLE it_rv_vbfa INTO wa_rv_vbfa WITH KEY
                                            vbelv = ivbrk-vbeln.
      IF sy-subrc EQ 0.
        MOVE wa_rv_vbfa-vbeln TO ti_formato-belnr.

*       Obtener Dias de Credito
        SELECT SINGLE zbd1t zbd2t
          FROM bsid
          INTO (lv_zbd1t, lv_zbd2t)
          WHERE belnr EQ wa_rv_vbfa-vbeln
            AND bukrs EQ ivbrk-bukrs
            AND gjahr EQ ivbrk-fkdat+0(4).

        IF sy-subrc NE 0.
          SELECT SINGLE zbd1t zbd2t
            FROM bsad
            INTO (lv_zbd1t, lv_zbd2t)
            WHERE belnr EQ wa_rv_vbfa-vbeln
              AND bukrs EQ ivbrk-bukrs
              AND gjahr EQ ivbrk-fkdat+0(4).
        ENDIF.

*       Obtener Cuenta Ïngresos Mayor
        SELECT SINGLE hkont
          FROM bsis
          INTO ti_formato-hkont
          WHERE belnr EQ wa_rv_vbfa-vbeln
            AND bukrs EQ ivbrk-bukrs
            AND gjahr EQ ivbrk-fkdat+0(4).

        IF sy-subrc NE 0.
          SELECT SINGLE hkont
          FROM bsas
          INTO ti_formato-hkont
          WHERE belnr EQ wa_rv_vbfa-vbeln
            AND bukrs EQ ivbrk-bukrs
            AND gjahr EQ ivbrk-fkdat+0(4).

        ENDIF.
      ENDIF.
*     JARV T_12057 - 21.06.2022


      CLEAR: p_kunnr.
      READ TABLE ti_vbpa WITH KEY vbeln = ivbrp-vbeln
                                  parvw = 'AG'.
      IF sy-subrc EQ 0.
        MOVE ti_vbpa-kunnr TO p_kunnr.
      ENDIF.

      IF sy-subrc EQ 0.
*      JAG:BEGIN - AGREGADO POR JOALBERT 18-08-2014
*      NOTA: SE DEBERIA CORREGIR EL PERFORMANCE DE (FORM cargar_datos)
*      YA QUE ESTAN LOS SELECTS ESTAN DENTRO DE LOOPS

        CALL FUNCTION 'ISP_GET_MONTH_NAME'
          EXPORTING
            date     = ivbrk-fkdat
            language = 'S'
          IMPORTING
            longtext = gv_string.
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        MOVE gv_string TO ti_formato-audat.

        CLEAR ti_formato-abgru.
        READ TABLE ti_vbap WITH KEY vbeln = ivbrp-vgbel.
        IF sy-subrc = 0.
          MOVE ti_vbap-abgru TO ti_formato-abgru.
        ENDIF.
*
        LOOP AT gt_caract WHERE objek EQ ivbrp-matnr.
*
          IF gt_caract-atnam EQ 'Z_COLOR'.
            ti_formato-atwrt_c = gt_caract-atwrt.
          ELSEIF gt_caract-atnam EQ 'Z_REFERENCIA'.
            ti_formato-atwrt_r = gt_caract-atwrt.
          ENDIF.
*
        ENDLOOP.
*
        READ TABLE it_t023t INTO wa_t023t WITH KEY matkl = ivbrp-matkl.
        IF sy-subrc EQ 0.
          MOVE wa_t023t-wgbez TO ti_formato-wgbez.
        ENDIF.
*
        READ TABLE it_tvkmt INTO wa_tvkmt WITH KEY ktgrm = ivbrp-ktgrm.
        IF sy-subrc EQ 0.
          MOVE wa_tvkmt-vtext TO ti_formato-vtext_gi.
        ENDIF.
*
*       BOM JGP 180422 T_11772
        READ TABLE ivbkd WITH KEY vbeln = ivbrp-aubel.
        IF sy-subrc EQ 0.
          MOVE ivbkd-bstkd TO ti_formato-bstkd.
        ENDIF.
****  EOM JGP 180422 T_11772

*&&& Si la factura esta hecha en Unidades, Entonces Coloco en cantidad
*&&& Real, la cantidad notificacada para ese lote
        CLEAR: ti_formato-cant.
        IF ivbrp-vrkme EQ 'ST'
          AND ivbrp-charg IS NOT INITIAL.
        ELSEIF ivbrp-vrkme EQ 'KG'.
          MOVE: ivbrp-fkimg TO ti_formato-cant.
        ENDIF.
*
        IF ivbrp-pstyv = 'TATX'.
          ti_formato-cant = 0.
        ENDIF.
*
        READ TABLE it_tvgrt INTO wa_tvgrt WITH KEY vkgrp = ivbrp-vkgrp.
        IF sy-subrc EQ 0.
          MOVE wa_tvgrt-bezei TO ti_formato-bezei.
        ENDIF.


        CLEAR ti_formato-ename.
        READ TABLE ti_vbpa WITH KEY parvw = 'VE'
                                    vbeln = ivbrk-vbeln.
        IF sy-subrc EQ 0.

          MOVE ti_vbpa-pernr TO ti_formato-pernr.

          READ TABLE it_pa0002 INTO wa_pa0002
                               WITH KEY pernr = ti_vbpa-pernr.
          IF sy-subrc EQ 0.
*           JARV T_12083 - 17.06.2022
            CONCATENATE wa_pa0002-vorna wa_pa0002-nachn
            INTO ti_formato-ename SEPARATED BY space.
          ENDIF.

        ENDIF.

        CHECK ti_formato-pernr IN pernr.

***** Calcula Monto Neto y I.V.A según parámetro de Selección*********
        CLEAR monto18.  "BDBC 22.07.2022 T_12304

        IF p_waerk <> ivbrk-waerk.

          IF p_waerk = 'USD'.

            MOVE p_waerk TO ti_formato-waerk.

            lv_monto = ivbrp-netwr * 10.


            CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
              EXPORTING
                date             = ti_formato-fkdat
                foreign_amount   = ivbrp-netwr "BDBC 22.07.2022 T_12304
               "foreign_amount   = lv_monto    "BDBC 22.07.2022 T_12304
                foreign_currency = ivbrk-waerk
                local_currency   = ti_formato-waerk
              IMPORTING
                local_amount     = monto18.    "BDBC 22.07.2022 T_12304
               "local_amount     = monto2.     "BDBC 22.07.2022 T_12304
            IF sy-subrc <> 0.
*             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

            monto_usd = ivbrp-netwr / ivbrp-fkimg.
            monto_usd = monto_usd * 100.

            CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
              EXPORTING
                date             = ti_formato-fkdat
                foreign_amount   = monto_usd
                foreign_currency = ivbrk-waerk
                local_currency   = ti_formato-waerk
              IMPORTING
                exchange_rate    = kurrf
                local_amount     = monto2.
            IF sy-subrc <> 0.
*             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

            kurrf = kurrf * 10000.
            kurrf = kurrf * -1.

            MOVE ivbrk-kurrf TO kurrf.

           "BOM BDBC 22.07.2022 T_12304 {
            ti_formato-netwr1 = monto18.
           "ti_formato-netwr1 = ( ivbrp-netwr * 100 ) / kurrf.
           "} EOM BDBC 22.07.2022 T_12304

            CLEAR: lv_monto, monto2.

            lv_monto = ivbrp-mwsbp * 10.

            CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
              EXPORTING
                date             = ti_formato-fkdat
                foreign_amount   = lv_monto
                foreign_currency = ivbrk-waerk
                local_currency   = ti_formato-waerk
              IMPORTING
                local_amount     = monto2.
            IF sy-subrc <> 0.
*             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

            MOVE monto2 TO ti_formato-mwsbp.
            CLEAR: lv_monto, monto2.

          ELSE.

            ti_formato-waerk = p_waerk.

            ti_formato-netwr1 = ivbrp-netwr * ti_formato-kurrf.

            kurrf = ( ivbrk-kurrf / 1000 ).

            CLEAR: lv_monto, monto2.

            CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
              EXPORTING
*           CLIENT                  = SY-MANDT
                date                    = ti_formato-fkdat
                foreign_amount          = ivbrp-mwsbp
                foreign_currency        = ivbrk-waerk
                local_currency          = ti_formato-waerk
             IMPORTING
               local_amount            = monto2
                      .
            IF sy-subrc <> 0.
*             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

            CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
              EXPORTING
                currency        = ti_formato-waerk
                amount_internal = monto2
              IMPORTING
                amount_external = lv_monto.


            MOVE lv_monto TO ti_formato-mwsbp.
            CLEAR: lv_monto, monto2.

          ENDIF.

        ELSE.

          MOVE ivbrp-netwr TO ti_formato-netwr1.

          IF ivbrk-waerk = 'COP'.
            ti_formato-netwr1 = ti_formato-netwr1 * 100.
          ENDIF.

          MOVE ivbrp-mwsbp TO ti_formato-mwsbp.
          MOVE ivbrk-waerk TO ti_formato-waerk.

        ENDIF.


*&-- Calcula precio unitario e importe final

        IF  p_waerk <> ivbrk-waerk.

          IF p_waerk = 'COP'.

            IF ti_formato-fkimg NE 0.

              READ TABLE ti_konv WITH KEY knumv = ivbrk-knumv
                                          kposn = ivbrp-posnr.
              IF sy-subrc = 0.
               ti_formato-preun = ( ( ti_konv-kbetr / ti_konv-kpein ) *
                                      ti_formato-kurrf ) .
              ENDIF.
            ENDIF.

          ELSE.

            IF ti_formato-fkimg NE 0.
              monto_usd = ivbrp-netwr / ti_formato-fkimg.
              monto_usd = monto_usd * 100.

              CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
                EXPORTING
                  date             = ti_formato-fkdat
                  foreign_amount   = monto_usd
                  foreign_currency = ivbrk-waerk
                  local_currency   = ti_formato-waerk
                IMPORTING
                  exchange_rate    = kurrf
                  local_amount     = monto2.
              IF sy-subrc <> 0.
*               MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
              ENDIF.

              kurrf = kurrf * 10000.
              kurrf = kurrf * -1.
              ti_formato-kurrf = kurrf.

              READ TABLE ti_konv WITH KEY knumv = ivbrk-knumv
                                          kposn = ivbrp-posnr.
              IF sy-subrc = 0.
                ti_formato-preun = ( ( ( ti_konv-kbetr * 100 ) /
                ti_konv-kpein ) / ti_formato-kurrf ).
              ENDIF.
            ENDIF.
          ENDIF.

        ELSE.

          IF p_waerk = 'USD'.

            READ TABLE ti_konv WITH KEY knumv = ivbrk-knumv
                                        kposn = ivbrp-posnr.
            IF sy-subrc = 0.
              ti_formato-preun = ti_konv-kbetr / ti_konv-kpein.
            ENDIF.

          ELSE.

            READ TABLE ti_konv WITH KEY knumv = ivbrk-knumv
                                        kposn = ivbrp-posnr.
            IF sy-subrc = 0.
              ti_formato-preun = ( ti_konv-kbetr * 100 ) /
                                  ti_konv-kpein. "
            ENDIF.

          ENDIF.
        ENDIF.


        IF p_waerk = 'USD'.
          ti_formato-impfi = ti_formato-mwsbp + ti_formato-netwr1.
        ELSE.
          ti_formato-impfi = ( ti_formato-mwsbp * 100 )
                             + ti_formato-netwr1.
        ENDIF.


        READ TABLE ti_konv2 WITH KEY knumv = ivbrk-knumv
                                     kposn = ivbrp-posnr.
        IF sy-subrc EQ 0.
          MOVE ti_konv2-kwert TO ti_formato-kwert.
          MOVE ti_konv2-kbetr TO ti_formato-kbetr.
        ENDIF.

        IF p_waerk <> ivbrk-waerk.
          IF p_waerk = 'COP'.

            kurrf = ( ivbrk-kurrf / 1000 ).
            lv_monto = ti_formato-kbetr * kurrf.

            MOVE lv_monto TO ti_formato-kbetr.
            CLEAR: lv_monto, monto2.

            kurrf = ( ivbrk-kurrf / 100 ).
            monto2 = ti_formato-kwert * kurrf.

            MOVE monto2 TO ti_formato-kwert.
            CLEAR: lv_monto, monto2.
          ELSE.
            lv_monto = ti_formato-kwert * 10.

            CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
              EXPORTING
                date             = ti_formato-fkdat
                foreign_amount   = lv_monto
                foreign_currency = ivbrk-waerk
                local_currency   = ti_formato-waerk
              IMPORTING
                local_amount     = monto2.
            IF sy-subrc <> 0.
*             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

            MOVE monto2 TO ti_formato-kwert.
            CLEAR: lv_monto, monto2.

            lv_monto = ti_formato-kbetr * 10.
            CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
              EXPORTING
                date             = ti_formato-fkdat
                foreign_amount   = lv_monto
                foreign_currency = ivbrk-waerk
                local_currency   = ti_formato-waerk
              IMPORTING
                local_amount     = monto2.
            IF sy-subrc <> 0.
*             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

            MOVE monto2 TO ti_formato-kbetr.
            CLEAR: lv_monto, monto2.
          ENDIF.
        ELSE.

        ENDIF.


*&-- Busca texto de Canal de Distrib.
        READ TABLE it_tvtwt INTO wa_tvtwt WITH KEY vtweg = ivbrk-vtweg.
        IF sy-subrc EQ 0.
          MOVE wa_tvtwt-vtext TO ti_formato-vtweg_t.
        ENDIF.

*&-- Busca textos de Grupos de clientes
        READ TABLE it_t151t INTO wa_t151t WITH KEY kdgrp = ivbrk-kdgrp.
        IF sy-subrc EQ 0.
          MOVE wa_t151t-ktext TO ti_formato-ktext.
        ENDIF.

*&-- Busca textos de Zonas de Ventas
        READ TABLE it_t171t INTO wa_t171t WITH KEY bzirk = ivbrk-bzirk.
        IF sy-subrc EQ 0.
          MOVE wa_t171t-bztxt TO ti_formato-bztxt.
        ENDIF.

*&-- Busca descripcion del Sector
        READ TABLE it_tspat INTO wa_tspat WITH KEY spart = ivbrp-spart.
        IF sy-subrc EQ 0.
          MOVE wa_tspat-vtext TO ti_formato-spart_t.
        ENDIF.

*&-- Busca nombre del cliente
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = p_kunnr.
        IF sy-subrc EQ 0.
          MOVE wa_kna1-name1 TO ti_formato-name1.
          MOVE wa_kna1-stcd1 TO ti_formato-stcd1.
          MOVE wa_kna1-ort01 TO ti_formato-ort01.
          MOVE wa_kna1-ort01 TO ti_formato-ort01_2.
        ENDIF.

*        READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbeln = ivbrk-vbeln
*                                                 vbtyp_v = 'C'.
*        IF sy-subrc EQ 0.
*          READ TABLE it_vbpa INTO wa_vbpa
*            WITH KEY vbeln = wa_vbfa-vbelv.
*          IF sy-subrc EQ 0.
*            READ TABLE it_kna1 INTO wa_kna1
*              WITH KEY kunnr = wa_vbpa-kunnr .
*            IF sy-subrc EQ 0.
*              ti_formato-ort01 = wa_kna1-ort01.
*              ti_formato-ort01_2 = wa_kna1-ort01."L/N JGP 210422
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*        READ TABLE it_vbfa INTO wa_vbfa WITH KEY
*                                        vbeln = ti_formato-vbeln
*                                        vbtyp_v = 'J' .
*        IF sy-subrc EQ 0.
*          READ TABLE it_vbpa INTO wa_vbpa
*            WITH KEY vbeln = wa_vbfa-vbelv .
*          IF sy-subrc EQ 0.
*            READ TABLE it_kna1 INTO wa_kna1
*              WITH KEY kunnr = wa_vbpa-kunnr .
*            IF sy-subrc EQ 0.
*              READ TABLE it_t005u INTO wa_t005u
*                WITH KEY bland = wa_kna1-regio
*                         land1 = wa_kna1-land1
*                         spras = 'S' .
*              IF sy-subrc EQ 0.
*                ti_formato-bezei_r = wa_t005u-bezei.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*         ENDIF.


***BOM JGP T_11772 10.05.22 - DEPARTAMENTO DESTINATARIO
        CLEAR: wa_kna1_2, wa_t005u_2.
        READ TABLE ti_vbpa2 INTO wa_vbpa2 WITH KEY vbeln = ivbrp-vgbel.
        IF sy-subrc EQ 0.
          READ TABLE gt_kna1 INTO wa_kna1_2 WITH KEY
                                            kunnr = wa_vbpa2-kunnr.
          IF sy-subrc EQ 0.

            MOVE wa_kna1_2-ort01  TO ti_formato-ort01_2.

            READ TABLE gt_t005u INTO wa_t005u_2 WITH KEY
                                      bland = wa_kna1_2-regio
                                      land1 = wa_kna1_2-land1.
            IF sy-subrc EQ 0.
              MOVE wa_t005u_2-bezei TO ti_formato-bezei_2.
            ENDIF.
          ENDIF.
        ENDIF.
***}EOM JGP T_11772 10.05.22

***BOM JGP T_11772 12.05.22 - DEPARTAMENTO DESPACHO
        CLEAR: wa_kna1_2, wa_t005u_2.
        READ TABLE ti_vbpa INTO wa_vbpa WITH KEY vbeln = ivbrp-vbeln
                                                 parvw = 'AG'.
        IF sy-subrc EQ 0.
          READ TABLE gt_kna1 INTO wa_kna1_2 WITH KEY
                                            kunnr = wa_vbpa-kunnr.
          IF sy-subrc EQ 0.
*           Ciudad del Despacho
            MOVE wa_kna1_2-ort01  TO ti_formato-ort01.

            READ TABLE gt_t005u INTO wa_t005u_2 WITH KEY
            bland = wa_kna1_2-regio
            land1 = wa_kna1_2-land1.
            IF sy-subrc EQ 0.
              MOVE wa_t005u_2-bezei TO ti_formato-bezei_r.
            ENDIF.
          ENDIF.
        ENDIF.
***}EOM JGP T_11772 12.05.22





**** EOB FAA 12/11/2014
***************************************************************
*       DOC contable
*      JARV T_12008  26.05.2022
*        READ TABLE it_bkpf INTO wa_bkpf WITH KEY awkey = ivbrk-vbeln.
*        IF sy-subrc EQ 0.
*          MOVE wa_bkpf-belnr TO ti_formato-belnr.
*        ENDIF.


*      JARV T_12008  26.05.2022


        MOVE p_kunnr TO ti_formato-kunnr.

        IF ivbrk-fksto NE space.
          READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbelv = ivbrk-vbeln
                                                   posnv = ivbrp-posnr.
          IF sy-subrc EQ 0.
            MOVE wa_vbfa-vbeln TO ti_formato-stblg.
          ENDIF.
        ENDIF.


*&-- Busca monto del anticipo para FACTURAS DE ANTICIPOS

******* Validacion para FKART = FAZ no existe en COMOLSA.      *******
******* Se comenta las busquedas en la BSEG y se deja el ELSE. *******
        IF sy-subrc EQ 0 AND ivbrk-fkart EQ 'FAZ'.
*
*          SELECT SINGLE * FROM bseg
*          WHERE bukrs EQ ivbrk-bukrs
*            AND belnr EQ bkpf-belnr
*            AND gjahr EQ bkpf-gjahr.
*          IF sy-subrc EQ 0 AND bseg-augbl NE space.
*            ti_formato-sttco = 'CONT'.
*            belnr = bseg-augbl.
*
*            SELECT SINGLE * FROM bseg
*            WHERE bukrs EQ ivbrk-bukrs
*              AND belnr EQ belnr
*              AND gjahr EQ bkpf-gjahr.
*
*            MOVE bseg-wrbtr TO ti_formato-wrbtr.
*          ELSE.
*            ti_formato-sttco = 'NC'.
*          ENDIF.
        ELSE.
          ti_formato-sttco = 'NC'.
        ENDIF.

*&-- Busca formula para calculo de vencimiento
        READ TABLE it_tvzbt INTO wa_tvzbt WITH KEY zterm = ivbrk-zterm.
        IF sy-subrc EQ 0.
          MOVE wa_tvzbt-vtext TO ti_formato-vtext.
        ENDIF.

*&-- Calculo fecha de vencimiento segun cantidad de dias
        READ TABLE it_t052 INTO wa_t052 WITH KEY zterm = ivbrk-zterm.
        IF sy-subrc EQ 0.
          IF wa_t052-ztag1 NE space.

            IF lv_zbd2t IS NOT INITIAL.
              dias = lv_zbd2t.
              ti_formato-fvenc = ivbrk-fkdat + dias.

            ELSEIF lv_zbd1t IS NOT INITIAL.
              dias = lv_zbd1t.
              ti_formato-fvenc = ivbrk-fkdat + dias.
            ENDIF.

*&-- Calculo fecha de vencimiento segun dia natural
          ELSEIF wa_t052-zfael NE space.
            dias = ivbrk-fkdat+6(2) - wa_t052-zfael.
            ti_formato-fvenc = ivbrk-fkdat + dias.

*&-- Calculo fecha de vencimiento segun dia fijo
          ELSEIF wa_t052-zstg1 NE space.
            IF ivbrk-fkdat+6(2) GT wa_t052-zstg1.
              dias = ivbrk-fkdat+6(2) - wa_t052-zstg2.
            ELSE.
              dias = ivbrk-fkdat+6(2) - wa_t052-zstg1.
            ENDIF.

            ti_formato-fvenc = ivbrk-fkdat + dias.
          ENDIF.

*&-- Calculo fecha de vencimiento para pago inmediato
        ELSE.
          ti_formato-fvenc = ivbrk-fkdat.
        ENDIF.

*JOAG:BEGIN AGREGADO 21/08/2014
        IF ti_formato-fvenc IS NOT INITIAL.

          CALL FUNCTION 'ISP_GET_MONTH_NAME'
            EXPORTING
              date     = ti_formato-fvenc
              language = 'S'
            IMPORTING
              longtext = gv_string.
          IF sy-subrc <> 0.
*           MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.

          ti_formato-fven_txt = gv_string.

        ENDIF.
**JOAG:END AGREGADO 21/08/2014

*&-- Calcula dias de morosidad
        ti_formato-moros = sy-datum - ti_formato-fvenc.


        IF ivbrp-meins EQ 'SER'.
          CLEAR: ti_formato-fklmg.
        ENDIF.

        MOVE  ti_konv-kbetr TO ti_formato-kbetr1.


*&-- Cambia signo a precios para anulaciones de factura y abonos
        IF ti_formato-vbtyp EQ 'N' OR
           ti_formato-vbtyp EQ 'O' OR
           ti_formato-vbtyp EQ '6'.

          ti_formato-preun  = ti_formato-preun  * -1.
          ti_formato-netwr1 = ti_formato-netwr1 * -1.
          ti_formato-mwsbp  = ti_formato-mwsbp  * -1.
          ti_formato-impfi  = ti_formato-impfi  * -1.
          ti_formato-fkimg  = ti_formato-fkimg  * -1.
          ti_formato-fklmg  = ti_formato-fklmg  * -1.
          ti_formato-cant   = ti_formato-cant   * -1.
          ti_formato-kwert  = ti_formato-kwert  * -1.
          ti_formato-kbetr  = ti_formato-kbetr  * -1.
        ENDIF.

***njgm-str***17.06.2013
***Agregamos dos columnas solicitadas por el usuario para mostrar la
***moneda local en los casos de exportaciones.
        IF p_waerk <> ivbrk-waerk.

          IF ivbrk-waerk EQ 'USD'.
            kurrf = ( ivbrk-kurrf / 1000 ).
            ti_formato-netwr_cop = ti_formato-netwr1 / 100. "
            ti_formato-waerk_cop = 'COP'.
          ELSEIF ivbrk-waerk EQ 'COP'.
            ti_formato-netwr_cop = ti_formato-netwr1 * ti_formato-kurrf.
            ti_formato-waerk_cop = 'COP'.
          ENDIF.

        ELSE.

          IF ivbrk-waerk EQ 'USD'.
            kurrf = ( ivbrk-kurrf / 1000 ).
            ti_formato-netwr_cop = ti_formato-netwr1 * ti_formato-kurrf.
            ti_formato-waerk_cop = 'COP'.
          ELSEIF ivbrk-waerk EQ 'COP'.
            ti_formato-netwr_cop = ti_formato-netwr1 / 100  .
            ti_formato-waerk_cop = 'COP'.
            ti_formato-kbetr     = ti_formato-kbetr * 100.
          ENDIF.

        ENDIF.


****    En los casos de las clase de facturas sea ZF24 / ZSD14
****    el importe neto debe ser cero
        IF ivbrk-fkart EQ 'ZF24' OR ivbrk-fkart EQ 'ZS14'.
          CLEAR: ti_formato-netwr1,
                 ti_formato-impfi,
                 ti_formato-netwr_cop.
        ENDIF.

****    Notas de credito.
        IF ivbrk-fkart EQ 'ZND1' OR
           ivbrk-fkart EQ 'ZNC1' OR
           ivbrk-fkart EQ 'ZNC3'.

          ti_formato-fklmg = 0.
          ti_formato-cant  = 0.
        ENDIF.


*&-- Agrega registro a la tabla interna
        APPEND ti_formato.

***/// Total de Ingresos Por Moneda
        MOVE-CORRESPONDING ti_formato TO itab.
        COLLECT itab.

*&-------- 18.11.2010 JADM
        READ TABLE ti_lipsf INTO wa_lipsf WITH KEY vbeln = ivbrp-vgbel.
        IF sy-subrc EQ 0.
          wa_formato-lfimg = wa_lipsf-lfimg.
        ENDIF.


***///  NL JGP 19032021
***///  Si es Resumido / R. Totalizado Cliente / Ampliado
        IF resume EQ 'X' OR tot_cli EQ 'X' OR amplia EQ 'X'.

          MOVE: ti_formato-kunnr  TO wa_formato-kunnr,
                ti_formato-name1  TO wa_formato-name1,
                ti_formato-fkart  TO wa_formato-fkart,
                ti_formato-vbeln  TO wa_formato-vbeln,
                ti_formato-fkdat  TO wa_formato-fkdat,
                ti_formato-bztxt  TO wa_formato-bztxt,
                ti_formato-fkimg  TO wa_formato-fkimg,
                ti_formato-vrkme  TO wa_formato-vrkme,
                ti_formato-preun  TO wa_formato-preun,
                ti_formato-netwr1 TO wa_formato-netwr1,
                ti_formato-mwsbp  TO wa_formato-mwsbp,
                ti_formato-impfi  TO wa_formato-impfi,
                ti_formato-waerk  TO wa_formato-waerk,
                ti_formato-stblg  TO wa_formato-stblg,
                ti_formato-fklmg  TO wa_formato-fklmg,
                ti_formato-cant   TO wa_formato-cant,
                ti_formato-ort01  TO wa_formato-ort01,
                ti_formato-belnr  TO wa_formato-belnr,
                ti_formato-hkont  TO wa_formato-hkont,
*
*               JARV T_10957 10.11.2021
                ti_formato-werks  TO wa_formato-werks,
*               JARV T_12008 26.05.2022
*
***njgm-str***17.06.2013
                ti_formato-netwr_cop TO wa_formato-netwr_cop,
                ti_formato-waerk_cop TO wa_formato-waerk_cop,
***njgm-end***17.06.2013

                ti_formato-btgew TO wa_formato-btgew,
                ti_formato-meins  TO wa_formato-meins,
*               "L/N 200422 T_11772
                ti_formato-bstkd   TO wa_formato-bstkd,
                ti_formato-ort01_2 TO wa_formato-ort01_2,
                ti_formato-aubel   TO wa_formato-aubel,
                ti_formato-bezei_2 TO wa_formato-bezei_2,
                ti_formato-bezei_r TO wa_formato-bezei_r.
*               "L/N 200422 T_11772


          CLEAR: wa_likp-btgew, wa_likp-gewei.
          READ TABLE ti_entregas INTO wa_entregas WITH KEY
                                                vbeln = ivbrp-vbeln.
          IF sy-subrc EQ 0.
            READ TABLE it_likp INTO wa_likp WITH KEY
                                          vbeln = wa_entregas-vgbel.
            IF sy-subrc EQ 0.
              MOVE wa_likp-btgew TO wa_formato-btgew.
              MOVE wa_likp-gewei TO wa_formato-gewei.
            ENDIF.
          ENDIF.

          READ TABLE ti_formato2 WITH KEY vbeln = ti_formato-vbeln.
          IF sy-subrc EQ 0.
            CLEAR: wa_formato-btgew.
            IF ti_formato2-preun IS NOT INITIAL.
              CLEAR: wa_formato-preun.
            ENDIF.
          ENDIF.

          IF ti_formato-vbtyp EQ 'N' OR ti_formato-vbtyp EQ 'O'.
            wa_formato-btgew = wa_formato-btgew * -1.
          ENDIF.

          IF wa_formato-btgew NE 0.
            wa_formato-preur = wa_formato-netwr1 / wa_formato-btgew.
          ELSE.
            wa_formato-preur = 0.
          ENDIF.

          wa_formato-stcd1 = ti_formato-stcd1.

          "Nuevas columnas para opción AMPLIACIÓN
          "1.Destinario
         READ TABLE ti_vbpa2 INTO wa_vbpa2 WITH KEY vbeln = ivbrp-vgbel.
          IF sy-subrc EQ 0.
            "L/N JGP 200422 T_11772
            MOVE wa_vbpa2-kunnr TO wa_formato-kunnr_2.
          ENDIF.

          READ TABLE gt_vbpa INTO wa_vbpa1 WITH KEY vbeln = ivbrp-vgbel
                                                    parvw = 'SP'.
          IF sy-subrc EQ 0.
            "5.Código Empresa Transportadora
            MOVE wa_vbpa1-lifnr TO wa_formato-lifnr.

            READ TABLE gt_lfa1 INTO wa_lfa1 WITH KEY
                                            lifnr = wa_vbpa1-lifnr.
            IF sy-subrc EQ 0.
              "6.Nombre Empresa transportadora
              MOVE wa_lfa1-name1 TO wa_formato-name1a.
            ENDIF.
          ENDIF.

          READ TABLE gt_vbpa INTO wa_vbpa1 WITH KEY vbeln = ivbrp-vgbel
                                                    parvw = 'ZC'.
          IF sy-subrc EQ 0.

            READ TABLE gt_lfa1 INTO wa_lfa1 WITH KEY
                                            lifnr = wa_vbpa1-lifnr.
            IF sy-subrc EQ 0.
              "7. Placa del Vehículo
              MOVE wa_lfa1-sortl TO wa_formato-sortl.
              "8. Nombre del Conductor
              MOVE wa_lfa1-name1 TO wa_formato-name1b.
              "9. Doc. Identidad del Conductor
              MOVE wa_lfa1-stcd1 TO wa_formato-stcd1_2.
            ENDIF.

          ENDIF.

          COLLECT wa_formato INTO ti_formato2.

          CLEAR wa_formato.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR ti_formato.
  ENDLOOP.

ENDFORM.                    " cargar_datos

*&---------------------------------------------------------------------*
*&      Form  CATALOGO
*&---------------------------------------------------------------------*
FORM catalogo .
  alv_field-fieldname   = 'KBETR1'.
  alv_field-seltext_l   = 'IMPORTE'.
  alv_field-seltext_m   = 'IMPORTE'.
  alv_field-seltext_s   = 'IMPORTE'.
  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '3'.
  alv_field-col_pos     = '4'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'KUNNR'.
  alv_field-seltext_l   = 'Cliente'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-key         = 'X'.
  alv_field-just        = 'C'.
  alv_field-col_pos     = '2'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'NAME1'.
  alv_field-seltext_l   = 'Razón Social'.
  alv_field-seltext_m   = 'Razón Social'.
  alv_field-seltext_s   = 'Razón Social'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-key         = 'X'.
  alv_field-just        = 'L'.
*  alv_field-row_pos     = '2'.
  alv_field-col_pos     = '3'.
  APPEND alv_field. CLEAR alv_field.


  alv_field-fieldname   = 'AUDAT'.
  alv_field-seltext_l   = 'Mes'.
  alv_field-seltext_m   = 'Mes'.
  alv_field-seltext_s   = 'Mes'.
  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '3'.
  alv_field-col_pos     = '4'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'STCD1'.
  alv_field-seltext_s   = 'Identificación Fiscal'.
  alv_field-seltext_m   = 'Identificación Fiscal'.
  alv_field-seltext_l   = 'Identificación Fiscal'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-key         = 'X'.
  alv_field-just        = 'L'.
*  alv_field-row_pos     = '4'.
  alv_field-col_pos     = '5'.
  APPEND alv_field. CLEAR alv_field.

***NJGM-STR***29-02-2012
*  alv_field-fieldname   = 'XBLNR'.
*  alv_field-seltext_l   = 'Referencia'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-key         = 'X'.
*  alv_field-just        = 'L'.
*  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'BKTXT'.
  alv_field-seltext_l   = 'Número de Control'.
  alv_field-seltext_m   = 'N° de Control'.
  alv_field-seltext_s   = 'N° Control'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-key         = 'X'.
  alv_field-just        = 'L'.
  alv_field-row_pos     = '5'.
  APPEND alv_field. CLEAR alv_field.
***NJGM-END***29-02-2012

  alv_field-fieldname   = 'VBELN'.
  alv_field-seltext_l   = 'Factura'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-hotspot     = 'X'.
  alv_field-key         = 'X'.
  alv_field-just        = 'C'.
*  alv_field-row_pos     = '6'.
  alv_field-col_pos     = '9'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'BELNR'.
  alv_field-seltext_s   = 'Doc.Contable'.
  alv_field-seltext_m   = 'Doc. Contable'.
  alv_field-seltext_l   = 'Documento Contable'.
  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-col_pos     = '10'.
  APPEND alv_field. CLEAR alv_field.

***  BOM JGP 180422 T_11772
  alv_field-fieldname   = 'AUBEL'.
  alv_field-seltext_l   = 'Número de Pedido'.
  alv_field-seltext_m   = 'Nro. Pedido'.
  alv_field-seltext_s   = 'Nro. Pedido'.
  alv_field-reptext_ddic  = 'Nro. Pedido'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'C'.
  alv_field-col_pos     = '9'.
  APPEND alv_field. CLEAR alv_field.
***  EOM JGP 180422 T_11772

  alv_field-fieldname   = 'WERKS'.            "JARV T_10957 13.10.2021
  alv_field-seltext_l   = 'Centro'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'C'.
  alv_field-col_pos     = '9'.
  APPEND alv_field. CLEAR alv_field.

***njgm-str***22-02-2012 hkont
  alv_field-fieldname   = 'HKONT'.
  alv_field-seltext_l   = 'Cuenta de Ingresos'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'C'.
  alv_field-row_pos     = '7'.
  APPEND alv_field. CLEAR alv_field.
***njgm-end***22-02-2012

  alv_field-fieldname   = 'POSNR'.
  alv_field-seltext_l   = 'Pos.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'C'.
  alv_field-row_pos     = '8'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'FKART'.    "Clase factura
  alv_field-seltext_l   = 'Doc.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'C'.
*  alv_field-row_pos     = '9'.
  alv_field-col_pos     = '6'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'BSTKD'.
***  BOM JGP 180422 T_11772
***  alv_field-seltext_l   = 'Número de pedido del cliente'.
***  alv_field-seltext_m   = 'Número de pedido'.
***  alv_field-seltext_s   = '# Pedido'.
  alv_field-seltext_l   = 'Orden de Compra'.
  alv_field-seltext_m   = 'Ord. Compra '.
  alv_field-seltext_s   = 'Ord. Compra '.
  alv_field-reptext_ddic  = 'Ord. Compra '.
***  EOM JGP 180422 T_11772
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-col_pos     = '7'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'CHARG'.
  alv_field-seltext_l   = 'Número de lote'.
  alv_field-seltext_m   = 'Número lote'.
  alv_field-seltext_s   = '# Lote'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-col_pos     = '8'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'ABGRU'.
  alv_field-seltext_l   = 'Motivo de rechazo de ofertas'.
  alv_field-seltext_m   = 'Motivo de anulación'.
  alv_field-seltext_s   = 'Anulación'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-col_pos     = '9'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'ERNAM'.
  alv_field-seltext_l   = 'Solicitante de anulación'.
  alv_field-seltext_m   = 'Solicitante anulación'.
  alv_field-seltext_s   = 'Sol. anu.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-col_pos     = '10'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'MATKL'.
  alv_field-seltext_l   = 'Grupo de artículos'.
  alv_field-seltext_m   = 'Grupo de artículos'.
  alv_field-seltext_s   = 'Gr art.'.
  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '11'.
  alv_field-col_pos     = '13'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'ATWRT_R'.
  alv_field-seltext_l   = 'Grupo'.
  alv_field-seltext_m   = 'Grupo'.
  alv_field-seltext_s   = 'Grupo'.
  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '11'.
  alv_field-col_pos     = '14'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'ATWRT_C'.
  alv_field-seltext_l   = 'Color'.
  alv_field-seltext_m   = 'Color'.
  alv_field-seltext_s   = 'Color'.
  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '11'.
  alv_field-col_pos     = '15'.
  APPEND alv_field. CLEAR alv_field.

*no aparece en el alv, por eso le comento el alv_field-row_pos
  alv_field-fieldname   = 'KTEXT'.
  alv_field-seltext_m   = 'G. Client'.
  alv_field-seltext_s   = 'G. Cliente'.
  alv_field-seltext_l   = 'Grp.Cliente'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'C'.
*  alv_field-row_pos     = '11'.
  APPEND alv_field. CLEAR alv_field.

*/////BM   frantillo

  alv_field-fieldname   = 'BEZEI'.
  alv_field-seltext_l   = 'Grp.Vend.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'L'.
  alv_field-row_pos     = '12'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname      = 'ENAME'.
  alv_field-seltext_l      = 'Vendedor'.
  alv_field-tabname        = 'TI_FORMATO'.
  alv_field-ddictxt        = 'L'.
  alv_field-row_pos        = '13'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname      = 'PERNR'.
  alv_field-seltext_l      = 'Num. Vendedor'.
  alv_field-tabname        = 'TI_FORMATO'.
  alv_field-ddictxt        = 'L'.
  alv_field-no_out         = 'X'.
  alv_field-row_pos     = '14'.
  APPEND alv_field. CLEAR alv_field.

*/////EM   frantillo
  alv_field-fieldname   = 'BZTXT'.
  alv_field-seltext_l   = 'Zona Venta'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '15'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'SPART_T'.
  alv_field-seltext_l   = 'Sector'.
  alv_field-seltext_m    = 'Sector'.
  alv_field-seltext_s    = 'Sector'.
  alv_field-reptext_ddic = 'Sector'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '16'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'VTWEG_T'.
  alv_field-seltext_l   = 'Canal'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '17'.
  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'ZTERM'.
*  alv_field-seltext_l   = 'Cd.Pago'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'VTEXT'.
  alv_field-seltext_l   = 'Cond.Pago'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '18'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'FKDAT'.
  alv_field-seltext_l   = 'Fec.Factura'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '19'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'FVENC'.
  alv_field-seltext_l   = 'Fec.Venc.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-ddictxt     = 'L'.
  alv_field-row_pos     = '20'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'FVEN_TXT'.
  alv_field-seltext_l   = 'Vence mes'.
  alv_field-seltext_m   = 'Vence mes'.
  alv_field-seltext_s   = 'Ven. mes'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-ddictxt     = 'L'.
*  alv_field-row_pos     = '20'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'MOROS'.
  alv_field-seltext_l   = 'Mor.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '21'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'MATNR'.
  alv_field-seltext_l   = 'Material'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-just        = 'R'.
  alv_field-row_pos     = '22'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'MAKTX'.
  alv_field-seltext_l   = 'Descripción'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '23'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'VRKME'.
  alv_field-seltext_l   = 'Unid.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '24'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'PREUN'.
  alv_field-seltext_l   = 'Precio Unit.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '25'.
  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'NETWR'.
*  alv_field-seltext_l   = 'Monto Bruto'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '26'.
**  ALV_FIELD-NO_OUT        = 'X'.
*  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'NETWR1'.
  alv_field-seltext_l   = 'Monto Neto'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '26'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'MWSBP'.
  alv_field-seltext_l   = 'I.V.A.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '27'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'IMPFI'.
  alv_field-seltext_l   = 'Importe Final'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '28'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname  = 'KBETR'.
  alv_field-seltext_l   = 'Costo Uni.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '29'.
  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'ORT01'.
*  alv_field-seltext_l   = 'Población'.
*  alv_field-seltext_m   = 'Población'.
*  alv_field-seltext_s   = 'Población'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-key         = 'X'.
*  alv_field-just        = 'L'.
*  alv_field-col_pos     = '31'.
*  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'ORT01'.
  alv_field-seltext_l   = 'Ciudad Desp.'.
  alv_field-seltext_m   = 'Ciudad Despacho'.
  alv_field-seltext_s   = 'Ciudad de Despacho'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-key         = 'X'.
  alv_field-just        = 'L'.
  alv_field-col_pos     = '31'.
  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'BEZEI_R'.
*  alv_field-seltext_l   = 'Región'.
*  alv_field-seltext_m   = 'Región'.
*  alv_field-seltext_s   = 'Región'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-key         = 'X'.
*  alv_field-just        = 'L'.
*  alv_field-col_pos     = '32'.
*  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'BEZEI_R'.
  "BOM JGP 12.05.22 T_11772
*  alv_field-seltext_l   = 'Dpto. Despacho'.
*  alv_field-seltext_m   = 'Dpto. de Despacho'.
*  alv_field-seltext_s   = 'Departamento de Despacho'.
*  alv_field-reptext_ddic = 'Dpto. Despac.'.

  alv_field-seltext_l   = 'Departamento'.
  alv_field-seltext_m   = 'Departamento'.
  alv_field-seltext_s   = 'Departamento'.
  alv_field-reptext_ddic = 'Departamento'.
  "EOM JGP 12.05.22 T_11772
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-key         = 'X'.
  alv_field-just        = 'L'.
  alv_field-col_pos     = '32'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'KWERT'.
  alv_field-seltext_l   = 'Costo Tot.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '30'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'WRBTR'.
  alv_field-seltext_l   = 'Anticipo'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '31'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'WAERK'.
  alv_field-seltext_l   = 'Mon.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '32'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'STTCO'.
  alv_field-seltext_l   = 'SC'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '33'.
  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'STTAN'.
*  alv_field-seltext_l   = 'SA'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'STBLG'.
  alv_field-seltext_l   = 'Anulado por'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '34'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'LFIMG'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-col_pos     = 44.
  alv_field-seltext_l   = 'Cant. Conversion'.
  alv_field-seltext_m   = 'Cant. Conversion'.
  alv_field-seltext_s   = 'Cant.Conv'.
  alv_field-ddictxt     = 'Cant.Conv'.
  alv_field-reptext_ddic = 'Cant.Conv'.
  alv_field-currency       = 'X'.
  alv_field-row_pos     = '35'.
  APPEND alv_field. CLEAR alv_field. " 15.11.2010 JADM

*  alv_field-fieldname   = 'FKLMG'.
*  alv_field-seltext_l   = 'Cant. Kg'.
*  alv_field-ddictxt     = 'L'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '36'.
*  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'FKLMG'.
  alv_field-seltext_l   = 'Convers.Cant.Fact'.
  alv_field-ddictxt     = 'L'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '36'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'FKIMG'.
  alv_field-seltext_l   = 'Cant. Facturada'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '37'.
  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'BTGEW'.
*  alv_field-seltext_l   = 'Cant.Conversion'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  APPEND alv_field. CLEAR alv_field. " 15.11.2010 JADM


  alv_field-fieldname   = 'MEINS'.
  alv_field-seltext_l   = 'Unid. Conv.'.
  alv_field-ddictxt     = 'L'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '38'.
  APPEND alv_field. CLEAR alv_field.

***NJGM-STR***17.06.2013

  alv_field-fieldname   = 'NETWR_COP'.
  alv_field-seltext_l   = 'Monto COP'.
*  alv_field-seltext_m   = 'Monto b.COP'.
  alv_field-seltext_m   = 'Monto Neto COP'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '40'.
  alv_field-cfieldname  = 'WAERK_COP'.
  alv_field-ctabname    = 'TI_FORMATO'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'WAERK_COP'.
  alv_field-seltext_l   = 'Mon. Local'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '41'.
  APPEND alv_field. CLEAR alv_field.

***NJGM-END***17.06.2013
  alv_field-fieldname   = 'KURRF'.
  alv_field-seltext_l   = 'Tasa Cambio'.
*  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '42'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'KUNNR_2'.
  alv_field-seltext_l   = 'Destinatario'.
  alv_field-seltext_m   = 'Destinatario'.
  alv_field-seltext_s   = 'Destinatario'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '43'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'STRAS'.
  alv_field-seltext_l   = 'Dirección. Destinatario'.
  alv_field-seltext_m   = 'Dirección Dest.'.
  alv_field-seltext_s   = 'Dir. Dest.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '44'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'BEZEI_2'.
  alv_field-seltext_l   = 'Departamento Destinatario'.
  alv_field-seltext_m   = 'Departamento Dest.'.
  alv_field-seltext_s   = 'Dept. Dest.'.
  alv_field-reptext_ddic = 'Dpto. Desti.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '45'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'ORT01_2'.
  alv_field-seltext_l   = 'Ciudad Destinatario'.
  alv_field-seltext_m   = 'Ciudad Dest.'.
  alv_field-seltext_s   = 'Ciudad Dest.'.
  alv_field-reptext_ddic = 'Ciudad Destinatario'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '46'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'LIFNR'.
  alv_field-seltext_l   = 'Cod. Emp. Trans.'.
  alv_field-seltext_m   = 'Cod. Emp. Trans.'.
  alv_field-seltext_s   = 'Cod. Emp.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '47'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'NAME1A'.
  alv_field-seltext_l   = 'Nom. Emp. Trans.'.
  alv_field-seltext_m   = 'Nom. Emp. Trans.'.
  alv_field-seltext_s   = 'Nom. Emp.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '48'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'SORTL'.
  alv_field-seltext_l   = 'Placa del Vehículo.'.
  alv_field-seltext_m   = 'Placa Vehículo'.
  alv_field-seltext_s   = 'Placa Veh.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '49'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname   = 'NAME1B'.
  alv_field-seltext_l   = 'Nombre del Conductor'.
  alv_field-seltext_m   = 'Nom. Conductor'.
  alv_field-seltext_s   = 'Nom. Cond.'.
  alv_field-tabname     = 'TI_FORMATO'.
  alv_field-row_pos     = '50'.
  APPEND alv_field. CLEAR alv_field.

  alv_field-fieldname    = 'STCD1_2'.
  alv_field-seltext_l    = 'Doc. Id. Conductor'.
  alv_field-seltext_m    = 'Doc. Id. Conductor'.
  alv_field-seltext_s    = 'Doc. Id. Conduc.'.
  alv_field-reptext_ddic = 'Id. Conductor'.
  alv_field-tabname      = 'TI_FORMATO'.
  alv_field-row_pos      = '51'.
  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'PREUN_USD'.
*  alv_field-seltext_l   = 'Precio Unit. USD'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  alv_field-row_pos     = '43'.
*  alv_field-no_out      = 'X'.
**  alv_field-cfieldname  = 'KURRF'.
**  alv_field-ctabname    = 'TI_FORMATO'.
*  APPEND alv_field. CLEAR alv_field.

*  alv_field-fieldname   = 'PRCTR'.
*  alv_field-seltext_s   = 'Ce.Be'.
*  alv_field-seltext_m   = 'Ce.Be'.
*  alv_field-seltext_l   = 'Centro Beneficio'.
*  alv_field-tabname     = 'TI_FORMATO'.
*  APPEND alv_field. CLEAR alv_field.

*  IF resume EQ ' ' AND                            "NLC011007.sn
*    tot_cli EQ ' '.                               "NLC011007.en
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = repid
      i_internal_tabname = 'TI_FORMATO'
      i_inclname         = repid
    CHANGING
      ct_fieldcat        = alv_field[].
  IF sy-subrc <> 0.
    WRITE: 'SY-SUBRC: ', sy-subrc, 'REUSE_ALV_FIELDCATALOG_MERGE'.
  ENDIF.
*  ENDIF.                                            "NLC011007.n
*
*  READ TABLE alv_field WITH KEY fieldname = 'NETWR'.
*  alv_field-no_out = 'X'.
*  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'KDGRP'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'KTEXT'.
  alv_field-seltext_m    = 'G. Client'.
  alv_field-seltext_s    = 'G. Cliente'.
  alv_field-seltext_l    = 'Grp.Cliente'.
  alv_field-reptext_ddic = 'Grp.Cliente'.
  alv_field-tabname      = 'TI_FORMATO'.
  alv_field-no_out       = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'SPART'.
  alv_field-seltext_m    = 'C.Sector'.
  alv_field-seltext_s    = 'C.Sector'.
  alv_field-seltext_l    = 'C.Sector'.
  alv_field-reptext_ddic = 'C.Sector'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'VTWEG'.
  alv_field-seltext_m    = 'Canal'.
  alv_field-seltext_s    = 'Canal'.
  alv_field-seltext_l    = 'Canal'.
  alv_field-reptext_ddic = 'Canal'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'BZIRK'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'VBTYP'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'ZTERM'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'PRCTR'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'VKGRP'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'PREUR'.
*    alv_field-no_out = 'X'.
  alv_field-seltext_l   = 'Precio Unitario Real'.
  alv_field-seltext_m   = 'Precio Uni. Real'.
  alv_field-seltext_s   = 'Prec. Un. Real'.
  alv_field-no_out      = 'X'.                     "NLC210807.o
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'KWERT'.
*    alv_field-no_out = 'X'.
  alv_field-seltext_l   = 'Costo Tot'.
  alv_field-seltext_m   = 'Costo Tot'.
  alv_field-seltext_s   = 'Cost Total'.
*  alv_field-no_out      = 'X'.                    "NLC210807.o

  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'KBETR'.
*    alv_field-no_out = 'X'.
  alv_field-seltext_l   = 'Costo Uni'.
  alv_field-seltext_m   = 'Costo Unitario'.
  alv_field-seltext_s   = 'Cost Unitario'.
  alv_field-reptext_ddic =  'Costo Uni'.

*  alv_field-no_out      = 'X'.                    "NLC210807.o

  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'FKIMG'.
  alv_field-seltext_l   = 'Cant. Facturada'.
  alv_field-seltext_m   = 'Cant. Facturada'.
  alv_field-seltext_s   = 'Cant.Fact'.
  MODIFY alv_field INDEX sy-tabix.
*  READ TABLE alv_field WITH KEY fieldname = 'BTGEW'.
*  alv_field-seltext_l   = 'Cant. Conversion'.
*  alv_field-seltext_m   = 'Cant. Conversion'.
*  alv_field-seltext_s   = 'Cant.Conv'.
*  alv_field-ddictxt     = 'Cant.Conv'.
*  alv_field-reptext_ddic = 'Cant.Conv'.
*  alv_field-currency       = 'X'.
**  alv_field-no_out      = 'X'.
*  MODIFY alv_field INDEX sy-tabix.

*READ TABLE alv_field WITH KEY fieldname = 'FKIMG'.
*  alv_field-seltext_l   = 'Cant. Despachada'.
*  alv_field-seltext_m   = 'Cant. Despachada'.
*  alv_field-seltext_s   = 'Cant.Desp'.
*  alv_field-ddictxt     = 'Cant.Desp'.
*  alv_field-reptext_ddic = 'Cant.Desp'.
*  alv_field-currency       = 'X'.
**  alv_field-no_out      = 'X'.
*  MODIFY alv_field INDEX sy-tabix.
*READ TABLE alv_field WITH KEY fieldname = 'LFIMG'.
*  alv_field-seltext_l   = 'Cant. Conversion'.
*  alv_field-seltext_m   = 'Cant. Conversion'.
*  alv_field-seltext_s   = 'Cant.Conv'.
*  alv_field-ddictxt     = 'Cant.Conv'.
*  alv_field-reptext_ddic = 'Cant.Conv'.
*  alv_field-currency       = 'X'.
**  alv_field-no_out      = 'X'.
*  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'GEWEI'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'VGBEL'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.

  READ TABLE alv_field WITH KEY fieldname = 'CANT'.
*    alv_field-no_out = 'X'.
*  alv_field-ddictxt     = 'Cant Real'.
*  alv_field-seltext_l   = 'Cantidad Real Entrega'.
*  alv_field-seltext_m   = 'Cant Real Entrega'.
*  alv_field-seltext_s   = 'Cant Real'.
  alv_field-ddictxt     = 'Cant Prod'.
  alv_field-seltext_l   = 'Cantidad Producción'.
  alv_field-seltext_m   = 'Cant Producción'.
  alv_field-seltext_s   = 'Cant Prod'.

  READ TABLE alv_field WITH KEY fieldname = 'MEINS'.
  alv_field-qfieldname  = 'MEINS'.                         "NLC250907.sn
  alv_field-qtabname    = 'TI_FORMATO'.                    "NLC250907.en
  alv_field-no_out      = ' '.                             "NLC210807.n
  MODIFY alv_field INDEX sy-tabix.
*                                                          "NLC210807.sn
  READ TABLE alv_field WITH KEY fieldname = 'LINE_COLOR'.
  IF sy-subrc EQ 0.
    alv_field-hotspot      = ' '.
    alv_field-no_out       = 'X'.
    alv_field-no_zero      = ' '.
    alv_field-do_sum       = ' '.
    alv_field-just         = 'L'.
    MODIFY alv_field INDEX sy-tabix.
  ENDIF.
*                                                          "NLC210807.en
  READ TABLE alv_field WITH KEY fieldname = 'VTWEG_T'.
  alv_field-seltext_m    = 'tipo factu'.
  alv_field-seltext_s    = 'tipo factu'.
  alv_field-seltext_l    = 'tipo de factura'.
  alv_field-reptext_ddic = 'tipo de factura'.
  alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.
  READ TABLE alv_field WITH KEY fieldname = 'VTEXT_GI'.
  alv_field-seltext_m    = 'Mat.GrP.cuent'.
  alv_field-seltext_s    = 'Mat.GrP.cuent'.
  alv_field-seltext_l    = 'Mat. Grupo de cuentas'.
  alv_field-reptext_ddic = 'Mat. Grupo de cuentas'.
*    alv_field-no_out = 'X'.
  MODIFY alv_field INDEX sy-tabix.
*BM FXGARCIA 24.04.2006

* IF resume EQ 'X'                                          "NLC210807.o
  IF resume EQ 'X' OR                                      "NLC210807.sn
    tot_cli EQ 'X'.                                        "NLC210807.en

*    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*      EXPORTING
*        i_program_name     = repid
*        i_internal_tabname = 'TI_FORMATO2'
*        i_inclname         = repid
*      CHANGING
*        ct_fieldcat        = alv_field[].
*    IF sy-subrc <> 0.
*      WRITE: 'SY-SUBRC: ', sy-subrc, 'REUSE_ALV_FIELDCATALOG_MERGE'.
*    ENDIF.

    "JARV T_10957 10.11.2021
*    READ TABLE alv_field WITH KEY fieldname = 'WERKS'.
*    alv_field-no_out = 'X'.
*    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'SPART_T'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'VTWEG_T'.
    alv_field-seltext_m    = 'tipo factu'.
    alv_field-seltext_s    = 'tipo factu'.
    alv_field-seltext_l    = 'tipo de factura'.
    alv_field-reptext_ddic = 'tipo de factura'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'MATNR'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'MAKTX'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

*    READ TABLE alv_field WITH KEY fieldname = 'MATKL'.
*    alv_field-no_out = 'X'.
*    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'WGBEZ'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'KTGRM'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'VTEXT_GI'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'POSNR'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'ZTERM'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'MOROS'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'WRBTR'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'STTCO'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'BEZEI'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'VKGRP'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'PERNR'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'ENAME'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'BKTXT'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'VTEXT'.
    alv_field-seltext_m    = 'Cond.Pago'.
    alv_field-seltext_s    = 'Cond.Pago'.
    alv_field-seltext_l    = 'Cond. DE Pago'.
    alv_field-reptext_ddic = 'Cond.Pago'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'FVENC'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

*    READ TABLE alv_field WITH KEY fieldname = 'PREUR'.
*    alv_field-no_out      = ' '.
*    MODIFY alv_field INDEX sy-tabix.
    READ TABLE alv_field WITH KEY fieldname = 'BTGEW'.
    alv_field-seltext_l   = 'Cant. Conversion'.
    alv_field-seltext_m   = 'Cant. Conversion'.
    alv_field-seltext_s   = 'Cant.Conv'.
    alv_field-ddictxt     = 'Cant.Conv'.
*    alv_field-no_out      = ' '.
    alv_field-currency       = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'GEWEI'.
    alv_field-no_out = ' '.
    MODIFY alv_field INDEX sy-tabix.
*-------------------------------------------------------------
*                                                          "NLC210807.sn

*    READ TABLE alv_field WITH KEY fieldname   = 'FKIMG'.
*    alv_field-currency       = 'X'.
*    MODIFY alv_field INDEX sy-tabix.

***njgm-str***10-02-2012
*    READ TABLE alv_field WITH KEY fieldname   = 'PREUN'.
*    alv_field-currency       = 'X'.
*    MODIFY alv_field INDEX sy-tabix.
*
*    READ TABLE alv_field WITH KEY fieldname   = 'NETWR'.
*    alv_field-currency       = 'X'.
*    MODIFY alv_field INDEX sy-tabix.
*
*    READ TABLE alv_field WITH KEY fieldname   = 'MWSBP'.
*    alv_field-currency       = 'X'.
*    MODIFY alv_field INDEX sy-tabix.
*
*    READ TABLE alv_field WITH KEY fieldname   = 'IMPFI'.
*    alv_field-currency       = 'X'.
*    MODIFY alv_field INDEX sy-tabix.
***njgm-end***10-02-2012

    READ TABLE alv_field WITH KEY fieldname   = 'FKLMG'.
    alv_field-seltext_l   = 'Cant. Despachada'.
    alv_field-seltext_m   = 'Cant. Despachada'.
    alv_field-seltext_s   = 'Cant. Despachada'.
*    alv_field-currency       = 'X'.
    MODIFY alv_field INDEX sy-tabix.

*    READ TABLE alv_field WITH KEY fieldname   = 'BTGEW'.
*    alv_field-currency       = 'X'.
*    MODIFY alv_field INDEX sy-tabix.
    "NLC210807.en
*-------------------------------------------------------------
  ENDIF.

  IF amplia NE 'X'.
    READ TABLE alv_field WITH KEY fieldname = 'KUNNR_2'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'STRAS'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'BEZEI_2'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'ORT01_2'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'LIFNR'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'NAME1A'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'SORTL'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'NAME1B'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.

    READ TABLE alv_field WITH KEY fieldname = 'STCD1_2'.
    alv_field-no_out = 'X'.
    MODIFY alv_field INDEX sy-tabix.
  ENDIF.


*EM FXGARCIA 24.04.2006

ENDFORM.                    " CATALOGO

*&---------------------------------------------------------------------*
*&      Form  layout
*&---------------------------------------------------------------------*
FORM layout .
*  alv_layout-zebra             = 'X'.
  alv_layout-detail_popup      = 'X'.
  alv_layout-colwidth_optimize = 'X'.
  alv_layout-no_keyfix         = 'X'.
*  alv_layout-no_sumchoice      = 'X'.
*  alv_layout-no_subtotals      = 'X'.

*  FORM layout .
*  ti_config-zebra             = 'X'.
*  ti_config-colwidth_optimize = 'X'.
*  ti_config-f2code            = '&ETA'.
*  ti_config-detail_popup      = 'X'.
  alv_layout-totals_text       = 'TOTAL'.
  alv_layout-info_fieldname    = 'LINE_COLOR'.
*  ti_config-no_sumchoice      = 'X'.
*  ti_config-no_totalline      = 'X'.
*  ti_config-no_subchoice      = 'X'.
*  ti_config-no_subtotals      = 'X'.

*ENDFORM.                    " layout





ENDFORM.                    " layout

*&---------------------------------------------------------------------*
*&      Form  mostrar_dventas
*&---------------------------------------------------------------------*
FORM mostrar_dventas .
  IF NOT p_vari IS INITIAL.
    CLEAR w_es_variant.
    MOVE p_vari TO w_es_variant-variant.
    MOVE sy-repid TO w_es_variant-report.
    CALL FUNCTION 'LVC_VARIANT_EXISTENCE_CHECK'
      EXPORTING
        i_save        = 'X'
      CHANGING
        cs_variant    = w_es_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
* IF resume EQ 'X' OR                                       "NLC210807.o
  IF resume EQ 'X' OR                                      "NLC210807.sn
    tot_cli EQ 'X' OR
    amplia EQ 'X'."JGP 19032021
    IF tot_cli EQ 'X'.
      PERFORM ordenar_totalizar_cliente.
    ENDIF.
*                                                          "NLC210807.en
*Muestro el Resumen del Diario de Ventas con el Total por
*Factura
*         w_sort-spos = 1.                                 "NLC210807.so
*         w_sort-fieldname = 'KUNNR'.
*         w_sort-tabname = 'TI_FORMATO2'.
*         w_sort-up = 'X'.
*         w_sort-subtot = 'X'.
*         APPEND w_sort to sort.                           "NLC210807.eo


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = repid
        i_callback_user_command = 'COMMAND'
*        i_background_id         = 'TRVPICTURE01'
        is_layout               = alv_layout
        it_fieldcat             = alv_field[]
        is_variant              = w_es_variant
        i_save                  = 'X'
        it_events               = ti_evento[]
*        IT_SORT                 = sort[]                   "NLC210807.o
      TABLES
        t_outtab                = ti_formato2
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
*Muestro el Diario de Ventas Normal sin resumirlo
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = repid
        i_callback_user_command = 'COMMAND'
*        i_background_id         = 'TRVPICTURE01'
        is_layout               = alv_layout
        it_fieldcat             = alv_field[]
        is_variant              = w_es_variant
        i_save                  = 'X'
        it_events               = ti_evento[]
      TABLES
        t_outtab                = ti_formato
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " mostrar_dventas

*&---------------------------------------------------------------------*
*&      Form  VARIANTES
*&---------------------------------------------------------------------*
FORM variantes .
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant         = w_disvariant
      i_save             = 'X'
      i_display_via_grid = 'X'
    IMPORTING
      e_exit             = w_variant_exit
      es_variant         = w_es_variant
    EXCEPTIONS
      OTHERS             = 3.
  IF sy-subrc = 0.
    IF w_variant_exit <> 'X'.
      p_vari = w_es_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.                    " VARIANTES

*&--------------------------------------------------------------------*
*&      Form  eventos
*&--------------------------------------------------------------------*
FORM eventos  USING p_ti_evento TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = p_ti_evento.
*&-- Agrega evento TOP_OF_PAGE
  READ TABLE p_ti_evento WITH KEY name = slis_ev_top_of_page
                         INTO ls_event.
  IF sy-subrc = 0.
    MOVE formname_top_of_page TO ls_event-form.
    APPEND ls_event TO p_ti_evento.
  ENDIF.
ENDFORM.                    " EVENTOS

*&---------------------------------------------------------------------*
*&      Form  hacer_cabecera
*&---------------------------------------------------------------------*
FORM hacer_cabecera.
  DATA: fecha_i     LIKE sy-datum,
        fecha_f     LIKE sy-datum,
        hline       TYPE slis_listheader,
        hline2      TYPE slis_listheader,
        f_text1(30) TYPE c,
        f_text2(30) TYPE c,
        f_text3(30) TYPE c,
        t_text(100) TYPE c.

  CLEAR: hline, t_text, f_text1, f_text2, f_text3.
  fecha_i = fecha-low.
  fecha_f = fecha-high.

  WRITE sy-title TO t_text.
  hline-typ  = 'H'.
  hline-info = t_text.
  APPEND hline TO ti_cabecera.

  WRITE: fecha_i TO f_text1 DD/MM/YYYY.
  WRITE: fecha_f TO f_text2 DD/MM/YYYY.
  CONCATENATE f_text1 'al' f_text2 INTO f_text3 SEPARATED BY space.

  hline-typ  = 'S'.
  hline-key  = 'Fecha Consultada: '.
  hline-info = f_text3.
  APPEND hline TO ti_cabecera.

  hline-typ  = 'S'.
  hline-key  = 'Usuario: '.
  hline-info = sy-uname.
  APPEND hline TO ti_cabecera.

ENDFORM.                    " hacer_cabecera

*&---------------------------------------------------------------------*
*&      Form  cabecera2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cabecera2.
  DATA: fecha1 LIKE sy-datum,
        text_s(30) TYPE c,
        hline TYPE slis_listheader,
        text_s1(30) TYPE c.

  hline-typ  = 'H'.
  fecha1 = sy-datum.

  NEW-LINE.
  WRITE fecha1 TO text_s DD/MM/YYYY.
  hline-info = text_s.

*  APPEND hline TO ti_cabecera.
*  CLEAR TEXT_S.

  CONCATENATE 'Fecha de Impresión: ' text_s INTO text_s1
  SEPARATED BY space.
  hline-info = text_s1.
  APPEND hline TO ti_cabecera.

ENDFORM.                    "cabecera2


*&--------------------------------------------------------------------*
*&      Form  top_of_page
*&--------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = ti_cabecera
      i_logo             = 'LOGO_COMOLSA'.
ENDFORM.                    "TOP_OF_PAGE

*&--------------------------------------------------------------------*
*&      Form  command
*&--------------------------------------------------------------------*
FORM command USING ucomm    LIKE sy-ucomm
                   selfield TYPE slis_selfield.
  DATA: billingdocument LIKE bapivbrksuccess-bill_doc.

  CASE ucomm.
    WHEN '&IC1'.
      CHECK selfield-sel_tab_field EQ 'TI_FORMATO-VBELN'.
      MOVE selfield-value TO billingdocument.
      CALL FUNCTION 'BAPI_BILLINGDOC_DISPLAY'
        EXPORTING
          billingdocument = billingdocument.
  ENDCASE.
ENDFORM.                    "command

*Text elements
*----------------------------------------------------------
* 001 Parametros de Seleccion
* 002 Disposición
*003 SC: Status de Contabilizacion     (CONT: Contabilizado; NC: No
*Contabilizado)
* 004 SA: Status de Anulacion           (NA: No Anulado)


*Selection texts
*----------------------------------------------------------
* CANAL D       Canal distribución
* FECHA D       Fecha factura
* GR_CLI D       Grupo de clientes
* KUNNR D       Cliente
* MATNR D       Material
* OFIC D       Oficina de ventas
* ORG D       Organización ventas
* P_VARI D       Disposición
* SECTOR D       Sector
* SOC D       Sociedad
* ZONE D       Zona de ventas

*                                                          "NLC210807.sn
*&---------------------------------------------------------------------*
*&      Form  ordenar_totalizar_cliente
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ordenar_totalizar_cliente .

*LOOP AT ti_formato2.

  SORT ti_formato2 BY kunnr name1.
  LOOP AT ti_formato2.
    MOVE-CORRESPONDING ti_formato2 TO ti_totales.
    APPEND ti_totales.
    MOVE-CORRESPONDING ti_formato2 TO ti_totales_aux.
*    CONCATENATE ti_formato2-kunnr ti_formato2-meins
*                             INTO ti_totales_aux-descr.
    APPEND ti_totales_aux.
    DELETE ti_formato2.
  ENDLOOP.
  LOOP AT ti_totales.
    COLLECT ti_totales INTO wa_totales.
*    DELETE ti_totales.
  ENDLOOP.
  CLEAR ti_totales_aux.

  SORT wa_totales BY impfi DESCENDING.
  LOOP AT wa_totales.
    LOOP AT ti_totales_aux WHERE kunnr = wa_totales-kunnr.
      CLEAR ti_formato2.
      MOVE-CORRESPONDING ti_totales_aux TO ti_formato2.
      APPEND ti_formato2.

      AT END OF kunnr.
        CLEAR ti_formato2.
        CONCATENATE 'SUB-TOTAL POR CLIENTE' wa_totales-kunnr INTO
             wa_totales-name1 SEPARATED BY space.
        MOVE 'C511' TO ti_formato2-line_color.
        MOVE-CORRESPONDING wa_totales TO ti_formato2.
        APPEND ti_formato2.
      ENDAT.
    ENDLOOP.
  ENDLOOP.
*  SORT ti_formato2 BY kunnr name1.


ENDFORM.                    " ordenar_totalizar_cliente

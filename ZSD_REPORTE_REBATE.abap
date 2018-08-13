*&---------------------------------------------------------------------*
REPORT zsd_reporte_rebate.
*----------------------------------------------------------------------*

TABLES: vbak, kna1, likp, ztrn.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_vkorg LIKE vbak-vkorg OBLIGATORY , " Organización de Ventas
            p_kunnr LIKE kna1-kunnr            , " Solicitante
            p_navie LIKE ztrn-navie            , " Naviera
            p_pdest LIKE ztrn-pdest            , " País Destino
            p_fzarp LIKE ztrn-fdesd            . " Fecha Zarpe
SELECTION-SCREEN END OF BLOCK b1.


*///// Structures & Interal Tables
TYPES: BEGIN OF st_out,
         vtext          TYPE tvkot-vtext          , " Descripción de la sociedad
         vbeln          TYPE likp-vbeln           , " N° de Entrega
         atwrt          TYPE api_vali-atwrt       , " Temporada
         zztip_transpor TYPE vbak-zztip_transpor  , " Tipo de Transporte
         navie          TYPE likp-znaviera        , " Naviera
         pdest          TYPE vbak-zz_pais         , " País Destino
         kna1           TYPE kna1-name1           , " Solicitante
         zbl            TYPE vbak-zbl             , " BL
         zcont          TYPE likp-zzcontenedor    , " N° de Contenedor
         zzpuerto_des   TYPE likp-zzpuerto_des    , " Puerto Destino
         zetd           TYPE likp-zetd           , " Fecha de Zarpe
         atwrt2         TYPE api_vali-atwrt       , " Especie
         znave          TYPE vbak-znave           , " Nave
         zadus          TYPE likp-zzaduanasalida  , " Puerto de Embarque
         sdesp          TYPE i                    , " Semana Despacho
         lfimg          TYPE lips-lfimg           , " Bultos
         zdesc          TYPE int1                 , " Descuento
       END   OF st_out.

DATA: it_out      TYPE TABLE OF st_out   , " Layout
      it_vbfa     TYPE TABLE OF vbfa     , " Flujo de documentos comerciales
      it_vbak     TYPE TABLE OF vbak     , " Documento de Ventas (Cabecera)
      it_knvv     TYPE TABLE OF knvv     , " Maestro de clientes (Datos Comerciales)
      it_kna1     TYPE TABLE OF kna1     , " JARV
      it_tvkot    TYPE TABLE OF tvkot    , " Unidades org.: Organizaciones de ventas - Textos
      it_likp     TYPE TABLE OF likp     , " Doc.comercial: Entrega - Datos de cabecera
      it_lips     TYPE TABLE OF lips     , " Doc.comercial: Entrega - Datos de posición
      it_ztrn     TYPE TABLE OF ztrn     , " Tabla Z para referenciar descuentos
      it_api_vali TYPE TABLE OF api_vali . " Tabla importada de QC01_BATCH_VALUES_READ


*///// Work Areas
DATA: wa_out      TYPE st_out,
      wa_vbfa     TYPE vbfa,
      wa_vbak     TYPE vbak,
      wa_knvv     TYPE knvv,
      wa_kna1     TYPE kna1,
      wa_tvkot    TYPE tvkot,
      wa_likp     TYPE likp,
      wa_lips     TYPE lips,
      wa_ztrn     TYPE ztrn,
      wa_api_vali TYPE api_vali,
      week        TYPE scal-week,
      date        TYPE scal-date.


*///// Queries
IF p_kunnr IS INITIAL.

  SELECT *
    FROM vbak
INTO TABLE it_vbak
   WHERE vkorg = p_vkorg.

ELSE.

  SELECT *
    FROM vbak
INTO TABLE it_vbak
   WHERE vkorg = p_vkorg AND
         kunnr = p_kunnr.
ENDIF.

SELECT *
  FROM knvv
INTO TABLE it_knvv
FOR ALL ENTRIES IN it_vbak
 WHERE vkorg = it_vbak-vkorg AND
       kunnr = it_vbak-kunnr. " Note 2

SELECT *
  FROM vbfa
INTO TABLE it_vbfa
FOR ALL ENTRIES IN it_vbak
 WHERE vbelv = it_vbak-vbeln. " Note 1

SELECT *
  FROM kna1
INTO TABLE it_kna1
FOR ALL ENTRIES IN it_vbak
 WHERE kunnr = it_vbak-kunnr.

SELECT *
  FROM likp
INTO TABLE it_likp
FOR ALL ENTRIES IN it_vbfa
 WHERE vbeln = it_vbfa-vbeln AND
       wbstk = 'C'           AND
       fkstk = 'C'. " Note 4

SELECT *
  FROM lips
INTO TABLE it_lips
FOR ALL ENTRIES IN it_likp
 WHERE vbeln = it_likp-vbeln.

SELECT *
  FROM tvkot
INTO TABLE it_tvkot
FOR ALL ENTRIES IN it_likp
 WHERE vkorg = it_likp-vkorg AND
       spras = 'S' ." Note 3

SELECT *
  FROM ztrn
INTO TABLE it_ztrn
FOR ALL ENTRIES IN it_vbak
 WHERE solic = it_vbak-kunnr    AND
       navie = p_navie          AND
       pdest = p_pdest          AND
       fdesd = p_fzarp.


LOOP AT it_vbak INTO wa_vbak.

  READ TABLE it_knvv INTO wa_knvv WITH KEY kunnr = wa_vbak-kunnr " Note 5
                                           vkorg = wa_vbak-vkorg.
  IF wa_knvv-kvgr1 <> '01'.
    CONTINUE.
  ENDIF.

  READ TABLE it_vbfa INTO wa_vbfa   WITH KEY vbelv = wa_vbak-vbeln.

  READ TABLE it_likp INTO wa_likp   WITH KEY vbeln = wa_vbfa-vbeln.

  READ TABLE it_lips INTO wa_lips   WITH KEY vbeln = wa_likp-vbeln.

  READ TABLE it_kna1 INTO wa_kna1   WITH KEY kunnr = wa_vbak-kunnr.  "Solicitante

  READ TABLE it_tvkot INTO wa_tvkot WITH KEY vkorg = wa_likp-vkorg
                                             spras = 'S'.

  READ TABLE it_ztrn INTO wa_ztrn WITH KEY navie = wa_vbak-znaviera
                                           solic = wa_vbak-kunnr.

  IF wa_lips-matnr <> '' AND wa_lips-charg <> ''.

    IF it_api_vali[] IS INITIAL.

      CALL FUNCTION 'QC01_BATCH_VALUES_READ'
        EXPORTING
          i_val_matnr  = wa_lips-matnr
          i_val_charge = wa_lips-charg
          i_language   = sy-langu
          i_date       = sy-datum
        TABLES
          t_val_tab    = it_api_vali.
    ENDIF.
  ENDIF.

  READ TABLE it_api_vali INTO wa_api_vali WITH KEY atnam = 'ZMM_E_TEMPORADA'.
  wa_out-vtext           = wa_tvkot-vtext        . " Descripción de la sociedad
  wa_out-vbeln           = wa_likp-vbeln         . " N° de Entrega
  wa_out-atwrt           = wa_api_vali-atwrt     . " Temporada

  IF wa_vbfa-vbtyp_v = 'C'.
    wa_out-zztip_transpor = wa_vbak-zztip_transpor . "
  ENDIF.

  wa_out-navie         = wa_likp-znaviera        . " Naviera
  wa_out-pdest         = wa_vbak-zz_pais         . " País Destino
  wa_out-kna1          = wa_kna1-name1           . " Solicitante
*  WA_OUT-KUNNR         = WA_LIKP-KUNNR           . " Solicitante
  wa_out-zbl           = wa_vbak-zbl             . " BL
  wa_out-zcont         = wa_likp-zzcontenedor    . " N° de Contenedor
  wa_out-zzpuerto_des  = wa_likp-zzpuerto_des    . " Puerto Destino
  wa_out-zetd         = wa_likp-zetd           . " Fecha de Zarpe

  READ TABLE it_api_vali INTO wa_api_vali WITH KEY atnam = 'ZMM_E_ESPECIE'.
  wa_out-atwrt2        = wa_api_vali-atwrt       . " Especie
  wa_out-znave         = wa_vbak-znave           . " Nave
  wa_out-zadus         = wa_likp-zzaduanasalida  . " Puerto de embarque


***IF WA_LIKP-ZZETD <> ''.
  IF wa_likp-zetd <> ''.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date = sy-datum "WA_LIKP-ZZETD
      IMPORTING
        week = week. " FORMAT YYYY/MM
  ENDIF.

  wa_out-sdesp = week+4(2).
  wa_out-lfimg         = wa_lips-lfimg           . " Bultos
  wa_out-zdesc         = wa_ztrn-descu           . " Descuento

  APPEND wa_out TO it_out.

  CLEAR: wa_out      ,
         wa_vbfa     ,
         wa_vbak     ,
         wa_knvv     ,
         wa_tvkot    ,
         wa_likp     ,
         wa_lips     ,
         wa_api_vali .

ENDLOOP.


*///// ALV
PERFORM alv_report USING it_out[].

TYPE-POOLS: slis.

DATA: lf_sp_group TYPE slis_t_sp_group_alv,                         "Manejar grupos de campos
      lf_layout   TYPE slis_layout_alv.                             "Manejar diseño de layout
*      it_topheader  TYPE slis_t_listheader,                         "Manejar cabecera del rep
*      wa_top        LIKE LINE OF it_topheader.                      "Línea para cabecera

DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.   "Parametros del catalogo


*///// ALV Performs
FORM alv_report  USING  pp_itab LIKE it_out[].

  PERFORM sp_group_build USING lf_sp_group[].         "1
  PERFORM alv_ini_fieldcat.                           "2
  PERFORM layout_build USING lf_layout.               "3
  PERFORM alv_listado USING pp_itab[].                "4

ENDFORM.


*///// ALV Perform_1
FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = TEXT-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.


*///// ALV Perform_2
FORM alv_ini_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'VBELN'.                  "Entrega
  alv_git_fieldcat-seltext_m   = 'Entrega'.
  alv_git_fieldcat-seltext_l   = 'Entrega'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'VTEXT'.                  "Organización de Ventas
  alv_git_fieldcat-seltext_m   = 'Organización de Ventas'.
  alv_git_fieldcat-seltext_l   = 'Organización de Ventas'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ATWRT'.                   "Temporada
  alv_git_fieldcat-seltext_m   = 'Temporada'.
  alv_git_fieldcat-seltext_l   = 'Temporada'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZZTIP_TRANSPOR'.         "Tipo Transporte
  alv_git_fieldcat-seltext_m   = 'Tipo Transporte'.
  alv_git_fieldcat-seltext_l   = 'Tipo Transporte'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZNAVIERA'.                "Naviera
  alv_git_fieldcat-seltext_m   = 'Naviera'.
  alv_git_fieldcat-seltext_l   = 'Naviera'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZZ_PAIS'.                "País Destino
  alv_git_fieldcat-seltext_m   = 'País Destino'.
  alv_git_fieldcat-seltext_l   = 'País Destino'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'KNA1'.                   "Solicitante
  alv_git_fieldcat-seltext_m   = 'Solicitante'.
  alv_git_fieldcat-seltext_l   = 'Solicitante'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZBL'.                      "BL
  alv_git_fieldcat-seltext_m   = 'BL'.
  alv_git_fieldcat-seltext_l   = 'BL'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZZCONTENEDOR'.             "N° Contenedor
  alv_git_fieldcat-seltext_m   = 'N° Contenedor'.
  alv_git_fieldcat-seltext_l   = 'N° Contenedor'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZZPUERTO_DES'.             "Puerto Destino
  alv_git_fieldcat-seltext_m   = 'Puerto Destino'.
  alv_git_fieldcat-seltext_l   = 'Puerto Destino'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZZETD'.                    "Fecha Zarpe
  alv_git_fieldcat-seltext_m   = 'Fecha Zarpe'.
  alv_git_fieldcat-seltext_l   = 'Fecha Zarpe'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ATWRT2'.                    "Especie
  alv_git_fieldcat-seltext_m   = 'Especie'.
  alv_git_fieldcat-seltext_l   = 'Especie'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZNAVE'.                    "Nombre Nave
  alv_git_fieldcat-seltext_m   = 'Nombre Nave'.
  alv_git_fieldcat-seltext_l   = 'Nombre Nave'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZZADUANASALIDA'.           "Puerto Embarque
  alv_git_fieldcat-seltext_m   = 'Puerto Embarque'.
  alv_git_fieldcat-seltext_l   = 'Puerto Embarque'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'SDESP'.                    "Semana Despacho
  alv_git_fieldcat-seltext_m   = 'Semana Despacho'.
  alv_git_fieldcat-seltext_l   = 'Semana Despacho'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'LFIMG'.                    "Bultos
  alv_git_fieldcat-seltext_m   = 'Bultos'.
  alv_git_fieldcat-seltext_l   = 'Bultos'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'DESCU'.                    "Descuento
  alv_git_fieldcat-seltext_m   = 'Descuento'.
  alv_git_fieldcat-seltext_l   = 'Descuento'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-sp_group = 'A'.
  MODIFY alv_git_fieldcat FROM alv_git_fieldcat
  TRANSPORTING sp_group WHERE fieldname = 'VBTYP'.

ENDFORM.


*///// ALV Perform_3
FORM layout_build USING u_lf_layout TYPE slis_layout_alv.

*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.


*///// ALV Perform_4
FORM alv_listado  USING ppp_itab LIKE it_out[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_buffer_active    = 'X'
*     I_BACKGROUND_ID    = 'ALV_BACKGROUND'
*     i_callback_top_of_page    =  'TOP_OF_PAGE'
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      is_layout          = lf_layout
      it_fieldcat        = alv_git_fieldcat[]
*     it_special_groups  = lf_sp_group
      i_save             = 'X'
    TABLES
      t_outtab           = ppp_itab.

ENDFORM.


*///// Standard Buttons
*FORM pf_status USING rt_extab TYPE slis_t_extab.
*
*  SET PF-STATUS 'STANDARD'.
*
*ENDFORM.


*--------------------------------------------------------------------* Notes
* Note 1:
* VBAK-VBELN will exist in VBFA if there it is part of a document flow.
* In other words, if you have a corresponding sales inquiry or delivery,
* VBAK-VBELN would be in VBFA-VBELN or VBFA-VBELV respectively.

*Note 2:
* IT_KNVV It´s a depuration's table, if KVGR1 <> 01 then the information
* will be erased because KUNNR does not contract with Naviera.

*Note 3-4:
* Text with relation to VKORG.
* Doc. Delivery = Doc. sub.

*Note 5
* Skip the loop if client, has no contract with shipping company.

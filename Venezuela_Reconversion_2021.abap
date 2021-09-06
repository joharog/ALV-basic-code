FORM reconversion_2021.
*&---------------------------------------------------------------------*
*&      Reconversion Monetaria Octubre 2021 - JARV
*&---------------------------------------------------------------------*
DATA: it_zrcv_conf_mon TYPE TABLE OF zrcv_conf_mon,
      wa_zrcv_conf_mon TYPE zrcv_conf_mon,
      lv_index TYPE i.

DATA:
mon_destin  TYPE  waers,
mon_origen  TYPE  waerk,
monto_dest  TYPE  netwr,
monto_reci  TYPE  netwr,
fecha_tasa  TYPE  bldat,
l_subrc     TYPE  char01,
ex_kurs     LIKE  tcurr-ukurs.


*Moneda de la sociedad.
SELECT *
  FROM zrcv_conf_mon
  INTO TABLE it_zrcv_conf_mon
  WHERE bukrs IN s_socied.

  IF sy-subrc EQ 0.
    DESCRIBE TABLE it_zrcv_conf_mon LINES lv_index.
    READ TABLE it_zrcv_conf_mon INTO wa_zrcv_conf_mon INDEX lv_index.

    mon_destin = wa_zrcv_conf_mon-hwaer.
  ENDIF.


UNASSIGN: <fs_alv_reconv>.

LOOP AT itab ASSIGNING <fs_alv_reconv>.

  CLEAR: mon_origen, fecha_tasa.
  mon_origen = <fs_alv_reconv>-waers.
  fecha_tasa = <fs_alv_reconv>-bldat.


  IF mon_destin EQ mon_origen.

    "BS. por Documento
    IF <fs_alv_reconv>-dmbtr IS NOT INITIAL.

      CLEAR: monto_dest, monto_reci.
      monto_dest = <fs_alv_reconv>-dmbtr.
      CLEAR: <fs_alv_reconv>-dmbtr.

      PERFORM bs_a_extranjera
            USING    mon_origen               "MONEDA DE ORIGEN
                     mon_destin               "MONEDA DE DESTINO
                     monto_dest               "MONTO A CONVERTIR

            CHANGING monto_reci               "MONTO CONVERTIDO
                     fecha_tasa               "FECHA DE TASA
                     ex_kurs
                     l_subrc.

      <fs_alv_reconv>-dmbtr = monto_reci.

    ENDIF.


  ENDIF.

ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Reconversion Monetaria Octubre 2021 - JARV
*&      Form  BS_A_EXTRANJERA
*&---------------------------------------------------------------------*
FORM bs_a_extranjera  USING   q_wae     TYPE any   ""MONEDA DE ORIGEN
                              z_wae     TYPE any   ""MONEDA DE DESTINO
                              mount     TYPE any   ""MONTO A CONVERTIR
                     CHANGING mont_rs   TYPE any   ""MONTO CONVERTIDO
                              fecha     TYPE any   ""FECHA DE TASA.
                              ex_kurs
                              l_subrc.


DATA: ffact LIKE tcurr-ffact,
      tfact LIKE tcurr-tfact,
      kurst LIKE tcurr-kurst,
      kurs LIKE tcurr-ukurs,
      fix_kurs LIKE kurs,
      mount_rec TYPE wrbtr.


CLEAR: ffact, tfact, ex_kurs, kurst, mont_rs, fix_kurs.
CLEAR: l_subrc .
CLEAR: mount_rec.


CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
  EXPORTING
    date                    = fecha
    foreign_amount          = mount
    foreign_currency        = q_wae
    local_currency          = z_wae
    rate                    = kurs
    type_of_rate            = 'M'
  IMPORTING
    foreign_factor          = ffact
    local_factor            = tfact
    exchange_rate           = ex_kurs
    derived_rate_type       = kurst
    local_amount            = mont_rs
    fixed_rate              = fix_kurs
  EXCEPTIONS
    OTHERS                  = 6
          .
IF sy-subrc <> 0.

l_subrc = 'X' .
* MOVE-CORRESPONDING wa_bkpf TO wa_data.
*              CONCATENATE 'Cargue tasa de' Q_WAE '-' Z_WAE FECHA INTO wa_data-desc SEPARATED BY space.
*           APPEND wa_data TO it_data.
*           CLEAR wa_data.

ENDIF.

ENDFORM.

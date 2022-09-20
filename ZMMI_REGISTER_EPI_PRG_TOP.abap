*======================================================================*
*                                                                      *
*                                                                      *
*======================================================================*
* Programa...: ZMMR_REGISTER_EPI                                       *
* Include....: ZMMR_REGISTER_EPI_TOP                                   *
* Módulo.....: MM                                                      *
* Descrição..: Integração SAPxSOC - Cadastro EPI                       *
*----------------------------------------------------------------------*
* Autor......: Vitor Crepaldi Carlessi                                 *
* Data.......: 01.02.2022                                              *
*----------------------------------------------------------------------*
* Última modificação:                                                  *
* Nº Request | Data       | Modificado Por | Motivo                    *
*----------------------------------------------------------------------*
*======================================================================*
*----------------------------------------------------------------*
* Tables                                                         *
*----------------------------------------------------------------*
TABLES: mara.

*----------------------------------------------------------------*
* Table Types                                                    *
*----------------------------------------------------------------*
TYPES: tt_mara         TYPE STANDARD TABLE OF zmms_soc_mara,
       tt_makt         TYPE STANDARD TABLE OF zmms_soc_makt,
       tt_marc         TYPE STANDARD TABLE OF zmms_soc_marc,
       tt_mbew         TYPE STANDARD TABLE OF zmms_soc_mbew,
       tt_zmmt_cad_epi TYPE STANDARD TABLE OF zmmt_cad_epi.
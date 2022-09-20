*======================================================================*
*                                                                      *
*                                                                      *
*                                                                      *
*======================================================================*
* Programa...: ZMMR_REGISTER_EPI_PRG                                   *
* Módulo.....: MM                                                      *
* Descrição..: Integração SAPxSOC - Cadastro EPI                       *
*----------------------------------------------------------------------*
* Autor......: Vitor Crepaldi Carlessi                                 *
* Data.......: 01.02.2022                                              *
*----------------------------------------------------------------------*
* Última modificação:                                                  *
* Nº Request | Data       | Modificado Por | Motivo                    *
*----------------------------------------------------------------------*
REPORT zmmr_register_epi_prg.

INCLUDE zmmi_register_epi_prg_top.
INCLUDE zmmi_register_epi_prg_src.
INCLUDE zmmi_register_epi_prg_f01.

INITIALIZATION.
  PERFORM: f_fill_datum.

START-OF-SELECTION.
  PERFORM: f_validate_interface.
  PERFORM: f_validate_instance.
  PERFORM: f_full_process.
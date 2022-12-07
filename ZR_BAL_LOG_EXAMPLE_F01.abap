*======================================================================*
* Program.....: ZR_BAL_LOG_EXAMPLE                                     *
* Include.....: ZR_BAL_LOG_EXAMPLE_F01                                 *
* Module......: ALL                                                    *
* Description.: BAL LOG - Basic Example                                *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 07.12.2022                                             *
*======================================================================*
*&---------------------------------------------------------------------*
*&      Form  F_FULL_PROCESS                                           *
*&---------------------------------------------------------------------*
FORM f_full_process.

*&---------------------------------------------------------------------*
*& Variables                                                           *
*&---------------------------------------------------------------------*

  "1)Create BAL_LOG
  PERFORM: f_create_log.

  "2)ADD msg to BAL_LOG
  PERFORM: f_add_log.

  "3)Display BAL_LOG
  PERFORM: f_display_log.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_LOG
*&---------------------------------------------------------------------*
FORM f_create_log.

*----------------------------------------------------------------*
* Constants                                                      *
*----------------------------------------------------------------*
  CONSTANTS: lc_bal_log_create TYPE char14 VALUE 'BAL_LOG_CREATE'.

*----------------------------------------------------------------*
* Structures                                                     *
*----------------------------------------------------------------*
  DATA: ls_log TYPE bal_s_log.

  "1)Log de aplicação: dados do cabeçalho de log - Log de aplicação: identificação externa
  ls_log-extnumber = sy-cprog.

  "2)Log de aplicação: dados do cabeçalho de log - Log de aplicação: nome do usuário
  ls_log-aluser    = sy-uname.

  "3)Log de aplicação: dados do cabeçalho de log - Log de aplicação: nome do programa
  ls_log-alprog    = sy-repid.

  "4)BAL_LOG Create
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log = ls_log
    EXCEPTIONS
      OTHERS  = 1.

  "Check the return
  IF sy-subrc IS NOT INITIAL.
    "Erro interno no módulo de função &1
    MESSAGE e465(/sapapo/atp) WITH lc_bal_log_create.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ADD_LOG                                                *
*&---------------------------------------------------------------------*
FORM f_add_log.

*----------------------------------------------------------------*
* Constants                                                      *
*----------------------------------------------------------------*
  CONSTANTS: lc_bal_log_msg_add TYPE char15 VALUE 'BAL_LOG_MSG_ADD'.

*----------------------------------------------------------------*
* Structures                                                     *
*----------------------------------------------------------------*
  DATA: ls_log_msg TYPE bal_s_msg.

  ls_log_msg-msgv1 = 'Mensagem Teste'.
  ls_log_msg-msgty = 'S'.
  ls_log_msg-msgid = '00'.
  ls_log_msg-msgno = '001'.

  "BAL_LOG ADD
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_s_msg       = ls_log_msg
    EXCEPTIONS
      log_not_found = 0
      OTHERS        = 1.

  "Se não deu certo a inserção no BAL_LOG_MSG_ADD -> Retorna
  IF sy-subrc IS NOT INITIAL.
    "Erro interno no módulo de função &1
    MESSAGE e465(/sapapo/atp) WITH lc_bal_log_msg_add.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_LOG
*&---------------------------------------------------------------------*
FORM f_display_log.

*----------------------------------------------------------------*
* Constants                                                      *
*----------------------------------------------------------------*
  CONSTANTS: lc_bal_dsp_log_display TYPE char19 VALUE 'BAL_DSP_LOG_DISPLAY'.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXCEPTIONS
      OTHERS = 1.

  "Se não deu certo a inserção no BAL_DSP_LOG_DISPLAY -> Retorna
  IF sy-subrc IS NOT INITIAL.
    "Erro interno no módulo de função &1
    MESSAGE e465(/sapapo/atp) WITH lc_bal_dsp_log_display.
    RETURN.
  ENDIF.

ENDFORM.
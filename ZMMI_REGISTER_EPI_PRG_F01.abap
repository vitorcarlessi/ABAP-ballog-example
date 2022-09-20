*======================================================================*
*                                                                      *
*                          ROFF Consulting                             *
*                                                                      *
*======================================================================*
* Programa...: ZMMR_REGISTER_EPI                                       *
* Include....: ZMMR_REGISTER_EPI_F01                                   *
* Módulo.....: MM                                                      *
* Descrição..: Integração SAPxSOC - Cadastro EPI                       *
*----------------------------------------------------------------------*
* Autor......: Vitor Crepaldi Carlessi                                 *
* Data.......: 01.02.2022                                              *
*----------------------------------------------------------------------*
* Última modificação:                                                  *
* Nº Request | Data       | Modificado Por | Motivo                    *
*----------------------------------------------------------------------*
* S4DK934979 | 01.02.2022 | Vitor Carlessi | Desenv. Inicial           *
*======================================================================*
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_INSTANCE
*&---------------------------------------------------------------------*
FORM f_validate_instance.

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_msg_error TYPE c VALUE 'E'.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_srtfd TYPE indx-srtfd.

  "Bloqueia a execução de uma segunda instancia desse programa
  lv_srtfd = sy-repid.

  CALL FUNCTION 'ENQUEUE_ESINDX'
    EXPORTING
      srtfd          = lv_srtfd
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.

  IF sy-subrc IS NOT INITIAL.
    "Programa em execução noutra instancia. Tente novamente mais tarde.
    MESSAGE TEXT-m01 TYPE lc_msg_error.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FULL_PROCESS
*&---------------------------------------------------------------------*
FORM f_full_process.

*----------------------------------------------------------------*
* Tabelas                                                        *
*----------------------------------------------------------------*
  DATA: lt_mara_cri     TYPE tt_mara,
        lt_mara_alt     TYPE tt_mara,
        lt_makt         TYPE tt_makt,
        lt_marc         TYPE tt_marc,
        lt_mbew         TYPE tt_mbew,
        lt_zmmt_cad_epi TYPE tt_zmmt_cad_epi.

  "Seleciona dados para processamento
  PERFORM: f_get_data CHANGING lt_mara_cri      "Dados gerais de material -> Criação
                               lt_mara_alt      "Dados gerais de material -> Alteração
                               lt_makt          "Textos breves de material
                               lt_marc          "Dados de centro para material
                               lt_mbew          "Avaliação do material
                               lt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI

  IF sy-batch IS INITIAL.
    "Processamento Online
    PERFORM: f_online_process USING lt_mara_cri      "Dados gerais de material -> Criação
                                    lt_mara_alt      "Dados gerais de material -> Alteração
                                    lt_makt          "Textos breves de material
                                    lt_marc          "Dados de centro para material
                                    lt_mbew          "Avaliação do material
                                    lt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI.
  ELSE.
    "Processamento Background
    PERFORM: f_background_process USING lt_mara_cri      "Dados gerais de material -> Criação
                                        lt_mara_alt      "Dados gerais de material -> Alteração
                                        lt_makt          "Textos breves de material
                                        lt_marc          "Dados de centro para material
                                        lt_mbew          "Avaliação do material
                                        lt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_DATUM
*&---------------------------------------------------------------------*
FORM f_fill_datum.

  READ TABLE s_data ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX 1.
  IF sy-subrc IS INITIAL.
    <fs_data>-high = sy-datum.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data CHANGING pt_mara_cri     TYPE tt_mara          "Dados gerais de material -> Criação
                         pt_mara_alt     TYPE tt_mara          "Dados gerais de material -> Alteração
                         pt_makt         TYPE tt_makt          "Textos breves de material
                         pt_marc         TYPE tt_marc          "Dados de centro para material
                         pt_mbew         TYPE tt_mbew          "Avaliação do material
                         pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI

*                       "========================================================================================
*                       "=   "Etapa de Seleção de Informações, dividido em 5 seleções                           =
*                       "=   "(1)MARA         => Dados gerais de material                                       =
*                       "=   "(2)MAKT         => Textos breves de material                                      =
*                       "=   "(3)MARC         => Dados de centro para material                                  =
*                       "=   "(4)MARD         => Dados de depósito para material                                =
*                       "=   "(5)MBEW         => Avaliação do material                                          =
*                       "=   "(6)ZMMT_CAD_EPI => SAPxSOC - Cadastro de EPI                                      =
*                       "========================================================================================

*----------------------------------------------------------------*
* Tabelas                                                        *
*----------------------------------------------------------------*
  DATA: lt_mara TYPE tt_mara.

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_bklas_m063 TYPE mbew-bklas VALUE 'M063'.

  IF s_matnr IS NOT INITIAL. "Processamento Manual

    "<<<<<<<<<<Seleção da tabela MARA de Criação de Alteração>>>>>>>>>>

    "1°) Seleção MARA -> Dados gerais de material
    SELECT matnr "Coluna 2
           ersda "Coluna 4
           laeda "Coluna 6
           lvorm "Coluna 10
           zeinr "Coluna 17
           mstae "Coluna 88
    FROM mara
    INTO TABLE lt_mara
    WHERE matnr IN s_matnr.

    "Verifica se a seleção deu certo: MARA -> Criação
    IF lt_mara IS NOT INITIAL.

      "Separa os registros entre Criação e Alteração
      LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>).

        "Verifica se deve entrar no fluxo de "Criação"
        IF <fs_mara>-ersda IS NOT INITIAL.
          "Data de criação do material no SAP preenchida
          APPEND INITIAL LINE TO pt_mara_cri ASSIGNING FIELD-SYMBOL(<fs_mara_cri>).
          MOVE-CORRESPONDING <fs_mara> TO <fs_mara_cri>.
        ENDIF.

        "Verifica se deve entrar no fluxo de "Alteração"
        IF <fs_mara>-laeda IS NOT INITIAL.
          "Data da última modificação do material no SAP
          APPEND INITIAL LINE TO pt_mara_alt ASSIGNING FIELD-SYMBOL(<fs_mara_alt>).
          MOVE-CORRESPONDING <fs_mara> TO <fs_mara_alt>.
        ENDIF.

      ENDLOOP.

      "2°) Join MARAxMAKT: Textos breves de material
      SELECT b~matnr "Coluna 2
             b~spras "Coluna 3
             b~maktx "Coluna 4
      INTO TABLE pt_makt
      FROM mara AS a
      INNER JOIN makt AS b
      ON a~matnr = b~matnr
      WHERE a~matnr IN s_matnr.
      SORT pt_makt BY matnr.

      "3°) Join MARAxMARC: Dados de centro para material
      SELECT b~matnr "Coluna 2
             b~werks "Coluna 3
             b~lvorm "Coluna 7
      INTO TABLE pt_marc
      FROM mara AS a
      INNER JOIN marc AS b
      ON  a~matnr = b~matnr
      WHERE a~matnr IN s_matnr.
      SORT pt_marc BY matnr.

      "4°) Join MARAxMBEW: Dados gerais de material e Avaliação do material
      SELECT b~matnr "Coluna 2
             b~bwkey "Coluna 3
             b~bwtar "Coluna 4
             b~bklas "Coluna 14
      INTO TABLE pt_mbew
      FROM mara AS a
      INNER JOIN mbew AS b
      ON  a~matnr = b~matnr
      WHERE a~matnr IN s_matnr.
      SORT pt_mbew BY matnr.

    ENDIF.

  ELSE. "Processamento Background

    "<<<<<<<<<<Seleção da tabela MARA de Criação>>>>>>>>>>

    "1°) Seleção -> Dados gerais de material
    SELECT matnr "Coluna 2
           ersda "Coluna 4
           laeda "Coluna 6
           lvorm "Coluna 10
           zeinr "Coluna 17
           mstae "Coluna 88
    FROM mara
    INTO TABLE pt_mara_cri
    WHERE ersda IN s_data.  "Data de criação do material no SAP

    "Verifica se a seleção deu certo: MARA -> Criação
    IF pt_mara_cri IS NOT INITIAL.

      "2°) Join MARAxMAKT: Textos breves de material
      SELECT matnr "Coluna 2
             spras "Coluna 3
             maktx "Coluna 4
      INTO TABLE pt_makt
      FROM makt
      FOR ALL ENTRIES IN pt_mara_cri
      WHERE matnr EQ pt_mara_cri-matnr.
      SORT pt_makt BY matnr.

      "3°) Seleção MARC -> Dados de centro para material
      SELECT matnr "Coluna 2
             werks "Coluna 3
             lvorm "Coluna 7
      FROM marc
      INTO TABLE pt_marc
      FOR ALL ENTRIES IN pt_mara_cri
      WHERE matnr EQ pt_mara_cri-matnr.
      SORT pt_marc BY matnr.

      "4°) Seleção MBEW -> Avaliação do material
      SELECT matnr "Coluna 2
             bwkey "Coluna 3
             bwtar "Coluna 4
             bklas "Coluna 14
      FROM mbew
      INTO TABLE pt_mbew
      FOR ALL ENTRIES IN pt_mara_cri
      WHERE matnr EQ pt_mara_cri-matnr.
      SORT pt_mbew BY matnr.

    ENDIF.
    "<<<<<<<<<<Seleção da tabela MARA de Alteração>>>>>>>>>>

    "1°) Seleção -> Dados gerais de material
    SELECT matnr "Coluna 2
           ersda "Coluna 4
           laeda "Coluna 6
           lvorm "Coluna 10
           zeinr "Coluna 17
           mstae "Coluna 88
    FROM mara
    INTO TABLE pt_mara_alt
    WHERE laeda IN s_data.  "Data da última modificação do material no SAP

    "Verifica se a seleção deu certo: MARA -> Alteração
    IF pt_mara_alt IS NOT INITIAL.

      "2°) Join MARAxMAKT: Textos breves de material
      SELECT matnr "Coluna 2
             spras "Coluna 3
             maktx "Coluna 4
      APPENDING TABLE pt_makt
      FROM makt
      FOR ALL ENTRIES IN pt_mara_alt
      WHERE matnr EQ pt_mara_alt-matnr.
      SORT pt_makt BY matnr.

      "3°) Seleção MARC -> Dados de centro para material
      SELECT matnr "Coluna 2
             werks "Coluna 3
             lvorm "Coluna 7
      FROM marc
      APPENDING TABLE pt_marc
      FOR ALL ENTRIES IN pt_mara_alt
      WHERE matnr EQ pt_mara_alt-matnr.
      SORT pt_marc BY matnr.

      "4°) Seleção MBEW -> Avaliação do material
      SELECT matnr "Coluna 2
             bwkey "Coluna 3
             bwtar "Coluna 4
             bklas "Coluna 14
      FROM mbew
      APPENDING TABLE pt_mbew
      FOR ALL ENTRIES IN pt_mara_alt
      WHERE matnr EQ pt_mara_alt-matnr.
      SORT pt_mbew BY matnr.

    ENDIF.

  ENDIF.

  "Tabela MARA -> Criação
  IF pt_mara_cri IS NOT INITIAL.

    "5°) SAPxSOC - Cadastro de EPI
    SELECT *
    FROM zmmt_cad_epi
    INTO TABLE pt_zmmt_cad_epi
    FOR ALL ENTRIES IN pt_mara_cri
    WHERE cod_sap EQ pt_mara_cri-matnr.

  ENDIF.

  "Tabela MARA -> Alteração
  IF pt_mara_alt IS NOT INITIAL.

    "5°) SAPxSOC - Cadastro de EPI
    SELECT *
    FROM zmmt_cad_epi
    APPENDING TABLE pt_zmmt_cad_epi
    FOR ALL ENTRIES IN pt_mara_alt
    WHERE cod_sap EQ pt_mara_alt-matnr.

  ENDIF.

  "Processamento independete da opção(Manual ou Background)
  DELETE pt_mbew WHERE bklas NE lc_bklas_m063.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_ALL_ONLINE
*&---------------------------------------------------------------------*
FORM f_send_all_online USING pt_mara_cri     TYPE tt_mara            "Dados gerais de material -> Criação
                             pt_mara_alt     TYPE tt_mara            "Dados gerais de material -> Alteração
                             pt_makt         TYPE tt_makt            "Textos breves de material
                             pt_marc         TYPE tt_marc            "Dados de centro para material
                             pt_mbew         TYPE tt_mbew            "Avaliação do material
                             pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi.   "SAPxSOC - Cadastro de EPI

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_msgty_warning TYPE c                       VALUE 'W', "Constante para utilização no BAL LOG -> Atenção
             lc_tipo_envio    TYPE zmmt_cad_epi-tipo_envio VALUE 1.   "Criação

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_msg         TYPE string,
        lv_ger_cod_soc TYPE abap_bool.

*----------------------------------------------------------------*
* Field-Symbols                                                  *
*----------------------------------------------------------------*
  FIELD-SYMBOLS: <fs_mara_cri> TYPE zmms_soc_mara,
                 <fs_mara_alt> TYPE zmms_soc_mara,
                 <fs_makt>     TYPE zmms_soc_makt,
                 <fs_marc>     TYPE zmms_soc_marc,
                 <fs_mbew>     TYPE zmms_soc_mbew.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_cri ASSIGNING <fs_mara_cri>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc, lv_msg.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi TRANSPORTING NO FIELDS WHERE cod_sap    EQ <fs_mara_cri>-matnr
                                                     AND tipo_envio EQ lc_tipo_envio
                                                     AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_true.
      "Existe SIM registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Já existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m10.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m11.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m12.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Avaliação do material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m13.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_INCLUDE'
      EXPORTING
        ps_mara    = <fs_mara_cri> "Dados gerais de material
        ps_makt    = <fs_makt>     "Textos breves de material
      IMPORTING
        ps_msg_log = ls_msg_log. "Log de aplicação: dados de uma mensagem

    "Adiciona mensagem ao LOG
    PERFORM: f_add_log USING ls_msg_log.

  ENDLOOP.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_alt ASSIGNING <fs_mara_alt>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi ASSIGNING FIELD-SYMBOL(<fs_zmmt_cad_epi>) WHERE cod_sap    EQ <fs_mara_alt>-matnr
                                                                        AND tipo_envio EQ lc_tipo_envio
                                                                        AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_false.
      "NÃO existe registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Não existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Alteração EPI
      lv_msg = TEXT-m17.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Alteração EPI
      lv_msg = TEXT-m18.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Alteração EPI
      lv_msg = TEXT-m19.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Dados de centro para material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Alteração EPI
      lv_msg = TEXT-m20.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_CHANGE'
      EXPORTING
        ps_mara         = <fs_mara_alt>     "Dados gerais de material
        ps_makt         = <fs_makt>         "Textos breves de material
        ps_zmmt_cad_epi = <fs_zmmt_cad_epi> "SAPxSOC - Cadastro de EPI
      IMPORTING
        ps_msg_log      = ls_msg_log.       "Log de aplicação: dados de uma mensagem

    "Adiciona mensagem ao LOG
    PERFORM: f_add_log USING ls_msg_log.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_ALL_BACKGROUND
*&---------------------------------------------------------------------*
FORM f_send_all_background USING pt_mara_cri     TYPE tt_mara            "Dados gerais de material -> Criação
                                 pt_mara_alt     TYPE tt_mara            "Dados gerais de material -> Alteração
                                 pt_makt         TYPE tt_makt            "Textos breves de material
                                 pt_marc         TYPE tt_marc            "Dados de centro para material
                                 pt_mbew         TYPE tt_mbew            "Avaliação do material
                                 pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi.   "SAPxSOC - Cadastro de EPI

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_msg_sucess TYPE c                       VALUE 'S', "Constante para utilização no BAL LOG -> Atenção
             lc_tipo_envio  TYPE zmmt_cad_epi-tipo_envio VALUE 1.   "Criação

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_ger_cod_soc TYPE abap_bool,
        lv_msg         TYPE string,
        lv_msgty       TYPE bal_s_msg-msgty.

*----------------------------------------------------------------*
* Field-Symbols                                                  *
*----------------------------------------------------------------*
  FIELD-SYMBOLS: <fs_mara_cri> TYPE zmms_soc_mara,
                 <fs_mara_alt> TYPE zmms_soc_mara,
                 <fs_makt>     TYPE zmms_soc_makt,
                 <fs_marc>     TYPE zmms_soc_marc,
                 <fs_mbew>     TYPE zmms_soc_mbew.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_cri ASSIGNING <fs_mara_cri>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc, lv_msg, lv_msgty.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi TRANSPORTING NO FIELDS WHERE cod_sap    EQ <fs_mara_cri>-matnr
                                                     AND tipo_envio EQ lc_tipo_envio
                                                     AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_true.
      "Existe SIM registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Já existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m10 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m11 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m12 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Avaliação do material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m13 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_INCLUDE'
      EXPORTING
        ps_mara    = <fs_mara_cri> "Dados gerais de material
        ps_makt    = <fs_makt>     "Textos breves de material
      IMPORTING
        ps_msg_log = ls_msg_log.   "Log de aplicação: dados de uma mensagem

    "Concatena mensagem
    lv_msg      = |{ ls_msg_log-msgv1 }{ ls_msg_log-msgv2 }{ ls_msg_log-msgv3 }{ ls_msg_log-msgv4 }|.

    "Pega Tipo de Mensagem
    lv_msgty = ls_msg_log-msgty.

    "Processa mensagem escrita durante consumo da função
    MESSAGE lv_msg TYPE lv_msgty.

  ENDLOOP.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_alt ASSIGNING <fs_mara_alt>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc, lv_msg, lv_msgty.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi ASSIGNING FIELD-SYMBOL(<fs_zmmt_cad_epi>) WHERE cod_sap    EQ <fs_mara_alt>-matnr
                                                                        AND tipo_envio EQ lc_tipo_envio
                                                                        AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_false.
      "NÃO existe registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Não existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Alteração EPI
      MESSAGE TEXT-m17 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Alteração EPI
      MESSAGE TEXT-m18 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Alteração EPI
      MESSAGE TEXT-m19 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Dados de centro para material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Alteração EPI
      MESSAGE TEXT-m20 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_CHANGE'
      EXPORTING
        ps_mara         = <fs_mara_alt>     "Dados gerais de material
        ps_makt         = <fs_makt>         "Textos breves de material
        ps_zmmt_cad_epi = <fs_zmmt_cad_epi> "SAPxSOC - Cadastro de EPI
      IMPORTING
        ps_msg_log      = ls_msg_log.       "Log de aplicação: dados de uma mensagem

    "Concatena mensagem
    lv_msg      = |{ ls_msg_log-msgv1 }{ ls_msg_log-msgv2 }{ ls_msg_log-msgv3 }{ ls_msg_log-msgv4 }|.

    "Pega Tipo de Mensagem
    lv_msgty = ls_msg_log-msgty.

    "Processa mensagem escrita durante consumo da função
    MESSAGE lv_msg TYPE lv_msgty.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_INCLUDE_ONLINE
*&---------------------------------------------------------------------*
FORM f_send_include_online USING pt_mara_cri     TYPE tt_mara           "Dados gerais de material -> Criação
                                 pt_makt         TYPE tt_makt           "Textos breves de material
                                 pt_marc         TYPE tt_marc           "Dados de centro para material
                                 pt_mbew         TYPE tt_mbew           "Avaliação do material
                                 pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi.  "SAPxSOC - Cadastro de EPI

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  DATA: lc_tipo_envio    TYPE zmmt_cad_epi-tipo_envio VALUE 1,   "Criação
        lc_msgty_warning TYPE c                       VALUE 'W'. "Constante para utilização no BAL LOG -> Atenção

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_ger_cod_soc TYPE abap_bool,
        lv_msg         TYPE string.

*----------------------------------------------------------------*
* Field-Symbols                                                  *
*----------------------------------------------------------------*
  FIELD-SYMBOLS: <fs_mara_cri> TYPE zmms_soc_mara,
                 <fs_makt>     TYPE zmms_soc_makt,
                 <fs_marc>     TYPE zmms_soc_marc,
                 <fs_mbew>     TYPE zmms_soc_mbew.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_cri ASSIGNING <fs_mara_cri>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc, lv_msg.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi TRANSPORTING NO FIELDS WHERE cod_sap    EQ <fs_mara_cri>-matnr
                                                     AND tipo_envio EQ lc_tipo_envio
                                                     AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_true.
      "Existe SIM registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Já existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m10.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m11.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m12.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Avaliação do material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Inclusão EPI
      lv_msg = TEXT-m13.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_cri>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_INCLUDE'
      EXPORTING
        ps_mara    = <fs_mara_cri> "Dados gerais de material
        ps_makt    = <fs_makt>     "Textos breves de material
      IMPORTING
        ps_msg_log = ls_msg_log. "Log de aplicação: dados de uma mensagem

    "Adiciona mensagem ao LOG
    PERFORM: f_add_log USING ls_msg_log.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_INCLUDE_BACKGROUND
*&---------------------------------------------------------------------*
FORM f_send_include_background USING pt_mara_cri     TYPE tt_mara           "Dados gerais de material -> Criação
                                     pt_makt         TYPE tt_makt           "Textos breves de material
                                     pt_marc         TYPE tt_marc           "Dados de centro para material
                                     pt_mbew         TYPE tt_mbew           "Avaliação do material
                                     pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi.  "SAPxSOC - Cadastro de EPI

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  DATA: lc_tipo_envio  TYPE zmmt_cad_epi-tipo_envio VALUE 1,   "Criação
        lc_msg_sucess TYPE c                       VALUE 'S'. "Constante para utilização no BAL LOG -> Atenção

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_ger_cod_soc TYPE abap_bool,
        lv_msg         TYPE string,
        lv_msgty       TYPE bal_s_msg-msgty.

*----------------------------------------------------------------*
* Field-Symbols                                                  *
*----------------------------------------------------------------*
  FIELD-SYMBOLS: <fs_mara_cri> TYPE zmms_soc_mara,
                 <fs_makt>     TYPE zmms_soc_makt,
                 <fs_marc>     TYPE zmms_soc_marc,
                 <fs_mbew>     TYPE zmms_soc_mbew.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_cri ASSIGNING <fs_mara_cri>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc, lv_msg, lv_msgty.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi TRANSPORTING NO FIELDS WHERE cod_sap    EQ <fs_mara_cri>-matnr
                                                     AND tipo_envio EQ lc_tipo_envio
                                                     AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_true.
      "Existe SIM registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Já existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m10 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m11 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m12 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Avaliação do material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_cri>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Inclusão EPI
      MESSAGE TEXT-m13 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_INCLUDE'
      EXPORTING
        ps_mara    = <fs_mara_cri> "Dados gerais de material
        ps_makt    = <fs_makt>     "Textos breves de material
      IMPORTING
        ps_msg_log = ls_msg_log. "Log de aplicação: dados de uma mensagem

    "Concatena mensagem
    lv_msg      = |{ ls_msg_log-msgv1 }{ ls_msg_log-msgv2 }{ ls_msg_log-msgv3 }{ ls_msg_log-msgv4 }|.

    "Pega Tipo de Mensagem
    lv_msgty = ls_msg_log-msgty.

    "Processa mensagem escrita durante consumo da função
    MESSAGE lv_msg TYPE lv_msgty.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_CHANGE_ONLINE
*&---------------------------------------------------------------------*
FORM f_send_change_online USING pt_mara_alt     TYPE tt_mara           "Dados gerais de material -> Alteração
                                pt_makt         TYPE tt_makt           "Textos breves de material
                                pt_marc         TYPE tt_marc           "Dados de centro para material
                                pt_mbew         TYPE tt_mbew           "Avaliação do material
                                pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi.  "SAPxSOC - Cadastro de EPI

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  DATA: lc_tipo_envio    TYPE zmmt_cad_epi-tipo_envio VALUE 1,   "Criação
        lc_msgty_warning TYPE c                       VALUE 'W'. "Constante para utilização no BAL LOG -> Atenção

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_ger_cod_soc TYPE abap_bool,
        lv_msg         TYPE string.

*----------------------------------------------------------------*
* Field-Symbols                                                  *
*----------------------------------------------------------------*
  FIELD-SYMBOLS: <fs_mara_alt> TYPE zmms_soc_mara,
                 <fs_makt>     TYPE zmms_soc_makt,
                 <fs_marc>     TYPE zmms_soc_marc,
                 <fs_mbew>     TYPE zmms_soc_mbew.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_alt ASSIGNING <fs_mara_alt>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi ASSIGNING FIELD-SYMBOL(<fs_zmmt_cad_epi>) WHERE cod_sap    EQ <fs_mara_alt>-matnr
                                                                        AND tipo_envio EQ lc_tipo_envio
                                                                        AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_false.
      "NÃO existe registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Não existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Alteração EPI
      lv_msg = TEXT-m17.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Alteração EPI
      lv_msg = TEXT-m18.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Alteração EPI
      lv_msg = TEXT-m19.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Dados de centro para material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Alteração EPI
      lv_msg = TEXT-m20.

      "Inclui erro no BALLOG
      PERFORM: f_fill_bal_log_msg_01 USING <fs_mara_alt>-matnr "Material
                                           lc_msgty_warning    "Tipo de mensagem
                                           lv_msg              "Mensagem
                                  CHANGING ls_msg_log.         "Estrutura BAL LOG

      "Adiciona mensagem ao LOG
      PERFORM: f_add_log USING ls_msg_log.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_CHANGE'
      EXPORTING
        ps_mara         = <fs_mara_alt>     "Dados gerais de material
        ps_makt         = <fs_makt>         "Textos breves de material
        ps_zmmt_cad_epi = <fs_zmmt_cad_epi> "SAPxSOC - Cadastro de EPI
      IMPORTING
        ps_msg_log      = ls_msg_log.       "Log de aplicação: dados de uma mensagem

    "Adiciona mensagem ao LOG
    PERFORM: f_add_log USING ls_msg_log.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_CHANGE_BACKGROUND
*&---------------------------------------------------------------------*
FORM f_send_change_background USING pt_mara_alt     TYPE tt_mara           "Dados gerais de material -> Alteração
                                    pt_makt         TYPE tt_makt           "Textos breves de material
                                    pt_marc         TYPE tt_marc           "Dados de centro para material
                                    pt_mbew         TYPE tt_mbew           "Avaliação do material
                                    pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi.  "SAPxSOC - Cadastro de EPI

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  DATA: lc_tipo_envio TYPE zmmt_cad_epi-tipo_envio VALUE  1,   "Criação
        lc_msg_sucess TYPE c                       VALUE 'S'.  "Constante para utilização no BAL LOG -> Atenção

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_ger_cod_soc TYPE abap_bool,
        lv_msg         TYPE string,
        lv_msgty       TYPE bal_s_msg-msgty.

*----------------------------------------------------------------*
* Field-Symbols                                                  *
*----------------------------------------------------------------*
  FIELD-SYMBOLS: <fs_mara_alt> TYPE zmms_soc_mara,
                 <fs_makt>     TYPE zmms_soc_makt,
                 <fs_marc>     TYPE zmms_soc_marc,
                 <fs_mbew>     TYPE zmms_soc_mbew.

  "Loop -> Dados gerais de material
  LOOP AT pt_mara_alt ASSIGNING <fs_mara_alt>.

    "Limpa variáveis/estruturas
    CLEAR: ls_msg_log, lv_ger_cod_soc, lv_msg, lv_msgty.

    "1)Verifica se já existe um registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC
    LOOP AT pt_zmmt_cad_epi ASSIGNING FIELD-SYMBOL(<fs_zmmt_cad_epi>) WHERE cod_sap    EQ <fs_mara_alt>-matnr
                                                                        AND tipo_envio EQ lc_tipo_envio
                                                                        AND cod_soc    IS NOT INITIAL.
      "Existe sim registro na tabela ZMMT_CAD_EPI de inclusão que gerou Código SOC, marca variável para não tentar criar novamente
      lv_ger_cod_soc = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_ger_cod_soc EQ abap_false.
      "NÃO existe registro de inclusão na tabela ZMMT_CAD_EPI(SAPxSOC - Cadastro de EPI) que gerou Código SOC -> não processa

      "Não existe registro de inclusão na tabela ZMMT_CAD_EPI que gerou Código SOC, registro não será processado - Alteração EPI
      MESSAGE TEXT-m17 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "2)Leitura -> Textos breves de material
    READ TABLE pt_makt ASSIGNING <fs_makt> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MAKT, registro não será processado - Alteração EPI
      MESSAGE TEXT-m18 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "3)Leitura -> Dados de centro para material
    READ TABLE pt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MARC, registro não será processado - Alteração EPI
      MESSAGE TEXT-m19 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "4)Leitura -> Dados de centro para material
    READ TABLE pt_mbew ASSIGNING <fs_mbew> WITH KEY matnr = <fs_mara_alt>-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.

      "Registro não encontrado na tabela MBEW, registro não será processado - Alteração EPI
      MESSAGE TEXT-m20 TYPE lc_msg_sucess.

      "Pula para próximo registro
      CONTINUE.

    ENDIF.

    "Insere no Cockpit e Envia pelo PI SAPxSOC
    CALL FUNCTION 'ZMMFM_REGISTER_EPI_CHANGE'
      EXPORTING
        ps_mara         = <fs_mara_alt>     "Dados gerais de material
        ps_makt         = <fs_makt>         "Textos breves de material
        ps_zmmt_cad_epi = <fs_zmmt_cad_epi> "SAPxSOC - Cadastro de EPI
      IMPORTING
        ps_msg_log      = ls_msg_log.       "Log de aplicação: dados de uma mensagem

    "Concatena mensagem
    lv_msg      = |{ ls_msg_log-msgv1 }{ ls_msg_log-msgv2 }{ ls_msg_log-msgv3 }{ ls_msg_log-msgv4 }|.

    "Pega Tipo de Mensagem
    lv_msgty = ls_msg_log-msgty.

    "Processa mensagem escrita durante consumo da função
    MESSAGE lv_msg TYPE lv_msgty.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDATE_INTERFACE
*&---------------------------------------------------------------------*
FORM f_validate_interface.

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_register_epi_include TYPE rvari_vnam VALUE 'ZMM_INTERFACE_REGISTER_EPI_I',
             lc_register_epi_exclude TYPE rvari_vnam VALUE 'ZMM_INTERFACE_REGISTER_EPI_E',
             lc_register_epi_change  TYPE rvari_vnam VALUE 'ZMM_INTERFACE_REGISTER_EPI_C',
             lc_msg_error            TYPE c          VALUE 'E'.

*----------------------------------------------------------------*
* Types                                                          *
*----------------------------------------------------------------*
  TYPES: ty_lr_name TYPE RANGE OF rvari_vnam.

*----------------------------------------------------------------*
* Ranges                                                         *
*----------------------------------------------------------------*
  DATA: lr_name    TYPE ty_lr_name.

  "Monta Range de Interfaces
  lr_name = VALUE ty_lr_name(   sign   = 'I'
                                option = 'EQ'
                              ( low    = lc_register_epi_include )
                              ( low    = lc_register_epi_exclude )
                              ( low    = lc_register_epi_change  )
                            ).

  "Seleção -> Tabela das variáveis de variante (dependente mandante)
  SELECT *
  FROM tvarvc
  INTO TABLE @DATA(lt_tvarvc)
  WHERE name IN @lr_name.
  SORT lt_tvarvc BY name.

  IF sy-subrc IS NOT INITIAL.
    "Nenhuma Interface Cadastrada na STVARVC
    MESSAGE TEXT-m02 TYPE lc_msg_error.
  ENDIF.

  "Leitura -> ZMM_INTERFACE_REGISTER_EPI_I
  READ TABLE lt_tvarvc TRANSPORTING NO FIELDS WITH KEY name = lc_register_epi_include BINARY SEARCH.
  IF sy-subrc IS NOT INITIAL.
    "Interface não está ativada.Verificar TVARV ZMM_INTERFACE_REGISTER_EPI_I
    MESSAGE TEXT-m03 TYPE lc_msg_error.
  ENDIF.

  "Leitura -> ZMM_INTERFACE_REGISTER_EPI_E
  READ TABLE lt_tvarvc TRANSPORTING NO FIELDS WITH KEY name = lc_register_epi_exclude BINARY SEARCH.
  IF sy-subrc IS NOT INITIAL.
    "Nenhuma Interface Cadastrada na TVARVC
    MESSAGE TEXT-m04 TYPE lc_msg_error.
  ENDIF.

  "Leitura -> ZMM_INTERFACE_REGISTER_EPI_C
  READ TABLE lt_tvarvc TRANSPORTING NO FIELDS WITH KEY name = lc_register_epi_change BINARY SEARCH.
  IF sy-subrc IS NOT INITIAL.
    "Nenhuma Interface Cadastrada na TVARVC
    MESSAGE TEXT-m05 TYPE lc_msg_error.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LOG_CREATE
*&---------------------------------------------------------------------*
FORM f_log_create.

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_bal_log_create TYPE char14 VALUE 'BAL_LOG_CREATE'.

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_log TYPE bal_s_log.

  "1)Log de aplicação: dados do cabeçalho de log - Log de aplicação: identificação externa
  ls_log-extnumber = sy-cprog.

  "2)Log de aplicação: dados do cabeçalho de log - Log de aplicação: nome do usuário
  ls_log-aluser    = sy-uname.

  "3)Log de aplicação: dados do cabeçalho de log - Log de aplicação: nome do programa
  ls_log-alprog    = sy-repid.

  "4)Cria o BAL_LOG
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log = ls_log
    EXCEPTIONS
      OTHERS  = 1.

  "Se não deu certo a criação do BAL_LOG_CREATE -> Retorna
  IF sy-subrc IS NOT INITIAL.
    "Erro interno no módulo de função &1
    MESSAGE e465(/sapapo/atp) WITH lc_bal_log_create.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_LOG
*&---------------------------------------------------------------------*
FORM f_add_log USING ps_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_bal_log_msg_add TYPE char15 VALUE 'BAL_LOG_MSG_ADD'.

  "Adiciona mensagem ao BAL LOG
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_s_msg       = ps_msg_log
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
*& Form F_LOG_DISPLAY
*&---------------------------------------------------------------------*
FORM f_log_display.

*----------------------------------------------------------------*
* Constantes                                                     *
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
*&---------------------------------------------------------------------*
*& Form F_LOG_DISPLAY
*&---------------------------------------------------------------------*
FORM f_fill_bal_log_msg_01 USING pv_matnr   TYPE mara-matnr "Material
                                 pv_msgty   TYPE c          "Tipo de mensagem
                                 pv_msg     TYPE string     "Mensagem
                        CHANGING ps_msg_log TYPE bal_s_msg. "Estrutura BAL LOG

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_material TYPE char8 VALUE 'Material' ##NO_TEXT,
             lc_length   TYPE i     VALUE 49.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_matnr  TYPE char18,
        lv_string TYPE string.

*----------------------------------------------------------------*
* Tabelas                                                        *
*----------------------------------------------------------------*
  DATA: lt_string_components TYPE STANDARD TABLE OF swastrtab.

  "Recebi o material e retira zeros a esquerda
  lv_matnr = pv_matnr.
  SHIFT lv_matnr LEFT DELETING LEADING '0'.

  "Monta mensagem que será exibida no Cockpit
  CONCATENATE lc_material lv_matnr pv_msg INTO lv_string SEPARATED BY space.

  "Quebra string para distribuição nas variáveis do BAL LOG
  CALL FUNCTION 'SWA_STRING_SPLIT'
    EXPORTING
      input_string                 = lv_string
      max_component_length         = lc_length
    TABLES
      string_components            = lt_string_components
    EXCEPTIONS
      max_component_length_invalid = 1
      OTHERS                       = 2.

  IF sy-subrc IS INITIAL.

    "Coleta as strings já quebradas
    LOOP AT lt_string_components ASSIGNING FIELD-SYMBOL(<fs_string_components>).

      CASE sy-tabix.

        WHEN 1. "Primeira STRING
          "Variável mensagens 1
          ps_msg_log-msgv1 = <fs_string_components>-str.

        WHEN 2. "Segunda STRING
          "Variável mensagens 2
          CONCATENATE space <fs_string_components>-str INTO ps_msg_log-msgv2 SEPARATED BY space.

        WHEN 3. "Terceira String
          "Variável mensagens 3
          CONCATENATE space <fs_string_components>-str INTO ps_msg_log-msgv3 SEPARATED BY space.

        WHEN 4. "Quarta String
          "Variável mensagens 4
          CONCATENATE space <fs_string_components>-str INTO ps_msg_log-msgv4 SEPARATED BY space.

      ENDCASE.

    ENDLOOP.

  ENDIF.

  "Inserção no BAL LOG
  ps_msg_log-msgty = pv_msgty.
  ps_msg_log-msgid = '00'.
  ps_msg_log-msgno = '001'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LOG_DISPLAY
*&---------------------------------------------------------------------*
FORM f_fill_bal_log_msg_02 USING pv_msgty   TYPE c          "Tipo de mensagem
                                 pv_msg     TYPE string     "Mensagem
                        CHANGING ps_msg_log TYPE bal_s_msg. "Estrutura BAL LOG

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_length   TYPE i     VALUE 49.

*----------------------------------------------------------------*
* Tabelas                                                        *
*----------------------------------------------------------------*
  DATA: lt_string_components TYPE STANDARD TABLE OF swastrtab.

  "Quebra string para distribuição nas variáveis do BAL LOG
  CALL FUNCTION 'SWA_STRING_SPLIT'
    EXPORTING
      input_string                 = pv_msg
      max_component_length         = lc_length
    TABLES
      string_components            = lt_string_components
    EXCEPTIONS
      max_component_length_invalid = 1
      OTHERS                       = 2.

  IF sy-subrc IS INITIAL.

    "Coleta as strings já quebradas
    LOOP AT lt_string_components ASSIGNING FIELD-SYMBOL(<fs_string_components>).

      CASE sy-tabix.

        WHEN 1. "Primeira STRING
          "Variável mensagens 1
          ps_msg_log-msgv1 = <fs_string_components>-str.

        WHEN 2. "Segunda STRING
          "Variável mensagens 2
          CONCATENATE space <fs_string_components>-str INTO ps_msg_log-msgv2 SEPARATED BY space.

        WHEN 3. "Terceira String
          "Variável mensagens 3
          CONCATENATE space <fs_string_components>-str INTO ps_msg_log-msgv3 SEPARATED BY space.

        WHEN 4. "Quarta String
          "Variável mensagens 4
          CONCATENATE space <fs_string_components>-str INTO ps_msg_log-msgv4 SEPARATED BY space.

      ENDCASE.

    ENDLOOP.

  ENDIF.

  "Inserção no BAL LOG
  ps_msg_log-msgty = pv_msgty.
  ps_msg_log-msgid = '00'.
  ps_msg_log-msgno = '001'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ONLINE_PROCESS
*&---------------------------------------------------------------------*
FORM f_online_process USING pt_mara_cri     TYPE tt_mara          "Dados gerais de material -> Criação
                            pt_mara_alt     TYPE tt_mara          "Dados gerais de material -> Alteração
                            pt_makt         TYPE tt_makt          "Textos breves de material
                            pt_marc         TYPE tt_marc          "Dados de centro para material
                            pt_mbew         TYPE tt_mbew          "Avaliação do material
                            pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI.

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_msgty_warning TYPE c VALUE 'W'. "Constante para utilização no BAL LOG -> Atenção

*----------------------------------------------------------------*
* Estruturas e Work Areas                                        *
*----------------------------------------------------------------*
  DATA: ls_msg_log TYPE bal_s_msg.

*----------------------------------------------------------------*
* Variáveis                                                      *
*----------------------------------------------------------------*
  DATA: lv_msg TYPE string.

  "Cria BAL_LOG
  PERFORM f_log_create.

  CASE abap_true.

    WHEN p_all.   "Todas Opções

      IF  pt_mara_cri IS INITIAL  "Dados gerais de material -> Criação
      AND pt_mara_alt IS INITIAL. "Dados gerais de material -> Alteração

        "Nenhum EPI encontrado para Criação, Alteração ou Exclusão na SOC.
        lv_msg = TEXT-m06.

        "Inclui erro no BALLOG
        PERFORM: f_fill_bal_log_msg_02 USING lc_msgty_warning    "Tipo de mensagem
                                             lv_msg              "Mensagem
                                    CHANGING ls_msg_log.         "Estrutura BAL LOG

        "Adiciona mensagem ao LOG
        PERFORM: f_add_log USING ls_msg_log.

      ELSE.

        "Envia Inclusão, Alteração ou Exclusão de EPI
        PERFORM: f_send_all_online USING pt_mara_cri      "Dados gerais de material -> Criação
                                         pt_mara_alt      "Dados gerais de material -> Alteração ou Deleção
                                         pt_makt          "Textos breves de material
                                         pt_marc          "Dados de centro para material
                                         pt_mbew          "Avaliação do material
                                         pt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI

      ENDIF.

    WHEN p_incld. "Incluir EPI

      IF pt_mara_cri IS INITIAL.  "Dados gerais de material -> Criação

        "Integração processada com sucesso. Nenhum material encontrado para Criação na SOC.
        lv_msg = TEXT-m07.

        "Inclui erro no BALLOG
        PERFORM: f_fill_bal_log_msg_02 USING lc_msgty_warning    "Tipo de mensagem
                                             lv_msg              "Mensagem
                                    CHANGING ls_msg_log.         "Estrutura BAL LOG

        "Adiciona mensagem ao LOG
        PERFORM: f_add_log USING ls_msg_log.

      ELSE.

        "Envia Inclusão de EPI
        PERFORM: f_send_include_online USING pt_mara_cri      "Dados gerais de material -> Criação
                                             pt_makt          "Textos breves de material
                                             pt_marc          "Dados de centro para material
                                             pt_mbew          "Avaliação do material
                                             pt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI

      ENDIF.

    WHEN p_alter. "Alterar EPI

      IF pt_mara_alt IS INITIAL.  "Dados gerais de material -> Alteração

        "Integração processada com sucesso. Nenhum material encontrado para Alteração na SOC.
        lv_msg = TEXT-m08.

        "Inclui erro no BALLOG
        PERFORM: f_fill_bal_log_msg_02 USING lc_msgty_warning "Tipo de mensagem
                                             lv_msg           "Mensagem
                                    CHANGING ls_msg_log.      "Estrutura BAL LOG

        "Adiciona mensagem ao LOG
        PERFORM: f_add_log USING ls_msg_log.

      ELSE.

        "Envia Alteração de EPI
        PERFORM: f_send_change_online USING pt_mara_alt      "Dados gerais de material -> Alteração
                                            pt_makt           "Textos breves de material
                                            pt_marc           "Dados de centro para material
                                            pt_mbew           "Avaliação do material
                                            pt_zmmt_cad_epi.  "SAPxSOC - Cadastro de EPI

      ENDIF.

  ENDCASE.

  "Exibe BAL_LOG
  PERFORM f_log_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BACKGROUND_PROCESS
*&---------------------------------------------------------------------*
FORM f_background_process USING pt_mara_cri     TYPE tt_mara          "Dados gerais de material -> Criação
                                pt_mara_alt     TYPE tt_mara          "Dados gerais de material -> Alteração
                                pt_makt         TYPE tt_makt          "Textos breves de material
                                pt_marc         TYPE tt_marc          "Dados de centro para material
                                pt_mbew         TYPE tt_mbew          "Avaliação do material
                                pt_zmmt_cad_epi TYPE tt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI..

*----------------------------------------------------------------*
* Constantes                                                     *
*----------------------------------------------------------------*
  CONSTANTS: lc_msg_sucess TYPE c VALUE 'S'. "Constante para utilização no BAL LOG -> Atenção

  CASE abap_true.

    WHEN p_all.   "Todas Opções

      IF  pt_mara_cri IS INITIAL  "Dados gerais de material -> Criação
      AND pt_mara_alt IS INITIAL. "Dados gerais de material -> Alteração

        "Nenhum EPI encontrado para Criação, Alteração ou Exclusão na SOC.
        MESSAGE TEXT-m06 TYPE lc_msg_sucess.

      ELSE.

        "Envia Inclusão, Alteração ou Exclusão de EPI
        PERFORM: f_send_all_background USING pt_mara_cri      "Dados gerais de material -> Criação
                                             pt_mara_alt      "Dados gerais de material -> Alteração ou Deleção
                                             pt_makt          "Textos breves de material
                                             pt_marc          "Dados de centro para material
                                             pt_mbew          "Avaliação do material
                                             pt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI

      ENDIF.

    WHEN p_incld. "Incluir EPI

      IF pt_mara_cri IS INITIAL.  "Dados gerais de material -> Criação

        "Integração processada com sucesso. Nenhum material encontrado para Criação na SOC.
        MESSAGE TEXT-m07 TYPE lc_msg_sucess.

      ELSE.

        "Envia Inclusão de EPI
        PERFORM: f_send_include_background USING pt_mara_cri      "Dados gerais de material -> Criação
                                                 pt_makt          "Textos breves de material
                                                 pt_marc          "Dados de centro para material
                                                 pt_mbew          "Avaliação do material
                                                 pt_zmmt_cad_epi. "SAPxSOC - Cadastro de EPI

      ENDIF.

    WHEN p_alter. "Alterar EPI

      IF pt_mara_alt IS INITIAL.  "Dados gerais de material -> Alteração

        "Integração processada com sucesso. Nenhum material encontrado para Alteração na SOC.
        MESSAGE TEXT-m08 TYPE lc_msg_sucess.

      ELSE.

        "Envia Alteração de EPI
        PERFORM: f_send_change_background USING pt_mara_alt      "Dados gerais de material -> Alteração
                                                pt_makt           "Textos breves de material
                                                pt_marc           "Dados de centro para material
                                                pt_mbew           "Avaliação do material
                                                pt_zmmt_cad_epi.  "SAPxSOC - Cadastro de EPI

      ENDIF.

  ENDCASE.

ENDFORM.
*======================================================================*
*                                                                      *
*                                                                      *
*                                                                      *
*======================================================================*
* Programa...: ZMMR_REGISTER_EPI                                       *
* Include....: ZMMR_REGISTER_EPI_SCR                                   *
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

*----------------------------------------------------------------------*
*              Tela de seleção                                         *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS: s_data FOR mara-laeda DEFAULT sy-datum NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
SELECT-OPTIONS: s_matnr FOR mara-matnr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
PARAMETERS: p_all   RADIOBUTTON GROUP grp1 DEFAULT 'X',
            p_incld RADIOBUTTON GROUP grp1,
            p_alter RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b3.
*======================================================================*
* Program.....: ZR_BAL_LOG_EXAMPLE                                     *
* Module......: ALL                                                    *
* Description.: BAL LOG - Basic Example                                *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 07.12.2022                                             *
*======================================================================*
REPORT zr_bal_log_example.

INCLUDE zr_bal_log_example_f01.

START-OF-SELECTION.
  PERFORM: f_full_process.
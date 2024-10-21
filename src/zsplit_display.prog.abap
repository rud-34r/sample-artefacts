*&---------------------------------------------------------------------*
*& Report ZSPLIT_DISPLAY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsplit_display.
DATA : tab_salv_left  TYPE REF TO cl_salv_table,
       tab_salv_right TYPE REF TO cl_salv_table.

SELECT vbeln,
erdat,
erzet,
ernam,
audat,
vbtyp,
trvog
FROM vbak INTO TABLE @DATA(tab_vbak).

CHECK tab_vbak IS NOT INITIAL.
SELECT vbeln,
posnr,
etenr,
ettyp,
wmeng,
vrkme
FROM vbep FOR ALL ENTRIES IN @tab_vbak WHERE vbeln EQ @tab_vbak-vbeln INTO TABLE @DATA(tab_vbep).

DATA(tab_salv_vbep_buffer) = tab_vbep[].
CLEAR tab_salv_vbep_buffer.


CALL SCREEN 100.


CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.



CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_double_click.

   tab_salv_vbep_buffer = value #( for ls_vbep in tab_vbep WHERE ( vbeln = tab_vbak[ row ]-vbeln )
                                 ( ls_vbep ) ).

   tab_salv_right->display( ).
  ENDMETHOD.                    "on_double_click
ENDCLASS.


*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZSTAT_SPLIT'.
  DATA(lo_split) = NEW cl_gui_splitter_container( parent                  = cl_gui_container=>default_screen
                                                  no_autodef_progid_dynnr = abap_true
                                                  rows                    = 1
                                                  columns                 = 2 ).
  cl_salv_table=>factory( EXPORTING r_container  = lo_split->get_container( row = 1 column = 1 )
                          IMPORTING r_salv_table = tab_salv_left
                          CHANGING  t_table      = tab_vbak ).
  cl_salv_table=>factory( EXPORTING r_container  = lo_split->get_container( row = 1 column = 2 )
                          IMPORTING r_salv_table = tab_salv_right
                          CHANGING  t_table      = tab_salv_vbep_buffer ).

  tab_salv_left->get_functions( )->set_all( abap_true ).
  tab_salv_left->get_columns( )->set_optimize( abap_true ).

  tab_salv_right->get_functions( )->set_all( abap_true ).
  tab_salv_left->get_functions( )->set_all( abap_true ).

  DATA(evt_handler) = NEW lcl_handle_events( ).
  SET HANDLER evt_handler->on_double_click FOR tab_salv_left->get_event( ).

  tab_salv_left->display( ).
  tab_salv_right->display( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.

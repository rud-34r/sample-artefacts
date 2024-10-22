*&---------------------------------------------------------------------*
*& Report ZSPLIT_DISPLAY_V2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsplit_display_v2.

TYPES : BEGIN OF ty_vbep,
          vbeln TYPE vbeln,
          posnr TYPE posnr,
          etenr TYPE etenr,
          ettyp TYPE ettyp,
          wmeng TYPE wmeng,
          vrkme TYPE vrkme,
        END OF ty_vbep.

DATA : tab_vbep TYPE STANDARD TABLE OF ty_vbep.

DATA : tab_salv_left  TYPE REF TO cl_salv_table,
       tab_salv_right TYPE REF TO cl_salv_table.


DATA : svbak TYPE vbak.
SELECT-OPTIONS : s_vbeln FOR svbak-vbeln.

START-OF-SELECTION.

  SELECT vbeln,
  erdat,
  erzet,
  ernam,
  audat,
  vbtyp,
  trvog
  FROM vbak INTO TABLE @DATA(tab_vbak) UP TO 200 ROWS WHERE vbeln IN @s_vbeln.

  CALL SCREEN 100.


CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.



CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_double_click.

    IF line_exists( tab_vbak[ row ] ).
      DATA(vbeln) = tab_vbak[ row ]-vbeln.
      SELECT vbeln,
            posnr,
            etenr,
            ettyp,
            wmeng,
            vrkme
     FROM vbep  WHERE vbeln EQ @vbeln INTO TABLE @tab_vbep.
    ENDIF.

    tab_salv_right->refresh( ).

  ENDMETHOD.                    "on_double_click
ENDCLASS.


*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZSTAT_SPLIT'.
  DATA(lcontainer) = NEW cl_gui_custom_container( container_name = 'LCONT' ).
  DATA(rcontainer) = NEW cl_gui_custom_container( container_name = 'RCONT' ).

  cl_salv_table=>factory( EXPORTING r_container    = lcontainer
                                    container_name = 'LCONT'
                          IMPORTING r_salv_table   = tab_salv_left
                          CHANGING  t_table        = tab_vbak ).

  cl_salv_table=>factory( EXPORTING r_container    = rcontainer
                                    container_name = 'RCONT'
                          IMPORTING r_salv_table   = tab_salv_right
                          CHANGING  t_table        = tab_vbep ).

  tab_salv_left->get_functions( )->set_all( abap_true ).
  tab_salv_left->get_columns( )->set_optimize( abap_true ).

  tab_salv_right->get_functions( )->set_all( abap_true ).
  tab_salv_right->get_columns( )->set_optimize( abap_true ).

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

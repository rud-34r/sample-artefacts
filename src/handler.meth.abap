  METHOD handler.
    READ TABLE lt_data INTO ls_data INDEX row.
    IF sy-subrc = 0.
      SELECT vbeln posnr matnr
      FROM vbap
      INTO TABLE lt_data1
      WHERE vbeln = ls_data-vbeln.
    ENDIF.
    IF lo_obj2 IS NOT BOUND.
      CREATE OBJECT lo_obj2
        EXPORTING
          container_name = 'CONT2'.
      TRY.
          CALL METHOD cl_salv_table=>factory
            EXPORTING
              r_container    = lo_obj2
              container_name = 'CONT2'
            IMPORTING
              r_salv_table   = lo_alv2
            CHANGING
              t_table        = lt_data1.
        CATCH cx_salv_msg .
      ENDTRY.

      DATA(lo_functions2) = lo_alv2->get_functions( ).
      lo_functions2->set_all( ).

      lo_alv2->display( ).
    ELSE.
      lo_alv2->refresh( ).
    ENDIF.

  ENDMETHOD.

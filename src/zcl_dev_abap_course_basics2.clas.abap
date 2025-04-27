    CLASS zcl_dev_abap_course_basics2 DEFINITION
      PUBLIC
      FINAL
      CREATE PUBLIC .

      PUBLIC SECTION.

        INTERFACES if_oo_adt_classrun .
        INTERFACES zif_abap_course_basics .
      PROTECTED SECTION.
      PRIVATE SECTION.
    ENDCLASS.



    CLASS zcl_dev_abap_course_basics2 IMPLEMENTATION.


      METHOD if_oo_adt_classrun~main.
       DATA(res) = zif_abap_course_basics~date_parsing( `8 December 2018` ).
       out->write( res ).
      ENDMETHOD.


      METHOD zif_abap_course_basics~calculator.
       IF iv_operator = '+'.
        rv_result = iv_first_number + iv_second_number.
       ELSEIF iv_operator = '-'.
        rv_result = iv_first_number - iv_second_number.
       ELSEIF iv_operator = '*'.
        rv_result = iv_first_number * iv_second_number.
       ELSEIF iv_operator = '/'.
        rv_result = iv_first_number / iv_second_number.
       ENDIF.
      ENDMETHOD.


      METHOD zif_abap_course_basics~date_parsing.
       SPLIT iv_date AT space INTO DATA(day) DATA(month) DATA(year).

       CASE month.
        WHEN 'January' or '1'.   month = '01'.
        WHEN 'February' or '2'.  month = '02'.
        WHEN 'March' or '3'.     month = '03'.
        WHEN 'April' or '4'.     month = '04'.
        WHEN 'May' or '5'.       month = '05'.
        WHEN 'June' or '6'.      month = '06'.
        WHEN 'July' or '7'.      month = '07'.
        WHEN 'August' or '8'.    month = '08'.
        WHEN 'September' or '9'. month = '09'.
        WHEN 'October'.          month = '10'.
        WHEN 'November'.         month = '11'.
        WHEN 'December'.         month = '12'.
       ENDCASE.

       IF day >= 1 AND day <= 9.
        day = |0{ day }|.
       ENDIF.

       rv_result = |{ year }{ month }{ day }|.
      ENDMETHOD.


      METHOD zif_abap_course_basics~fizz_buzz.
       DO 100 TIMES.
          IF sy-index MOD 15 = 0.
            rv_result = rv_result && 'FizzBuzz'.
          ELSEIF sy-index MOD 3 = 0.
            rv_result = rv_result && 'Fizz'.
          ELSEIF sy-index MOD 5 = 0.
            rv_result = rv_result && 'Buzz'.
          ELSE.
            rv_result = rv_result && sy-index.
          ENDIF.

          rv_result = rv_result && CL_ABAP_CHAR_UTILITIES=>NEWLINE.
        ENDDO.
      ENDMETHOD.


      METHOD zif_abap_course_basics~get_current_date_time.
      ENDMETHOD.


      METHOD zif_abap_course_basics~hello_world.
       rv_result = |Hello { IV_NAME }, your system user is { sy-uname }.|.
      ENDMETHOD.


      METHOD zif_abap_course_basics~internal_tables.
      ENDMETHOD.


      METHOD zif_abap_course_basics~open_sql.
      ENDMETHOD.


      METHOD zif_abap_course_basics~scrabble_score.
      ENDMETHOD.
    ENDCLASS.

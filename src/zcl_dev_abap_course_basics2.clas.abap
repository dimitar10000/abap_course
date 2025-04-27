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
   DATA(res) = zif_abap_course_basics~fizz_buzz(  ).
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

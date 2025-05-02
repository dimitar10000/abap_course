CLASS zcl_university DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS ZCL_STUDENT.

  PUBLIC SECTION.
   TYPES: BEGIN OF uni_row,
           id TYPE i,
           uni TYPE REF TO zcl_university,
          END OF uni_row.

   TYPES: BEGIN OF student_row,
          id TYPE i,
          student TYPE REF TO zcl_student,
          END OF student_row.

   CLASS-METHODS create_university IMPORTING iv_university_name TYPE string
                                             iv_university_location TYPE string
                                   RETURNING VALUE(rv_university_id) TYPE i.

   CLASS-METHODS add_student IMPORTING iv_student_id TYPE i
                                       iv_university_id TYPE i
                                       RAISING cx_sy_itab_line_not_found.

   CLASS-METHODS get_university IMPORTING iv_university_id TYPE i
                                RETURNING VALUE(uni) TYPE REF TO zcl_university
                                RAISING cx_sy_itab_line_not_found.

   METHODS delete_student IMPORTING iv_student_id TYPE i.

   METHODS constructor IMPORTING name TYPE string
                                 location TYPE string.

   METHODS list_students RETURNING VALUE(rv_students) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
   CLASS-DATA university_counter TYPE i VALUE 0.
   CLASS-DATA uni_table TYPE SORTED TABLE OF uni_row WITH UNIQUE KEY id.
   DATA students TYPE SORTED TABLE OF student_row WITH UNIQUE KEY id.
   DATA id TYPE i.
   DATA name TYPE string.
   DATA location TYPE string.

   CLASS-METHODS update_student IMPORTING iv_university_id TYPE i
                                          iv_student_id TYPE i
                                          iv_student TYPE REF TO zcl_student.

ENDCLASS.

CLASS zcl_university IMPLEMENTATION.

  METHOD create_university.
   DATA uni TYPE REF TO ZCL_UNIVERSITY.
   rv_university_id = UNIVERSITY_COUNTER.
   uni = NEW #( location = iv_university_location name = iv_university_name ).
   "counter gets incremented in constructor
   INSERT VALUE #( uni = uni id = rv_university_id ) INTO TABLE uni_table.

  ENDMETHOD.

  METHOD constructor.
   me->id = university_counter.
   university_counter = university_counter + 1.

   me->name = name.
   me->location = location.
  ENDMETHOD.

  METHOD add_student.
   DATA(uni) = uni_table[ id = iv_university_id ]-uni.
   DATA(student_obj) = zcl_student=>get_student( EXPORTING iv_student_id = iv_student_id ).

   INSERT VALUE #( id = iv_student_id student = student_obj ) INTO TABLE uni->students.
   IF sy-subrc <> 4. "duplicate values check
    EXIT.
   ENDIF.
   student_obj->set_university_id( iv_university_id ).
  ENDMETHOD.

  METHOD delete_student.
   DELETE students WHERE id = iv_student_id.
  ENDMETHOD.

  METHOD get_university.
   uni = uni_table[ id = iv_university_id ]-uni.
  ENDMETHOD.

  METHOD list_students.
   LOOP AT students INTO DATA(row).
    rv_students = rv_students && | { row-id } { row-student->get_name(  ) } |
    && CL_ABAP_CHAR_UTILITIES=>newline
    && | { row-student->get_age(  ) } |
    && CL_ABAP_CHAR_UTILITIES=>newline
    && | { row-student->get_major(  ) } |
    && CL_ABAP_CHAR_UTILITIES=>newline
    && | { row-student->get_email(  ) } |
    && CL_ABAP_CHAR_UTILITIES=>newline
    && CL_ABAP_CHAR_UTILITIES=>newline.
   ENDLOOP.

  ENDMETHOD.

  METHOD update_student.
   DATA(uni) = uni_table[ id = iv_university_id ]-uni.
   DATA(row) = uni->students[ id = iv_student_id ].
   row-student = iv_student.
   MODIFY TABLE uni->students FROM row.

  ENDMETHOD.

ENDCLASS.

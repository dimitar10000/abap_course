CLASS zcl_student DEFINITION
PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS ZCL_UNIVERSITY.

 PUBLIC SECTION.
  TYPES: BEGIN OF student_row,
           id TYPE i,
           student TYPE REF TO ZCL_STUDENT,
          END OF student_row.

  CLASS-METHODS create_student IMPORTING iv_student_name TYPE string
                                         iv_student_age TYPE i
                                         iv_major TYPE string
                                         iv_email TYPE string
                               RETURNING VALUE(rv_student_id) TYPE i.

  CLASS-METHODS get_student importing iv_student_id TYPE i
                            returning value(rs_student) TYPE REF TO ZCL_STUDENT
                            RAISING cx_sy_itab_line_not_found.

  CLASS-METHODS update_student importing iv_student_id TYPE i
                                         iv_name TYPE string OPTIONAL
                                         iv_age TYPE i OPTIONAL
                                         iv_major TYPE string OPTIONAL
                                         iv_email TYPE string OPTIONAL
                               RAISING cx_sy_itab_line_not_found.

  METHODS get_id RETURNING VALUE(rv_id) TYPE i.

  METHODS get_name RETURNING VALUE(rv_name) TYPE string.

  METHODS get_age RETURNING VALUE(rv_age) TYPE i.

  METHODS get_major RETURNING VALUE(rv_major) TYPE string.

  METHODS get_email RETURNING VALUE(rv_email) TYPE string.

  METHODS constructor IMPORTING name TYPE string
                                age TYPE i
                                major TYPE string
                                email TYPE string.

 PROTECTED SECTION.
 PRIVATE SECTION.
  CLASS-DATA students TYPE SORTED TABLE OF student_row WITH UNIQUE KEY id.
  CLASS-DATA students_counter TYPE i VALUE 0.
  DATA student_id TYPE i.
  DATA university_id TYPE i.
  DATA name TYPE string.
  DATA age TYPE i.
  DATA major TYPE string.
  DATA email TYPE string.

  METHODS set_university_id IMPORTING iv_university_id TYPE i.

ENDCLASS.



CLASS ZCL_STUDENT IMPLEMENTATION.


  METHOD constructor.
   me->student_id = students_counter.
   students_counter = students_counter + 1.

   me->name = name.
   me->age = age.
   me->major = major.
   me->email = email.
  ENDMETHOD.


  METHOD create_student.
   DATA student TYPE REF TO ZCL_STUDENT.
    rv_student_id = students_counter.
    student = NEW #( name = iv_student_name
                     age = iv_student_age
                     major = iv_major
                     email = iv_email ).
    INSERT VALUE #( id = rv_student_id student = student ) INTO TABLE students.
  ENDMETHOD.


  METHOD get_age.
   rv_age = me->age.
  ENDMETHOD.


  METHOD get_email.
   rv_email = me->email.
  ENDMETHOD.


  METHOD get_id.
   rv_id = me->student_id.
  ENDMETHOD.


  METHOD get_major.
   rv_major = me->major.
  ENDMETHOD.


  METHOD get_name.
   rv_name = me->name.
  ENDMETHOD.


  METHOD get_student.
    DATA(record) = students[ id = iv_student_id ].
    rs_student = record-student.
  ENDMETHOD.


  METHOD set_university_id.
    me->university_id = iv_university_id.
  ENDMETHOD.


  METHOD update_student.
   DATA row LIKE LINE OF students.
   row = students[ id = iv_student_id ].

   IF iv_name IS SUPPLIED.
    row-student->name = iv_name.
   ENDIF.
   IF iv_age IS SUPPLIED.
    row-student->age = iv_age.
   ENDIF.
   IF iv_major IS SUPPLIED.
    row-student->major = iv_major.
   ENDIF.
   IF iv_email IS SUPPLIED.
    row-student->email = iv_email.
   ENDIF.

   MODIFY TABLE students FROM row.

   DATA(student) = row-student.
   zcl_university=>update_student( EXPORTING iv_university_id = student->university_id
                                  iv_student_id = student->student_id
                                  iv_student = row-student ).
  ENDMETHOD.
ENDCLASS.

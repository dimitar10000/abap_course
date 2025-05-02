CLASS zcl_tester DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_tester IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
   DATA(uni_id) = zcl_university=>create_university( EXPORTING
    iv_university_name = `Sofia University`
    iv_university_location = `Sofia` ).
   DATA(uni_inst) = ZCL_UNIVERSITY=>get_university( EXPORTING iv_university_id = uni_id ).

   TRY.
    DATA(uni2) = ZCL_UNIVERSITY=>get_university( EXPORTING iv_university_id = 42 ).
   CATCH cx_sy_itab_line_not_found.
    out->write( 'No such university id!' ).
   ENDTRY.

   DATA(student_id) = zcl_student=>create_student(  iv_student_name = 'Dimitar'
                                                    iv_student_age = 25
                                                    iv_major = 'Comp Sc'
                                                    iv_email = '123@gmail.com' ).
   DATA(student) = ZCL_STUDENT=>get_student( iv_student_id = student_id ).

   DATA(student_id2) = zcl_student=>create_student(  iv_student_name = 'Petar'
                                                    iv_student_age = 20
                                                    iv_major = 'Physics'
                                                    iv_email = '444@gmail.com' ).

   TRY.
    DATA(student3) = ZCL_STUDENT=>get_student( iv_student_id = 42 ).
   CATCH cx_sy_itab_line_not_found.
    out->write( 'No such student id!' ).
   ENDTRY.

   zcl_university=>add_student( EXPORTING iv_student_id = student_id
    iv_university_id = uni_id ).

   "adding duplicate should produce no effect
   zcl_university=>add_student( EXPORTING iv_student_id = student_id
    iv_university_id = uni_id ).

   zcl_university=>add_student( EXPORTING iv_student_id = student_id2
    iv_university_id = uni_id ).

   TRY.
    zcl_university=>add_student( EXPORTING iv_student_id = 42
    iv_university_id = 42 ).
   CATCH cx_sy_itab_line_not_found.
    out->write( 'Add failed, incorrect uni or student id!' ).
   ENDTRY.

   DATA(res) = uni_inst->list_students(  ).
   out->write( res ).

   zcl_student=>update_student( EXPORTING iv_student_id = 0
                                          iv_age = 15
                                          iv_major = `Chemistry` ).

   res = uni_inst->list_students(  ).
   out->write( res ).

   TRY.
    zcl_student=>update_student( EXPORTING iv_student_id = 42
                                          iv_major = `Arts` ).
   CATCH cx_sy_itab_line_not_found.
    out->write( 'Update failed, wrong student id!' ).
   ENDTRY.

   uni_inst->delete_student( EXPORTING iv_student_id = 0 ).

   "delete non existent student, nothing happens
   uni_inst->delete_student( EXPORTING iv_student_id = 42 ).

   res = uni_inst->list_students(  ).
   out->write( res ).

  ENDMETHOD.
ENDCLASS.

*"* use this source file for your ABAP unit test classes
class ltcl_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      hello_world_test for testing,
      calculator_test for testing,
      fizzbuzz_test for testing,
      date_parse_test for testing.

    METHODS setup.
    DATA basics TYPE REF TO zif_abap_course_basics.
    DATA class_instance TYPE REF TO ZCL_DEV_ABAP_COURSE_BASICS2.
endclass.


class ltcl_test implementation.

  method hello_world_test.
   DATA(res) = basics->hello_world( `Dimitar` ).
   DATA(expected) = `Hello Dimitar, your system user is ` && sy-uname && `.`.
   cl_abap_unit_assert=>assert_equals(
   act = res
   exp = expected
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

  endmethod.



  method setup.
   class_instance = NEW #(  ).
   basics = class_instance.
  endmethod.

  method calculator_test.
   DATA(res) = basics->calculator( iv_first_number = 5
                                   iv_second_number = 10
                                   iv_operator = '+' ).

   IF res <> 15.
     cl_abap_unit_assert=>fail( msg = `Sum error`
      quit = if_abap_unit_constant=>quit-no ).
   ENDIF.

   res = basics->calculator( iv_first_number = 5
                                   iv_second_number = 10
                                   iv_operator = '-' ).

   IF res <> -5.
     cl_abap_unit_assert=>fail( msg = `Subtraction error`
      quit = if_abap_unit_constant=>quit-no ).
   ENDIF.

   res = basics->calculator( iv_first_number = 5
                                   iv_second_number = 10
                                   iv_operator = '*' ).

   IF res <> 50.
     cl_abap_unit_assert=>fail( msg = `Multiplication error`
      quit = if_abap_unit_constant=>quit-no ).
   ENDIF.

   res = basics->calculator( iv_first_number = 5
                                   iv_second_number = 10
                                   iv_operator = '/' ).

   IF res <> 1.
     cl_abap_unit_assert=>fail( msg = `Division error`
      quit = if_abap_unit_constant=>quit-no ).
   ENDIF.

  endmethod.

  method date_parse_test.
   DATA(res) = basics->date_parsing( iv_date = `12 April 2017` ).
   DATA as_string TYPE string.
   as_string = res.
   DATA(expected) = `20170412`.

   cl_abap_unit_assert=>assert_equals(
   act = as_string
   exp = expected
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

   res = basics->date_parsing( iv_date = `12 4 2017` ).
   as_string = res.
   cl_abap_unit_assert=>assert_equals(
   act = res
   exp = expected
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

   res = basics->date_parsing( iv_date = `9 4 2017` ).
   as_string = res.
   expected = `20170409`.
   cl_abap_unit_assert=>assert_equals(
   act = res
   exp = expected
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

   res = basics->date_parsing( iv_date = `31 December 2030` ).
   as_string = res.
   expected = `20301231`.
   cl_abap_unit_assert=>assert_equals(
   act = res
   exp = expected
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

  endmethod.

  method fizzbuzz_test.
   DATA(res) = basics->fizz_buzz(  ).
   DATA strings TYPE TABLE OF string.
   SPLIT res AT CL_ABAP_CHAR_UTILITIES=>NEWLINE INTO TABLE strings.
   DATA(num_lines) = lines( strings ).

   cl_abap_unit_assert=>assert_equals(
   act = num_lines
   exp = 100
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-test ).

   DATA(element) = strings[ 1 ].
   cl_abap_unit_assert=>assert_equals(
   act = element
   exp = 1
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

   element = strings[ 3 ].
   cl_abap_unit_assert=>assert_equals(
   act = element
   exp = `Fizz`
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

   element = strings[ 5 ].
   cl_abap_unit_assert=>assert_equals(
   act = element
   exp = `Buzz`
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

   element = strings[ 15 ].
   cl_abap_unit_assert=>assert_equals(
   act = element
   exp = `FizzBuzz`
   msg = `Check method logic, wrong result`
   quit = if_abap_unit_constant=>quit-no ).

  endmethod.

endclass.

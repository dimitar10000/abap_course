            CLASS zcl_dev_abap_course_basics2 DEFINITION
              PUBLIC
              FINAL
              CREATE PUBLIC.

              PUBLIC SECTION.
                CLASS-METHODS:
                 convert_currency_to_usd IMPORTING amount TYPE /dmo/total_price
                                                   currency_code TYPE /dmo/currency_code
                                         RETURNING VALUE(new_amount) TYPE i.

                INTERFACES if_oo_adt_classrun .
                INTERFACES zif_abap_course_basics .
              PROTECTED SECTION.
              PRIVATE SECTION.
            ENDCLASS.



            CLASS zcl_dev_abap_course_basics2 IMPLEMENTATION.


              METHOD if_oo_adt_classrun~main.
                TYPES: BEGIN OF lts_travel_id,
                       travel_id TYPE /dmo/travel_id,
                       END OF lts_travel_id.

                TYPES ltty_travel_id TYPE TABLE OF lts_travel_id.

                DATA table1 TYPE ltty_travel_id.
                DATA table2 TYPE ltty_travel_id.
                DATA table3 TYPE ltty_travel_id.

                zif_abap_course_basics~internal_tables( IMPORTING et_travel_ids_task7_1 = table1
                    et_travel_ids_task7_2 = table2  et_travel_ids_task7_3 = table3 ).

                "out->write( table1 ).
                "out->write( table2 ).
                out->write( table3 ).
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
               GET TIME STAMP FIELD rv_result.
              ENDMETHOD.


              METHOD zif_abap_course_basics~hello_world.
               rv_result = |Hello { IV_NAME }, your system user is { sy-uname }.|.
              ENDMETHOD.


                  METHOD zif_abap_course_basics~internal_tables.

                   SELECT * FROM ZTRAVEL_DKAL INTO TABLE @DATA(lt_ztravel).

                   IF lt_ztravel IS INITIAL.
                    INSERT ZTRAVEL_DKAL FROM
                    ( SELECT FROM /dmo/travel
                      FIELDS uuid( ) AS travel_uuid,
                         travel_id        AS travel_id,
                         agency_id        AS agency_id,
                         customer_id      AS customer_id,
                         begin_date       AS begin_date,
                         end_date         AS end_date,
                         booking_fee      AS booking_fee,
                         total_price      AS total_price,
                         currency_code    AS currency_code,
                         description      AS description,
                         CASE status
                          WHEN 'B' THEN  'A'  " ACCEPTED
                          WHEN 'X'  THEN 'X' " CANCELLED
                          ELSE 'O'         " open
                          END AS overall_status,
                         createdby        AS createdby,
                         createdat        AS createdat,
                         lastchangedby    AS last_changed_by,
                         lastchangedat    AS last_changed_at
                         ORDER BY travel_id ).
                         COMMIT WORK AND WAIT.
                         SELECT * FROM ZTRAVEL_DKAL INTO TABLE @lt_ztravel.
                   ENDIF.

                   TYPES: BEGIN OF lts_travel_id,
                          travel_id TYPE /dmo/travel_id,
                          END OF lts_travel_id.

                   TYPES ltty_travel_id TYPE TABLE OF lts_travel_id.

                   LOOP AT lt_ztravel INTO DATA(row).
                    IF row-agency_id = '070001' AND
                       row-booking_fee = 20 AND
                       row-currency_code = 'JPY'.
                     APPEND VALUE #( travel_id = row-travel_id ) TO et_travel_ids_task7_1.
                    ENDIF.
                   ENDLOOP.

                   LOOP AT lt_ztravel INTO row.
                    IF row-currency_code NE 'USD'.
                     DATA(usd_val) = zcl_dev_abap_course_basics2=>convert_currency_to_usd(
                     amount = row-total_price currency_code = row-currency_code
                     ).
                    ELSE.
                     usd_val = row-total_price.
                    ENDIF.

                    IF usd_val > 2000.
                      APPEND VALUE #( travel_id = row-travel_id ) TO et_travel_ids_task7_2.
                    ENDIF.
                   ENDLOOP.

                   DATA not_euro_table TYPE TABLE OF ZTRAVEL_DKAL.

                   LOOP AT lt_ztravel INTO row.
                    IF row-currency_code NE 'EUR'.
                     APPEND row TO not_euro_table.
                    ENDIF.
                   ENDLOOP.

                   lt_ztravel = not_euro_table.

                   SORT lt_ztravel BY total_price begin_date ASCENDING.

                   LOOP AT lt_ztravel INTO row.
                    IF sy-tabix > 10.
                     EXIT.
                    ENDIF.
                    APPEND VALUE #( travel_id = row-travel_id ) TO et_travel_ids_task7_3.
                   ENDLOOP.

                  ENDMETHOD.


              METHOD zif_abap_course_basics~open_sql.
               SELECT * FROM ZTRAVEL_DKAL INTO TABLE @DATA(lt_ztravel).

                   IF lt_ztravel IS INITIAL.
                    INSERT ZTRAVEL_DKAL FROM
                    ( SELECT FROM /dmo/travel
                      FIELDS uuid( ) AS travel_uuid,
                         travel_id        AS travel_id,
                         agency_id        AS agency_id,
                         customer_id      AS customer_id,
                         begin_date       AS begin_date,
                         end_date         AS end_date,
                         booking_fee      AS booking_fee,
                         total_price      AS total_price,
                         currency_code    AS currency_code,
                         description      AS description,
                         CASE status
                          WHEN 'B' THEN  'A'  " ACCEPTED
                          WHEN 'X'  THEN 'X' " CANCELLED
                          ELSE 'O'         " open
                          END AS overall_status,
                         createdby        AS createdby,
                         createdat        AS createdat,
                         lastchangedby    AS last_changed_by,
                         lastchangedat    AS last_changed_at
                         ORDER BY travel_id ).
                         COMMIT WORK AND WAIT.
                   ENDIF.

                   TYPES: BEGIN OF lts_travel_id,
                          travel_id TYPE /dmo/travel_id,
                          END OF lts_travel_id.

                   TYPES ltty_travel_id TYPE TABLE OF lts_travel_id.

                   SELECT FROM ZTRAVEL_DKAL
                   FIELDS travel_id
                   WHERE agency_id = '070001' and
                   booking_fee = 20 and currency_code = 'JPY'
                   INTO TABLE @et_travel_ids_task8_1.

                   DATA tmp_table TYPE TABLE OF ZTRAVEL_DKAL.

        LOOP AT tmp_table INTO DATA(row).
          IF zcl_dev_abap_course_basics2=>convert_currency_to_usd(
          EXPORTING amount = row-total_price
                    currency_code = row-currency_code
          ) > 2000.
            APPEND VALUE #( travel_id = row-travel_id ) TO et_travel_ids_task8_2.
          ENDIF.
        ENDLOOP.
        "functions can't be used in where clauses

        SELECT FROM ZTRAVEL_DKAL
        FIELDS travel_id
        WHERE currency_code = 'EUR'
        ORDER BY total_price ASCENDING,
                 begin_date ASCENDING
        INTO TABLE @et_travel_ids_task8_3
        UP TO 10 ROWS.

              ENDMETHOD.


              METHOD zif_abap_course_basics~scrabble_score.
               DATA(len) = numofchar( iv_word ).
               DATA(alphabet) = `abcdefghijklmnopqrstuvwxyz`.

               DO len TIMES.
                DATA(char) = match( val = iv_word pcre = `[A-Za-z]` occ = sy-index ).
                char = to_lower( char ).
                DATA(val) = find( val = alphabet sub = char ).
                rv_result = rv_result + val + 1.
               ENDDO.

              ENDMETHOD.

              METHOD convert_currency_to_usd.
               CASE currency_code.
                WHEN 'AMD'. new_amount = floor( amount * '0.0026' ).
                WHEN 'AUD'. new_amount = floor( amount * '0.64' ).
                WHEN 'SGD'. new_amount = floor( amount * '0.76' ).
                WHEN 'CNY'. new_amount = floor( amount * '0.14' ).
                WHEN 'AED'. new_amount = floor( amount * '0.27' ).
                WHEN 'JPY'. new_amount = floor( amount * '0.0069' ).
                WHEN 'AFN'. new_amount = floor( amount * '0.014' ).
                WHEN 'ALL'. new_amount = floor( amount * '0.012' ).
                WHEN 'EUR'. new_amount = floor( amount * '1.14' ).
                WHEN 'INR'. new_amount = floor( amount * '0.012' ).
                WHEN 'USD'. new_amount = amount.
                WHEN OTHERS. new_amount = 0.
               ENDCASE.
              ENDMETHOD.

    ENDCLASS.

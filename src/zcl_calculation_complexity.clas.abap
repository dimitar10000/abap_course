CLASS zcl_calculation_complexity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CALCULATION_COMPLEXITY IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.
   DATA orders TYPE STANDARD TABLE OF ZC_RAP_Order_Dkal.
   orders = CORRESPONDING #( it_original_data ).

   READ ENTITIES OF ZC_RAP_Order_Dkal
    ENTITY Order BY \_Item
    FIELDS ( OrderUuid )
    WITH CORRESPONDING #( orders )
    RESULT FINAL(items).

   SELECT FROM @items as i
   FIELDS OrderUuid, COUNT( * ) AS num_items
   GROUP BY OrderUuid
   INTO TABLE @DATA(items_per_order).

   LOOP AT orders ASSIGNING FIELD-SYMBOL(<order>).
    READ TABLE items_per_order
     WITH KEY OrderUuid = <order>-OrderUuid
     INTO DATA(item_counts).
    IF sy-subrc = 0.
     IF item_counts-num_items < 3.
      <order>-Complexity = 'Easy'.
     ELSEIF item_counts-num_items > 2 AND item_counts-num_items < 5.
      <order>-Complexity = 'Medium'.
     ELSEIF item_counts-num_items > 5.
      <order>-Complexity = 'Complex'.
     ENDIF.
    ENDIF.
   ENDLOOP.

   ct_calculated_data = CORRESPONDING #( orders ).

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<fs_calc_element>).
     CASE <fs_calc_element>.
      WHEN 'COMPLEXITY'.
       INSERT `ORDERUUID` INTO TABLE et_requested_orig_elements.
     ENDCASE.
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.

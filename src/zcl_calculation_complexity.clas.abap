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

   SELECT OrderUuid, COUNT( OrderUuid ) AS num_items
   FROM @items as i
   GROUP BY OrderUuid
   INTO TABLE @DATA(items_per_order).

   LOOP AT orders INTO DATA(order).
    READ TABLE items_per_order
     WITH KEY OrderUuid = order-OrderUuid
     INTO DATA(item_counts).
    IF sy-subrc = 0.
     IF item_counts-num_items < 3.
      order-Complexity = 'Easy'.
     ELSEIF item_counts-num_items > 2 AND item_counts-num_items < 5.
      order-Complexity = 'Medium'.
     ELSEIF item_counts-num_items > 5.
      order-Complexity = 'Complex'.
     ENDIF.
    ENDIF.
   ENDLOOP.

   ct_calculated_data = CORRESPONDING #( orders ).

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
  ENDMETHOD.
ENDCLASS.

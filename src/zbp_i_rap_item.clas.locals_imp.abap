CLASS lhc_Item DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calcTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Item~calcTotalPrice.

    METHODS setCurrency FOR DETERMINE ON SAVE
      IMPORTING keys FOR Item~setCurrency.

    METHODS validateName FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validateName.

    METHODS validatePrice FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validatePrice.
    METHODS setQuantity FOR DETERMINE ON SAVE
      IMPORTING keys FOR Item~setQuantity.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Item RESULT result.

ENDCLASS.

CLASS lhc_Item IMPLEMENTATION.

  METHOD calcTotalPrice.
     READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
      ENTITY Item BY \_Order
       FIELDS ( OrderUuid )
       WITH CORRESPONDING #( keys )
      RESULT DATA(orders)
      FAILED DATA(read_failed).

     MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
      ENTITY Order
       EXECUTE recalcTotalPrice
       FROM CORRESPONDING #( orders )
      REPORTED DATA(execute_reported).

     REPORTED = CORRESPONDING #( DEEP execute_reported ).

  ENDMETHOD.

  METHOD setCurrency.
   DATA update TYPE TABLE FOR UPDATE ZI_RAP_ORDER\\Item.

   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   LOOP AT orders INTO DATA(order).
    DATA(order_currency) = order-Currency.

    READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
     ENTITY Order BY \_Item
      FIELDS ( Currency )
     WITH VALUE #( ( %tky = order-%tky ) )
    RESULT DATA(items).

    LOOP AT items INTO DATA(item) WHERE currency IS INITIAL.
     APPEND VALUE #( %tky = item-%tky
                     Currency = order_currency )
                     TO update.
    ENDLOOP.

    MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
     ENTITY Item
      UPDATE FIELDS ( Currency ) WITH update
     REPORTED DATA(update_reported).

    REPORTED = CORRESPONDING #( DEEP update_reported ).

   ENDLOOP.

  ENDMETHOD.

  METHOD validateName.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

    LOOP AT orders INTO DATA(order).
     READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
      ENTITY Order BY \_Item
       FIELDS ( Currency )
       WITH VALUE #( ( %tky = order-%tky ) )
      RESULT DATA(items).

      LOOP AT items INTO DATA(item).
       APPEND VALUE #( %tky = order-%tky
                     %state_area = 'VALIDATE_NAME' )
      TO reported-order.

       IF item-Name IS INITIAL.
        APPEND VALUE #( %tky = item-%tky ) to failed-item.

        APPEND VALUE #( %tky = item-%tky
                     %state_area = 'VALIDATE_NAME'
                     %msg = NEW zcm_rap_dkal(
                     severity = if_abap_behv_message=>severity-error
                     textid = zcm_rap_dkal=>item_name_missing
                     item_name = item-Name )
                     %element-Name = if_abap_behv=>mk-on )
        TO reported-item.
       ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD validatePrice.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

    LOOP AT orders INTO DATA(order).
     READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
      ENTITY Order BY \_Item
       FIELDS ( Price )
       WITH VALUE #( ( %tky = order-%tky ) )
      RESULT DATA(items).

      LOOP AT items INTO DATA(item).
       APPEND VALUE #( %tky = order-%tky
                     %state_area = 'VALIDATE_PRICE' )
       TO reported-order.

       IF item-Price IS INITIAL.
        APPEND VALUE #( %tky = item-%tky ) to failed-item.

        APPEND VALUE #( %tky = item-%tky
                     %state_area = 'VALIDATE_PRICE'
                     %msg = NEW zcm_rap_dkal(
                     severity = if_abap_behv_message=>severity-error
                     textid = zcm_rap_dkal=>item_price_missing
                     price = item-Price )
                     %element-Price = if_abap_behv=>mk-on )
        TO reported-item.
       ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD setQuantity.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

    LOOP AT orders INTO DATA(order).
     READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
      ENTITY Order BY \_Item
       FIELDS ( Quantity )
       WITH VALUE #( ( %tky = order-%tky ) )
      RESULT DATA(items).

     DELETE items WHERE Quantity IS NOT INITIAL.
     CHECK items IS NOT INITIAL.

     MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
      ENTITY Item
       UPDATE
        FIELDS ( Quantity )
         WITH VALUE #( FOR item IN items
                    ( %tky = item-%tky
                      Quantity = 1 ) )
     REPORTED DATA(update_reported).

     reported = CORRESPONDING #( DEEP update_reported ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_features.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Item BY \_Order
     FIELDS ( Status )
     WITH CORRESPONDING #( keys )
    RESULT DATA(orders)
    FAILED failed.

   LOOP AT orders INTO DATA(order).
    READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
      ENTITY Order BY \_Item
       ALL FIELDS
       WITH VALUE #( ( %tky = order-%tky ) )
      RESULT DATA(items).

     result = VALUE #( FOR item in items
                      LET completed_or_cancelled = COND #( WHEN order-Status = 'COMPLETED'
                                                                OR order-Status = 'CANCELLED'
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled )
                      IN ( %tky = item-%tky
                           %delete = completed_or_cancelled ) ).
   ENDLOOP.

  ENDMETHOD.

ENDCLASS.

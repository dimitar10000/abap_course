CLASS lhc_Order DEFINITION INHERITING FROM cl_abap_behavior_handler.
 CONSTANTS:
     BEGIN OF order_status,
      in_process TYPE z_order_status_dkal VALUE 'IN PROCESS',
      completed TYPE z_order_status_dkal VALUE 'COMPLETED',
      cancelled TYPE z_order_status_dkal VALUE 'CANCELLED',
     END OF order_status.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Order RESULT result.

    METHODS incrementId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Order~incrementId.

    METHODS setDate FOR DETERMINE ON SAVE
      IMPORTING keys FOR Order~setDate.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Order~validateCustomer.

    METHODS validateName FOR VALIDATE ON SAVE
      IMPORTING keys FOR Order~validateName.

    METHODS validateOrderItems FOR VALIDATE ON SAVE
      IMPORTING keys FOR Order~validateOrderItems.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Order RESULT result.

    METHODS cancelOrder FOR MODIFY
      IMPORTING keys FOR ACTION Order~cancelOrder RESULT result.

    METHODS completeOrder FOR MODIFY
      IMPORTING keys FOR ACTION Order~completeOrder RESULT result.

    METHODS recalcTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Order~recalcTotalPrice.

    METHODS setInitialStatus FOR DETERMINE ON SAVE
      IMPORTING keys FOR Order~setInitialStatus.

    METHODS updateCurrency FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Order~updateCurrency.

ENDCLASS.

CLASS lhc_Order IMPLEMENTATION.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD incrementId.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     FIELDS ( OrderId ) WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   DELETE orders WHERE OrderId IS NOT INITIAL.
   CHECK orders IS NOT INITIAL.

   SELECT SINGLE
    FROM ZI_RAP_ORDER
     FIELDS MAX( OrderId ) as order_id
   INTO @DATA(max_order_id).

   MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     UPDATE
      FROM VALUE #( FOR order IN orders INDEX INTO i
                    ( %tky = order-%tky
                      OrderId = max_order_id + i
                      %control-OrderId = if_abap_behv=>mk-on ) )
    REPORTED DATA(update_reported).

    REPORTED = CORRESPONDING #( DEEP update_reported ).

  ENDMETHOD.

  METHOD setDate.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     FIELDS ( CreationDate ) WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   DELETE orders WHERE CreationDate IS NOT INITIAL.
   CHECK orders IS NOT INITIAL.

   MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     UPDATE
      FIELDS ( CreationDate )
      WITH VALUE #( FOR order IN orders
                    ( %tky = order-%tky
                      CreationDate = sy-datum ) )
      REPORTED DATA(update_reported).

    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  METHOD validateCustomer.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     FIELDS ( Customer ) WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.
   customers = CORRESPONDING #( orders DISCARDING DUPLICATES MAPPING customer_id = Customer ).
   DELETE customers WHERE customer_id IS INITIAL.

   IF customers IS NOT INITIAL.
    SELECT FROM /DMO/CUSTOMER FIELDS customer_id
     FOR ALL ENTRIES IN @customers
     WHERE customer_id = @customers-customer_id
     INTO TABLE @DATA(customers_db).
   ENDIF.

   LOOP AT orders INTO DATA(order).
    APPEND VALUE #( %tky = order-%tky
                    %state_area = 'VALIDATE_CUSTOMER' )
    TO reported-order.

    IF order-Customer IS INITIAL OR NOT line_exists( customers_db[ customer_id = order-Customer ] ).
      APPEND VALUE #( %tky = order-%tky ) to failed-order.

      APPEND VALUE #( %tky = order-%tky
                     %state_area = 'VALIDATE_CUSTOMER'
                     %msg = NEW zcm_rap_dkal(
                     severity = if_abap_behv_message=>severity-error
                     textid = zcm_rap_dkal=>customer_missing
                     customer = order-Customer )
                     %element-Name = if_abap_behv=>mk-on )
     TO reported-order.
    ENDIF.
   ENDLOOP.

  ENDMETHOD.

  METHOD validateName.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

    LOOP AT orders INTO DATA(order).
     APPEND VALUE #( %tky = order-%tky
                     %state_area = 'VALIDATE_NAME' )
     TO reported-order.

     IF order-Name IS INITIAL.
      APPEND VALUE #( %tky = order-%tky ) to failed-order.

      APPEND VALUE #( %tky = order-%tky
                     %state_area = 'VALIDATE_NAME'
                     %msg = NEW zcm_rap_dkal(
                     severity = if_abap_behv_message=>severity-error
                     textid = zcm_rap_dkal=>order_name_missing
                     order_name = order-Name )
                     %element-Name = if_abap_behv=>mk-on )
     TO reported-order.

     ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validateOrderItems.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order BY \_Item
    FIELDS ( OrderUuid )
    WITH CORRESPONDING #( orders )
    RESULT DATA(items).

   SELECT OrderUuid, COUNT( OrderUuid ) AS num_items
   FROM @items as i
   GROUP BY OrderUuid
   INTO TABLE @DATA(items_per_order).

   LOOP AT orders INTO DATA(order).
    APPEND VALUE #( %tky = order-%tky
                    %state_area = 'VALIDATE_ORDER_ITEMS' )
     TO reported-order.

    READ TABLE items_per_order
     WITH KEY OrderUuid = order-OrderUuid
     INTO DATA(item_counts).
    IF sy-subrc = 0.
     IF item_counts-num_items < 1.
      APPEND VALUE #( %tky = order-%tky ) to failed-order.

      APPEND VALUE #( %tky = order-%tky
                     %state_area = 'VALIDATE_ORDER_ITEMS'
                     %msg = NEW zcm_rap_dkal(
                     severity = if_abap_behv_message=>severity-error
                     textid = zcm_rap_dkal=>not_enough_items
                     order_uuid = order-OrderUuid )
                     %element-Name = if_abap_behv=>mk-on )
     TO reported-order.

     ENDIF.
    ENDIF.
   ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     FIELDS ( Status )
     WITH CORRESPONDING #( keys )
    RESULT DATA(orders)
    FAILED failed.

   result = VALUE #( FOR order in orders
                      LET completed_or_cancelled = COND #( WHEN order-Status = ORDER_STATUS-completed
                                                                OR order-Status = ORDER_STATUS-cancelled
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled )
                      IN ( %tky = order-%tky
                           %action-completeOrder = completed_or_cancelled
                           %action-cancelOrder = completed_or_cancelled
                           %update = completed_or_cancelled ) ).
  ENDMETHOD.

  METHOD cancelOrder.
   MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     UPDATE
      FIELDS ( Status CancellationDate )
       WITH VALUE #( FOR key in keys
                      ( %tky = key-%tky
                        Status = order_status-cancelled
                        CancellationDate = sy-datum ) )
   FAILED failed
   REPORTED reported.

   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   result = VALUE #( FOR order in orders
                     ( %tky = order-%tky
                       %param = order ) ).

  ENDMETHOD.

  METHOD completeOrder.
   MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     UPDATE
      FIELDS ( Status CompletionDate )
       WITH VALUE #( FOR key in keys
                      ( %tky = key-%tky
                        Status = order_status-completed
                        CompletionDate = sy-datum ) )
   FAILED failed
   REPORTED reported.

   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   result = VALUE #( FOR order in orders
                     ( %tky = order-%tky
                       %param = order ) ).
  ENDMETHOD.

  METHOD recalcTotalPrice.
   DATA update TYPE TABLE FOR UPDATE ZI_RAP_ORDER.
   TYPES:
       BEGIN OF amount_per_currency,
        amount TYPE z_order_total_price_dkal,
        currency TYPE z_order_currency_dkal,
       END OF AMOUNT_PER_CURRENCY.

   DATA amounts_currencies TYPE STANDARD TABLE OF AMOUNT_PER_CURRENCY.

   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     FIELDS ( Currency TotalPrice ) WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   LOOP AT orders INTO DATA(order).
    amounts_currencies = VALUE #( ( amount = 0 currency = order-Currency ) ).

    READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
     ENTITY Order BY \_Item
      FIELDS ( Price Currency Quantity )
     WITH VALUE #( ( %tky = order-%tky ) )
     RESULT DATA(items).

     LOOP AT items INTO DATA(item).
      COLLECT VALUE amount_per_currency( amount = item-Price * item-Quantity currency = item-Currency )
       INTO amounts_currencies.
     ENDLOOP.

     CLEAR order-TotalPrice.
     LOOP AT amounts_currencies INTO DATA(single_amount_per_currency).
      order-TotalPrice += single_amount_per_currency-amount.
     ENDLOOP.

     APPEND VALUE #( %tky = order-%tky
                     TotalPrice = order-TotalPrice
                     Currency = order-Currency )
                     TO update.
   ENDLOOP.

   MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     UPDATE FIELDS ( TotalPrice Currency ) WITH update
     REPORTED DATA(update_reported).

   REPORTED = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  METHOD setInitialStatus.
   READ ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     FIELDS ( Customer ) WITH CORRESPONDING #( keys )
    RESULT DATA(orders).

   DELETE orders WHERE Status IS NOT INITIAL.
   CHECK orders IS NOT INITIAL.

   MODIFY ENTITIES OF ZI_RAP_ORDER IN LOCAL MODE
    ENTITY Order
     UPDATE
      FIELDS ( Status )
      WITH VALUE #( FOR order IN orders
                    ( %tky = order-%tky
                      Status = order_status-in_process ) )
      REPORTED DATA(update_reported).

    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  METHOD updateCurrency.
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

ENDCLASS.

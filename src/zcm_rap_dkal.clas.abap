CLASS zcm_rap_dkal DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_abap_behv_message.

    CONSTANTS:
        BEGIN OF order_name_missing,
            msgid TYPE symsgid VALUE 'ZRAP_MSG_DKAL',
            msgno TYPE symsgno VALUE '001',
            attr1 TYPE scx_attrname VALUE 'ORDER_NAME',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
        END OF order_name_missing.

    CONSTANTS:
        BEGIN OF customer_missing,
            msgid TYPE symsgid VALUE 'ZRAP_MSG_DKAL',
            msgno TYPE symsgno VALUE '002',
            attr1 TYPE scx_attrname VALUE 'CUSTOMER',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
        END OF customer_missing.

    CONSTANTS:
        BEGIN OF item_name_missing,
            msgid TYPE symsgid VALUE 'ZRAP_MSG_DKAL',
            msgno TYPE symsgno VALUE '003',
            attr1 TYPE scx_attrname VALUE 'ITEM_NAME',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
        END OF item_name_missing.

    CONSTANTS:
        BEGIN OF item_price_missing,
            msgid TYPE symsgid VALUE 'ZRAP_MSG_DKAL',
            msgno TYPE symsgno VALUE '004',
            attr1 TYPE scx_attrname VALUE 'PRICE',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
        END OF item_price_missing.

    CONSTANTS:
        BEGIN OF not_enough_items,
            msgid TYPE symsgid VALUE 'ZRAP_MSG_DKAL',
            msgno TYPE symsgno VALUE '005',
            attr1 TYPE scx_attrname VALUE 'ORDER_ITEMS',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
        END OF not_enough_items.

    CONSTANTS:
        BEGIN OF generic_error,
            msgid TYPE symsgid VALUE 'ZRAP_MSG_DKAL',
            msgno TYPE symsgno VALUE '006',
            attr1 TYPE scx_attrname VALUE 'GENERIC',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
        END OF generic_error.

    METHODS constructor
      IMPORTING
        severity TYPE IF_ABAP_BEHV_MESSAGE=>t_severity DEFAULT IF_ABAP_BEHV_MESSAGE=>severity-error
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous TYPE REF TO cx_root OPTIONAL
        order_uuid TYPE uuid OPTIONAL
        order_name TYPE z_order_name_dkal OPTIONAL
        customer TYPE /dmo/customer_id OPTIONAL
        item_name TYPE z_item_name_dkal OPTIONAL
        price TYPE z_item_price_dkal OPTIONAL.

    DATA order_uuid TYPE uuid READ-ONLY.
    DATA order_name TYPE z_order_name_dkal READ-ONLY.
    DATA customer TYPE /dmo/customer_id READ-ONLY.
    DATA item_name TYPE z_item_name_dkal READ-ONLY.
    DATA price TYPE z_item_price_dkal READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcm_rap_dkal IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
   CALL METHOD super->constructor
    EXPORTING
     previous = previous.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->if_abap_behv_message~m_severity = SEVERITY.

    me->order_uuid = order_uuid.
    me->order_name = | { order_name ALPHA = out } |.
    me->customer = | { customer ALPHA = out } |.
    me->item_name = | { item_name ALPHA = out } |.
    me->price = price.
  ENDMETHOD.
ENDCLASS.

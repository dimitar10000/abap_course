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

ENDCLASS.

CLASS lhc_Item IMPLEMENTATION.

  METHOD calcTotalPrice.
  ENDMETHOD.

  METHOD setCurrency.
  ENDMETHOD.

  METHOD validateName.
  ENDMETHOD.

  METHOD validatePrice.
  ENDMETHOD.

ENDCLASS.

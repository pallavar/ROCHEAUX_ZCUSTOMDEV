CLASS ztest_po_eml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZTEST_PO_EML IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    MODIFY ENTITIES OF i_purchaseordertp_2 PRIVILEGED
                   ENTITY purchaseorderitem
                  CREATE BY \_purordpricingelement SET FIELDS WITH VALUE #(
                   ( %key-purchaseorder    = '0102020006'   "lr_key->%tky
                     %key-purchaseorderitem = '00010'
                     %target =   VALUE #( (
                       %cid                = 'CIDPOFREIGHT'
                       conditiontype       = 'FVA1'
                       conditionrateamount = '6'
                       conditioncurrency   = 'USD' ) ) ) )
                       MAPPED DATA(lm1)
                   FAILED DATA(ls_modify_failed)
                   REPORTED DATA(ls_modify_reported).

    "COMMIT ENTITIES.

    MODIFY ENTITIES OF i_purchaseordertp_2 PRIVILEGED
    ENTITY purchaseorderitem
   CREATE BY \_purordpricingelement SET FIELDS WITH VALUE #(
    ( %key-purchaseorder    = '0101020001'   "lr_key->%tky
      %key-purchaseorderitem = '00010'
      %target =   VALUE #( (
        %cid                = 'CIDPOFREIGHT1'
        conditiontype       = 'FVA1'
        conditionrateamount = '1'
        conditioncurrency   = 'USD' ) ) ) )
        MAPPED DATA(lm2)
    FAILED DATA(ls_modify_failed2)
    REPORTED DATA(ls_modify_reported2).

    COMMIT ENTITIES.

    out->write( lm1 ).
    out->write( lm2 ).

    DATA(test) = '3'.
  ENDMETHOD.
ENDCLASS.

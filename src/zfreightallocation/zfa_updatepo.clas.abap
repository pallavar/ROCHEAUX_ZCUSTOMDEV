CLASS zfa_updatepo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: updatepo_item_fa
      IMPORTING
        po                  TYPE zrfai-order_po
        poitem              TYPE zrfai-item_id
        conditiontype       TYPE string
        conditionrateamount TYPE zrfai-con_value
        conditioncurrency   TYPE zrfai-cuky_field
      RETURNING
        VALUE(rv_status)    TYPE string. " Calculated SHA-256 hash.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZFA_UPDATEPO IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

  ENDMETHOD.


  METHOD updatepo_item_fa.

    MODIFY ENTITIES OF i_purchaseordertp_2 FORWARDING PRIVILEGED
                  ENTITY purchaseorderitem
                 CREATE BY \_purordpricingelement SET FIELDS WITH VALUE #(
                  ( %key-purchaseorder    =  po "'0102020006'   "lr_key->%tky
                  "  %cid_ref = po
                   " %tky-PurchaseOrder = po
                   " %tky-PurchaseOrderItem = poitem
                    %key-purchaseorderitem = poitem "'00010'
                    %target =   VALUE #( (
                      %cid                =  |CIDPOFREIGHT{ sy-tabix }|       "'CIDPOFREIGHT'
                      %key-PurchaseOrder = po
                      %key-PurchaseOrderItem = poitem
*                      %key-PurchaseOrderItem = poitem
                      conditiontype       =  conditiontype "'FVA1'
                      conditionrateamount = conditionrateamount "'6'
                      conditioncurrency   = conditioncurrency "'USD'
                                       ) )
                    ) )
                  MAPPED DATA(lm1)
                  FAILED DATA(ls_modify_failed)
                  REPORTED DATA(ls_modify_reported).

    "COMMIT ENTITIES.

  ENDMETHOD.
ENDCLASS.

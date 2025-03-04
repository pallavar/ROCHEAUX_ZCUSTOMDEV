CLASS z_poitem_modify_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_mm_pur_s4_po_modify_item .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z_POITEM_MODIFY_IMPL IMPLEMENTATION.


  METHOD if_mm_pur_s4_po_modify_item~modify_item.

    "read PO item account assignment table
    READ TABLE purorderacctassignmt_table
     WITH KEY purchaseorder = purchaseorderitem-purchaseorder
              purchaseorderitem = purchaseorderitem-purchaseorderitem
              ASSIGNING FIELD-SYMBOL(<ls_pur_accitem>).

    "get SO id and SO item ID
    IF <ls_pur_accitem> IS ASSIGNED.

      DATA(so) = <ls_pur_accitem>-salesorder.
      DATA(soitem) = <ls_pur_accitem>-salesorderitem.

      "query sales order and get customer material number
      SELECT materialbycustomer FROM i_salesorderitem WITH PRIVILEGED ACCESS
         WHERE salesorder = @so AND salesorderitem = @soitem
           INTO @DATA(lt_cust_mat_number).
      ENDSELECT.

      "Assign Customer material number
      DATA(cust_mat_number) = lt_cust_mat_number.
      purchaseorderitemchange-yy1_customermaterialpo_pdi = cust_mat_number.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

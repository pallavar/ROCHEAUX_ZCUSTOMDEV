
CLASS lhc_afi DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR afi~calculatetotalprice.

    METHODS calculatetotalprice1 FOR DETERMINE ON SAVE
      IMPORTING keys FOR afi~calculatetotalprice1.

ENDCLASS.

CLASS lhc_afi IMPLEMENTATION.

  METHOD calculatetotalprice.
    DATA: lv_rcvd_qty          TYPE zrfai-rcvd_qty,
          lv_po_unit_price     TYPE zrfai-po_unit_price,
          lv_rcvd_amount       TYPE zrfai-rcvd_amount,
          lv_alloc_frt         TYPE zrfai-alloc_frt,
          lv_frt_value         TYPE zrfai-frt_value,
          lv_con_value         TYPE zrfai-con_value,
          lv_rcvd_amount_total TYPE zrfai-rcvd_amount.

    DATA afi_update TYPE TABLE FOR UPDATE zr_rfa\\afi.


    READ ENTITIES OF zr_rfa IN LOCAL MODE
      ENTITY afi BY \_af
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(afs).
    IF sy-subrc = 0.
      LOOP AT afs INTO DATA(af).
        READ ENTITIES OF zr_rfa IN LOCAL MODE
          ENTITY af BY \_fai
            FIELDS ( rcvdqty pounitprice )
            WITH VALUE #( ( %tky = af-%tky ) )
          RESULT DATA(afis).

        IF sy-subrc = 0.
          lv_rcvd_amount = '0000'.
          lv_rcvd_amount_total = '0000'.
          lv_alloc_frt = '0000'.
          lv_frt_value = '0000'.
          lv_con_value = '0000'.
          LOOP AT afis INTO DATA(afi).
            IF afi-rcvdqty IS NOT INITIAL AND afi-ordqty IS NOT INITIAL.
              lv_rcvd_amount_total = lv_rcvd_amount_total + ( afi-pounitprice * afi-rcvdqty ).
            ENDIF.
          ENDLOOP.

          LOOP AT afis INTO DATA(afi1).
            IF afi1-rcvdqty IS NOT INITIAL AND afi1-ordqty IS NOT INITIAL.
              lv_rcvd_amount = afi1-pounitprice * afi1-rcvdqty.
              lv_alloc_frt =  lv_rcvd_amount /  lv_rcvd_amount_total.
              lv_frt_value = af-freightamount *  lv_alloc_frt.
              lv_con_value = lv_frt_value * (   afi1-ordqty / afi1-rcvdqty ).


              APPEND VALUE #( %tky         = afi1-%tky
                                         rcvdamount   = lv_rcvd_amount
                                         allocfrt = lv_alloc_frt
                                        frtvalue =  lv_frt_value
                                        convalue = lv_con_value
                                  %control     = VALUE #( allocfrt    = if_abap_behv=>mk-on
                                  rcvdamount = if_abap_behv=>mk-on
                                  frtvalue = if_abap_behv=>mk-on
                                  convalue = if_abap_behv=>mk-on ) ) TO afi_update.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDLOOP.



      MODIFY ENTITIES OF  zr_rfa IN LOCAL MODE
      ENTITY afi
       UPDATE FIELDS ( rcvdamount frtvalue allocfrt convalue )
       WITH afi_update.
    ENDIF.
  ENDMETHOD.

  METHOD calculatetotalprice1.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_af DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR af RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR af RESULT result.

    METHODS retrievepo FOR MODIFY
      IMPORTING keys FOR ACTION af~retrievepo.

    METHODS retrievepo1 FOR MODIFY
      IMPORTING keys FOR ACTION af~retrievepo1 RESULT result.

    METHODS updatepocondition FOR MODIFY
      IMPORTING keys FOR ACTION af~updatepocondition.
    METHODS setcurrencycode FOR DETERMINE ON SAVE
      IMPORTING keys FOR af~setcurrencycode.

ENDCLASS.

CLASS lhc_af IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD retrievepo.


    READ ENTITIES OF zr_rfa IN LOCAL MODE
      ENTITY af

      ALL FIELDS WITH VALUE #( ( %key-faid = keys[ 1 ]-%key-faid ) )
      RESULT DATA(result_header).

    IF sy-subrc = 0.
      LOOP AT result_header INTO DATA(header).
        READ ENTITIES OF zr_rfa IN LOCAL MODE
          ENTITY af BY \_zfapo
          ALL FIELDS WITH VALUE #( ( %tky = header-%tky ) )
          RESULT DATA(fapos).

        IF sy-subrc = 0.

          DATA lt_item TYPE TABLE FOR DELETE zr_rfa\\afi.

          "Read instance item data that needs to be deleted
          READ ENTITIES OF zr_rfa IN LOCAL MODE """"""""""""""""""""""""
          ENTITY af BY \_fai
          ALL FIELDS WITH VALUE #( ( %tky = header-%tky ) )
          RESULT DATA(item).

          lt_item = CORRESPONDING #( item ).

          "Delete item
          MODIFY ENTITIES OF zr_rfa IN LOCAL MODE
          ENTITY afi
          DELETE FROM lt_item
          FAILED DATA(failed_item)
          REPORTED DATA(reported_item).

          LOOP AT fapos INTO DATA(fapo).
            DATA: lt_purchase_orders    TYPE TABLE OF i_purchaseorderapi01,
                  ls_purchase_orderitem TYPE i_purchaseorderitemapi01,
                  lt_fai                TYPE TABLE FOR CREATE zr_rfa\_fai,
                  ls_fai                LIKE LINE OF lt_fai,
                  lt_targetfai          LIKE ls_fai-%target,
                  ls_targetinvfai       LIKE LINE OF lt_targetfai,
                  lt_zpoitem            TYPE STANDARD TABLE OF i_purchaseorderitemapi01.

* " Delete existing data from the zr_rfa_fai table before adding new entries
*         MODIFY ENTITIES OF ZR_RFAI IN LOCAL MODE
*  ENTITY afi
*  DELETE FROM #( %tky = header-%tky )

            " Query the Purchase Order items
            SELECT * FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
              WHERE purchaseorder = @fapo-poid
              INTO TABLE @lt_zpoitem.

            IF sy-subrc = 0.
              LOOP AT lt_zpoitem INTO ls_purchase_orderitem.
                ls_targetinvfai = VALUE #(
                  %cid         = |CREATETEXTCID{ sy-tabix }|
                  orderpo      = ls_purchase_orderitem-purchaseorder
                  cukyfield    = ls_purchase_orderitem-documentcurrency
                  itemid       = ls_purchase_orderitem-purchaseorderitem
                  ordqty       = ls_purchase_orderitem-orderquantity
                  pounitprice  = ls_purchase_orderitem-netpriceamount
                  unitfield    = ls_purchase_orderitem-purchaseorderquantityunit
                  %control     = VALUE #(
                                    itemid      = if_abap_behv=>mk-on
                                    orderpo     = if_abap_behv=>mk-on
                                    ordqty      = if_abap_behv=>mk-on
                                    pounitprice = if_abap_behv=>mk-on
                                    cukyfield   = if_abap_behv=>mk-on )
                ).


                APPEND ls_targetinvfai TO lt_targetfai.
              ENDLOOP.

              ls_fai = VALUE #( %key-faid = header-faid %target = lt_targetfai ).
              APPEND ls_fai TO lt_fai.

              " MODIFY ENTITIES for each fapo
              MODIFY ENTITIES OF zr_rfa IN LOCAL MODE
                ENTITY af
                CREATE BY \_fai FROM lt_fai
                MAPPED DATA(mapped_data)
                FAILED DATA(failed_data)
                REPORTED DATA(reported_data).

              " Handle potential failures
              IF failed_data IS NOT INITIAL.
                " ... handle failures
              ENDIF.

              CLEAR: lt_fai, lt_targetfai.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD retrievepo1.
  ENDMETHOD.

  METHOD updatepocondition.

    READ ENTITIES OF zr_rfa IN LOCAL MODE
               ENTITY af
               ALL FIELDS WITH VALUE #( ( %key-faid = keys[ 1 ]-%key-faid ) )
               RESULT DATA(afs_val).

    IF sy-subrc = 0.
      LOOP AT afs_val INTO DATA(af_val).
        READ ENTITIES OF zr_rfa IN LOCAL MODE
          ENTITY af BY \_fai
            ALL FIELDS WITH VALUE #( ( %tky = af_val-%tky ) )
          RESULT DATA(afis_val_result).

        IF sy-subrc = 0.
          DATA: lt_item_pricing_element TYPE TABLE FOR CREATE i_purchaseorderitemtp_2\_purordpricingelement.

          LOOP AT afis_val_result INTO DATA(afi_val).

            MODIFY ENTITIES OF i_purchaseordertp_2 PRIVILEGED
               ENTITY purchaseorderitem
               CREATE BY \_purordpricingelement SET FIELDS WITH VALUE #(
                 ( %key-purchaseorder    = afi_val-orderpo
                   %key-purchaseorderitem = afi_val-itemid
                   %target = VALUE #( (
                     %cid                = |CID{ sy-tabix }|
                     conditiontype       = 'FVA1'
                     conditionrateamount = afi_val-convalue
                     conditioncurrency   = afi_val-cukyfield ) ) ) )
               MAPPED DATA(mapped_pe)
               FAILED DATA(failed_pe)
               REPORTED DATA(reported_pe).

            DATA(test) = `end`.

          ENDLOOP.

          " DATA: lt_po_header TYPE TABLE FOR CREATE i_purchaseordertp_2\_purchaseorderitem.


        ENDIF.

      ENDLOOP.
    ENDIF.


  ENDMETHOD.
  METHOD setcurrencycode.

    READ ENTITIES OF zr_rfa IN LOCAL MODE
      ENTITY af
      ALL FIELDS WITH VALUE #(  ( %key-faid = keys[ 1 ]-%key-faid ) )
      RESULT DATA(header).

    LOOP AT header INTO DATA(ls_h).
      DATA(fa_id) = ls_h-faid.
      DATA(fa_amount)  = ls_h-freightamount.
      DATA(curr_code) = ls_h-currencycode.


      IF fa_amount IS NOT INITIAL AND curr_code IS INITIAL.
        MODIFY ENTITIES OF zr_rfa IN LOCAL MODE
           ENTITY af
           UPDATE FROM VALUE #( (  %key-faid = fa_id
                                   %data-currencycode = 'USD'
                                   %control-currencycode = if_abap_behv=>mk-on
                               ) )
           FAILED DATA(faileddata)
           REPORTED DATA(reporteddata).
      ENDIF.

    ENDLOOP.

*
    DATA(ff) = '3'.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zr_rfa DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

*    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.
    METHODS adjust_numbers REDEFINITION.

ENDCLASS.

CLASS lsc_zr_rfa IMPLEMENTATION.

*  METHOD save_modified.
*
*
*  ENDMETHOD.


  METHOD cleanup_finalize.
  ENDMETHOD.

  METHOD adjust_numbers.
    "  SELECT MAX( fa_id )
    SELECT MAX( CAST( fa_id AS INT4 ) )
          FROM zrfa
            INTO @DATA(lv_max_faid) UP TO 1 ROWS.

    DATA: lv_unique_faid TYPE string.

    lv_unique_faid = lv_max_faid + 1.
    CONDENSE lv_unique_faid NO-GAPS.

    LOOP AT mapped-af REFERENCE INTO DATA(map).
      map->faid = lv_unique_faid.
    ENDLOOP.

    DATA(test) = '3'.
  ENDMETHOD.

ENDCLASS.

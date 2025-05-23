CLASS zcl_salescomm_addinvoice DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apj_dt_exec_object.
    INTERFACES if_apj_rt_exec_object.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SALESCOMM_ADDINVOICE IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
* You can Insert code here for getting Job parameters
  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.

*************************** HEADER ********************************************************************************
    "Declare date variables
    DATA: lv_from_date   TYPE d,
          lv_to_date     TYPE d,
          lv_system_date TYPE d.

    " Get the current system date
    lv_system_date = cl_abap_context_info=>get_system_date( ).

    "Get run date of Max RunID from sales commission table
    SELECT  rundate FROM zi_salescomt ORDER BY runid DESCENDING INTO @DATA(lastrundate) UP TO 1 ROWS.
    ENDSELECT.

    "Set commission from date, commission to date
    IF lastrundate IS INITIAL.
      lv_from_date = lv_system_date - 30.
    ELSE.
      lv_from_date = lastrundate.
    ENDIF.

    lv_to_date = lv_system_date - 1.

    IF lv_to_date  > lv_from_date. "added - 06.08.2024 """"" to be uncommented
      "IF lv_to_date IS NOT INITIAL.

      "Declare table type for sales commission table - used to create records
      DATA lt_run TYPE TABLE FOR CREATE zi_salescomt.
      DATA ls_run LIKE LINE OF lt_run.

      "assign values- header table type
      ls_run = VALUE #( %cid = 'runheader'
              processingstatus = 'Not Started'
              commissionfromdate = lv_from_date
              commissiontodate = lv_to_date
              rundate = lv_system_date
              commissionpaymentstatus = 'Unpaid Invoices' "'1'
              invoicepaymentstatus = 'Unpaid'   "'1'
              %control = VALUE #( processingstatus = if_abap_behv=>mk-on
              commissionfromdate = if_abap_behv=>mk-on
              commissiontodate = if_abap_behv=>mk-on
              rundate = if_abap_behv=>mk-on
              commissionpaymentstatus = if_abap_behv=>mk-on
              invoicepaymentstatus = if_abap_behv=>mk-on
              ) ).

      "Append header values
      APPEND ls_run TO lt_run.


*************************** ITEM ********************************************************************************

      "Declare internal table and work area - Billing Document Header
      TYPES : BEGIN OF ty_billingdoc,
                billingdocument    TYPE i_billingdocument-billingdocument,
                clearingstatus     TYPE i_billingdocument-invoiceclearingstatus,
                accountingdocument TYPE i_billingdocument-accountingdocument,

*                ****************** Pallava added 21052025***************
                billingdoctype    TYPE i_billingdocument-BillingDocumentType,

              END OF ty_billingdoc.

      DATA: lt_billingdocu TYPE TABLE OF ty_billingdoc,
            ls_billingdocu TYPE ty_billingdoc.

      "Declare internal table and work area - Billing Document Item
      TYPES : BEGIN OF ty_billingdocitem,
                item              TYPE i_billingdocumentitem-billingdocumentitem,
                customergroup     TYPE i_billingdocumentitem-customergroup,
                product           TYPE i_billingdocumentitem-product,
                salesdocument     TYPE i_billingdocumentitem-salesdocument,
                quantity          TYPE i_billingdocumentitem-billingquantity,
                uom               TYPE i_billingdocumentitem-billingquantityunit,
                salesorganisation TYPE i_billingdocumentitem-salesorganization,
                customerid        TYPE i_billingdocumentitem-payerparty,
                invoicedate       TYPE i_billingdocumentitem-billingdocumentdate,
                amount            TYPE i_billingdocumentitem-netamount,
                amountcurrency    TYPE i_billingdocumentitem-transactioncurrency,
                salesorder        TYPE i_billingdocumentitem-salesdocument,
              END OF ty_billingdocitem.

      DATA: lt_billingdocuitem TYPE TABLE OF ty_billingdocitem,
            ls_billingdocuitem TYPE ty_billingdocitem.


      "Declare internal table and work area - Billing Document Item price element
      TYPES : BEGIN OF ty_billdocitemprcelement1,
                rateamount     TYPE i_billingdocumentitemprcgelmnt-conditionrateamount,
                commamount     TYPE i_billingdocumentitemprcgelmnt-conditionamount,
                commamountcurr TYPE i_billingdocumentitemprcgelmnt-transactioncurrency,
                " commratecurr   TYPE i_billingdocumentitemprcgelmnt-conditioncurrency,
              END OF ty_billdocitemprcelement1.

      TYPES : BEGIN OF ty_billdocitemprcelement2,
                rateamount     TYPE i_billingdocumentitemprcgelmnt-conditionrateamount,
                commamount     TYPE i_billingdocumentitemprcgelmnt-conditionamount,
                commamountcurr TYPE i_billingdocumentitemprcgelmnt-transactioncurrency,
                " commratecurr   TYPE i_billingdocumentitemprcgelmnt-conditioncurrency,
              END OF ty_billdocitemprcelement2.

      TYPES : BEGIN OF ty_billdocitemprcelement3,
                rateamount     TYPE i_billingdocumentitemprcgelmnt-conditionrateamount,
                commamount     TYPE i_billingdocumentitemprcgelmnt-conditionamount,
                commamountcurr TYPE i_billingdocumentitemprcgelmnt-transactioncurrency,
                " commratecurr   TYPE i_billingdocumentitemprcgelmnt-conditioncurrency,
              END OF ty_billdocitemprcelement3.

      DATA: lt_billdocitemprcelement1 TYPE TABLE OF ty_billdocitemprcelement1,
            ls_billdocitemprcelement1 TYPE ty_billdocitemprcelement1.

      DATA: lt_billdocitemprcelement2 TYPE TABLE OF ty_billdocitemprcelement2,
            ls_billdocitemprcelement2 TYPE ty_billdocitemprcelement2.

      DATA: lt_billdocitemprcelement3 TYPE TABLE OF ty_billdocitemprcelement3,
            ls_billdocitemprcelement3 TYPE ty_billdocitemprcelement3.

      "Declare internal table and work area - Sales order
      TYPES : BEGIN OF ty_salesorderitem,
                unitprice         TYPE i_salesorderitem-netpriceamount,
                unitpricequantity TYPE i_salesorderitem-netpricequantity,
                unitpricecurrency TYPE i_salesorderitem-transactioncurrency,
              END OF ty_salesorderitem.

      DATA: lt_salesorderitem TYPE TABLE OF ty_salesorderitem,
            ls_salesorderitem TYPE ty_salesorderitem.


      DATA: amount_f          TYPE f.
      DATA  amount            TYPE p LENGTH 10 DECIMALS 2.

      DATA: unitprice_f       TYPE f.
      DATA  unitprice         TYPE p LENGTH 10 DECIMALS 2.

     DATA: commissionamount_f TYPE f.
     DATA  commissionamount  TYPE p LENGTH 10 DECIMALS 2.





*      "Query billing document, payment status = fully cleared, posting status = completed
****************** Pallava added billingdoctype on 21052025***************

      SELECT billingdocument,invoiceclearingstatus,accountingdocument,billingdocumenttype
        FROM i_billingdocument  WITH PRIVILEGED ACCESS
        "WHERE billingdocument BETWEEN '0090000004' AND '0090000011'
       " WHERE billingdocument = '0090000007'
          WHERE accountingpostingstatus = 'C' AND billingdocumentdate BETWEEN @lv_from_date AND @lv_to_date "C- Posted status
         INTO TABLE @lt_billingdocu. " UP TO 100 ROWS.


*    " Check if data is retrieved
      IF sy-subrc = 0.
        LOOP AT lt_billingdocu INTO ls_billingdocu.

          "Query billing document item
          SELECT billingdocumentitem, customergroup, product, salesdocument, billingquantity, billingquantityunit, salesorganization, payerparty, billingdocumentdate,
          netamount, transactioncurrency, salesdocument
          FROM i_billingdocumentitem
          WHERE billingdocument = @ls_billingdocu-billingdocument
          "AND billingdocumentitem = '000030'
           AND
*           Pallava addded on 21-01-2025
            billingquantity <> 0
*           Pallava addded on 21-01-2025
          INTO TABLE @lt_billingdocuitem.

          IF sy-subrc = 0.
            LOOP AT lt_billingdocuitem INTO ls_billingdocuitem.
              DATA(item) = ls_billingdocuitem-item.
              DATA(product) = ls_billingdocuitem-product.
              DATA(salesdocument) = ls_billingdocuitem-salesdocument.
              DATA(quantity) = ls_billingdocuitem-quantity.

*              Code added by Pallava on 04042025

*              DATA(amount) = ls_billingdocuitem-amount.

******************* Pallava added billingdoctype on 21052025***************

            amount_f = ls_billingdocuitem-amount.

            IF ls_billingdocu-billingdoctype = 'G2' OR ls_billingdocu-billingdoctype = 'S1' OR ls_billingdocu-billingdoctype = 'CBRE'.
             amount = - amount_f.
            ELSE.
            amount = amount_f.
            ENDIF.

            amount = amount_f.

*            Code added by Pallava on 04042025

              "Query customer ID to get customer name
              SELECT SINGLE customername
              FROM i_customer
              WHERE customer  = @ls_billingdocuitem-customerid


              INTO @DATA(retrievedcustomername) .

              "Query billing document pricing element
              SELECT conditionratevalue,conditionamount,transactioncurrency
              FROM i_billingdocumentitemprcgelmnt
              WHERE billingdocument = @ls_billingdocu-billingdocument AND billingdocumentitem = @ls_billingdocuitem-item
                                                                         AND conditiontype = 'PPR0'
              INTO TABLE @lt_billdocitemprcelement1.
              IF sy-subrc = 0.
                LOOP AT lt_billdocitemprcelement1 INTO ls_billdocitemprcelement1.
                  DATA(conditionamount1) = ls_billdocitemprcelement1-commamount.  "PPR0 Condition value
                  DATA(ppro_trans_currency) = ls_billdocitemprcelement1-commamountcurr.
                ENDLOOP.
              ENDIF.

              "other charges = Net Amount - PPR0 Condition value
              DATA: other_charges_amt TYPE p LENGTH 10 DECIMALS 2.
*              other_charges_amt =  conditionamount1 - amount.

*               Changed vice versa formula
               other_charges_amt =  amount - conditionamount1.

              SELECT conditionratevalue,conditionamount,transactioncurrency
              FROM i_billingdocumentitemprcgelmnt
              WHERE billingdocument = @ls_billingdocu-billingdocument AND billingdocumentitem = @ls_billingdocuitem-item
                                                                          AND conditiontype = 'PCIP'
              INTO TABLE @lt_billdocitemprcelement2.
              IF sy-subrc = 0.
                LOOP AT lt_billdocitemprcelement2 INTO ls_billdocitemprcelement2.
                  DATA(conditionamount2) = ls_billdocitemprcelement2-commamount.
                ENDLOOP.
              ENDIF.


*              DATA(total) = conditionamount1 - conditionamount2.
*              "DATA(gp) = total / conditionamount1 * total.   "DIV BY ZERO TO BE HANDLED
*              DATA(gp) = total / conditionamount1 * 100.   "DIV BY ZERO TO BE HANDLED

              DATA: lv_text             TYPE string,
                    lv_position         TYPE i,
                    lv_substring_before TYPE string,
                    lv_substring_after  TYPE string,
                    lv_gp               TYPE string,
                    total               TYPE string.

              total = conditionamount1 - conditionamount2.
              IF conditionamount1 IS NOT INITIAL OR conditionamount1 <> 0 .   "17/01/2025 TO AVOID Runtime Error: 'BCD_ZERODIVIDE'
                lv_text = total / conditionamount1 * 100.   "DIV BY ZERO TO BE HANDLED
              ELSE.
*                lv_text = total * 100.
                 lv_text = 0.
              ENDIF.

              IF lv_text <> 0.
                " Find the position of the dot (.)
                FIND '.' IN lv_text MATCH OFFSET lv_position.

                " Extract the substring starting from the dot (.)
                lv_substring_after = substring( val = lv_text off = lv_position + 1 len = 2 ).

                lv_substring_before = substring(  val = lv_text off = 0 len = lv_position ).
                lv_gp = lv_substring_before && '.' && lv_substring_after.
              ELSE.
                lv_gp = lv_text.
              ENDIF.

              SELECT conditionratevalue,conditionamount,transactioncurrency
              FROM i_billingdocumentitemprcgelmnt
              WHERE billingdocument = @ls_billingdocu-billingdocument AND billingdocumentitem = @ls_billingdocuitem-item AND conditiontype = 'ZC01'
              INTO TABLE @lt_billdocitemprcelement3.

              IF sy-subrc = 0.
                LOOP AT lt_billdocitemprcelement3 INTO ls_billdocitemprcelement3.
                  DATA(commrate) = ls_billdocitemprcelement3-rateamount.
*                  DATA(commissionamount) = ls_billdocitemprcelement3-commamount.

                    commissionamount_f = ls_billdocitemprcelement3-commamount.
                    commissionamount = commissionamount_f.

                  DATA(trans_currency) = ls_billdocitemprcelement3-commamountcurr.
                  "    DATA(rate_currency) = ls_billdocitemprcelement-commratecurr.
                ENDLOOP.
              ENDIF.

              "Query sales order
              SELECT SINGLE salesorderdate FROM i_salesorder
                WHERE salesorder = @ls_billingdocuitem-salesdocument
                  INTO @DATA(sales_order_date).

              "Query sales order item
              SELECT netpriceamount, netpricequantity, transactioncurrency
              FROM i_salesorderitem
              WHERE  salesorder = @ls_billingdocuitem-salesdocument AND salesorderitem = @ls_billingdocuitem-item  "??ITEM
              INTO TABLE @lt_salesorderitem.

              IF sy-subrc = 0.
                LOOP AT lt_salesorderitem INTO ls_salesorderitem.
*                 DATA(unitprice) = ls_salesorderitem-unitprice.
                  unitprice_f = ls_salesorderitem-unitprice.
                  unitprice = unitprice_f.

                  DATA(unitpricequantity) =  ls_salesorderitem-unitpricequantity.
                  IF unitpricequantity IS NOT INITIAL OR unitpricequantity <> 0.  "17/01/2025 TO AVOID Runtime Error: 'BCD_ZERODIVIDE'
                    unitprice = unitprice / unitpricequantity.
                  ELSE.
*                   unitprice = unitprice.
                    unitprice = 0.
                  ENDIF.
                  DATA(unitpricecurrency) = ls_salesorderitem-unitpricecurrency.
                ENDLOOP.
              ENDIF.



              ""Declare sales commission table item node variables- used to create records
              DATA lt_invsalescom TYPE TABLE FOR CREATE zi_salescomt\_invsalescom.
              DATA ls_invsalescom LIKE LINE OF lt_invsalescom.

              DATA lt_targetinvsalescom LIKE ls_invsalescom-%target.
              DATA ls_targetinvsalescom LIKE LINE OF lt_targetinvsalescom.

              "Append item values
              ls_targetinvsalescom = VALUE #( %cid = 'invoiceitem' && ls_billingdocu-billingdocument  && ls_billingdocuitem-item
              "ls_targetinvsalescom = VALUE #( %cid = |CREATETEXTCID{ sy-tabix }|
                                       billingdocumentid   = ls_billingdocu-billingdocument
                                       commissionstatus = 'Unpaid Invoices' "'1'
                                       commissionpayfromdate = lv_from_date  "lv_system_date - 30
                                       commissionpaytodate = lv_to_date  "lv_system_date - 1
                                       salesman =  ls_billingdocuitem-customergroup  "'01'
                                       clearingstatus = ls_billingdocu-clearingstatus
                                       salesorderid = |{ ls_billingdocuitem-salesdocument ALPHA = OUT }|
                                       salesorderdate = sales_order_date  "ls_salesorder-salesorderdate
                                       billingdocumentdate = ls_billingdocuitem-invoicedate

*                                       ******************* Pallava added billingdoctype on 21052025***************
*                                      amount = ls_billingdocuitem-amount
                                       amount = amount

                                       amountcurrencycode = ls_billingdocuitem-amountcurrency
                                       customerid = |{ ls_billingdocuitem-customerid ALPHA = OUT }|
                                       customername = retrievedcustomername
                                       salesorganisation = ls_billingdocuitem-salesorganisation
                                       itemid = ls_billingdocuitem-item
                                       materialid = ls_billingdocuitem-product
                                       quantity = ls_billingdocuitem-quantity
                                       quantityuom = ls_billingdocuitem-uom
                                       unitprice = unitprice   "ls_salesorderitem-unitprice
                                       unitpricecurrencycode = ls_salesorderitem-unitpricecurrency
                                       gppercentage = lv_gp   "gp
                                       commissionrate = commrate
                                       commissionamount = commissionamount
                                       commamtcurrencycode = trans_currency
                                       lastupdateat = lv_system_date
                                       othercharges = other_charges_amt
                                       otherchargescurrencycode = ppro_trans_currency
                                       accountingdocument = ls_billingdocu-accountingdocument
                                       %control = VALUE #( billingdocumentid = if_abap_behv=>mk-on
                                                           commissionstatus = if_abap_behv=>mk-on
                                                           salesman = if_abap_behv=>mk-on
                                                           salesorderid = if_abap_behv=>mk-on
                                                           salesorderdate = if_abap_behv=>mk-on
                                                           materialid = if_abap_behv=>mk-on
                                                           quantity = if_abap_behv=>mk-on
                                                           customerid = if_abap_behv=>mk-on
                                                           customername = if_abap_behv=>mk-on
                                                           salesorganisation = if_abap_behv=>mk-on
                                                           amount = if_abap_behv=>mk-on
                                                           quantityuom = if_abap_behv=>mk-on
                                                           amountcurrencycode = if_abap_behv=>mk-on
                                                           billingdocumentdate = if_abap_behv=>mk-on
                                                           unitprice = if_abap_behv=>mk-on
                                                           unitpricecurrencycode = if_abap_behv=>mk-on
                                                           gppercentage = if_abap_behv=>mk-on
                                                           commissionpayfromdate = if_abap_behv=>mk-on
                                                           commissionpaytodate = if_abap_behv=>mk-on
                                                           commissionrate = if_abap_behv=>mk-on
                                                           clearingstatus = if_abap_behv=>mk-on
                                                           itemid = if_abap_behv=>mk-on
                                                           commissionamount = if_abap_behv=>mk-on
                                                           commamtcurrencycode = if_abap_behv=>mk-on
                                                           lastupdateat = if_abap_behv=>mk-on
                                                           othercharges = if_abap_behv=>mk-on
                                                           otherchargescurrencycode = if_abap_behv=>mk-on
                                                           accountingdocument = if_abap_behv=>mk-on

                                                            ) ).

              APPEND ls_targetinvsalescom TO lt_targetinvsalescom.

              ls_invsalescom =   VALUE #( %cid_ref = 'runheader' %target = lt_targetinvsalescom ).
              APPEND ls_invsalescom TO lt_invsalescom.
              CLEAR lt_targetinvsalescom.
              CLEAR commrate.
              CLEAR commissionamount.
              CLEAR trans_currency.
              CLEAR lv_gp.

              CLEAR other_charges_amt.
              CLEAR ppro_trans_currency.


            ENDLOOP.        "billdoc Item
          ENDIF.           "if value exist in billdoc Item
        ENDLOOP.           "BILL DOC - lt_billingdocu

        DATA(temp) = lt_invsalescom.
        DATA(temprun) = lt_run.

        "Create records in sales commission table
        MODIFY ENTITIES OF zi_salescomt
           ENTITY salescommission
           CREATE FROM lt_run
           CREATE BY \_invsalescom FROM lt_invsalescom
           MAPPED DATA(mapped_data)
           FAILED DATA(failed_data)
           REPORTED DATA(reported_data).
        COMMIT ENTITIES  .

        DATA(test) = '2'.

      ENDIF.


      "success or fail
      IF ( failed_data IS INITIAL OR reported_data IS INITIAL ).
        COMMIT ENTITIES .

        TRY.
            "Application Log
            DATA(l_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object =
                     'ZSALESCOMT_APPL_LOG' subobject = 'ZSUBOBJECT1' ) ).

            DATA(l_free_text) = cl_bali_free_text_setter=>create( severity =
                     if_bali_constants=>c_severity_information
                     text = 'Records created: ' && 'To Date: ' && lv_to_date && ', From Date: ' && lv_from_date ).

            l_log->add_item( item = l_free_text ).

            "Save the log into the database
            cl_bali_log_db=>get_instance( )->save_log( log = l_log assign_to_current_appl_job = abap_true ).
            COMMIT WORK.
          CATCH cx_bali_runtime INTO DATA(lx_bali_runtime).
        ENDTRY.

      ELSE.
        TRY.
            "Application Log
            l_log = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object =
                     'ZSALESCOMT_APPL_LOG' subobject = 'ZSUBOBJECT1' ) ).

            l_free_text = cl_bali_free_text_setter=>create( severity =
                      if_bali_constants=>c_severity_error
                      text = 'Records Not created' ).

            l_log->add_item( item = l_free_text ).

            "Save the log into the database
            cl_bali_log_db=>get_instance( )->save_log( log = l_log assign_to_current_appl_job = abap_true ).
            COMMIT WORK.
          CATCH cx_bali_runtime INTO DATA(lx_bali_runtime3).
            " Handle the exception, e.g., log it or display a message
        ENDTRY.

      ENDIF.


**        "Add a message as item to the log
*    DATA(l_message) = cl_bali_message_setter=>create( severity =
*                     if_bali_constants=>c_severity_information
*                                                       id = 'PO'
*                                                      number = '000' ).
*    l_log->add_item( item = l_message ).
*
**        "Add a second message, this time from system fields SY-MSGID, ...
*    "MESSAGE ID 'ZTEST' TYPE 'S' NUMBER '058' INTO DATA(l_text).
*
*    l_log->add_item( item = cl_bali_message_setter=>create_from_sy( ) ).
*
*    "Add a free text to the log
*
*    DATA(l_free_text) = cl_bali_free_text_setter=>create( severity =
*                        if_bali_constants=>c_severity_information
*                        text = 'Success' ).
*
*    l_log->add_item( item = l_free_text ).
*
*    " Add an exception to the log
*    DATA: i TYPE i.
*    TRY.
*        i = 1 / 0.
*      CATCH cx_sy_zerodivide INTO DATA(l_ref).
*    ENDTRY.
*
*    DATA(l_exception) = cl_bali_exception_setter=>create( severity =
*                               if_bali_constants=>c_severity_information
*                               exception = l_ref ).
*    l_log->add_item( item = l_exception ).
*
*    "Save the log into the database
*    cl_bali_log_db=>get_instance( )->save_log( log = l_log assign_to_current_appl_job = abap_true ).
*    COMMIT WORK.
    ELSE.
      TRY.
          "Application Log
          l_log = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object =
                   'ZSALESCOMT_APPL_LOG' subobject = 'ZSUBOBJECT1' ) ).

          l_free_text = cl_bali_free_text_setter=>create( severity =
                    if_bali_constants=>c_severity_warning
                    text = 'Records already processed for commission date range' ).

          l_log->add_item( item = l_free_text ).

          "Save the log into the database
          cl_bali_log_db=>get_instance( )->save_log( log = l_log assign_to_current_appl_job = abap_true ).
          COMMIT WORK.
        CATCH cx_bali_runtime INTO DATA(lx_bali_runtime1).
      ENDTRY.
    ENDIF.  "added if condition - 06.08.2024
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
***************************** HEADER ********************************************************************************
    "Declare date variables
    DATA: lv_from_date   TYPE d,
          lv_to_date     TYPE d,
          lv_system_date TYPE d.

    " Get the current system date
    lv_system_date = cl_abap_context_info=>get_system_date( ).

    "Get run date of Max RunID from sales commission table
    SELECT  rundate FROM zi_salescomt ORDER BY runid DESCENDING INTO @DATA(lastrundate) UP TO 1 ROWS.
    ENDSELECT.

    "Set commission from date, commission to date
    IF lastrundate IS INITIAL.
      lv_from_date = lastrundate - 30.
    ELSE.
      lv_from_date = lastrundate.
    ENDIF.

    lv_to_date = lv_system_date - 1.
*
*    IF lv_to_date  > lv_from_date. "added - 06.08.2024
*
    "Declare table type for sales commission table - used to create records
    DATA lt_run TYPE TABLE FOR CREATE zi_salescomt.
    DATA ls_run LIKE LINE OF lt_run.

    "assign values- header table type
    ls_run = VALUE #( %cid = 'runheader'
            processingstatus = 'Not Started'
            commissionfromdate = lv_from_date
            commissiontodate = lv_to_date
            rundate = lv_system_date
            commissionpaymentstatus = 'Unpaid Invoices' "'1'
            invoicepaymentstatus = 'Unpaid'   "'1'
            %control = VALUE #( processingstatus = if_abap_behv=>mk-on
            commissionfromdate = if_abap_behv=>mk-on
            commissiontodate = if_abap_behv=>mk-on
            rundate = if_abap_behv=>mk-on
            commissionpaymentstatus = if_abap_behv=>mk-on
            invoicepaymentstatus = if_abap_behv=>mk-on
            ) ).

    "Append header values
    APPEND ls_run TO lt_run.


**************************** ITEM ********************************************************************************

    "Declare internal table and work area - Billing Document Header
    TYPES : BEGIN OF ty_billingdoc,
              billingdocument    TYPE i_billingdocument-billingdocument,
              clearingstatus     TYPE i_billingdocument-invoiceclearingstatus,
              accountingdocument TYPE i_billingdocument-accountingdocument,
            END OF ty_billingdoc.

    DATA: lt_billingdocu TYPE TABLE OF ty_billingdoc,
          ls_billingdocu TYPE ty_billingdoc.

    "Declare internal table and work area - Billing Document Item
    TYPES : BEGIN OF ty_billingdocitem,
              item              TYPE i_billingdocumentitem-billingdocumentitem,
              customergroup     TYPE i_billingdocumentitem-customergroup,
              product           TYPE i_billingdocumentitem-product,
              salesdocument     TYPE i_billingdocumentitem-salesdocument,
              quantity          TYPE i_billingdocumentitem-billingquantity,
              uom               TYPE i_billingdocumentitem-billingquantityunit,
              salesorganisation TYPE i_billingdocumentitem-salesorganization,
              customerid        TYPE i_billingdocumentitem-payerparty,
              invoicedate       TYPE i_billingdocumentitem-billingdocumentdate,
              amount            TYPE i_billingdocumentitem-netamount,
              amountcurrency    TYPE i_billingdocumentitem-transactioncurrency,
              salesorder        TYPE i_billingdocumentitem-salesdocument,
            END OF ty_billingdocitem.

    DATA: lt_billingdocuitem TYPE TABLE OF ty_billingdocitem,
          ls_billingdocuitem TYPE ty_billingdocitem.


    "Declare internal table and work area - Billing Document Item price element
    TYPES : BEGIN OF ty_billdocitemprcelement1,
              rateamount     TYPE i_billingdocumentitemprcgelmnt-conditionrateamount,
              commamount     TYPE i_billingdocumentitemprcgelmnt-conditionamount,
              commamountcurr TYPE i_billingdocumentitemprcgelmnt-transactioncurrency,
            END OF ty_billdocitemprcelement1.

    TYPES : BEGIN OF ty_billdocitemprcelement2,
              rateamount     TYPE i_billingdocumentitemprcgelmnt-conditionrateamount,
              commamount     TYPE i_billingdocumentitemprcgelmnt-conditionamount,
              commamountcurr TYPE i_billingdocumentitemprcgelmnt-transactioncurrency,
            END OF ty_billdocitemprcelement2.

    TYPES : BEGIN OF ty_billdocitemprcelement3,
              rateamount     TYPE i_billingdocumentitemprcgelmnt-conditionrateamount,
              commamount     TYPE i_billingdocumentitemprcgelmnt-conditionamount,
              commamountcurr TYPE i_billingdocumentitemprcgelmnt-transactioncurrency,
            END OF ty_billdocitemprcelement3.

    DATA: lt_billdocitemprcelement1 TYPE TABLE OF ty_billdocitemprcelement1,
          ls_billdocitemprcelement1 TYPE ty_billdocitemprcelement1.

    DATA: lt_billdocitemprcelement2 TYPE TABLE OF ty_billdocitemprcelement2,
          ls_billdocitemprcelement2 TYPE ty_billdocitemprcelement2.

    DATA: lt_billdocitemprcelement3 TYPE TABLE OF ty_billdocitemprcelement3,
          ls_billdocitemprcelement3 TYPE ty_billdocitemprcelement3.

    "Declare internal table and work area - Sales order
    TYPES : BEGIN OF ty_salesorderitem,
              unitprice         TYPE i_salesorderitem-netpriceamount,
              unitpricequantity TYPE i_salesorderitem-netpricequantity,
              unitpricecurrency TYPE i_salesorderitem-transactioncurrency,
            END OF ty_salesorderitem.

    DATA: lt_salesorderitem TYPE TABLE OF ty_salesorderitem,
          ls_salesorderitem TYPE ty_salesorderitem.

    """"""""""""""""""""
    "Query billing document, payment status = fully cleared, posting status = completed
    SELECT billingdocument,invoiceclearingstatus,accountingdocument
      FROM i_billingdocument  WITH PRIVILEGED ACCESS
     "WHERE billingdocument = '0090000004' AND '0090000011'
     "  WHERE billingdocument BETWEEN '0090000004' AND '0090000011'
      WHERE billingdocument = '0090000069'
     "  WHERE accountingpostingstatus = 'C' AND billingdocumentdate BETWEEN @lv_from_date AND @lv_to_date "C- Posted status
       INTO TABLE @lt_billingdocu. " UP TO 100 ROWS.


*    " Check if data is retrieved
    IF sy-subrc = 0.
      LOOP AT lt_billingdocu INTO ls_billingdocu.

        "Query billing document item
        SELECT billingdocumentitem, customergroup, product, salesdocument, billingquantity, billingquantityunit, salesorganization, payerparty, billingdocumentdate,
        netamount, transactioncurrency, salesdocument
        FROM i_billingdocumentitem
        WHERE billingdocument = @ls_billingdocu-billingdocument
*        AND billingdocumentitem = '000030'

        INTO TABLE @lt_billingdocuitem.

        IF sy-subrc = 0.
          LOOP AT lt_billingdocuitem INTO ls_billingdocuitem.
            DATA(item) = ls_billingdocuitem-item.
            DATA(product) = ls_billingdocuitem-product.
            DATA(salesdocument) = ls_billingdocuitem-salesdocument.
            DATA(quantity) = ls_billingdocuitem-quantity.
            DATA(amount) = ls_billingdocuitem-amount. "net amount


            "Query customer ID to get customer name
            SELECT SINGLE customername
            FROM i_customer
            WHERE customer  = @ls_billingdocuitem-customerid
            INTO @DATA(retrievedcustomername) .
            """""


            "Query billing document pricing element
            SELECT conditionratevalue,conditionamount,transactioncurrency
            FROM i_billingdocumentitemprcgelmnt
            WHERE billingdocument = @ls_billingdocu-billingdocument AND billingdocumentitem = @ls_billingdocuitem-item
                                                                       AND conditiontype = 'PPR0'
            INTO TABLE @lt_billdocitemprcelement1.
            IF sy-subrc = 0.
              LOOP AT lt_billdocitemprcelement1 INTO ls_billdocitemprcelement1.
                DATA(conditionamount1) = ls_billdocitemprcelement1-commamount.
                DATA(ppro_trans_currency) = ls_billdocitemprcelement1-commamountcurr.
              ENDLOOP.
            ENDIF.

            "other charges = PPR0 Condition value - Net Amount
            DATA: other_charges_amt TYPE p LENGTH 10 DECIMALS 2.
            other_charges_amt =  conditionamount1 - amount.

            SELECT conditionratevalue,conditionamount,transactioncurrency
            FROM i_billingdocumentitemprcgelmnt
            WHERE billingdocument = @ls_billingdocu-billingdocument AND billingdocumentitem = @ls_billingdocuitem-item
                                                                        AND conditiontype = 'PCIP'
            INTO TABLE @lt_billdocitemprcelement2.
            IF sy-subrc = 0.
              LOOP AT lt_billdocitemprcelement2 INTO ls_billdocitemprcelement2.
                DATA(conditionamount2) = ls_billdocitemprcelement2-commamount.
              ENDLOOP.
            ENDIF.


            DATA: lv_text             TYPE string,
                  lv_position         TYPE i,
                  lv_substring_before TYPE string,
                  lv_substring_after  TYPE string,
                  lv_gp               TYPE string,
                  total               TYPE string.

            total = conditionamount1 - conditionamount2.
            lv_text = total / conditionamount1 * 100.   "DIV BY ZERO TO BE HANDLED

            IF lv_text <> 0.

              " Find the position of the dot (.)
              FIND '.' IN lv_text MATCH OFFSET lv_position.

              " Extract the substring starting from the dot (.)
              lv_substring_after = substring( val = lv_text off = lv_position + 1 len = 2 ).

              lv_substring_before = substring(  val = lv_text off = 0 len = lv_position ).
              lv_gp = lv_substring_before && '.' && lv_substring_after.
            ELSE.
              lv_gp = lv_text.
            ENDIF.


            SELECT conditionratevalue,conditionamount,transactioncurrency
            FROM i_billingdocumentitemprcgelmnt
            WHERE billingdocument = @ls_billingdocu-billingdocument AND billingdocumentitem = @ls_billingdocuitem-item AND conditiontype = 'DRN1'
            INTO TABLE @lt_billdocitemprcelement3.

            IF sy-subrc = 0.
              LOOP AT lt_billdocitemprcelement3 INTO ls_billdocitemprcelement3.
                DATA(commrate) = ls_billdocitemprcelement3-rateamount.
                DATA(commissionamount) = ls_billdocitemprcelement3-commamount.
                DATA(trans_currency) = ls_billdocitemprcelement3-commamountcurr.
              ENDLOOP.
            ENDIF.

            "Query sales order
            SELECT SINGLE salesorderdate FROM i_salesorder
              WHERE salesorder = @ls_billingdocuitem-salesdocument
                INTO @DATA(sales_order_date).

            "Query sales order item
            SELECT netpriceamount, netpricequantity, transactioncurrency
            FROM i_salesorderitem
            WHERE  salesorder = @ls_billingdocuitem-salesdocument AND salesorderitem = @ls_billingdocuitem-item  "??ITEM
            INTO TABLE @lt_salesorderitem.

            IF sy-subrc = 0.
              LOOP AT lt_salesorderitem INTO ls_salesorderitem.
                DATA(unitprice) = ls_salesorderitem-unitprice.
                DATA(unitpricequantity) =  ls_salesorderitem-unitpricequantity.
                unitprice = unitprice / unitpricequantity.
                DATA(unitpricecurrency) = ls_salesorderitem-unitpricecurrency.
              ENDLOOP.
            ENDIF.

            ""Declare sales commission table item node variables- used to create records
            DATA lt_invsalescom TYPE TABLE FOR CREATE zi_salescomt\_invsalescom.
            DATA ls_invsalescom LIKE LINE OF lt_invsalescom.

            DATA lt_targetinvsalescom LIKE ls_invsalescom-%target.
            DATA ls_targetinvsalescom LIKE LINE OF lt_targetinvsalescom.

            "Append item values
            ls_targetinvsalescom = VALUE #( %cid = 'invoiceitem' && ls_billingdocu-billingdocument  && ls_billingdocuitem-item
           "ls_targetinvsalescom = VALUE #( %cid = |CREATETEXTCID{ sy-tabix }|
                                    billingdocumentid   = ls_billingdocu-billingdocument
                                    commissionstatus = 'Unpaid Invoices' "'1'
                                    commissionpayfromdate = lv_from_date "lv_system_date - 30
                                    commissionpaytodate = lv_to_date  "lv_system_date - 1
                                    salesman =  ls_billingdocuitem-customergroup  "'01'
                                    clearingstatus = ls_billingdocu-clearingstatus
                                    salesorderid = |{ ls_billingdocuitem-salesdocument ALPHA = OUT }|
                                    salesorderdate = sales_order_date  "ls_salesorder-salesorderdate
                                    billingdocumentdate = ls_billingdocuitem-invoicedate
                                    amount = ls_billingdocuitem-amount
                                    amountcurrencycode = ls_billingdocuitem-amountcurrency
                                    customerid = |{ ls_billingdocuitem-customerid ALPHA = OUT }|
                                    customername = retrievedcustomername
                                    salesorganisation = ls_billingdocuitem-salesorganisation
                                    itemid = ls_billingdocuitem-item
                                    materialid = ls_billingdocuitem-product
                                    quantity = ls_billingdocuitem-quantity
                                    quantityuom = ls_billingdocuitem-uom
                                    unitprice = unitprice   "ls_salesorderitem-unitprice
                                    unitpricecurrencycode = ls_salesorderitem-unitpricecurrency
                                    gppercentage = lv_gp " gp
                                    commissionrate = commrate
                                    commissionamount = commissionamount
                                    commamtcurrencycode = trans_currency """"""
                                    lastupdateat = lv_system_date
                                    othercharges = other_charges_amt
                                    otherchargescurrencycode = ppro_trans_currency
                                    accountingdocument = ls_billingdocu-accountingdocument
                                    %control = VALUE #( billingdocumentid = if_abap_behv=>mk-on
                                                        commissionstatus = if_abap_behv=>mk-on
                                                        salesman = if_abap_behv=>mk-on
                                                        salesorderid = if_abap_behv=>mk-on
                                                        salesorderdate = if_abap_behv=>mk-on
                                                        materialid = if_abap_behv=>mk-on
                                                        quantity = if_abap_behv=>mk-on
                                                        customerid = if_abap_behv=>mk-on
                                                        customername = if_abap_behv=>mk-on
                                                        salesorganisation = if_abap_behv=>mk-on
                                                        amount = if_abap_behv=>mk-on
                                                        quantityuom = if_abap_behv=>mk-on
                                                        amountcurrencycode = if_abap_behv=>mk-on
                                                        billingdocumentdate = if_abap_behv=>mk-on
                                                        unitprice = if_abap_behv=>mk-on
                                                        unitpricecurrencycode = if_abap_behv=>mk-on
                                                        gppercentage = if_abap_behv=>mk-on
                                                        commissionpayfromdate = if_abap_behv=>mk-on
                                                        commissionpaytodate = if_abap_behv=>mk-on
                                                        commissionrate = if_abap_behv=>mk-on
                                                        commamtcurrencycode = if_abap_behv=>mk-on """"
                                                        clearingstatus = if_abap_behv=>mk-on
                                                        itemid = if_abap_behv=>mk-on
                                                        commissionamount = if_abap_behv=>mk-on
                                                        lastupdateat = if_abap_behv=>mk-on
                                                        othercharges = if_abap_behv=>mk-on
                                                        otherchargescurrencycode = if_abap_behv=>mk-on
                                                        accountingdocument = if_abap_behv=>mk-on
                                                         ) ).

            APPEND ls_targetinvsalescom TO lt_targetinvsalescom.
*
            ls_invsalescom =   VALUE #( %cid_ref = 'runheader' %target = lt_targetinvsalescom ).
            APPEND ls_invsalescom TO lt_invsalescom.
            CLEAR lt_targetinvsalescom.
            CLEAR commrate.
            CLEAR commissionamount.
            CLEAR trans_currency.
            CLEAR lv_gp.

            CLEAR other_charges_amt.
            CLEAR ppro_trans_currency.

          ENDLOOP.        "billdoc Item
        ENDIF.           "if value exist in billdoc Item
      ENDLOOP.           "BILL DOC - lt_billingdocu
*
      DATA(temp) = lt_invsalescom.
      DATA(temprun) = lt_run.

**"Create records in sales commission table
      MODIFY ENTITIES OF zi_salescomt
         ENTITY salescommission
         CREATE FROM lt_run
         CREATE BY \_invsalescom FROM lt_invsalescom
         MAPPED DATA(mapped_data)
         FAILED DATA(failed_data)
         REPORTED DATA(reported_data).
      COMMIT ENTITIES.

      DATA(test) = '2'.
*
*ENDIF.
    ELSE.
*out->write( 'Records already processed for commission date range' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

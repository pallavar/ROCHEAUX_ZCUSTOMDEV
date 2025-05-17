CLASS lhc_salescommission DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR salescommission RESULT result.

    METHODS addinvoicesact FOR MODIFY
      IMPORTING keys FOR ACTION salescommission~addinvoicesact.

ENDCLASS.

CLASS lhc_salescommission IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD addinvoicesact. "addinvoicesact- this is a temporary method added for supporting Testing in QA, in prod it will not be enabled-10th sep.

***************************** HEADER ********************************************************************************
    "Declare date variables
    DATA: lv_from_date   TYPE d,
          lv_to_date     TYPE d,
          lv_system_date TYPE d.

    " Get the current system date
    lv_system_date = cl_abap_context_info=>get_system_date( ).

    READ ENTITIES OF zi_salescomt IN LOCAL MODE
       ENTITY salescommission
       FIELDS ( runid commissionfromdate commissiontodate rundate ) WITH VALUE #( ( %key-runid = keys[ 1 ]-%key-runid ) )
       RESULT DATA(result_header).

    LOOP AT result_header INTO DATA(header).
      DATA(from_date) = header-commissionfromdate.
      DATA(to_date) = header-commissiontodate.
      DATA(run_date)  = header-rundate.

      lv_from_date = from_date.
      lv_to_date = to_date.

      IF ( lv_to_date  > lv_from_date OR lv_to_date  = lv_from_date ).

        "Declare table type for sales commission table - used to create records
        DATA lt_run TYPE TABLE FOR UPDATE zi_salescomt.
        DATA ls_run LIKE LINE OF lt_run.

        "Append header values
        ls_run = VALUE #(    runid = header-runid
        processingstatus = 'Not Started'
          rundate = lv_system_date
          commissionpaymentstatus = 'Unpaid Invoices' "'1'
          invoicepaymentstatus = 'Unpaid'   "'1'
          %control = VALUE #( processingstatus = if_abap_behv=>mk-on
          rundate = if_abap_behv=>mk-on
          commissionpaymentstatus = if_abap_behv=>mk-on
          invoicepaymentstatus = if_abap_behv=>mk-on
          ) ).
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


        "Query billing document, payment status = fully cleared, posting status = completed
        SELECT billingdocument,invoiceclearingstatus,accountingdocument
          FROM i_billingdocument  WITH PRIVILEGED ACCESS
         " WHERE billingdocument BETWEEN '0090000004' AND '0090000011'
        "  WHERE billingdocument = '0090000070'
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
                DATA(amount) = ls_billingdocuitem-amount.

                "Query customer ID and get name customer name
                SELECT SINGLE customername
                FROM i_customer
                WHERE customer  = @ls_billingdocuitem-customerid
                                           " WHERE customer  = '0001000010'

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

                "other charges = PPR0 Condition value - Net Amount
                DATA: other_charges_amt TYPE p LENGTH 10 DECIMALS 2.
*                other_charges_amt =  conditionamount1 - amount.

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
*                DATA(total) = conditionamount1 - conditionamount2.
*                DATA(gp) = total / conditionamount1 * 100.   "DIV BY ZERO TO BE HANDLED

                SELECT conditionratevalue,conditionamount,transactioncurrency
                FROM i_billingdocumentitemprcgelmnt
                WHERE billingdocument = @ls_billingdocu-billingdocument AND billingdocumentitem = @ls_billingdocuitem-item AND conditiontype = 'ZC01'
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
                    IF unitpricequantity IS NOT INITIAL OR unitpricequantity <> 0.  "17/01/2025 TO AVOID Runtime Error: 'BCD_ZERODIVIDE'
                      unitprice = unitprice / unitpricequantity.
                    ELSE.
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
*                ls_targetinvsalescom = VALUE #( %cid = 'invoiceitem' && '1'
*                                         billingdocumentid   = '123'
*                                         commissionstatus = 'Unpaid Invoice1' "'1'
*                                         %control = VALUE #( billingdocumentid = if_abap_behv=>mk-on
*                                                             commissionstatus = if_abap_behv=>mk-on
*                                                              ) ).

                ls_targetinvsalescom = VALUE #( %cid = 'invoiceitem' && ls_billingdocu-billingdocument  && ls_billingdocuitem-item
           " ls_targetinvsalescom = VALUE #( %cid = |CREATETEXTCID{ sy-tabix }|
                                               billingdocumentid   = ls_billingdocu-billingdocument
                                               commissionstatus = 'Unpaid Invoices' "'1'
                                               commissionpayfromdate = lv_from_date  "lv_system_date - 30
                                               commissionpaytodate = lv_to_date  "lv_system_date - 1
                                               salesman =  ls_billingdocuitem-customergroup  "'01'
                                               clearingstatus = ls_billingdocu-clearingstatus
                                               salesorderid = |{ ls_billingdocuitem-salesdocument ALPHA = OUT }|
                                               salesorderdate = sales_order_date  "ls_salesorder-salesorderdate
                                               billingdocumentdate = ls_billingdocuitem-invoicedate
                                               amount = ls_billingdocuitem-amount
                                               amountcurrencycode = ls_billingdocuitem-amountcurrency
                                               customerid = |{ ls_billingdocuitem-customerid ALPHA = OUT }|
                                               customername = retrievedcustomername                                               salesorganisation = ls_billingdocuitem-salesorganisation
                                               itemid = ls_billingdocuitem-item
                                               materialid = ls_billingdocuitem-product
                                               quantity = ls_billingdocuitem-quantity
                                               quantityuom = ls_billingdocuitem-uom
                                               unitprice = unitprice  "ls_salesorderitem-unitprice
                                               unitpricecurrencycode = ls_salesorderitem-unitpricecurrency
                                               gppercentage = lv_gp  "gp
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

                ls_invsalescom =   VALUE #( %key-runid = header-runid %target = lt_targetinvsalescom ). "note this difference
                APPEND ls_invsalescom TO lt_invsalescom.
                CLEAR lt_targetinvsalescom.
                CLEAR commrate.
                CLEAR commissionamount.
                CLEAR lv_gp.

              ENDLOOP.
            ENDIF.           "if value exist in billdoc Item
          ENDLOOP.
        ENDIF.

      ENDIF.
    ENDLOOP.

    DATA(temp) = lt_invsalescom.
    DATA(temprun) = lt_run.

    "Perform CRUD - header update and items create
    MODIFY ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY salescommission
    UPDATE FROM lt_run
    CREATE BY \_invsalescom FROM lt_invsalescom
    MAPPED DATA(mapped_data)
    FAILED DATA(failed_data)
    REPORTED DATA(reported_data).
    "   COMMIT ENTITIES.

    DATA(test) = '2'.


    "success or fail
    IF ( failed_data IS INITIAL OR reported_data IS INITIAL ).
      "'Records created
    ELSE.
      "'Records NOT created
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_invsalescom DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS setoverallinvoicestatusitem FOR DETERMINE ON SAVE
      IMPORTING keys FOR invsalescom~setoverallinvoicestatusitem.
    METHODS commissionpaymentaction FOR MODIFY
      IMPORTING keys FOR ACTION invsalescom~commissionpaymentaction.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR invsalescom RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR invsalescom RESULT result.
    METHODS autocalculate FOR DETERMINE ON MODIFY
      IMPORTING keys FOR invsalescom~autocalculate.

ENDCLASS.

CLASS lhc_invsalescom IMPLEMENTATION.

  METHOD setoverallinvoicestatusitem.

    READ ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY invsalescom "//item
    FIELDS ( runid ) WITH VALUE #( ( %key-itemuuid = keys[ 1 ]-%key-itemuuid ) )
    RESULT DATA(result_runid).

    READ ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY salescommission BY \_invsalescom
    FIELDS ( commissionstatus ) WITH VALUE #( ( %key-runid = result_runid[ 1 ]-runid ) )
    RESULT DATA(result_items).

    DATA: overall_inv_status  TYPE string, overall_comm_status TYPE string.
    DATA: paid_count       TYPE i, unpaid_count TYPE i, commission_count TYPE i.
*
****Sales Commission status: 1- unpaid invoice, 2- paid invoice, 3- commission paid
**** overall status: 1- unpaid, 2- partially paid, 3- fully paid

    LOOP AT result_items INTO DATA(item).

      CASE item-commissionstatus.
        WHEN 'Unpaid Invoices'.
          unpaid_count = unpaid_count + 1.
        WHEN 'Paid Invoices'. "paid invoice
          paid_count = paid_count + 1.
        WHEN 'Commission Paid'.
          commission_count = commission_count + 1.
          paid_count = paid_count + 1.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    "set overall invoice clearing status
    IF ( unpaid_count EQ lines( result_items ) ).
      overall_inv_status = 'Unpaid'.  "unpaid invoice
    ELSE.
      IF ( paid_count EQ lines( result_items ) ).
        overall_inv_status = 'Fully Paid'.  "fully paid invoice
      ELSE.
        overall_inv_status = 'Partially Paid'.  "partially paid invoice
      ENDIF.
    ENDIF.

    "set overall commission status
    IF ( commission_count EQ lines( result_items ) ).
      overall_comm_status = 'Fully Paid'.
    ELSE.
      IF ( line_exists( result_items[ commissionstatus = 'Commission Paid' ] ) ).
        overall_comm_status = 'Partially Paid'.
      ELSE.
        overall_comm_status = 'Unpaid'.
      ENDIF.
    ENDIF.

    "update overall status
    MODIFY ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY salescommission
    UPDATE FROM VALUE #( ( %key-runid = result_runid[ 1 ]-runid
                            invoicepaymentstatus = overall_inv_status
                            commissionpaymentstatus = overall_comm_status
                            %control-invoicepaymentstatus = if_abap_behv=>mk-on
                            %control-commissionpaymentstatus = if_abap_behv=>mk-on ) )
    FAILED DATA(fail_overall_status)
    REPORTED DATA(report_overall_status).

    IF ( fail_overall_status IS INITIAL OR report_overall_status IS INITIAL ).
      DATA(success) = 'success'.
    ELSE.
      DATA(fail) = 'fail'.
    ENDIF.



  ENDMETHOD.

  METHOD commissionpaymentaction.

    DATA lt_header_u TYPE TABLE FOR UPDATE zi_invsalescom.
    DATA ls_header_u LIKE LINE OF lt_header_u.

    DATA lv_system_date TYPE d.
    " Get the current system date
    lv_system_date = cl_abap_context_info=>get_system_date( ).

*          "Assign Commission Pay date (when user clicks commission paid button on screen, last updated at will be updated.)
*          "Commission Pay date = last date of month (last update at field)

    DATA: lv_date      TYPE d ,  " Example: October 14, 2024
          lv_year      TYPE i,
          lv_month     TYPE i,
          lv_last_day  TYPE d,
          lv_month_str TYPE string.

    " Assign the value from the field symbol
    lv_date = lv_system_date.   "<fs_invsalescom>-last_updated_at.

    lv_year  = lv_date(4).      " Extract year
    lv_month = lv_date+4(2).    " Extract month

    " Increment the month to get the first day of the next month
    lv_month = lv_month + 1.

    " Handle year transitions (e.g., from December to January)
    IF lv_month > 12.
      lv_month = 1.
      lv_year  = lv_year + 1.
    ENDIF.

    " Format the month with leading zeroes (pad to 2 digits)
    lv_month_str = COND string( WHEN lv_month < 10 THEN |0{ lv_month }| ELSE |{ lv_month }| ).

    " Set the first day of the next month
    lv_last_day = |{ lv_year }{ lv_month_str }01|.

    " Subtract 1 day to get the last day of the current month
    lv_last_day = lv_last_day - 1.
    " <fs_invsalescom>-commission_pay_date = lv_last_day.
*          out->write( 'Comm pay date: ' && <fs_invsalescom>-commission_pay_date ).

    READ ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY invsalescom
        ALL FIELDS WITH VALUE #(  ( %key-itemuuid = keys[ 1 ]-%key-itemuuid ) )
        RESULT DATA(invoice_items).

    LOOP AT invoice_items INTO DATA(invoice_item).
      ls_header_u = CORRESPONDING #( invoice_item ).
      ls_header_u-commissionstatus = 'Commission Paid'. "'3'.
      ls_header_u-lastupdateat = lv_system_date.  "added 25/10/24
      ls_header_u-commissionpaydate = lv_last_day."added 25/10/24
      ls_header_u-%control-commissionstatus = if_abap_behv=>mk-on.
      ls_header_u-%control-lastupdateat = if_abap_behv=>mk-on.
      ls_header_u-%control-commissionpaydate = if_abap_behv=>mk-on.
      APPEND ls_header_u TO lt_header_u.
    ENDLOOP.

    MODIFY ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY invsalescom
    UPDATE FROM lt_header_u
    FAILED DATA(fail)
    REPORTED DATA(rep).

  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY invsalescom
        ALL FIELDS WITH VALUE #(  ( %key-itemuuid = keys[ 1 ]-%key-itemuuid ) )
        RESULT DATA(invoice_items).

    LOOP AT invoice_items INTO DATA(invoice_item).
      DATA(statusvalue) = COND #( WHEN invoice_item-commissionstatus EQ ''
                                 THEN if_abap_behv=>fc-o-disabled

                                 WHEN invoice_item-commissionstatus EQ 'Commission Paid' "'3'
                                 THEN if_abap_behv=>fc-o-disabled

                                 WHEN invoice_item-commissionstatus EQ 'Unpaid Invoices' "'1'
                                 THEN if_abap_behv=>fc-o-disabled

                                 ELSE if_abap_behv=>fc-o-enabled ).

      result = VALUE #( (
                          %tky = invoice_item-%tky
                          %action-commissionpaymentaction = statusvalue ) ).


      reported-invsalescom = VALUE #( ( %tky = invoice_item-%tky
            %action-commissionpaymentaction = statusvalue
            %msg = new_message_with_text(
               severity = if_abap_behv_message=>severity-error
            text = 'Invalid ' && ' - ' && invoice_item-billingdocumentid && '-' && invoice_item-itemid
            "text = 'Status change not possible for invoice ID ' && invoice_item-billingdocumentid
            )
*                                 %msg = new_message_with_text(
*                                     severity = if_abap_behv_message=>severity-information
*                                     text = 'Status change not possible for invoice ID ' && invoice_item-ItemID
*                                 )
          ) ).


    ENDLOOP.

  ENDMETHOD.


*Autocalculation for Net Amount and Commmission Amount

  METHOD autocalculate.

    READ ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY invsalescom
    ALL FIELDS WITH VALUE #(  ( %key-itemuuid = keys[ 1 ]-%key-itemuuid ) )
    RESULT DATA(invoiceitem).

    CHECK ( invoiceitem IS NOT INITIAL ).

    DATA(invoice_item_uuid) = invoiceitem[ 1 ]-itemuuid.
    DATA(commission_rate) = invoiceitem[ 1 ]-commissionrate.

*    new code added 04042025 by Pallava - BEGIN

    " Convert to floating point type for higher precision
    DATA: lv_amount          TYPE f,
          lv_othercharges    TYPE f,
          lv_discountamount  TYPE f,
          lv_commission_rate TYPE f,
          lv_new_commission_amount TYPE f.

    lv_amount          = invoiceitem[ 1 ]-amount.
    lv_othercharges    = invoiceitem[ 1 ]-othercharges.
    lv_discountamount  = invoiceitem[ 1 ]-discountamount.
    lv_commission_rate = commission_rate.

    " Perform calculation with floating point
    DATA(lv_new_net_amount) = lv_amount - lv_othercharges - lv_discountamount.
    lv_new_commission_amount = lv_new_net_amount * lv_commission_rate / 100.

    " Convert back to currency type
    DATA new_net_amount        TYPE p LENGTH 10 DECIMALS 2.
    DATA new_commission_amount   TYPE p LENGTH 10 DECIMALS 2.

    " Avoid modifying if NetAmountCalc is already same
*    DATA(new_net_amount) = invoiceitem[ 1 ]-amount -  invoiceitem[ 1 ]-othercharges - invoiceitem[ 1 ]-discountamount.
     new_net_amount = lv_new_net_amount.

*    DATA(new_commission_amount) = invoiceitem[ 1 ]-netamountcalc * commission_rate / 100.
     new_commission_amount = new_net_amount * commission_rate / 100.

*    new code added 04042025 by Pallava - END


    IF invoiceitem[ 1 ]-netamountcalc <> new_net_amount.
      .
    ELSEIF
    invoiceitem[ 1 ]-commissionamount <> new_commission_amount.

    ELSE.
      RETURN.
      " No need to update, avoid infinite loop
    ENDIF.

    MODIFY ENTITIES OF zi_salescomt IN LOCAL MODE
    ENTITY invsalescom
    UPDATE FROM VALUE #( (  %key-itemuuid = invoice_item_uuid

                         %data-netamountcalc = new_net_amount
                         %data-netamtcalccurrencycode = invoiceitem[ 1 ]-AmountCurrencycode
                         %data-commissionamount = new_commission_amount
                         %data-commamtcurrencycode = invoiceitem[ 1 ]-AmountCurrencycode

                         %control-netamountcalc = if_abap_behv=>mk-on
                         %control-netamtcalccurrencycode = if_abap_behv=>mk-on
                         %control-commissionamount = if_abap_behv=>mk-on
                         %control-commamtcurrencycode = if_abap_behv=>mk-on ) )
    FAILED DATA(faileddata)
    REPORTED DATA(reporteddata).

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zi_salescomt DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.
    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zi_salescomt IMPLEMENTATION.

  METHOD adjust_numbers.

    SELECT MAX( run_id )
      FROM zsalescomt
        INTO @DATA(lv_max_runid) UP TO 1 ROWS.

    DATA(lv_unique_runid) = lv_max_runid + 1.

    LOOP AT mapped-salescommission REFERENCE INTO DATA(map).
      map->runid = lv_unique_runid.
    ENDLOOP.

  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

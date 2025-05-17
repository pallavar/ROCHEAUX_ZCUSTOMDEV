CLASS zcl_salescomm_deter_clearedinv DEFINITION
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



CLASS ZCL_SALESCOMM_DETER_CLEAREDINV IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
* You can Insert code here for getting Job parameters
  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.
    DATA lv_system_date TYPE d.
    " Get the current system date
    lv_system_date = cl_abap_context_info=>get_system_date( ).
*    " Declare the header internal table and work area
    DATA: lt_zsalescomt TYPE TABLE OF zsalescomt,
          ls_zsalescomt TYPE zsalescomt.
*
*    " Declare the item internal table and work area
    DATA: lt_zinvsalescom TYPE TABLE OF zinvsalescom,
          ls_zinvsalescom TYPE zinvsalescom.
*
*    "Declare field symbols
    FIELD-SYMBOLS: <fs_salescomt> TYPE zsalescomt.
    FIELD-SYMBOLS: <fs_invsalescom> TYPE zinvsalescom.

*    "Step 1: Select data from the custom table into the internal table
    DATA(fp) = 'Fully Paid'.
    SELECT * FROM zsalescomt
     "WHERE run_id = '7'
      WHERE processing_status = 'Not Started' OR invoice_pay_status <> @fp   "'3' "AND run_id = '51'
         INTO TABLE @lt_zsalescomt.
*
*    " Check if data is retrieved
    IF sy-subrc = 0.
*
*      "Step 2: Use field symbols to update the internal table entries
      LOOP AT lt_zsalescomt ASSIGNING <fs_salescomt>.

        SELECT * FROM zinvsalescom
             WHERE run_id = @<fs_salescomt>-run_id
                INTO TABLE @lt_zinvsalescom.

        LOOP AT lt_zinvsalescom ASSIGNING <fs_invsalescom>.

          "Assign clearing status
          SELECT SINGLE invoiceclearingstatus, accountingdocument FROM i_billingdocument  WITH PRIVILEGED ACCESS
                 WHERE invoiceclearingstatus = 'C'                                         "C- fully cleared
                   AND billingdocument =   @<fs_invsalescom>-billingdocument_id            "'0090000010'
                        INTO @DATA(clearing_status).

          IF sy-subrc = 0.
            "update field symbols - item fields
            <fs_invsalescom>-commission_status = 'Paid Invoices'.  "'2'.   "2-paid invoices
            <fs_invsalescom>-invoice_clearing_status = clearing_status-invoiceclearingstatus. "'C'. "clearing_status
            <fs_invsalescom>-z_accountingdocument = clearing_status-accountingdocument.  "invoice accounting number
            <fs_invsalescom>-last_updated_at = lv_system_date.
            " out->write( 'Invoice Paid: ' && <fs_invsalescom>-billingdocument_id ).
          ENDIF.

          "Assign clearing date
          SELECT SINGLE clearingjournalentry, clearingdate FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS
            WHERE billingdocument = @<fs_invsalescom>-billingdocument_id
            INTO @DATA(clearing_data).

          IF sy-subrc = 0.
            <fs_invsalescom>-invoice_clearing_date = clearing_data-clearingdate.
            "out->write( 'Invoice clearing date: ' && <fs_invsalescom>-invoice_clearing_date ).
          ENDIF.

          "Getting a handler for the currently active tenant
          DATA(ten) = xco_cp=>current->tenant( ).

          "Getting the UI URL of the currently active tenant
          DATA(ui_url) = ten->get_url( xco_cp_tenant=>url_type->ui ).
          DATA(host) = ui_url->get_host( ).

          IF host = 'my404162.s4hana.cloud.sap'.  "DEV
            DATA(gl_account) = '0071050000'.
          ENDIF.

          IF host = 'my403669.s4hana.cloud.sap'.  "UAT
            gl_account = '0000004100'.
          ENDIF.

          IF host = 'my403702.s4hana.cloud.sap'.  "PROD
            gl_account = '0000004100'.
          ENDIF.

          "Assign cash discount
          SELECT SINGLE amountincompanycodecurrency, companycodecurrency     "accountingdocument, glaccount, accountingdocumenttype
           FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS
             WHERE accountingdocument = @clearing_data-clearingjournalentry   "@clearing_je
               AND accountingdocumenttype = 'DZ'        "( DZ- Customer payment )
               AND glaccount = @gl_account "'0071050000'  " ( hardcoded or fixed)
             INTO @DATA(je_type).


*             Code added by Pallava on 07042025
                   DATA: discount_amount_f          TYPE f.
                   discount_amount_f = je_type-amountincompanycodecurrency.


          IF sy-subrc = 0.
*            <fs_invsalescom>-discount_amount = je_type-amountincompanycodecurrency.
            <fs_invsalescom>-discount_amount = discount_amount_f.
*            Code added by Pallava on 07042025

            <fs_invsalescom>-discamount_currencycode = je_type-companycodecurrency.
          ELSE.
            <fs_invsalescom>-discount_amount = 0.
            <fs_invsalescom>-discamount_currencycode = <fs_invsalescom>-amount_currencycode.

          ENDIF.

          "Assign Net Amount calculated, Net Amount calc = Invoice Amount – Other Amount – Discount
          <fs_invsalescom>-net_amount_calc =  <fs_invsalescom>-amount -  <fs_invsalescom>-other_charges - <fs_invsalescom>-discount_amount.

*          Commented this line since other charges currency code could be NULL in some case there is no other charges
*          <fs_invsalescom>-netamountcalc_currencycode = <fs_invsalescom>-othercharges_currencycode.

          <fs_invsalescom>-netamountcalc_currencycode = <fs_invsalescom>-amount_currencycode.

        ENDLOOP.

        "start - overall status
        DATA: overall_inv_status  TYPE string, overall_comm_status TYPE string.
        DATA: paid_count       TYPE i, unpaid_count TYPE i, commission_count TYPE i.

        LOOP AT lt_zinvsalescom INTO DATA(item).

          CASE item-commission_status.
            WHEN '1'.
              unpaid_count = unpaid_count + 1.
            WHEN '2'. "paid invoice
              paid_count = paid_count + 1.
            WHEN '3'.
              commission_count = commission_count + 1.
              paid_count = paid_count + 1.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        "set overall invoice clearing status
        IF ( unpaid_count EQ lines( lt_zinvsalescom ) ).
          overall_inv_status = 'Unpaid'.  "'1'.  "unpaid invoice
        ELSE.
          IF ( paid_count EQ lines( lt_zinvsalescom ) ).
            overall_inv_status = 'Fully Paid'. "'3'.  "fully paid invoice
          ELSE.
            overall_inv_status = 'Partially Paid'. "'2'.  "partially paid invoice
          ENDIF.
        ENDIF.

        "set overall commission status
        IF ( commission_count EQ lines( lt_zinvsalescom ) ).
          overall_comm_status = 'Fully Paid'.  "'3'.
        ELSE.
          IF ( line_exists( lt_zinvsalescom[ commission_status = '3' ] ) ).
            overall_comm_status = 'Partially Paid'. "'2'.
          ELSE.
            overall_comm_status = 'Unpaid'.  "'1'.
          ENDIF.
        ENDIF.
        "end - overall status


        MODIFY zinvsalescom FROM TABLE @lt_zinvsalescom.      "update item
        IF sy-subrc = 0.
          "out->write( 'Item Update successful' ).
          COMMIT WORK.
        ELSE.
          "out->write( 'Item Update failed' ).
        ENDIF.

        "update field symbols - header fields
        <fs_salescomt>-processing_status = 'Completed'.
        <fs_salescomt>-commission_pay_status = overall_comm_status.
        <fs_salescomt>-invoice_pay_status = overall_inv_status.

      ENDLOOP.

*      "Step 3: Update the database table with the modified internal table
      MODIFY zsalescomt FROM TABLE @lt_zsalescomt.           "update header


      IF sy-subrc = 0.
        TRY.
            "Application Log
            DATA(l_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object =
                     'ZSALESCOMT_APPL_LOG' subobject = 'ZSUBOBJECT1' ) ).
            "out->write( 'Header Update successful' ).
            DATA(l_free_text) = cl_bali_free_text_setter=>create( severity =
                           if_bali_constants=>c_severity_information
                           text = 'Update Success' ).

            l_log->add_item( item = l_free_text ).

            "Save the log into the database
            cl_bali_log_db=>get_instance( )->save_log( log = l_log assign_to_current_appl_job = abap_true ).
            COMMIT WORK.
          CATCH cx_bali_runtime INTO DATA(lx_bali_runtime).
            " Handle the exception, e.g., log it or display a message
            "WRITE: / 'An error occurred:', lx_bali_runtime->get_text( ).
        ENDTRY.

      ELSE.
        "out->write( 'Header Update failed' ).
        TRY.
            l_free_text = cl_bali_free_text_setter=>create( severity =
                           if_bali_constants=>c_severity_error
                           text = 'Update Failed' ).

            l_log->add_item( item = l_free_text ).

            "Save the log into the database
            cl_bali_log_db=>get_instance( )->save_log( log = l_log assign_to_current_appl_job = abap_true ).
            COMMIT WORK.
          CATCH cx_bali_runtime INTO DATA(lx_bali_runtime4).
            " Handle the exception, e.g., log it or display a message
        ENDTRY.
      ENDIF.
*
    ENDIF.
*
*    "send mail
*
    TRY.
        DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
        DATA : iv_content1 TYPE string, iv_content2 TYPE string, iv_content3 TYPE string, iv_content4 TYPE string.

        "Getting a handler for the currently active tenant
        DATA(tenant) = xco_cp=>current->tenant( ).

        "Getting the UI URL of the currently active tenant
        DATA(system_ui_url) = tenant->get_url( xco_cp_tenant=>url_type->ui ).
        DATA(system_host) = system_ui_url->get_host( ).

        iv_content1 =  '<p>Dear User,</p>'.
        iv_content2 = '<p>Sales Commission Determine Fully Cleared Invoices Job Run has been completed and the report can be accessed from the below link.</p>'.

        IF host = 'my404162.s4hana.cloud.sap'.  "DEV
          iv_content3 = '<p><a href="https://my404162.s4hana.cloud.sap/ui#AnalyticQuery-analyze?bsa_query=2CYY1_J8Z5F5LWSW1G&sap-app-origin-hint=&/sap-iapp-state=ASZWTU3Y8TFMHAQ72973FDFY9CC045AFTZXW5DB7">Click to View Sales Commission Log</a></p>'.
        ENDIF.

        IF host = 'my403669.s4hana.cloud.sap'.  "UAT
          "iv_content3 = '<p><a href="https://my403669.s4hana.cloud.sap/ui#AnalyticQuery-analyze?bsa_query=2CYY1_QKXEIBVJJZH4&sap-app-origin-hint=&/sap-iapp-state=ASKYJI9JJNPJTASWVDQ3J4ZSVTVHE15J00MPCR1N">Click to View Sales Commission Log</a></p>'.
          iv_content3 = '<p><a href="https://my403669.s4hana.cloud.sap/ui#AnalyticQuery-analyze?bsa_query=2CYY1_QKXEIBVJJZH4&sap-app-origin-hint=&/sap-iapp-state=ASPQNSQR6DZR4H15FP02UQBTANQ4055GWTBD5E6P">Click to View Sales Commission Log</a></p>'.
          lo_mail->add_recipient( 'sreddy@rocheux.com' ).
        ENDIF.

        IF host = 'my403702.s4hana.cloud.sap'.  "PROD
          iv_content3 = '<p><a href="https://my403702.s4hana.cloud.sap/ui#AnalyticQuery-analyze?bsa_query=2CYY1_QKXEIBVJJZH4&sap-app-origin-hint=&/sap-iapp-state=ASXI2HEZ1KSL1RWM4NMNZBJBRXXVYXBUOSKC5PQV">Click to View Sales Commission Log</a></p>'.
          lo_mail->add_recipient( 'sreddy@rocheux.com' ).
        ENDIF.


        iv_content4 =  '<p>This is system generated mail so please do not reply to this mail.</p>'.

        lo_mail->set_sender( 'do.not.reply@rocheux.com' ).
*        lo_mail->add_recipient( 'renugadevi.s@noblq.com' ).
*        lo_mail->add_recipient( 'pallavarajan.p@noblq.com' ).
*        lo_mail->add_recipient( 'vignesh.r@noblq.com' ).

        lo_mail->set_subject( 'Sales Commission Run Report' ).

        lo_mail->set_main( cl_bcs_mail_textpart=>create_instance(
          iv_content = iv_content1 && iv_content2 && iv_content3 && iv_content4
          iv_content_type = 'text/html' ) ).

        DATA: lo_att TYPE REF TO cl_bcs_mail_bodypart.  "importing io_attachment  type ref to cl_bcs_mail_bodypart

*        lo_mail->add_attachment( lo_att ).
        lo_mail->send( IMPORTING et_status = DATA(lt_status) ).

      CATCH cx_bcs_mail INTO DATA(lo_err).
        "out->write( lo_err->get_longtext( ) ).
    ENDTRY.




*        "Add a message as item to the log
*    DATA(l_message) = cl_bali_message_setter=>create( severity =
*                     if_bali_constants=>c_severity_information
*                                                       id = 'PO'
*                                                      number = '000' ).
*    l_log->add_item( item = l_message ).

*        "Add a second message, this time from system fields SY-MSGID, ...
    "MESSAGE ID 'ZTEST' TYPE 'S' NUMBER '058' INTO DATA(l_text).

    "l_log->add_item( item = cl_bali_message_setter=>create_from_sy( ) ).

    "Add a free text to the log



    " Add an exception to the log
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


    "COMMIT WORK.


  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA lv_system_date TYPE d.
    " Get the current system date
    lv_system_date = cl_abap_context_info=>get_system_date( ).

*    " Declare the header internal table and work area
    DATA: lt_zsalescomt TYPE TABLE OF zsalescomt,
          ls_zsalescomt TYPE zsalescomt.
*
*    " Declare the item internal table and work area
    DATA: lt_zinvsalescom TYPE TABLE OF zinvsalescom,
          ls_zinvsalescom TYPE zinvsalescom.
*
*    "Declare field symbols
    FIELD-SYMBOLS: <fs_salescomt> TYPE zsalescomt.
    FIELD-SYMBOLS: <fs_invsalescom> TYPE zinvsalescom.
*
*    "Step 1: Select data from the custom table into the internal table
    DATA(fp) = 'Fully Paid'.
    SELECT * FROM zsalescomt
     WHERE run_id = '4'
   "   WHERE processing_status = 'Not Started' OR invoice_pay_status <> @fp   "'3' "AND run_id = '51'
         INTO TABLE @lt_zsalescomt.
*
*    " Check if data is retrieved
    IF sy-subrc = 0.
*
*      "Step 2: Use field symbols to update the internal table entries
      LOOP AT lt_zsalescomt ASSIGNING <fs_salescomt>.

        SELECT * FROM zinvsalescom
             WHERE run_id = @<fs_salescomt>-run_id
            " and billingdocument_id = '0090000004'
                INTO TABLE @lt_zinvsalescom.


*        SELECT SINGLE FROM zinvsalescom
*        FIELDS
*         " add_months(  last_updated_at,3 ) AS  add_3_months
*         dats_add_months( last_updated_at, 3 ) AS add_3_months
*         INTO @DATA(result).


        LOOP AT lt_zinvsalescom ASSIGNING <fs_invsalescom>.

          "Assign clearing status
          SELECT SINGLE invoiceclearingstatus, accountingdocument FROM i_billingdocument  WITH PRIVILEGED ACCESS
                 WHERE invoiceclearingstatus = 'C'                                         "C- fully cleared
                   AND billingdocument =   @<fs_invsalescom>-billingdocument_id            "'0090000010'
                        INTO @DATA(clearing_status).

          IF sy-subrc = 0.
            "update field symbols - item fields
            <fs_invsalescom>-commission_status = 'Paid Invoices'.  "'2'.   "2-paid invoices
            <fs_invsalescom>-invoice_clearing_status = clearing_status-invoiceclearingstatus. "'C'. "clearing_status
            <fs_invsalescom>-z_accountingdocument = clearing_status-accountingdocument. "invoice accounting number
            <fs_invsalescom>-last_updated_at = lv_system_date.
            out->write( 'Invoice ID: ' && <fs_invsalescom>-billingdocument_id ).
            out->write( 'Invoice last time: ' && <fs_invsalescom>-last_updated_at ).
            out->write( 'accounting document: ' && <fs_invsalescom>-z_accountingdocument ).

          ENDIF.

          "Assign clearing date
          SELECT SINGLE clearingjournalentry, clearingdate FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS
            WHERE billingdocument = @<fs_invsalescom>-billingdocument_id
            INTO @DATA(clearing_data).

          IF sy-subrc = 0.
            <fs_invsalescom>-invoice_clearing_date = clearing_data-clearingdate.
            out->write( 'Invoice clearing date: ' && <fs_invsalescom>-invoice_clearing_date ).
          ENDIF.

          "Assign cash discount
*          SELECT SINGLE clearingjournalentry   "accountingdocument, GLAccount
*             FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS
*              WHERE billingdocument = @<fs_invsalescom>-billingdocument_id
*              INTO @DATA(clearing_je).

          SELECT SINGLE amountincompanycodecurrency, companycodecurrency     "accountingdocument, glaccount, accountingdocumenttype
           FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS
             WHERE accountingdocument = @clearing_data-clearingjournalentry   "@clearing_je
               AND accountingdocumenttype = 'DZ'        "( DZ- Customer payment )
               AND glaccount = '0071050000'  " ( hardcoded or fixed)
             INTO @DATA(je_type).

          IF sy-subrc = 0.
            <fs_invsalescom>-discount_amount = je_type-amountincompanycodecurrency.
            <fs_invsalescom>-discamount_currencycode = je_type-companycodecurrency.
            out->write( 'Cash Discount: ' && <fs_invsalescom>-discount_amount ).
            ELSE.
                 <fs_invsalescom>-discount_amount = 0.
                 <fs_invsalescom>-discamount_currencycode = <fs_invsalescom>-amount_currencycode.
          ENDIF.


          "Assign Net Amount calculated, Net Amount calc = Invoice Amount – Other Amount – Discount
          <fs_invsalescom>-net_amount_calc =  <fs_invsalescom>-amount -  <fs_invsalescom>-other_charges - <fs_invsalescom>-discount_amount.
          <fs_invsalescom>-netamountcalc_currencycode = <fs_invsalescom>-othercharges_currencycode.

          DATA(ww) = '5'.

        ENDLOOP.

        "start - overall status
        DATA: overall_inv_status  TYPE string, overall_comm_status TYPE string.
        DATA: paid_count       TYPE i, unpaid_count TYPE i, commission_count TYPE i.

        LOOP AT lt_zinvsalescom INTO DATA(item).

          CASE item-commission_status.
            WHEN '1'.
              unpaid_count = unpaid_count + 1.
            WHEN '2'. "paid invoice
              paid_count = paid_count + 1.
            WHEN '3'.
              commission_count = commission_count + 1.
              paid_count = paid_count + 1.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        "set overall invoice clearing status
        IF ( unpaid_count EQ lines( lt_zinvsalescom ) ).
          overall_inv_status = 'Unpaid'.  "'1'.  "unpaid invoice
        ELSE.
          IF ( paid_count EQ lines( lt_zinvsalescom ) ).
            overall_inv_status = 'Fully Paid'. "'3'.  "fully paid invoice
          ELSE.
            overall_inv_status = 'Partially Paid'. "'2'.  "partially paid invoice
          ENDIF.
        ENDIF.

        "set overall commission status
        IF ( commission_count EQ lines( lt_zinvsalescom ) ).
          overall_comm_status = 'Fully Paid'.  "'3'.
        ELSE.
          IF ( line_exists( lt_zinvsalescom[ commission_status = '3' ] ) ).
            overall_comm_status = 'Partially Paid'. "'2'.
          ELSE.
            overall_comm_status = 'Unpaid'.  "'1'.
          ENDIF.
        ENDIF.
        "end - overall status


        MODIFY zinvsalescom FROM TABLE @lt_zinvsalescom.      "update item
        IF sy-subrc = 0.
          out->write( 'Item Update successful' ).
          COMMIT WORK.
        ELSE.
          out->write( 'Item Update failed' ).
        ENDIF.

        "update field symbols - header fields
        <fs_salescomt>-processing_status = 'Completed'.
        <fs_salescomt>-commission_pay_status = overall_comm_status.
        <fs_salescomt>-invoice_pay_status = overall_inv_status.

      ENDLOOP.

      "Step 3: Update the database table with the modified internal table
      MODIFY zsalescomt FROM TABLE @lt_zsalescomt.           "update header
      IF sy-subrc = 0.
        out->write( 'Header Update successful' ).
        COMMIT WORK.
      ELSE.
        out->write( 'Header Update failed' ).
      ENDIF.

    ENDIF.

    "send mail

    "Getting a handler for the currently active tenant
    DATA(ten) = xco_cp=>current->tenant( ).
    DATA(id) = ten->get_id( ).

    "Getting the UI URL of the currently active tenant
    DATA(ui_url) = ten->get_url( xco_cp_tenant=>url_type->ui ).
    DATA(host) = ui_url->get_host( ).
    DATA(port) = ui_url->get_port( ).
    DATA(t) = '2'.

    TRY.
        DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
        DATA : iv_content1 TYPE string, iv_content2 TYPE string, iv_content3 TYPE string.
        iv_content1 =  '<p><h1>Sales Commission</h1></p>'.
        iv_content2 = '<p>See details below!</p>'.

        IF host = 'my403669.s4hana.cloud.sap'.
          iv_content3 = '<p><a href="https://my404162.s4hana.cloud.sap/ui#AnalyticQuery-analyze?bsa_query=2CYY1_J8Z5F5LWSW1G&sap-app-origin-hint=&/sap-iapp-state=ASZWTU3Y8TFMHAQ72973FDFY9CC045AFTZXW5DB7">Click to View Sales Commission Log</a></p>'.
        ENDIF.

        IF host = 'my404162.s4hana.cloud.sap'.
          iv_content3 = '<p><a href="https://my403669.s4hana.cloud.sap/ui#AnalyticQuery-analyze?bsa_query=2CYY1_QKXEIBVJJZH4&sap-app-origin-hint=&/sap-iapp-state=ASKYJI9JJNPJTASWVDQ3J4ZSVTVHE15J00MPCR1N">Click to View Sales Commission Log</a></p>'.
        ENDIF.



        lo_mail->set_sender( 'do.not.reply@rocheux.com' ).
        lo_mail->add_recipient( 'renugadevi.s@noblq.com' ).
        lo_mail->add_recipient( 'pallavarajan.p@noblq.com' ).


        lo_mail->set_subject( 'Test Mail 3' ).

        lo_mail->set_main( cl_bcs_mail_textpart=>create_instance(
            "iv_content      = '<h1>Hello</h1><p>Mail for sales commission!</p>'
            "iv_content      = '<p>Mail for sales commission!</p>'
          iv_content = iv_content1 && iv_content2 && iv_content3
        iv_content_type = 'text/html' ) ).

        DATA: lo_att TYPE REF TO cl_bcs_mail_bodypart.  "importing io_attachment  type ref to cl_bcs_mail_bodypart

*        lo_mail->add_attachment( lo_att ).
*        lo_mail->send( IMPORTING et_status = DATA(lt_status) ).
*
*        out->write( lt_status ).

      CATCH cx_bcs_mail INTO DATA(lo_err).
        out->write( lo_err->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

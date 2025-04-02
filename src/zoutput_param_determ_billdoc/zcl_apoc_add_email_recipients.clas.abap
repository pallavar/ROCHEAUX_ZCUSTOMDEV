CLASS zcl_apoc_add_email_recipients DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_apoc_add_email_recipients .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APOC_ADD_EMAIL_RECIPIENTS IMPLEMENTATION.


  METHOD if_apoc_add_email_recipients~add_email_recipients.

    "Declarations
    DATA: email_address TYPE string.
    DATA(lv_system_techname)  = cl_abap_context_info=>get_user_technical_name( ).   "logon user

    DATA(outputcontrolapplobjecttype1) = outputcontrolapplobjecttype.
    DATA(outputdocumenttype1) = outputdocumenttype.

    DATA: addemailrecipients TYPE string.

    "check the application object and output type
    CASE outputcontrolapplobjecttype.
      WHEN 'BILLING_DOCUMENT'.
        addemailrecipients = 'YES'.

*      Included this as new request by Surendar - START

      WHEN 'PAYM_ADV_EDI'.
        addemailrecipients = 'YES'.

*      Included this as new request by Surendar  - END


      WHEN 'PURCHASE_ORDER'.
        addemailrecipients = 'NO'.

        DATA: ls_additional_email_recipient LIKE LINE OF additionalemailrecipients.
*Using CDS view I_PURCHASEORDERAPI01, getting LV_PURCHASER by passing value of PURCHASERORDER as OUTPUTCONTROLAPPLICATIONOBJECT.
        SELECT SINGLE createdbyuser FROM i_purchaseorderapi01 WITH PRIVILEGED ACCESS
                                                              WHERE purchaseorder = @outputcontrolapplicationobject INTO @DATA(lv_purchaser).
*Check if Purchaser id is determined
        IF lv_purchaser IS NOT INITIAL.
*Based on purchaser id, fetching purchaser details such as indicator which say whether user is technical user or not and email address.
          SELECT SINGLE istechnicaluser, \_addrcurdefaultemailaddress-emailaddress FROM i_user WHERE userid = @lv_purchaser
                                                                                   INTO @DATA(lv_user_details) PRIVILEGED ACCESS.
*Check below condition before assigning email address as addintionalemailrecipients
*   1. If user is technical user, don't send email output
*   2. If email address is not determined, then will not assign recipient details
          IF NOT lv_user_details-istechnicaluser IS INITIAL.
* technical user don't send email
            EXIT.
          ELSEIF lv_user_details-emailaddress IS NOT INITIAL.
            ls_additional_email_recipient-outputrequestitememailrole = 'TO'.
            ls_additional_email_recipient-outputrequestitememailaddress = lv_user_details-emailaddress.
            APPEND ls_additional_email_recipient TO additionalemailrecipients.
          ENDIF.
        ENDIF.

      WHEN 'SALES_DOCUMENT'.
        addemailrecipients = 'YES'.

      WHEN OTHERS.
        addemailrecipients = 'YES'.
    ENDCASE.


    IF ( addemailrecipients ) = 'YES'.
      "select email address of created user = logon user
      SELECT SINGLE defaultemailaddress FROM i_businessuservh WITH PRIVILEGED ACCESS
         WHERE userid = @lv_system_techname                     "'CB9980000020'
             INTO @email_address.

      "Add Additional Email Recipients for Billing Documents by Using Custom Logic
      "DATA: ls_additional_email_recipient LIKE LINE OF additionalemailrecipients.

      ls_additional_email_recipient-outputrequestitememailrole = 'TO'.

      IF email_address IS INITIAL.
*  ls_additional_email_recipient-outputrequestitememailaddress = 'samraj.kumaravel@erplogic.com'.
*      APPEND ls_additional_email_recipient TO additionalemailrecipients.
      ELSE.
        ls_additional_email_recipient-outputrequestitememailaddress = email_address.
        APPEND ls_additional_email_recipient TO additionalemailrecipients.
      ENDIF.

    ENDIF.


  ENDMETHOD.
ENDCLASS.

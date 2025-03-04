//@AbapCatalog.sqlViewName: 'ZINVSALESCOM'
@EndUserText.label: 'Consumption View for ZINVSALESCOM'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_INVSALESCOM
  as projection on ZI_INVSALESCOM
{
  key ItemUUID,
      RunId,
      BillingDocumentId,
      ProcessingStatus, //added- renuga

      //       @ObjectModel.text.element:  [ 'clrstatus' ]
      //      ClearingStatus,
      //      _Clrstatus.InvoiceClearingStatus   as clrstatus,

      SalesMan,
      @ObjectModel.text.element:  [ 'cusgrpname' ]
      SalesManName,
      _customergrp.CustomerGroupName   as cusgrpname,

      SalesOrganisation,
      @ObjectModel.text.element:  [ 'salesorganame' ]
      SalesOrgName,
      _salesorg.SalesOrganizationName  as salesorganame,

      CommissionPayFromdate,
      CommissionPayTodate,
      CommissionStatus,

      CustomerId,
      //@ObjectModel.text.element:  [ 'cusname' ]
      CustomerName,
     // _customer.CustomerName           as cusname,

      ItemID,
      MaterialId,
      @ObjectModel.text.element:  [ 'materialdes' ]
      MaterialDesc,
      _MaterialText.ProductDescription as materialdes,

      SalesorderId,
      SalesorderDate,

      @Semantics.quantity.unitOfMeasure:'QuantityUom'
      Quantity,
      QuantityUom,

      @Semantics.amount.currencyCode: 'UnitpriceCurrencycode'
      Unitprice,
      @Semantics.amount.currencyCode: 'CommamtCurrencycode'
      Commissionamount,
      @Semantics.amount.currencyCode: 'AmountCurrencycode'
      Amount,
      @Semantics.amount.currencyCode: 'OtherChargesCurrencycode'
      OtherCharges,
      @Semantics.amount.currencyCode: 'DiscountAmountCurrencycode'
      DiscountAmount,
      @Semantics.amount.currencyCode: 'NetAmtCalcCurrencycode'
      NetAmountCalc,

      @Semantics.currencyCode: true
      UnitpriceCurrencycode,
      AmountCurrencycode,       
      CommamtCurrencycode,
      OtherChargesCurrencycode,
      DiscountAmountCurrencycode,
      NetAmtCalcCurrencycode,

      BillingdocumentDate,
      CommissionRate,
      GPPercentage,
      ClearingStatus,
      LastUpdateAt,
      CommissionPaydate,
      InvoiceClearingDate,
      AccountingDocument,


      /* Associations */
      _salescomt : redirected to parent ZC_SALESCOMT
}

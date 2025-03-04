@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for ZINVSALESCOM'
@Search.searchable: true
@AbapCatalog.viewEnhancementCategory: [ #NONE ]
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.resultSet.sizeCategory: #XS



define view entity ZI_INVSALESCOM
  as select from zinvsalescom
  association        to parent ZI_SALESCOMT     as _salescomt    on  $projection.RunId = _salescomt.RunId

  association [1..1] to I_Customer              as _customer     on  $projection.CustomerId = _customer.Customer
                                                                 and _customer.Language     = 'E'

  association [1..1] to I_CustomerGroupText     as _customergrp  on  $projection.SalesMan  = _customergrp.CustomerGroup
                                                                 and _customergrp.Language = 'E'

  association [1..1] to I_SalesOrganizationText as _salesorg     on  $projection.SalesOrganisation = _salesorg.SalesOrganization
                                                                 and _salesorg.Language            = 'E'

  association [1..1] to I_ProductDescription    as _MaterialText on  $projection.MaterialId = _MaterialText.Product
                                                                 and _MaterialText.Language = 'E'

  //  association [1..1] to I_BillingDocument as _Clrstatus         on  $projection.ClearingStatus = _Clrstatus.InvoiceClearingStatus



{
  key  zinvsalescom.item_uuid                  as ItemUUID,
       zinvsalescom.run_id                     as RunId,
       @Search.defaultSearchElement: true
       zinvsalescom.billingdocument_id         as BillingDocumentId,
       @Search.defaultSearchElement: true

       zinvsalescom.processing_status          as ProcessingStatus, //added- renuga
       @Search.defaultSearchElement: true

       zinvsalescom.invoice_clearing_status    as ClearingStatus,

       @Search.defaultSearchElement: true
       zinvsalescom.sales_man                  as SalesMan,

       @Search.defaultSearchElement: true
       zinvsalescom.sales_man_name             as SalesManName,
       @Search.defaultSearchElement: true

       zinvsalescom.sales_organisation         as SalesOrganisation,
       @Search.defaultSearchElement: true

       zinvsalescom.sales_org_name             as SalesOrgName,
       @Search.defaultSearchElement: true

       zinvsalescom.commission_pay_fromdate    as CommissionPayFromdate,
       @Search.defaultSearchElement: true

       zinvsalescom.commission_pay_todate      as CommissionPayTodate,
       @Search.defaultSearchElement: true

       zinvsalescom.commission_status          as CommissionStatus,
       @Search.defaultSearchElement: true

       zinvsalescom.customer_id                as CustomerId,
       @Search.defaultSearchElement: true

       zinvsalescom.customer_name              as CustomerName,
       @Search.defaultSearchElement: true

       zinvsalescom.item_id                    as ItemID,
       @Search.defaultSearchElement: true

       zinvsalescom.material_id                as MaterialId,
       @Search.defaultSearchElement: true

       zinvsalescom.material_desc              as MaterialDesc,

       @Search.defaultSearchElement: true

       zinvsalescom.salesorder_id              as SalesorderId,
       @Search.defaultSearchElement: true

       zinvsalescom.salesorder_date            as SalesorderDate,
       @Semantics.quantity.unitOfMeasure:'QuantityUom'
       zinvsalescom.quantity                   as Quantity,
       zinvsalescom.quantity_uom               as QuantityUom,
       @Semantics.amount.currencyCode: 'UnitpriceCurrencycode'
       zinvsalescom.unitprice                  as Unitprice,
       zinvsalescom.unitprice_currencycode     as UnitpriceCurrencycode,
       @Semantics.amount.currencyCode: 'AmountCurrencycode'
       zinvsalescom.amount                     as Amount,
       zinvsalescom.amount_currencycode        as AmountCurrencycode,
       @Search.defaultSearchElement: true

       zinvsalescom.billingdocument_date       as BillingdocumentDate,
       zinvsalescom.commission_rate            as CommissionRate,
       zinvsalescom.gp_percentage              as GPPercentage,
       @Semantics.amount.currencyCode: 'CommamtCurrencycode'
       zinvsalescom.commission_amount          as Commissionamount,
       @Search.defaultSearchElement: true
       zinvsalescom.commamount_currencycode    as CommamtCurrencycode,

       zinvsalescom.last_updated_at            as LastUpdateAt,

       zinvsalescom.commission_pay_date        as CommissionPaydate,
       zinvsalescom.invoice_clearing_date      as InvoiceClearingDate,

       @Semantics.amount.currencyCode: 'OtherChargesCurrencycode'
       zinvsalescom.other_charges              as OtherCharges,
       zinvsalescom.othercharges_currencycode  as OtherChargesCurrencycode,
       @Semantics.amount.currencyCode: 'DiscountAmountCurrencycode'
       zinvsalescom.discount_amount            as DiscountAmount,
       zinvsalescom.discamount_currencycode    as DiscountAmountCurrencycode,
       @Semantics.amount.currencyCode: 'NetAmtCalcCurrencycode'
       zinvsalescom.net_amount_calc            as NetAmountCalc,
       zinvsalescom.netamountcalc_currencycode as NetAmtCalcCurrencycode,
       zinvsalescom.z_accountingdocument as AccountingDocument,

       _salescomt, // Make association public
       @Search.defaultSearchElement: true
       _customer,
       @Search.defaultSearchElement: true
       _customergrp,
       @Search.defaultSearchElement: true
       _salesorg,
       @Search.defaultSearchElement: true
       _MaterialText
       //       _Clrstatus
}

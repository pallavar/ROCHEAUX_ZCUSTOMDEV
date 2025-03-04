@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for ZSALESCOMT'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//@ObjectModel.resultSet.sizeCategory: #XS --- this is dropdown annotation
//@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_DOMAIN_FIX_VALUES'
@Search.searchable: true



/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZI_SALESCOMT
  as select from zsalescomt
  composition [0..*] of ZI_INVSALESCOM as _InvSalescom
  association [1..1] to I_BusinessUserBasic as _ChangedUser     on  $projection.LastChangedBy = _ChangedUser.UserID                                                                    
{
      @Search.defaultSearchElement: true
  key run_id                  as RunId,

      @Search.defaultSearchElement: true
      processing_status       as ProcessingStatus,

      invoice_pay_status      as InvoicePaymentStatus,
      commission_pay_status   as CommissionPaymentStatus,

      commission_pay_fromdate as CommissionFromDate,
      commission_pay_todate   as CommissionToDate,

      run_date                as RunDate,

      @Semantics.user.createdBy: true
      created_by              as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      created_at              as CreatedAt,

      @Semantics.user.lastChangedBy: true
      last_changed_by         as LastChangedBy,

      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at         as LastChangedAt,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locl_last_changed_at    as LoclLastChangedAt,
      @Search.defaultSearchElement: true
      _InvSalescom,
      _ChangedUser

}

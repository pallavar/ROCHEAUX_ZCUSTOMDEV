@EndUserText.label: 'Consumption View for ZSALESCOMT'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

@Search.searchable: true
define root view entity ZC_SALESCOMT
  provider contract transactional_query
  as projection on ZI_SALESCOMT
{

      _InvSalescom : redirected to composition child ZC_INVSALESCOM,
      _ChangedUser,
  key RunId,
      ProcessingStatus,
      InvoicePaymentStatus,
      CommissionPaymentStatus,
      CommissionFromDate,
      CommissionToDate,
      RunDate,
      CreatedBy,
      CreatedAt,
      LastChangedBy,

      @ObjectModel.text.element:  [ 'changedusername' ]
      _ChangedUser.PersonFullName as changedusername,

      LastChangedAt,
      LoclLastChangedAt
}

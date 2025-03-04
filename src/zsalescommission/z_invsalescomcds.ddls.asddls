@AbapCatalog.sqlViewName: 'ZINVSALESCOMCDS'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SalesCom Item CDS View'
@Metadata.ignorePropagatedAnnotations: true
@Analytics.query: true
@Analytics.dataCategory: #CUBE
//@OData.publish: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@Search.searchable: true

define view Z_INVSalesCOMCDS as select from ZI_INVSALESCOM
{
key ItemUUID,
RunId,
BillingDocumentId,
ProcessingStatus,
ClearingStatus,
SalesMan,
SalesManName

/* Associations */
}

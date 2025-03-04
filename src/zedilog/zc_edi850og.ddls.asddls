@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for EDI 850 Log'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define root view entity ZC_EDI850OG provider contract transactional_query 
as projection on ZI_EDI850LOG
{
    key PoId,
    Edi850Status,
    Status,
    StatusDesc,
    OverallStatusCriticality,
    LocalCreatedBy,
    LocalCreatedAt,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt
}

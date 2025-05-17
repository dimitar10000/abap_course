@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Items view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_RAP_Item
as select from zitem_dkal
association to parent ZI_RAP_Order as _Order
on $projection.OrderUuid = _Order.OrderUuid
{
    key item_uuid as ItemUuid,
    key order_uuid as OrderUuid,
    name as Name,
    price as Price,
    currency as Currency,
    quantity as Quantity,
    
    _Order
}

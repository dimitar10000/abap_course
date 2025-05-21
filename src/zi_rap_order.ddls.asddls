@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Orders view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_RAP_Order
as select from zorder_dkal
composition [1..*] of ZI_RAP_Item as _Item
association [1..*] to /DMO/I_Customer as _Customer
on $projection.Customer = _Customer.CustomerID
association [0..*] to I_Country as _Country
on $projection.DeliveryCountry = _Country.Country
{
    key order_uuid as OrderUuid,
    order_id as OrderId,
    name as Name,
    status as Status,
    customer as Customer,
    creation_date as CreationDate,
    cancellation_date as CancellationDate,
    completion_date as CompletionDate,
    delivery_country as DeliveryCountry,
    total_price as TotalPrice,
    currency as Currency,
    last_changed as LastChanged,
    
    
    _Customer,
    _Country,
    _Item
}

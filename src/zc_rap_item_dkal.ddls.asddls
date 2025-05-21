@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Items projection view'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_RAP_Item_Dkal as projection on ZI_RAP_Item
{
    @Search.defaultSearchElement: true
    key ItemUuid,
    @Search.defaultSearchElement: true
    key OrderUuid,
    Name,
    Price,
    Currency,
    Quantity,
    LastChanged,
    /* Associations */
    _Order: redirected to parent ZC_RAP_Order_Dkal
}

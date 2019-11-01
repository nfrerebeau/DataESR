\donttest{
## Warning: an active internet connection is needed
item <- WikidataR::get_item("Q13342")

## Name
get_item_label(item)
## Alias
get_item_alias(item)
## Foundation date
get_statement_year(item, "P571")
## Formerly known as
get_statement_list(item, "P1365")
## Predecessor of
get_statement_qualifier(item, "P1365", "P585")
}

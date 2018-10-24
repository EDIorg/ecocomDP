# Datetime requirements

Datetime fields of ecocomDP tables must follow the ISO 8601 datetime standard, specifically:

__YYYY-MM-DDThh:mm:ss__

The degree to which this format is followed depends on the precision of the input datetime. 
* Example: If only a date is input, then the format doesn't include the time field, i.e. YYYY-MM-DD
* Example: If only a date and hour is input, then the format includes time but not minutes and seconds, i.e. YYYY-MM-DDThh
* Example: If the full date time is input, then the format should be YYYY-MM-DDThh::mm:ss

Use the `datetime_to_iso8601` function from the [EDIutils R package](https://github.com/EDIorg/EDI-utilities/tree/master/EDIutils) to help convert your data to the standard format. Conversely, use the `iso8601_to_datetime` function of the same package to read your ISO8601 formatted datetime strings into POSIXct and POSIXt data in R.

_Where possible: Indclude timezone offset to UTC. All data are assumed to be in local time otherwise._

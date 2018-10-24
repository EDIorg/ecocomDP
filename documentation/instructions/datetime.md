# Datetime requirements

Datetime fields of ecocomDP tables must have the standard format:

__YYYY-MM-DDThh:mm:ss__

The `datetime_to_iso8601` function of the [EDIutils R package](https://github.com/EDIorg/EDI-utilities/tree/master/EDIutils) will help convert your datetimes to this format.




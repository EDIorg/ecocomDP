# Recommendations for merging tables in ecocomDP

Introduction
---
The core table is the *observation* table. Other tables can be combined with it by merging on identifiers.

Observation - observation_ancillary
---
1. Run the QC function to confirm that the event_ids in observation and observation_ancillary agree, afe to join them.
2. The simplest join command could cause observation rows to be duplicated. Below are our recommendations for how to avoid this:
    * if you know you want only one variable_name from observation_ancillary, then filter observation_ancillary for that variable_name, and then join those records to observation on event_id.
    * if you want to explore all the ancillary data along with the observations, then use the R-tidyverse fuction "spread()", then paste the results onto the observation table matching on event_id. Now the observation table is wide, with a column added for each variable in the ancillary observation for that event, but there is only one row per observation.
    
<code>
    spread(observation_ancillary, variable_name, value) 
</code>

 



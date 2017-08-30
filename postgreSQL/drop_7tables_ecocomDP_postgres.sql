/* mob, 2017-08-02 */

/* short script to remove the tables prior to reload. 
generally, this schema is for demonstration purposes and the tables are empty. 
If they were populated, of course you would archive content before you ran this script

deletion order recommended by schemaSpy.
*/

DROP TABLE "ecocom_dp".observation;
DROP TABLE "ecocom_dp".taxon_ancillary;
DROP TABLE "ecocom_dp".sampling_location_ancillary;
DROP TABLE "ecocom_dp".sampling_location;
DROP TABLE "ecocom_dp".event;
DROP TABLE "ecocom_dp".dataset_summary;
DROP TABLE "ecocom_dp".taxon;



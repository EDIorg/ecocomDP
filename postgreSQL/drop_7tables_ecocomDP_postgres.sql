/* mob, 2017-08-02 */

/* short script to remove the tables prior to reload. 
generally, this schema is for demonstration purposes and the tables are empty. 
If they were populated, of course you would archive content before you ran this script

deletion order recommended by schemaSpy.
*/

DROP TABLE "ecocom_dp".observation_ancillary;
DROP TABLE "ecocom_dp".taxon_ancillary;
DROP TABLE "ecocom_dp".location_ancillary;
DROP TABLE "ecocom_dp".observation;
DROP TABLE "ecocom_dp".location;
DROP TABLE "ecocom_dp".dataset_summary;
DROP TABLE "ecocom_dp".taxon;

/* this table is a stub, that will not be included in csv implementations. 
It is necessary for sql implementations though, to connect obs and obs_ancillary. */
DROP TABLE "ecocom_dp".event;

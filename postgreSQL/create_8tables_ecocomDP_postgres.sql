-- implementation of ecocom design pattern in postgres. 

-- Dumped from database version 9.2.19
-- Dumped by pg_dump version 9.2.2
-- Started on 2016-12-16 15:28:07 PST
/*
created original from pgdump, edited. see personal notes.
*/

/* this version for 8 tables (Sept 2017):
main table is observation
linked tables for location, taxon, event. 
ancillary tables for each of location, taxon, event
summary table, one line table, could be exported to metadata
*/

/*
table population order - suggested (ie, parents first).
1. sampling_location
2. taxon
3. event
4. observation (refs sampling_location, taxon, event)
5. sampling_location_ancillary (refs sampling_location)
6. taxon_ancillary (refs taxon)
7. event_ancillary (refs event)
8. dataset_summary (refs observation)


*/
/*
SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = "ecocom_dp", pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;
*/

/*
-- Create 8 tables:
*/


-- DROP TABLE "ecocom_dp".sampling_location;
CREATE TABLE "ecocom_dp".sampling_location (
	sampling_location_id character varying(100) NOT NULL,
	sampling_location_name character varying(500),
	latitude float,
	longitude float,
	elevation float, -- relative to sealevel
	parent_sampling_location_id character varying(100) 
);

-- DROP TABLE "ecocom_dp".taxon;
CREATE TABLE "ecocom_dp".taxon (
	taxon_id character varying(100) NOT NULL,
	taxon_rank character varying(200),
	taxon_name character varying(200) NOT NULL,
	authority_system character varying(200),
	authority_taxon_id character varying(200)
);

-- DROP TABLE "ecocom_dp".event;
CREATE TABLE "ecocom_dp".event (
	event_id character varying(100) NOT NULL,
	event_name character varying(200) NOT NULL
);

-- DROP TABLE "ecocom_dp".observation;
CREATE TABLE "ecocom_dp".observation (
	observation_id character varying(100) NOT NULL,
	event_id character varying(100) NOT NULL,
	package_id character varying(100) NOT NULL,
	sampling_location_id character varying(100) NOT NULL,
	observation_datetime timestamp without time zone,
	taxon_id character varying(100) NOT NULL,
	variable_name character varying(200) NOT NULL,
	value character varying(200) NOT NULL,
	unit character varying(200) NOT NULL
);

-- DROP TABLE "ecocom_dp".sampling_location_ancillary;
CREATE TABLE "ecocom_dp".sampling_location_ancillary (
    sampling_location_ancillary_id character varying(100) NOT NULL,
    sampling_location_id character varying(100) NOT NULL,
    datetime timestamp without time zone NOT NULL, -- e.g. experimental treatment
    variable_name  character varying(200) NOT NULL,
    value  character varying(200) NOT NULL,
    unit  character varying(200)
);

-- DROP TABLE "ecocom_dp".taxon_ancillary;
CREATE TABLE "ecocom_dp".taxon_ancillary (
    taxon_ancillary_id character varying(100) NOT NULL,
	  taxon_id character varying(100) NOT NULL,
	  datetime timestamp without time zone NOT NULL,  
    variable_name  character varying(200) NOT NULL,
    value character varying(200) NOT NULL,
	  author character varying(200)
);

-- DROP TABLE "ecocom_dp".event_ancillary;
CREATE TABLE "ecocom_dp".event_ancillary (
    event_ancillary_id character varying(100) NOT NULL,
	  event_id character varying(100) NOT NULL,
    variable_name  character varying(200) NOT NULL,
    value character varying(200) NOT NULL,
    unit  character varying(200)
	  );
	  

-- DROP TABLE "ecocom_dp".dataset_summary;
CREATE TABLE "ecocom_dp".dataset_summary (
	package_id character varying(100) NOT NULL,
	original_package_id character varying(200),
	length_of_survey_years integer,
	number_of_years_sampled integer,
	std_dev_interval_betw_years float,
	max_num_taxa integer, 
	geo_extent_bounding_box_m2 float
);


ALTER TABLE "ecocom_dp".observation OWNER TO mob;
COMMENT ON TABLE "ecocom_dp".observation IS 'table holds all the primary obs, with links to taxa, locations, event, summary';

ALTER TABLE "ecocom_dp".sampling_location OWNER TO mob;
COMMENT ON TABLE "ecocom_dp".sampling_location IS 'self-referencing; parent of a loc can be another loc';

ALTER TABLE "ecocom_dp".taxon OWNER TO mob;

ALTER TABLE "ecocom_dp".event OWNER TO mob;
COMMENT ON TABLE "ecocom_dp".event IS 'holds info about a sampling event, eg, conditions, weather, observers, etc';

ALTER TABLE "ecocom_dp".taxon_ancillary OWNER TO mob;

ALTER TABLE "ecocom_dp".sampling_location_ancillary OWNER TO mob;

ALTER TABLE "ecocom_dp".event_ancillary OWNER TO mob;

ALTER TABLE "ecocom_dp".dataset_summary OWNER TO mob;



/* add PK constraints */
ALTER TABLE ONLY "ecocom_dp".observation
    ADD CONSTRAINT observation_pk PRIMARY KEY (observation_id);

ALTER TABLE ONLY "ecocom_dp".sampling_location
    ADD CONSTRAINT sampling_location_pk PRIMARY KEY (sampling_location_id);

ALTER TABLE ONLY "ecocom_dp".taxon
    ADD CONSTRAINT taxon_pk PRIMARY KEY (taxon_id);

ALTER TABLE ONLY "ecocom_dp".event
    ADD CONSTRAINT event_pk PRIMARY KEY (event_id);

ALTER  TABLE ONLY "ecocom_dp".sampling_location_ancillary
    ADD CONSTRAINT sampling_location_ancillary_pk PRIMARY KEY (sampling_location_ancillary_id);

ALTER TABLE ONLY "ecocom_dp".taxon_ancillary
    ADD CONSTRAINT taxon_ancillary_pk PRIMARY KEY (taxon_ancillary_id);
    
ALTER TABLE ONLY "ecocom_dp".event_ancillary
    ADD CONSTRAINT event_ancillary_pk PRIMARY KEY (event_ancillary_id);

ALTER TABLE ONLY "ecocom_dp".dataset_summary
    ADD CONSTRAINT dataset_summary_pk PRIMARY KEY (package_id);



/* add FK constraints
*/
-- observation refs sampling_loc, taxon, event
ALTER TABLE ONLY "ecocom_dp".observation
    ADD CONSTRAINT observation_sampling_location_fk FOREIGN KEY (sampling_location_id) REFERENCES "ecocom_dp".sampling_location (sampling_location_id) MATCH SIMPLE     
    ON UPDATE CASCADE;
ALTER TABLE ONLY "ecocom_dp".observation
    ADD CONSTRAINT observation_taxon_fk FOREIGN KEY (taxon_id) REFERENCES "ecocom_dp".taxon (taxon_id) MATCH SIMPLE     
    ON UPDATE CASCADE;
ALTER TABLE ONLY "ecocom_dp".observation
    ADD CONSTRAINT observation_event_fk FOREIGN KEY (event_id) REFERENCES "ecocom_dp".event (event_id) MATCH SIMPLE
    ON UPDATE CASCADE;
ALTER TABLE ONLY "ecocom_dp".observation
    ADD CONSTRAINT observation_package_fk FOREIGN KEY (package_id) REFERENCES "ecocom_dp".dataset_summary (package_id) MATCH SIMPLE
    ON UPDATE CASCADE;


--sampling_location (self-referencing)
ALTER TABLE ONLY "ecocom_dp".sampling_location
    ADD CONSTRAINT parent_sampling_location_fk FOREIGN KEY (parent_sampling_location_id) REFERENCES "ecocom_dp".sampling_location (sampling_location_id) MATCH SIMPLE     
    ON UPDATE CASCADE;  

-- sampling_location_ancillary refs sampling_loc
ALTER TABLE ONLY "ecocom_dp".sampling_location_ancillary
    ADD CONSTRAINT sampling_location_ancillary_fk FOREIGN KEY (sampling_location_id) REFERENCES "ecocom_dp".sampling_location (sampling_location_id) MATCH SIMPLE     
    ON UPDATE CASCADE;
  
-- taxon_ancillary refs taxon
ALTER TABLE ONLY "ecocom_dp".taxon_ancillary
    ADD CONSTRAINT taxon_ancillary_fk FOREIGN KEY (taxon_id) REFERENCES "ecocom_dp".taxon (taxon_id) MATCH SIMPLE     
    ON UPDATE CASCADE;

-- event_ancillary refs event
ALTER TABLE ONLY "ecocom_dp".event_ancillary
    ADD CONSTRAINT event_ancillary_fk FOREIGN KEY (event_id) REFERENCES "ecocom_dp".event (event_id) MATCH SIMPLE     
    ON UPDATE CASCADE;



/*
set perms
*/

REVOKE ALL ON SCHEMA "ecocom_dp" FROM PUBLIC;
GRANT ALL ON SCHEMA "ecocom_dp" TO mob;
GRANT USAGE ON SCHEMA "ecocom_dp" TO read_only_user;

GRANT SELECT ON TABLE "ecocom_dp".observation TO read_only_user;
GRANT SELECT ON TABLE "ecocom_dp".sampling_location TO read_only_user;
GRANT SELECT ON TABLE "ecocom_dp".taxon TO read_only_user;
GRANT SELECT ON TABLE "ecocom_dp".sampling_location_ancillary TO read_only_user;
GRANT SELECT ON TABLE "ecocom_dp".taxon_ancillary TO read_only_user;
GRANT SELECT ON TABLE "ecocom_dp".dataset_summary TO read_only_user;
GRANT SELECT ON TABLE "ecocom_dp".event TO read_only_user;
GRANT SELECT ON TABLE "ecocom_dp".event_ancillary TO read_only_user;

GRANT ALL ON TABLE "ecocom_dp".observation TO mob;
GRANT ALL ON TABLE "ecocom_dp".sampling_location TO mob;
GRANT ALL ON TABLE "ecocom_dp".taxon TO mob;
GRANT ALL ON TABLE "ecocom_dp".sampling_location_ancillary TO mob;
GRANT ALL ON TABLE "ecocom_dp".taxon_ancillary TO mob;
GRANT ALL ON TABLE "ecocom_dp".dataset_summary TO mob;
GRANT ALL ON TABLE "ecocom_dp".event TO mob;
GRANT ALL ON TABLE "ecocom_dp".event_ancillary TO mob;

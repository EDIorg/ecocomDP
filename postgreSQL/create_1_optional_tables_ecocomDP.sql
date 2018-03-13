-- copied from ~/BON/db_bon_data_packages/schema_mini_metabase/create_7_tables_schema.sql  
--
-- PostgreSQL database dump
--

-- Dumped from database version 9.2.19
-- Dumped by pg_dump version 9.2.2
-- Started on 2016-12-16 15:28:07 PST
/*
created original from pgdump, edited. see personal notes.
*/

/*
table population order - suggested (ie, parents first).
1. location
2. taxon
3. observation (refs location, taxon)
4. location_ancillary (refs location)
5. taxon_ancillary (refs taxon)
6. observation_ancillary (refs observation)
7. dataset_summary (refs observation)

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
-- iAdd this to the Create 7 tables. the 8th table.:
*/



/* The mappings table does not have a FK constraint, because in CSV implementations
it can be linked to any of 4 different tables, in a many:many. so we would have needed 
4 stubs. so in postgres, no linkages are shown. 
*/
--DROP TABLE "ecocom__dp.variable_mappings;
CREATE TABLE "ecocom_dp".variable_mappings (
    table_name character varying(200) NOT NULL,
    variable_name character varying(100) NOT NULL,
    mapped_system  character varying(200),
    mapped_id  character varying(200),
    mapped_label  character varying(200)


);

ALTER TABLE "ecocom_dp".variable_mappings OWNER TO mob;
COMMENT ON TABLE "ecocom_dp".variable_mappings IS 'table holds definitions of variable_names that were used in any of these 4 tables: observation, observation_ancillary, taxon_ancillary, location_ancillary.';

/* add PK constraints */
-- CONSIDER an ID like the other tables for the PK.
ALTER TABLE ONLY "ecocom_dp".variable_mappings
    ADD CONSTRAINT observation_pk PRIMARY KEY (table_name, variable_name, mapped_system, mapped_id);

/* uniq constraints */
ALTER TABLE ONLY "ecocom_dp".variable_mappings
   ADD CONSTRAINT taxon_ancillary_uniq UNIQUE (table_name, variable_name, mapped_system, mapped_id);

/*
set perms
*/

GRANT SELECT ON TABLE "ecocom_dp".variable_mappings TO read_only_user;
GRANT ALL ON TABLE "ecocom_dp".variable_mappings TO mob;

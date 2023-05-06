/* Wood Supply Project */
/*SQL Script and Instructions for Loading Mill Index Data*/

/*1. Log into fiadb01p using [anl_nrs_analysis] profile*/

/*2. Create new table*/
/*drop table MILL_INDEX;*/
create table MILL_INDEX 
(PLT_CN varchar2(15), 
PULP_INDEX NUMBER(12,4), 
PULP_INDEX_100 NUMBER(12,4), 
SAW_INDEX NUMBER(12,4), 
SAW_INDEX_500 NUMBER(12,4));

/*3. Load data*/
/*Load data using Tools > Text Importer*/
/*T:\FS\RD\FIA\NWOS\PROJECTS\WOOD_SUPPLY\DATA\MILLS\PLOT_MILL_INDICES.csv*/

/*4. Grant access*/
grant select on MILL_INDEX to bbutler01;
select * from MILL_INDEX;

/*5. Log in using default profile*/

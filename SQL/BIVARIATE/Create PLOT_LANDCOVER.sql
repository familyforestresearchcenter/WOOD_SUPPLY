/* Wood Supply Project */
/*SQL Script and Instructions for Loading Land Cover Data*/

/*1. Log into fiadb01p using [anl_nrs_analysis] profile*/

/*2. Create new table*/
/*drop table PLOT_LANDCOVER;*/
create table PLOT_LANDCOVER
(PLT_CN	varchar2(15), STATECD	NUMBER(2,0), 
PLT_CN_GIS varchar2(32), 
STATECD_GIS NUMBER(2,0), ORIG_FID varchar2(15), 
AG_PROP NUMBER(12,8),	URBAN_PROP NUMBER(12,8),	
FOREST_PROP  NUMBER(12,8));	


/*3. Load data*/
/*Load data using Tools > Text Importer*/
/*T:\FS\RD\FIA\NWOS\PROJECTS\WOOD_SUPPLY\DATA\LAND_COVER\PLOTS_LANDCOVER.csv*/

/*4. Grant access*/
grant select on PLOT_LANDCOVER to bbutler01;
select * from PLOT_LANDCOVER;

/*5. Log in using default profile*/
